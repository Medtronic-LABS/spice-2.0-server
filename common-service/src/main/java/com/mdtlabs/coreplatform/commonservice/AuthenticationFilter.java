package com.mdtlabs.coreplatform.commonservice;

import com.mdtlabs.coreplatform.commonservice.apiinterface.AuthServiceApiInterface;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.CustomDateSerializer;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ContextsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;
import com.mdtlabs.coreplatform.commonservice.common.repository.ApiRolePermissionRepository;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.util.ContentCachingRequestWrapper;
import org.springframework.web.util.ContentCachingResponseWrapper;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

/**
 * <p>
 * Used to do internal filter on token validation
 * </p>
 *
 * @author Nandhakumar created on Feb 05, 2024
 */
@Component
public class AuthenticationFilter extends OncePerRequestFilter {

    private final AuthServiceApiInterface authServiceApiInterface;

    private final RedisTemplate<String, Map<String, Map<String, List<String>>>> redisApiPermissionTemplate;

    private final RedisTemplate<String, Map<String, List<String>>> redisOpenUriTemplate;

    private final ApiRolePermissionRepository apiRolePermissionRepository;

    @Setter
    @Getter
    private String serviceName = Constants.AUTH_SERVICE;

    @Value("${app.domain-name:}")
    private String domainName;

    @Value("${app.environment:}")
    private String environment;

    private static final List<MediaType> VISIBLE_TYPES = Arrays.asList(MediaType.valueOf(Constants.TEXT),
            MediaType.APPLICATION_FORM_URLENCODED, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML,
            MediaType.valueOf(Constants.APPLICATIONJSON), MediaType.valueOf(Constants.APPLICATION_XML), MediaType.MULTIPART_FORM_DATA);

    org.apache.logging.log4j.Logger log = LogManager.getLogger(AuthenticationFilter.class);

    public AuthenticationFilter(AuthServiceApiInterface authServiceApiInterface, RedisTemplate<String, Map<String, Map<String, List<String>>>> redisApiPermissionTemplate, RedisTemplate<String, Map<String, List<String>>> redisOpenUriTemplate, ApiRolePermissionRepository apiRolePermissionRepository) {
        this.authServiceApiInterface = authServiceApiInterface;
        this.redisApiPermissionTemplate = redisApiPermissionTemplate;
        this.redisOpenUriTemplate = redisOpenUriTemplate;
        this.apiRolePermissionRepository = apiRolePermissionRepository;
    }

    /**
     * <p>
     * This is a Java function that filters incoming HTTP requests and checks for valid authentication
     * tokens before allowing access to protected resources.
     * </p>
     *
     * @param request     - An object representing the HTTP request made by the client.
     * @param response    - The HttpServletResponse object represents the response that will be sent back to
     *                    the client after the request has been processed
     * @param filterChain - The filterChain parameter is an object that represents the chain of filters
     *                    that will be applied to the request and response
     */
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        if (isOpenUri(request)) {
            UserSelectedTenantContextHolder.set(Constants.LONG_ZERO);
            UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken(null, null, null);
            SecurityContextHolder.getContext().setAuthentication(auth);
            filterChain.doFilter(request, response);
        } else {
            String token;
            String client = request.getHeader(Constants.HEADER_CLIENT);
            ResponseCookie cookie = null;
            String cookieString = null;

            if (Constants.CLIENT_WEB.equals(client) && !Objects.isNull(request.getCookies()) && Constants.ZERO != request.getCookies().length) {
                cookie = getValidCookie(request);
                cookieString = cookie.toString();
                token = new String(Base64.getDecoder().decode(cookie.getValue()));
            } else {
                token = request.getHeader(HttpHeaders.AUTHORIZATION);
            }
            if (StringUtils.isBlank(token) || !token.startsWith(Constants.BEARER)) {
                throw new Validation(20001);
            }
            String tenantId = request.getHeader(Constants.HEADER_TENANT_ID);
            try {
                ContextsDTO userContexts = authServiceApiInterface.validateToken(token, client, cookieString);
                setAuthenticationInSecurityContext(userContexts, tenantId);
                boolean isExist = isValidRoleExist(userContexts.getUserDetail(), request);
                if (!isExist) {
                    throw new Validation(20001);
                }
                if (Objects.nonNull(cookie)){
                    setCookie(token, response);
                }
                doLogApi(wrapRequest(request), wrapResponse(response), filterChain);
            } catch (Exception e) {
                Logger.logError(e);
                throw new Validation(20001);
            }
        }
    }

    /**
     * <p>
     * This method is used to set the expiry time of the cookie and add it to the response.
     * </p>
     *
     * @param authToken The authentication token to set in the cookie.
     * @param response The HTTP response object to which the cookie will be added.
     */
    private void setCookie(String authToken, HttpServletResponse response) {
        ResponseCookie cookie;
        if (Constants.DEV_ENVIRONMENT.equals(environment)) {
            cookie = ResponseCookie.from(Constants.AUTH_COOKIE_NAME,
                            Base64.getEncoder().encodeToString(authToken.getBytes()))
                    .sameSite(org.springframework.boot.web.server.Cookie.SameSite.NONE.attributeValue())
                    .domain(domainName)
                    .httpOnly(Boolean.TRUE).secure(Boolean.TRUE)
                    .maxAge(Duration.ofMinutes(Constants.AUTH_TOKEN_EXPIRY_MINUTES)).path(Constants.FORWARD_SLASH)
                    .build();
        } else {
            cookie = ResponseCookie.from(Constants.AUTH_COOKIE_NAME,
                            Base64.getEncoder().encodeToString(authToken.getBytes()))
                    .sameSite(org.springframework.boot.web.server.Cookie.SameSite.STRICT.attributeValue())
                    .domain(domainName)
                    .httpOnly(Boolean.TRUE).secure(Boolean.TRUE)
                    .maxAge(Duration.ofMinutes(Constants.AUTH_TOKEN_EXPIRY_MINUTES)).path(Constants.FORWARD_SLASH)
                    .build();
        }
        response.addHeader(HttpHeaders.SET_COOKIE, cookie.toString());
    }

    /**
     * <p>
     * This method is used to retrieve a valid authentication cookie from the request.
     * </p>
     *
     * @param request The HTTP request object from which the cookie will be retrieved.
     * @return Cookie The valid authentication cookie.
     * @throws Validation If the authentication cookie is not found or is invalid.
     */
    private ResponseCookie getValidCookie(HttpServletRequest request) {
        Cookie validCookie = Arrays.stream(request.getCookies())
                .filter(cookie -> Constants.AUTH_COOKIE_NAME.equals(cookie.getName()))
                .findAny().orElseThrow(() -> new Validation(20001));
        return ResponseCookie.from(validCookie.getName(), validCookie.getValue())
                .domain(validCookie.getDomain()).httpOnly(validCookie.isHttpOnly()).secure(validCookie.getSecure())
                .maxAge(validCookie.getMaxAge()).path(validCookie.getPath())
                .build();
    }

    /**
     * <p>
     * This method is used to log request and response of api.
     * </p>
     *
     * @param request     {@link ContentCachingRequestWrapper} api request
     * @param response    {@link ContentCachingResponseWrapper} api response
     * @param filterChain {@link FilterChain} the chain proceeding of flow
     * @throws ServletException - servlet exception
     * @throws IOException      - IO exception
     */
    protected void doLogApi(ContentCachingRequestWrapper request, ContentCachingResponseWrapper response, FilterChain filterChain) throws ServletException, IOException {
        try {
//            beforeRequest(request, response);
            filterChain.doFilter(request, response);
        } finally {
//            afterRequest(request, response);
            response.copyBodyToResponse();
        }
    }

    /**
     * <p>
     * This method is used to log the request details of api.
     * </p>
     *
     * @param request  {@link ContentCachingRequestWrapper} api request
     * @param response {@link ContentCachingResponseWrapper} api response
     * @throws IOException IO exception
     */
    protected void beforeRequest(ContentCachingRequestWrapper request, ContentCachingResponseWrapper response) throws IOException {
        logRequestHeader(request, request.getRemoteAddr() + Constants.LOG_PREFIX_REQUEST);
    }

    /**
     * <p>
     * This method is used to log the response details of api.
     * </p>
     * <p>
     *
     * @param request  {@link ContentCachingRequestWrapper} api request
     * @param response {@link ContentCachingResponseWrapper} api response
     */
    protected void afterRequest(ContentCachingRequestWrapper request, ContentCachingResponseWrapper response) {
        logRequestBody(request, request.getRemoteAddr() + Constants.LOG_PREFIX_REQUEST);
        logResponse(response, request.getRemoteAddr() + Constants.LOG_PREFIX_RESPONSE);
    }

    /**
     * <p>
     * This method is used to log request header details of api.
     * </p>
     *
     * @param request {@link ContentCachingRequestWrapper} api request
     * @param prefix  {@link String} prefix text in log
     * @throws IOException IO exception
     */
    private void logRequestHeader(ContentCachingRequestWrapper request, String prefix) throws IOException {
        log.info("{} {} {}", prefix, request.getMethod(), request.getRequestURI());
        byte[] content = request.getContentAsByteArray();
        String contentString = new String(content, request.getCharacterEncoding());
        Stream.of(contentString.split(Constants.SPLIT_CONTENT)).forEach(line -> log.info("{}", line));
    }

    /**
     * <p>
     * This method is used to log request body details of api.
     * </p>
     *
     * @param request {@link ContentCachingRequestWrapper} api request
     * @param prefix  {@link String} prefix text in log
     */
    private void logRequestBody(ContentCachingRequestWrapper request, String prefix) {
        byte[] content = request.getContentAsByteArray();
        if (content.length > Constants.ZERO) {
            logContent(content, request.getContentType(), request.getCharacterEncoding(), prefix);
        }
    }

    /**
     * <p>
     * This method is used to log response details of api.
     * </p>
     *
     * @param response {@link ContentCachingResponseWrapper} api request
     * @param prefix   {@link String} prefix text in log
     */
    private void logResponse(ContentCachingResponseWrapper response, String prefix) {
        int status = response.getStatus();
        log.info("{} {} {}", prefix, status, HttpStatus.valueOf(status).getReasonPhrase());
        byte[] content = response.getContentAsByteArray();
        if (content.length > Constants.ZERO) {
            logContent(content, response.getContentType(), response.getCharacterEncoding(), prefix);
        }
    }

    /**
     * <p>
     * This method is used to print log on console.
     * </p>
     *
     * @param content         {@link byte[]} log content
     * @param contentType     content type
     * @param contentEncoding content encoding type
     * @param prefix          prefix text in log
     */
    private void logContent(byte[] content, String contentType, String contentEncoding, String prefix) {
        MediaType mediaType = MediaType.valueOf(contentType);
        boolean visible = VISIBLE_TYPES.stream().anyMatch(visibleType -> visibleType.includes(mediaType));
        if (visible) {
            try {
                String contentString = new String(content, contentEncoding);
                boolean logInfo = true;
                logInfo = setLogInfo(contentString, logInfo);
                if (logInfo) {
                    Stream.of(contentString.split(Constants.SPLIT_CONTENT)).forEach(line -> log.info("{}", line));
                }
            } catch (UnsupportedEncodingException e) {
                log.info("{} [{} bytes content]", prefix, content.length);
            }
        } else {
            log.info("{} [{} bytes content]", prefix, content.length);
        }
    }

    /**
     * <p>
     * Sets log information value based on content string.
     * </p>
     *
     * @param contentString content string
     * @param logInfo       true or false
     * @return boolean  true or false
     * @throws JSONException - Json exception
     */
    private boolean setLogInfo(String contentString, boolean logInfo) throws JSONException {
        if (contentString.startsWith("{") && contentString.endsWith("}")) {
            JSONObject json = new JSONObject(contentString);
            if (json.has(Constants.ENTITY_LIST) && json.get(Constants.ENTITY_LIST) instanceof JSONArray jsonArray) {
                JSONArray entityList = jsonArray;
                if (entityList.length() > 1) {
                    logInfo = false;
                }
            }
        }
        return logInfo;
    }

    /**
     * <p>
     * This is method is used to check whether valid role exist or not.
     * </p>
     *
     * @param userDto {@link UserDTO} user DTO
     * @param request - {@link HttpServletRequest} http request data
     * @return Boolean
     */
    private boolean isValidRoleExist(UserContextDTO userDto, HttpServletRequest request) {
        Map<String, Map<String, List<String>>> apiPermissionMap = getApiPermission();
        String apiRequest = Constants.EMPTY;
        if (Constants.BOOLEAN_TRUE.equals(userDto.getIsSuperUser()) || Constants.BOOLEAN_TRUE.equals(userDto.getIsJobUser())) {
            return true;
        } else if (Objects.nonNull(apiPermissionMap.get(request.getMethod()))) {
            Set<String> requestTypeSet = apiPermissionMap.get(request.getMethod()).keySet();
            for (String requestType : requestTypeSet) {
                if (request.getRequestURI().contains(requestType)) {
                    apiRequest = requestType;
                    break;
                }
            }
            for (RoleDTO role : userDto.getRoles()) {
                if (apiPermissionMap.get(request.getMethod()).get(apiRequest).contains(role.getName())) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * <p>
     * Used to find open uri to access directly without authorization.
     * </p>
     *
     * @param request {@link HttpServletRequest}
     * @return boolean
     */
    private boolean isOpenUri(HttpServletRequest request) {
        Map<String, List<String>> openUriMap = getOpenUri();
        return Objects.isNull(openUriMap.get(request.getMethod())) ? Boolean.FALSE :
                openUriMap.get(request.getMethod()).stream().anyMatch(uri -> request.getRequestURI().contains(uri));
    }

    /**
     * <p>
     * Used to set authentication in the security context.
     * </p>
     *
     * @param userContexts  {@link ContextsDTO}
     * @param tenantId {@link String}
     */
    private static void setAuthenticationInSecurityContext(ContextsDTO userContexts, String tenantId) {
        UserContextDTO userDto = userContexts.getUserDetail();
        if (Objects.nonNull(userDto.getTimezone())) {
            CustomDateSerializer.userZoneId = userDto.getTimezone().getOffset();
        } else {
            CustomDateSerializer.userZoneId = Constants.EMPTY;
        }
        UserContextHolder.setUserDto(userDto);
        UserTenantsContextHolder.set(userDto.getOrganizationIds().stream().toList());
        if (null != userDto.getAuthorization()) {
            String username = userDto.getUsername();
            if (username != null) {
                UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken(username, null, null);
                SecurityContextHolder.getContext().setAuthentication(auth);
            }
        }
        UserSelectedTenantContextHolder.set(StringUtils.isNotBlank(tenantId) && tenantId.matches("\\d+") ? Long.parseLong(tenantId) : Constants.LONG_ZERO);
    }

    /**
     * <p>
     * This method is used to fetch API permissions from Redis. If the permissions are not present in Redis,
     * it fetches them from the database, stores them in Redis for future use, and then returns them.
     * The permissions are stored in a map where the key is the HTTP method and the value is another map.
     * In the inner map, the key is the API endpoint and the value is a list of roles that have permission to access that endpoint.
     * </p>
     *
     * @return A map containing API permissions. The key is the HTTP method and the value is another map.
     *         In the inner map, the key is the API endpoint and the value is a list of roles that have permission to access that endpoint.
     */
    private Map<String, Map<String, List<String>>> getApiPermission() {
        HashOperations<String, String, Map<String, Map<String, List<String>>>> hashOps = redisApiPermissionTemplate.opsForHash();
        Map<String, Map<String, Map<String, List<String>>>> apiPermission = hashOps.entries(Constants.API_PERMISSION_KEY);
        if (Objects.isNull(apiPermission) || Objects.isNull(apiPermission.get(this.getServiceName()))) {
            Map<String, Map<String, List<String>>> apiPermissionMap = new HashMap<>();
            List<ApiRolePermission> apiRolePermissions = apiRolePermissionRepository.findByServiceNameAndTypeAndIsActiveTrueAndIsDeletedFalse(serviceName, Constants.API_PERMISSION);
            apiRolePermissions.stream().forEach(api -> {
                Map<String, List<String>> apiRoleMap = apiPermissionMap.get(api.getMethod()) == null ? new HashMap<>()
                        : apiPermissionMap.get(api.getMethod());
                apiRoleMap.put(api.getApi(), api.getRoles());
                apiPermissionMap.put(api.getMethod(), apiRoleMap);
            });
            hashOps.putAll(Constants.API_PERMISSION_KEY, Map.of(this.getServiceName(), apiPermissionMap));
            return apiPermissionMap;
        } else {
            return apiPermission.get(this.getServiceName());
        }
    }

    /**
     * <p>
     * This method is used to fetch Open URI from Redis. If the Open URI is not present in Redis,
     * it fetches them from the database, stores them in Redis for future use, and then returns them.
     * The Open URI are stored in a map where the key is the HTTP method and the value is a list of URIs that can be accessed directly without authorization.
     * </p>
     *
     * @return A map containing Open URI. The key is the HTTP method and the value is a list of URIs that can be accessed directly without authorization.
     */
    private Map<String, List<String>> getOpenUri() {
        HashOperations<String, String, Map<String, List<String>>> hashOps = redisOpenUriTemplate.opsForHash();
        Map<String, Map<String, List<String>>> openUri = hashOps.entries(Constants.OPEN_URI_KEY);
        if (Objects.isNull(openUri) || Objects.isNull(openUri.get(this.getServiceName()))) {
            Map<String, List<String>> openUriMap = new HashMap<>();
            List<ApiRolePermission> apiRolePermissions = apiRolePermissionRepository.findByServiceNameAndTypeAndIsActiveTrueAndIsDeletedFalse(serviceName, Constants.OPEN_URI);
            apiRolePermissions.stream().forEach(api -> {
                List<String> uriMap = openUriMap.get(api.getMethod()) == null ? new ArrayList<>()
                        : openUriMap.get(api.getMethod());
                uriMap.add(api.getApi());
                openUriMap.put(api.getMethod(), uriMap);
            });
            hashOps.putAll(Constants.OPEN_URI_KEY, Map.of(this.getServiceName(), openUriMap));
            return openUriMap;
        } else {
            return openUri.get(this.getServiceName());
        }
    }

    /**
     * <p>
     * This method is used to wrap the incoming HttpServletRequest into a ContentCachingRequestWrapper.
     * ContentCachingRequestWrapper is used to cache the content of the request, allowing it to be read multiple times.
     * </p>
     *
     * @param request the original HttpServletRequest that needs to be wrapped
     * @return a ContentCachingRequestWrapper that contains the cached content of the original request. If the original request is already an instance of ContentCachingRequestWrapper, it is returned as is.
     */
    private static ContentCachingRequestWrapper wrapRequest(HttpServletRequest request) {
        if (request instanceof ContentCachingRequestWrapper contentCachingRequestWrapper) {
            return contentCachingRequestWrapper;
        }
        return new ContentCachingRequestWrapper(request);
    }

    /**
     * <p>
     * This method is used to wrap the incoming HttpServletResponse into a ContentCachingResponseWrapper.
     * ContentCachingResponseWrapper is used to cache the content of the response, allowing it to be read multiple times.
     * </p>
     *
     * @param response the original HttpServletResponse that needs to be wrapped
     * @return a ContentCachingResponseWrapper that contains the cached content of the original response. If the original response is already an instance of ContentCachingResponseWrapper, it is returned as is.
     */
    private static ContentCachingResponseWrapper wrapResponse(HttpServletResponse response) {
        if (response instanceof ContentCachingResponseWrapper contentCachingResponseWrapper) {
            return contentCachingResponseWrapper;
        }
        return new ContentCachingResponseWrapper(response);
    }
}
