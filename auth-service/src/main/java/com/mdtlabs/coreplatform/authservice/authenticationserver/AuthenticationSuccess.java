package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.io.IOException;
import java.security.KeyFactory;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.LinkedHashSet;
import java.util.Comparator;
import java.util.stream.Collectors;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.server.Cookie;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseCookie;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.util.FileCopyUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;

import com.mdtlabs.coreplatform.authservice.ErrorConstants;
import com.mdtlabs.coreplatform.authservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.mdtlabs.coreplatform.authservice.repository.UserRepository;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;

import com.nimbusds.jose.EncryptionMethod;
import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.JWEAlgorithm;
import com.nimbusds.jose.JWEHeader;
import com.nimbusds.jose.crypto.RSAEncrypter;
import com.nimbusds.jwt.EncryptedJWT;
import com.nimbusds.jwt.JWTClaimsSet;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * <p>
 * <tt>AuthenticationSuccess</tt> Sent to successful authentication.
 * </p>
 *
 * @author Karthick Murugesan
 * @since Jan 11, 2024
 */
public class AuthenticationSuccess extends SimpleUrlAuthenticationSuccessHandler {

    private RSAPublicKey publicRsaKey;

    @Value("${app.public-key}")
    private String publicKey;

    @Value("${app.domain-name:}")
    private String domainName;

    @Value("${app.environment:}")
    private String environment;

    private final UserTokenService userTokenService;

    private final RoleRepository roleRepository;

    private final UserRepository userRepository;

    private final RedisTemplate<String, Map<String, List<String>>> redisTemplate;

    public AuthenticationSuccess(UserTokenService userTokenService, RoleRepository roleRepository, UserRepository userRepository, RedisTemplate<String, Map<String, List<String>>> redisTemplate) {
        this.userTokenService = userTokenService;
        this.roleRepository = roleRepository;
        this.userRepository = userRepository;
        this.redisTemplate = redisTemplate;
    }

    /**
     * <p>
     * This method is used to initialize a public RSA key from a file resource.
     * </p>
     */
    @PostConstruct
    public void init() {
        try {
            Resource resource = new ClassPathResource(publicKey);
            byte[] bdata = FileCopyUtils.copyToByteArray(resource.getInputStream());
            X509EncodedKeySpec spec = new X509EncodedKeySpec(bdata);
            KeyFactory kf = KeyFactory.getInstance(Constants.RSA);
            this.publicRsaKey = (RSAPublicKey) kf.generatePublic(spec);
        } catch (Exception e) {
            Logger.logError(ErrorConstants.EXCEPTION_TOKEN_UTILS, e);
        }
    }

    /**
     * <p>
     * This method is used to handle successful authentication and returns user information in JSON format.
     * </p>
     *
     * @param request        {@link HttpServletRequest} An object representing the HTTP request made by the client is given
     * @param response       {@link HttpServletResponse} The HttpServletResponse object that is used to send the response back to the
     *                       client after successful authentication is given
     * @param authentication {@link Authentication} The authentication object represents the user's authentication information is given
     */
    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
                                        Authentication authentication) {
        if (!response.isCommitted()) {
            response.setStatus(HttpStatus.OK.value());
            response.setContentType(Constants.CONTENT_TEXT_TYPE);
            response.setHeader(Constants.CACHE_HEADER_NAME, Constants.CACHE_HEADER_VALUE);
            response.setHeader(Constants.ACCESS_CONTROL_EXPOSE_HEADERS, Constants.AUTHORIZATION);
            try {
                UserContextDTO user = getLoggedInUser();
                if (user != null) {
                    String client = request.getHeader(Constants.HEADER_CLIENT);
                    user.setClient(client);
                    validateApplication(client, user);
                    user = assignUserRoleByClient(client, user);
                    user.setCurrentDate(new Date().getTime());
                    ObjectWriter objectWriter = new ObjectMapper().writer().withDefaultPrettyPrinter();
                    String json = objectWriter.writeValueAsString(user);
                    response.getWriter().write(json);
                    responseHeaderUser(response, user, client, request.getHeader(Constants.APP_VERSION));
                } else {
                    response.getWriter().write(ErrorConstants.INVALID_USER_ERROR);
                }
            } catch (IOException e) {
                Logger.logError(ErrorConstants.LOGIN_ERROR + e);
            }

        }
        clearAuthenticationAttributes(request);
    }

    /**
     * <p>
     * Assigns roles to a user based on the client application.
     * This method takes a client application name and a user object.
     * It retrieves the roles associated with the client application and checks if the user has any of these roles.
     * If the user has at least one role associated with the client application, these roles are assigned to the user.
     * If the user does not have any role associated with the client application, a BadCredentialsException is thrown.
     * </p>
     *
     * @param client The name of the client application.
     * @param user   The user to assign roles to.
     * @return The user with assigned roles.
     * @throws BadCredentialsException If the user does not have any role associated with the client application.
     */
    private UserContextDTO assignUserRoleByClient(String client, UserContextDTO user) {
        if (StringUtils.isBlank(client)) {
            throw new BadCredentialsException(ErrorConstants.ERROR_USER_DOESNT_ROLE);
        }
        Map<String, List<String>> applicationRoles = initiateRolesMap();
        boolean isExist = false;
        List<RoleDTO> userRoles = user.getRoles().stream().sorted(Comparator.comparing(RoleDTO::getLevel, Comparator.nullsFirst(Long::compareTo))).toList();
        LinkedHashSet<Role> roles = new LinkedHashSet<>();

        List<String> clients;
        if (Constants.CLIENT_WEB.equals(client)) {
            clients = Constants.WEB_CLIENTS;
        } else if (Constants.CLIENT_JOB.equals(client)) {
            clients = Constants.JOB_CLIENTS;
        } else {
            clients = Constants.MOB_CLIENTS;
        }

        for (RoleDTO userRole : userRoles) {
            for (String userClient : clients) {
                if (Objects.nonNull(applicationRoles.get(userClient)) &&
                        applicationRoles.get(userClient).contains(userRole.getName())) {
                    isExist = true;
                    Role role = new Role(userRole.getId(), userRole.getName(), userRole.getLevel(),
                            userRole.getSuiteAccessName(), userRole.getAppTypes());
                    roles.add(role);
                }
            }
        }
        user.setRoles(roles.stream().toList());
        if (!isExist) {
            throw new BadCredentialsException(ErrorConstants.ERROR_USER_DOESNT_ROLE);
        }
        return user;
    }

    /**
     * <p>
     * This method is used to set the response headers for a user's authentication token and tenant ID.
     * </p>
     *
     * @param response {@link HttpServletResponse} The HttpServletResponse object that represents the response to be sent back to
     *                 the client. It contains information such as the response status code, headers, and body is given
     * @param user     {@link UserContextDTO} An object of type AuthUserDTO which contains information about the authenticated
     *                 user is given
     * @param client   {@link String} The "client" parameter is a String that represents the client application that is
     *                 making the request to the server. It is used to create a user token for the client is given
     * @param appVersion    {@link String} The "appVersion" parameter is a String that represents the application version
     */
    private void responseHeaderUser(HttpServletResponse response, UserContextDTO user, String client, String appVersion) {
        Map<String, Object> userInfo = new ObjectMapper().convertValue(user, Map.class);
        String authToken = null;
        try {
            authToken = authTokenCreation(user, userInfo);
            createUserToken(authToken, client, appVersion);
            User existingUser = userRepository.getUserByUsername(user.getUsername(), true);
            existingUser.setLastLoggedIn(DateUtil.formatDate(new Date()));
            userRepository.save(existingUser);
            response.setHeader(Constants.HEADER_TENANT_ID, String.valueOf(user.getTenantId()));
            if (Constants.CLIENT_WEB.equals(client)) {
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
                            .sameSite(Cookie.SameSite.STRICT.attributeValue())
                            .domain(domainName)
                            .httpOnly(Boolean.TRUE).secure(Boolean.TRUE)
                            .maxAge(Duration.ofMinutes(Constants.AUTH_TOKEN_EXPIRY_MINUTES)).path(Constants.FORWARD_SLASH)
                            .build();
                }
                response.addHeader(HttpHeaders.SET_COOKIE, cookie.toString());
            } else {
                response.setHeader(Constants.AUTHORIZATION, authToken);
            }
        } catch (JOSEException exception) {
            Logger.logError(ErrorConstants.ERROR_JWE_TOKEN, exception);
        }
    }

    /**
     * <p>
     * This method is used to create an encrypted JWT authentication token with user and organization
     * information.
     * </p>
     *
     * @param user     {@link UserContextDTO} The user parameter is an instance of the AuthUserDTO class, which contains
     *                 information about the authenticated user, such as their ID and the organizations they belong to
     *                 is given
     * @param userInfo {@link Map<String, Object>} The userInfo parameter is a Map object that contains additional information
     *                 about the user, such as their name, email, and other relevant details is given
     * @return {@link String} The method returns a string that represents an encrypted JWT (JSON Web Token) is given
     */
    private String authTokenCreation(UserContextDTO user, Map<String, Object> userInfo) throws JOSEException {
        List<Long> tenantIds = new ArrayList<>();
        if (!user.getOrganizationIds().isEmpty()) {
            tenantIds = user.getOrganizationIds().stream().toList();
        }
        JWTClaimsSet.Builder claimsSet = new JWTClaimsSet.Builder();
        claimsSet.issuer(Constants.TOKEN_ISSUER);
        claimsSet.subject(Constants.AUTH_TOKEN_SUBJECT);
        claimsSet.claim(Constants.USER_DATA, userInfo);
        claimsSet.claim(Constants.TENANT_IDS_CLAIM, tenantIds);
        claimsSet.claim(Constants.APPLICATION_TYPE, Constants.WEB);
        claimsSet.expirationTime(
                Date.from(ZonedDateTime.now().plusMinutes(Constants.AUTH_TOKEN_EXPIRY_MINUTES).toInstant()));
        claimsSet.notBeforeTime(new Date());
        claimsSet.jwtID(UUID.randomUUID().toString());
        JWEHeader header = new JWEHeader(JWEAlgorithm.RSA_OAEP_256, EncryptionMethod.A128GCM);
        EncryptedJWT jwt = new EncryptedJWT(header, claimsSet.build());
        RSAEncrypter encrypter = new RSAEncrypter(this.publicRsaKey);
        jwt.encrypt(encrypter);
        return Constants.BEARER.concat(jwt.serialize());
    }

    /**
     * <p>
     * This method is used to create a user token by saving the JWT token, username, client, and user ID in the
     * userTokenService if the user is logged in.
     * </p>
     *
     * @param jwtToken {@link String} The JWT token that needs to be saved for the user is given
     * @param client   {@link String} The "client" parameter in this method refers to the client application that is
     *                 requesting the creation of a user token is given
     */
    private void createUserToken(String jwtToken, String client, String appVersion) {
        UserContextDTO user = getLoggedInUser();
        if (user != null) {
            userTokenService.saveUserToken(jwtToken.substring(Constants.BEARER.length()), user.getUsername(), client, user.getId(), appVersion);
        }
    }

    /**
     * <p>
     * This method is used to retrieve the currently logged in user's information and maps it to an AuthUserDTO
     * object.
     * </p>
     *
     * @return {@link UserContextDTO} The method is returning an instance of the class, which represents the
     * currently logged-in user is given
     */
    private UserContextDTO getLoggedInUser() {
        if (null == SecurityContextHolder.getContext() || null == SecurityContextHolder.getContext().getAuthentication()
                || null == SecurityContextHolder.getContext().getAuthentication().getPrincipal()) {
            return null;
        }
        if (SecurityContextHolder.getContext().getAuthentication().getPrincipal().equals(Constants.ANONYMOUS_USER)) {
            return null;
        }
        Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        UserContextDTO userContextDTO;
        userContextDTO = new ModelMapper().map(SecurityContextHolder.getContext().getAuthentication().getPrincipal(),
                UserContextDTO.class);
        User user = (User) principal;
        userContextDTO.setOrganizationIds(Objects.nonNull(user.getOrganizations()) ? user.getOrganizations().stream().map(BaseEntity::getId).collect(
                Collectors.toSet()) : new HashSet<>());
        userContextDTO.setIsSuperUser(user.getRoles().stream().anyMatch(role -> role.getName().equalsIgnoreCase(Constants.ROLE_SUPER_USER)));
        userContextDTO.setIsJobUser(user.getRoles().stream().anyMatch(role -> role.getName().equalsIgnoreCase(Constants.ROLE_JOB_USER)));
        return userContextDTO;
    }

    /**
     * <p>
     * Initializes the roles map from Redis.
     * This method retrieves a list of role maps from Redis using the predefined key.
     * If the retrieved list is null or empty, it calls the loadRedisData method to load the data into Redis.
     * It then returns the first map from the updated list.
     * </p>
     *
     * @return A map of roles retrieved from Redis.
     */
    private Map<String, List<String>> initiateRolesMap() {
        List<Map<String, List<String>>> roleListRedis = new ArrayList<>();
        try {
            roleListRedis = redisTemplate.opsForList()
                    .range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE);

        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        if (!Objects.isNull(roleListRedis) && roleListRedis.isEmpty()) {
            roleListRedis = loadRedisData(roleListRedis);
        }
        if (roleListRedis != null && !roleListRedis.isEmpty()) {
            return roleListRedis.getFirst();
        } else {
            return new HashMap<>();
        }
    }

    /**
     * <p>
     * Validates the client application against the user's suite access.
     * This method takes a client application name and a user object.
     * It checks if the user has access to the client application by comparing the client name with the user's suite access.
     * If the user does not have access to the client application, a BadCredentialsException is thrown.
     * </p>
     *
     * @param client The name of the client application.
     * @param user   The user to validate the application for.
     * @throws BadCredentialsException If the user does not have access to the client application.
     */
    private void validateApplication(String client, UserContextDTO user) {
        List<String> clients ;
        if (Constants.CLIENT_WEB.equals(client)) {
            clients = Constants.WEB_CLIENTS;
        } else if (Constants.CLIENT_JOB.equals(client)) {
            clients = Constants.JOB_CLIENTS;
        } else {
            clients = Constants.MOB_CLIENTS;
        }
        if (user.getSuiteAccess().stream().noneMatch(clients::contains)) {
            throw new BadCredentialsException(ErrorConstants.INVALID_CLIENT);
        }
    }

    /**
     * <p>
     * Loads role data into Redis.
     * This method takes a list of role maps and clears it.
     * It then retrieves a list of active roles from the role repository.
     * For each role, it checks if the role's suite access name exists in the application roles map.
     * If it does not exist, it adds a new entry to the map with the suite access name as the key and an empty list as the value.
     * It then adds the role name to the list associated with the suite access name in the application roles map.
     * The application roles map is then pushed to Redis using the predefined key.
     * The method retrieves the list of role maps from Redis again and assigns it to the role list.
     * If an exception occurs during this process, it is logged.
     * The method returns the updated list of role maps.
     * </p>
     *
     * @param roleListRedis A list of role maps to load into Redis.
     * @return The updated list of role maps retrieved from Redis.
     */
    private List<Map<String, List<String>>> loadRedisData(List<Map<String, List<String>>> roleListRedis) {
        try {
            Map<String, List<String>> applicationRoles = new HashMap<>();
            roleListRedis.clear();
            List<Role> roles = roleRepository.getActiveRoles();
            roles.forEach(role -> {
                if (!applicationRoles.containsKey(role.getSuiteAccessName())) {
                    applicationRoles.put(role.getSuiteAccessName(), new ArrayList<>());
                }
                applicationRoles.get(role.getSuiteAccessName()).add(role.getName());
            });
            redisTemplate.opsForList().leftPush(Constants.ROLE_REDIS_KEY, applicationRoles);
            roleListRedis = redisTemplate.opsForList()
                    .range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        return roleListRedis;
    }


}
