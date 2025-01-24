package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.text.ParseException;
import java.util.Arrays;
import java.util.Base64;
import java.util.Objects;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.authservice.helper.HelperService;
import com.nimbusds.jwt.EncryptedJWT;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;

import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;


/**
 * <p>
 * <tt>LogoutSuccess</tt> to handle successful logout.
 * </p>
 *
 * @author Vigneshkumar
 * @since 16 Oct 2020
 */
public class LogoutSuccess implements LogoutSuccessHandler {


    @Value("${app.domain-name:}")
    private String domainName;

    @Value("${app.environment:}")
    private String environment;


    private final UserTokenService userTokenService;
    private final HelperService commonUtil;

    public LogoutSuccess(UserTokenService userTokenService, HelperService commonUtil) {
        this.userTokenService = userTokenService;
        this.commonUtil = commonUtil;
    }

    /**
     * <p>
     * Handles the successful logout of a user.
     * This method retrieves the authorization token from the request header.
     * If the token is blank, it throws a DataNotFoundException.
     * It then gets the user details associated with the token and constructs a Redis key.
     * The user token is then deleted from Redis using the constructed key.
     * If any exceptions occur during the process, they are logged.
     * </p>
     *
     * @param request        The HttpServletRequest object containing the client request.
     * @param response       The HttpServletResponse object for the servlet's response.
     * @param authentication The Authentication object containing the principal's information.
     */
    @Override
    public void onLogoutSuccess(HttpServletRequest request, HttpServletResponse response,
            Authentication authentication) {
        String client = request.getHeader(Constants.HEADER_CLIENT);
        String token = request.getHeader(HttpHeaders.AUTHORIZATION);
        Cookie cookie = null;
        if (Constants.CLIENT_WEB.equals(client)) {
            cookie = getValidCookie(request);
            token = new String(Base64.getDecoder().decode(cookie.getValue()));
        }
        if (StringUtils.isBlank(token)) {
            throw new DataNotFoundException();
        }

        UserContextDTO userDetail = null;
        try {
            userDetail = getUserDetails(token.substring(Constants.BEARER.length()));
            String redisKey = StringUtil.concatString(Constants.SPICE, Constants.COLON, Constants.LOGIN,
                    Constants.COLON, userDetail.getUsername(), Constants.COLON,
                    token.substring(Constants.BEARER.length()));
            userTokenService.deleteUserTokenByToken(redisKey, token.substring(Constants.BEARER.length()));
            if (Constants.CLIENT_WEB.equals(client)) {
                ResponseCookie responseCookie;
                if (Constants.DEV_ENVIRONMENT.equals(environment)) {
                    responseCookie = ResponseCookie.from(Constants.AUTH_COOKIE_NAME)
                            .domain(domainName)
                            .sameSite(org.springframework.boot.web.server.Cookie.SameSite.NONE.attributeValue())
                            .httpOnly(Boolean.TRUE).secure(Boolean.TRUE)
                            .maxAge(Constants.LONG_ZERO).path(Constants.FORWARD_SLASH)
                            .build();
                } else {
                    responseCookie = ResponseCookie.from(Constants.AUTH_COOKIE_NAME)
                            .domain(domainName)
                            .sameSite(org.springframework.boot.web.server.Cookie.SameSite.STRICT.attributeValue())
                            .httpOnly(Boolean.TRUE).secure(Boolean.TRUE)
                            .maxAge(Constants.LONG_ZERO).path(Constants.FORWARD_SLASH)
                            .build();
                }
                response.addHeader(HttpHeaders.SET_COOKIE, responseCookie.toString());
                }
        } catch (ParseException e) {
            Logger.logError(e);
        }
    }

    /**
     * <p>
     * This method retrieves a valid cookie from the request.
     * If the cookie is not found or the cookie name does not match the expected name,
     * it throws a DataNotFoundException.
     * </p>
     *
     * @param request The HttpServletRequest object containing the cookies.
     * @return The valid cookie with the expected name.
     * @throws DataNotFoundException If the cookie is not found or the cookie name does not match the expected name.
     */
    private Cookie getValidCookie(HttpServletRequest request) {
        if (Objects.isNull(request.getCookies()) || Constants.ZERO == request.getCookies().length) {
            throw new DataNotFoundException(20002);
        }
        return Arrays.stream(request.getCookies())
                .filter(cookie -> Constants.AUTH_COOKIE_NAME.equals(cookie.getName()))
                .findAny().orElseThrow(() -> new DataNotFoundException(20002));
    }

    /**
     * <p>
     * This method is used to retrieve user details from an encrypted JWT token.
     * </p>
     *
     * @param jwtToken {@link String} The JWT token that contains the user's information is given
     * @return {@link UserContextDTO} object, which contains the details of a user obtained
     * from a JWT token is given
     */
    private UserContextDTO getUserDetails(String jwtToken) throws ParseException {
        if (StringUtils.isBlank(jwtToken)) {
            jwtToken = Constants.SPACE;
        }
        EncryptedJWT jwt = commonUtil.getJWT(jwtToken);
        UserContextDTO userDetail = null;
        Object rawJson = jwt.getJWTClaimsSet().getClaim(Constants.USER_DATA);
        ObjectMapper objectMapper = new ObjectMapper();
        userDetail = objectMapper.convertValue(rawJson, UserContextDTO.class);
        return userDetail;
    }
}
