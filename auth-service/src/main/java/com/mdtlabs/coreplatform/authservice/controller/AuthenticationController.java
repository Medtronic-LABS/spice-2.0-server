package com.mdtlabs.coreplatform.authservice.controller;

import java.text.ParseException;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.authservice.authenticationserver.AuthenticationValidation;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ContextsDTO;

/**
 * <p>
 * The AuthenticationController class is a REST controller that handles requests related to Authentication and Authorization.
 * </p>
 *
 * @author Karthick M
 */
@RestController
@RequestMapping
public class AuthenticationController {

    private final AuthenticationValidation authenticationValidation;
    private final UserTokenService userTokenService;

    @Autowired
    public AuthenticationController(AuthenticationValidation authenticationValidation, UserTokenService userTokenService) {
        this.authenticationValidation = authenticationValidation;
        this.userTokenService = userTokenService;
    }

    /**
     * <p>
     * Authenticates a user by validating their token.
     * This method takes an authorization token and a client name as parameters.
     * It calls the validateAspect method of the AuthenticationValidation service with the token and client name.
     * The validateAspect method validates the token and returns a ContextsDTO object.
     * This object contains the context information of the authenticated user.
     * </p>
     *
     * @param token  The authorization token of the user.
     * @param client The name of the client application.
     * @return A ContextsDTO object containing the context information of the authenticated user.
     * @throws ParseException          If a parsing error occurs.
     */
    @PostMapping("/authenticate")
    public ContextsDTO authenticateToken(@RequestHeader("Authorization") String token,
            @RequestHeader(value = "client", required = false) String client,
            @RequestHeader(value = "auth-cookie", required = false) String cookie)
            throws ParseException {
        return authenticationValidation.validateAspect(token, client, cookie);
    }

    /**
     * <p>
     * Deletes a user's token.
     * This method takes an authorization token, a client name, a username, and a user ID as parameters.
     * It first calls the validateAspect method of the AuthenticationValidation service with the token and client name to authenticate the user.
     * If the user is authenticated, it calls the deleteUserTokenByUserName method of the UserTokenService to delete the user's token.
     * </p>
     *
     * @param token    The authorization token of the user.
     * @param client   The name of the client application.
     * @param username The username of the user.
     * @param userId   The ID of the user.
     * @throws ParseException          If a parsing error occurs.
     */
    @PostMapping("/remove-token/{username}/{userId}")
    public void deleteToken(@RequestHeader("Authorization") String token, @RequestHeader(value = "auth-cookie", required = false) String cookie, @RequestHeader("client") String client, @PathVariable("username") String username, @PathVariable("userId") Long userId) throws ParseException {
        authenticationValidation.validateAspect(token, client, cookie);
        userTokenService.deleteUserTokenByUserName(username, userId);
    }

    /**
     * <p>
     * Deletes tokens for multiple users.
     * This method takes an authorization token, a client name, and a map of user IDs and usernames as parameters.
     * It first calls the validateAspect method of the AuthenticationValidation service with the token and client name to authenticate the user.
     * If the user is authenticated, it calls the deleteUsersToken method of the UserTokenService to delete the tokens for the users specified in the map.
     * </p>
     *
     * @param token         The authorization token of the user.
     * @param client        The name of the client application.
     * @param usernameIdMap A map of user IDs and usernames for which to delete tokens.
     * @throws ParseException          If a parsing error occurs.
     */
    @PostMapping("/users-remove-token")
    public void deleteUsersToken(@RequestHeader("Authorization") String token, @RequestHeader(value = "auth-cookie", required = false) String cookie, @RequestHeader("client") String client, @RequestBody Map<Long, String> usernameIdMap) throws ParseException {
        authenticationValidation.validateAspect(token, client, cookie);
        userTokenService.deleteUsersToken(usernameIdMap);
    }


}
