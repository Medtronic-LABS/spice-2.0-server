package com.mdtlabs.coreplatform.authservice.service;

import java.util.Map;

/**
 * <p>
 * This an interface class for User Token you can implement this
 * class in any class.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
public interface UserTokenService {

    /**
     * <p>
     * Saves a UserToken.
     * This method takes an authentication token, username, client, and user ID as parameters and saves a UserToken.
     * </p>
     *
     * @param authToken The authentication token of the user.
     * @param username  The username of the user.
     * @param client    The client from which the request is made.
     * @param appVersion    {@link String} The "appVersion" parameter is a String that represents the application version
     * @param userId    The ID of the user.
     */
    void saveUserToken(String authToken, String username, String client, long userId, String appVersion);

    /**
     * <p>
     * Deletes a UserToken by token.
     * This method takes a redis key and a token as parameters and deletes the corresponding UserToken.
     * </p>
     *
     * @param redisKey The redis key associated with the UserToken.
     * @param token    The token of the UserToken to be deleted.
     */
    void deleteUserTokenByToken(String redisKey, String token);

    /**
     * <p>
     * Deletes a UserToken by username and user ID.
     * This method takes a username and a user ID as parameters and deletes the corresponding UserToken.
     * </p>
     *
     * @param username The username of the user.
     * @param userId   The ID of the user.
     */
    void deleteUserTokenByUserName(String username, Long userId);

    /**
     * <p>
     * Deletes UserTokens by username and user ID.
     * This method takes a map where the key is the user ID and the value is the username, and deletes the corresponding UserTokens.
     * </p>
     *
     * @param usernameIdMap A map where the key is the user ID and the value is the username.
     */
   void deleteUsersToken(Map<Long, String> usernameIdMap);

}