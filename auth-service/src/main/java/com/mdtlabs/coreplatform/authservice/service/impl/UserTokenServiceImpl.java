package com.mdtlabs.coreplatform.authservice.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.repository.UserTokenRepository;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserToken;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 * This service class contain all the business logic for User Token Service and
 * perform all the organization operation here.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
@Service
public class UserTokenServiceImpl implements UserTokenService {

    private final UserTokenRepository userTokenRepository;
    
    private final RedisTemplate<String, String> redisTemplate;

    @Autowired
    public UserTokenServiceImpl(UserTokenRepository userTokenRepository, RedisTemplate<String, String> redisTemplate ) {
        this.userTokenRepository = userTokenRepository;
        this.redisTemplate = redisTemplate;
    }

    /**
     * {@inheritDoc}
     */
    public void saveUserToken(String authToken, String username, String client, long userId, String appVersion) {
        try {
            String key = Constants.SPICE + Constants.COLON + Constants.LOGIN +
                    Constants.COLON + username + Constants.COLON + authToken;
            redisTemplate.opsForList().leftPush(key, authToken);
            redisTemplate.expire(key, Constants.AUTH_TOKEN_EXPIRY_MINUTES, TimeUnit.MINUTES);
            UserToken usertoken = new UserToken();
            usertoken.setAuthToken(authToken);
            usertoken.setClient(client);
            usertoken.setLastLoggedIn(new Date());
            usertoken.setUserId(userId);
            usertoken.setAppVersion(appVersion);
            userTokenRepository.save(usertoken);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void deleteUserTokenByToken(String redisKey, String token) {
        try {
            redisTemplate.delete(redisKey);
            UserToken userToken = userTokenRepository.findByAuthToken(token);
            userToken.setActive(Constants.BOOLEAN_FALSE);
            userToken.setLastLoggedOut(new Date());
            userToken.setAuthToken(null);
            userTokenRepository.save(userToken);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void deleteUserTokenByUserName(String username, Long userId) {
        try {
            List<UserToken> userTokens = userTokenRepository.findByUserIdAndIsActiveTrue(userId);
            List<String> tokens = userTokens.stream().map(authToken -> Constants.SPICE + Constants.COLON + Constants.LOGIN +
                    Constants.COLON + username + Constants.COLON +
                    authToken.getAuthToken()).toList();
            redisTemplate.delete(tokens);
            userTokens.stream().forEach(token -> {
                token.setActive(Constants.BOOLEAN_FALSE);
                token.setAuthToken(null);
            });
            userTokenRepository.saveAll(userTokens);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void deleteUsersToken(Map<Long, String> usernameIdMap) {
        try {
            List<UserToken> userTokens = userTokenRepository.findByUserIdInAndIsActiveTrue(usernameIdMap.keySet());
            List<String> tokens = userTokens.stream().map(authToken -> Constants.SPICE + Constants.COLON + Constants.LOGIN +
                    Constants.COLON + usernameIdMap.get(authToken.getUserId()) + Constants.COLON +
                    authToken.getAuthToken()).toList();

            redisTemplate.delete(tokens);
            userTokens.stream().forEach(token -> {
                token.setActive(Constants.BOOLEAN_FALSE);
                token.setAuthToken(null);
            });
            userTokenRepository.saveAll(userTokens);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
    }

}
