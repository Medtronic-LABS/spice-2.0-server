package com.mdtlabs.coreplatform.authservice.service;

import static org.mockito.Mockito.*;

import java.util.*;
import java.util.concurrent.TimeUnit;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;

import com.mdtlabs.coreplatform.authservice.common.TestConstants;
import com.mdtlabs.coreplatform.authservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.repository.UserTokenRepository;
import com.mdtlabs.coreplatform.authservice.service.impl.UserTokenServiceImpl;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserToken;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserTokenServiceTest {
    
    @InjectMocks
    UserTokenServiceImpl userTokenService;
    
    @Mock
    UserTokenRepository userTokenRepository;

    @Mock
    private RedisTemplate<String, String> redisTemplate;
    
    @Test
    void saveUserToken() {
        String authToken = TestConstants.TOKEN;
        String userName = TestConstants.USER_NAME;
        String client = "client";
        String key = Constants.SPICE + Constants.COLON + Constants.LOGIN +
                Constants.COLON + userName + Constants.COLON + authToken;

        ListOperations operations = mock(ListOperations.class);
        when(redisTemplate.opsForList()).thenReturn(operations);
        when(redisTemplate.opsForList().leftPush(key, authToken)).thenReturn(TestConstants.ONE);
        when(operations.leftPush("authKey", authToken)).thenReturn(TestConstants.ONE);
        when(redisTemplate.expire(key, Constants.AUTH_TOKEN_EXPIRY_MINUTES, TimeUnit.MINUTES)).thenReturn(null);
        userTokenService.saveUserToken(authToken, userName, client, 1l, TestConstants.APP_VERSION);
        verify(redisTemplate, atLeastOnce()).opsForList();
    }

    @Test
    void saveUserTokenThrowsException() {
        String authToken = TestConstants.TOKEN;
        String userName = TestConstants.USER_NAME;
        String client = "client";
        doThrow(new RedisConnectionFailureException("")).when(redisTemplate).opsForList();
        userTokenService.saveUserToken(authToken, userName, client, 1l, TestConstants.APP_VERSION);
        verify(redisTemplate, atLeastOnce()).opsForList();
    }

    @Test
    void deleteUserTokenByToken() {
        String redisKey = TestConstants.REDIS_KEY;
        String token = TestConstants.TOKEN;
        ListOperations operations = mock(ListOperations.class);
        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.leftPush("authKey", token)).thenReturn(TestConstants.ONE);
        when(redisTemplate.delete(redisKey)).thenReturn(null);
        when(userTokenRepository.findByAuthToken(token)).thenReturn(TestDataProvider.getUserToken());
        userTokenService.deleteUserTokenByToken(redisKey, token);
        verify(userTokenRepository, atLeastOnce()).findByAuthToken(token);
    }

    @Test
    void deleteUserTokenByToken_ThrowsException() {
        String redisKey = TestConstants.REDIS_KEY;
        String token = TestConstants.TOKEN;
        doThrow(new RuntimeException("")).when(userTokenRepository).findByAuthToken(token);
        userTokenService.deleteUserTokenByToken(redisKey, token);
        verify(userTokenRepository).findByAuthToken(token);
    }
    
    @Test
    void deleteUserTokenByUserName() {
        String userName = TestConstants.USER_NAME;
        UserToken userToken = TestDataProvider.getUserToken();
        List<UserToken> userTokens = new ArrayList<UserToken>();
        userTokens.add(userToken);
        
        when(userTokenRepository.findByUserIdAndIsActiveTrue(1L)).thenReturn(userTokens);
        userTokenService.deleteUserTokenByUserName(userName, 1L);
        verify(userTokenRepository, atLeastOnce()).findByUserIdAndIsActiveTrue(1L);
    }

    @Test
    void deleteUserTokenByUserName_ThrowsException() {
        String userName = TestConstants.USER_NAME;
        doThrow(new RuntimeException("")).when(userTokenRepository).findByUserIdAndIsActiveTrue(1L);
        userTokenService.deleteUserTokenByUserName(userName, 1L);
        verify(userTokenRepository, atLeastOnce()).findByUserIdAndIsActiveTrue(1L);
    }
    
    @Test
    void deleteUsersToken() {
        Map<Long, String> usernameIdMap = new HashMap<>();
        usernameIdMap.put(1L, TestConstants.USER_NAME);
        UserToken userToken = TestDataProvider.getUserToken();
        List<UserToken> userTokens = new ArrayList<UserToken>();
        userTokens.add(userToken);

        when(userTokenRepository.findByUserIdInAndIsActiveTrue(usernameIdMap.keySet()))
            .thenReturn(userTokens);
        userTokenService.deleteUsersToken(usernameIdMap);
        verify(userTokenRepository).findByUserIdInAndIsActiveTrue(usernameIdMap.keySet());
    }

    @Test
    void deleteUsersToken_ThrowsException() {
        Map<Long, String> usernameIdMap = new HashMap<>();
        usernameIdMap.put(1L, TestConstants.USER_NAME);
        doThrow(new RuntimeException("")).when(userTokenRepository).findByUserIdInAndIsActiveTrue(usernameIdMap.keySet());
        userTokenService.deleteUsersToken(usernameIdMap);
        verify(userTokenRepository).findByUserIdInAndIsActiveTrue(usernameIdMap.keySet());
    }

}
