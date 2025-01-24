package com.mdtlabs.coreplatform.authservice.controller;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.text.ParseException;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.authservice.authenticationserver.AuthenticationValidation;
import com.mdtlabs.coreplatform.authservice.common.TestConstants;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ContextsDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AuthenticationControllerTest {
    
    @InjectMocks
    AuthenticationController authenticationController;
    
    @Mock
    AuthenticationValidation authenticationValidation;
    
    @Mock
    UserTokenService userTokenService;
    
    String token = TestConstants.TOKEN;
    String cookie = Constants.AUTH_COOKIE_NAME;
    String client = "client";
    
    @Test
    void authenticateToken() throws ParseException {
        ContextsDTO context = new ContextsDTO(null, null);
        
        when(authenticationValidation.validateAspect(token, client, null)).thenReturn(context);
        
        ContextsDTO response = authenticationController.authenticateToken(token, client, null);
        
        assertNotNull(response);
    }
    
    @Test
    void deleteToken() throws ParseException {
        doReturn(null).when(authenticationValidation).validateAspect(token, client, null);
        doNothing().when(userTokenService).deleteUserTokenByUserName(TestConstants.USER_NAME, 1L);
        
        authenticationController.deleteToken(token, cookie, client, TestConstants.USER_NAME, 1L);
    }
    
    @Test
    void deleteUsersToken() throws ParseException {
        Map<Long, String> usernameIdMap = Map.of(1L, TestConstants.USER_NAME);
        doReturn(null).when(authenticationValidation).validateAspect(token, client, null);
        doNothing().when(userTokenService).deleteUsersToken(usernameIdMap);
        
        authenticationController.deleteUsersToken(token, cookie, client, usernameIdMap);
    }

}
