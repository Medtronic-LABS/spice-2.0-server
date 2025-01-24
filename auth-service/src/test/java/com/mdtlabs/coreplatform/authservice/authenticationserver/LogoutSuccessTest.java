package com.mdtlabs.coreplatform.authservice.authenticationserver;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPrivateKey;
import java.util.*;

import com.mdtlabs.coreplatform.authservice.helper.HelperService;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.nimbusds.jose.EncryptionMethod;
import com.nimbusds.jose.JWEAlgorithm;
import com.nimbusds.jose.JWEHeader;
import com.nimbusds.jwt.EncryptedJWT;
import com.nimbusds.jwt.JWTClaimsSet;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.core.Authentication;

import com.mdtlabs.coreplatform.authservice.common.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.Constants;

import jakarta.servlet.http.HttpServletResponse;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class LogoutSuccessTest {

    @InjectMocks
    LogoutSuccess logoutSuccess;

    @Mock
    RSAPrivateKey privateRsaKey;

    @Mock
    HelperService commonUtil;

    @Mock
    UserTokenService userTokenService;


    @Test
    void onLogoutSuccess() throws NoSuchAlgorithmException {
        //given
        KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA");
        keyGen.initialize(2048);
        java.security.KeyPair keyPair = keyGen.generateKeyPair();
        privateRsaKey = (RSAPrivateKey) keyPair.getPrivate();
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(HttpHeaders.AUTHORIZATION, Constants.BEARER + TestConstants.TOKEN);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = mock(Authentication.class);
        List<Long> tenantIds = new ArrayList<>();
        Map<String, Object> userInfo = new HashMap<>();
        userInfo.put(Constants.USERNAME, TestConstants.FIRST_NAME);
        JWTClaimsSet.Builder claimsSet = new JWTClaimsSet.Builder();
        claimsSet.issuer(Constants.TOKEN_ISSUER);
        claimsSet.subject(Constants.AUTH_TOKEN_SUBJECT);
        claimsSet.claim(Constants.USER_DATA, userInfo);
        claimsSet.claim(Constants.TENANT_IDS_CLAIM, tenantIds);
        claimsSet.claim(Constants.APPLICATION_TYPE, Constants.WEB);
        JWEHeader header = new JWEHeader(JWEAlgorithm.RSA_OAEP_256, EncryptionMethod.A128GCM);
        EncryptedJWT jwt = new EncryptedJWT(header, claimsSet.build());
        //then
        when(commonUtil.getJWT(TestConstants.TOKEN)).thenReturn(jwt);
        doNothing().when(userTokenService).deleteUserTokenByToken(TestConstants.REDIS_KEY, TestConstants.TOKEN);
        logoutSuccess.onLogoutSuccess(httpServletRequest, httpServletResponse, authentication);
        verify(commonUtil,atLeastOnce()).getJWT(TestConstants.TOKEN);
    }

    @Test
    void throwDataNotFoundException() {
        //given
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(HttpHeaders.AUTHORIZATION, Constants.EMPTY);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = mock(Authentication.class);
        //then
        assertThrows(NullPointerException.class, () -> logoutSuccess.onLogoutSuccess(httpServletRequest, httpServletResponse,
                authentication));
    }

}
