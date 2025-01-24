package com.mdtlabs.coreplatform.authservice.authenticationserver;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.verify;

import com.mdtlabs.coreplatform.authservice.helper.HelperService;
import com.mdtlabs.coreplatform.authservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.authservice.repository.UserRepository;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * This class has the test methods for Security config class.
 * </p>
 *
 * @author karthick M created on Jan 30, 2023
 */
@ExtendWith(MockitoExtension.class)
class SecurityConfigTest {

    @InjectMocks
    SecurityConfig securityConfig;

    @Mock
    private HttpSecurity httpSecurity;

    @Mock
    private UserRepository userRepository;

    @Mock
    private RoleRepository roleRepository;

    @Mock
    private UserTokenService userTokenService;

    @Mock
    private RedisTemplate<String, Map<String, List<String>>> redisTemplate;

    @Mock
    private HelperService commonUtil;

    @Test
    void passwordEncoder() {
        //then
        PasswordEncoder actualPasswordEncoder = securityConfig.passwordEncoder();
        assertNotNull(actualPasswordEncoder);
    }

    @Test
    void authenticationSuccess() {
        //then
        AuthenticationSuccess authenticationSuccess = securityConfig.authenticationSuccess(userTokenService, roleRepository, userRepository, redisTemplate);
        assertNotNull(authenticationSuccess);
    }

    @Test
    void authenticationFailure() {
        //then
        AuthenticationFailure authenticationFailure = securityConfig.authenticationFailure();
        assertNotNull(authenticationFailure);
    }

    @Test
    void authenticationProvider() {
        //then
        AuthenticationProvider authenticationProvider = securityConfig.authenticationProvider(userRepository,roleRepository);
        assertNotNull(authenticationProvider);
    }

    @Test
    void logoutSuccess() {
        //then
        LogoutSuccess logoutSuccess = securityConfig.logoutSuccess(userTokenService, commonUtil);
        assertNotNull(logoutSuccess);
    }

    @Test
    void testSecurityFilterChain() throws Exception {

        Mockito.when(httpSecurity.cors(Mockito.any())).thenReturn(httpSecurity);
        Mockito.when(httpSecurity.authorizeHttpRequests(Mockito.any())).thenReturn(httpSecurity);
        Mockito.when(httpSecurity.formLogin(Mockito.any())).thenReturn(httpSecurity);
        Mockito.when(httpSecurity.exceptionHandling(Mockito.any())).thenReturn(httpSecurity);
        Mockito.when(httpSecurity.logout(Mockito.any())).thenReturn(httpSecurity);
        Mockito.when(httpSecurity.csrf(Mockito.any())).thenReturn(httpSecurity);

        securityConfig.securityFilterChain(httpSecurity, userTokenService, roleRepository, userRepository, redisTemplate, commonUtil);
        verify(httpSecurity).cors(Mockito.any());
        verify(httpSecurity).authorizeHttpRequests(Mockito.any());
        verify(httpSecurity).formLogin(Mockito.any());
        verify(httpSecurity).exceptionHandling(Mockito.any());
        verify(httpSecurity).logout(Mockito.any());
        verify(httpSecurity).csrf(Mockito.any());
    }
}
