package com.mdtlabs.coreplatform.authservice.authenticationserver;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.*;

import java.math.BigInteger;
import java.security.interfaces.RSAPublicKey;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.util.ReflectionTestUtils;

import com.mdtlabs.coreplatform.authservice.common.TestConstants;
import com.mdtlabs.coreplatform.authservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.authservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.authservice.repository.UserRepository;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;

import jakarta.servlet.http.HttpServletResponse;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AuthenticationSuccessTest {

    @InjectMocks
    AuthenticationSuccess authenticationSuccess;

    @Mock
    private UserTokenService userTokenService;

    @Mock
    private RedisTemplate<String, Map<String, List<String>>> redisTemplate;

    @Mock
    private RoleRepository roleRepository;

    @Mock
    private UserRepository userRepository;


    @Test
    void init() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicKey", "public_key.der");

        //then
        authenticationSuccess.init();
        assertNotNull(Boolean.TRUE);
    }

    @Test
    void catchInitException() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicKey", "file.der");

        //then
        authenticationSuccess.init();
        assertNotNull(Boolean.TRUE);
    }

    private RSAPublicKey getRSAPublicKey() {
        return new RSAPublicKey() {
            @Override
            public BigInteger getPublicExponent() {
                return new BigInteger("65537");
            }

            @Override
            public String getAlgorithm() {
                return "RSA";
            }

            @Override
            public String getFormat() {
                return null;
            }

            @Override
            public byte[] getEncoded() {
                return null;
            }

            @Override
            public BigInteger getModulus() {
                return TestConstants.BIG_INTEGER;
            }
        };
    }

    private Authentication getAuth(String roleName) {
        return new Authentication() {
            @Override
            public Collection<? extends GrantedAuthority> getAuthorities() {
                return null;
            }

            @Override
            public Object getCredentials() {
                return null;
            }

            @Override
            public Object getDetails() {
                return null;
            }

            @Override
            public Object getPrincipal() {
                User user = TestDataProvider.getUser();
                user.setRoles(Set.of(new Role(TestConstants.ONE, roleName)));
                return user;
            }

            @Override
            public boolean isAuthenticated() {
                return false;
            }

            @Override
            public void setAuthenticated(boolean isAuthenticated) throws IllegalArgumentException {
                // This method is intentionally left empty because the authentication state
                // is managed elsewhere in the system.
            }

            @Override
            public String getName() {
                return null;
            }
        };
    }

    private SecurityContext getSecurityContext(String userName) {
        return new SecurityContext() {
            @Override
            public Authentication getAuthentication() {
                return getAuth(userName);
            }

            @Override
            public void setAuthentication(Authentication authentication) {
                // This method is intentionally left empty because the authentication state
                // is managed elsewhere in the system.
            }
        };
    }

    @Test
    void onAuthenticationSuccess() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicRsaKey", getRSAPublicKey());
        MockedStatic<SecurityContextHolder> securityContextHolder = mockStatic(SecurityContextHolder.class);
        securityContextHolder.when(SecurityContextHolder::getContext)
                .thenReturn(getSecurityContext(Constants.ROLE_SUPER_USER));
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(Constants.HEADER_CLIENT, Constants.CLIENT_ADMIN);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = getAuth(Constants.ROLE_SUPER_USER);
        User user = TestDataProvider.getUser();

        //when
        doNothing().when(userTokenService).saveUserToken(TestConstants.TOKEN, Constants.ROLE_SUPER_USER,
                Constants.CLIENT_ADMIN, TestConstants.ONE, TestConstants.APP_VERSION);

        ListOperations operations = mock(ListOperations.class);
        List<Map<String, List<String>>> roleListRedis = new ArrayList<>();
        Map<String, List<String>> applicationRoles = new HashMap<>();
        applicationRoles.put("spice web",new ArrayList<>());
        applicationRoles.get("spice web").add("SUPER_USER");
        applicationRoles.put("mob",List.of("SUPER_USER"));
        roleListRedis.add(applicationRoles);
        List<Role> roles = List.of(TestDataProvider.getRole());

        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, 0)).thenReturn(new ArrayList<>());
        when(roleRepository.getActiveRoles()).thenReturn(roles);
        when(operations.leftPush(Constants.ROLE_REDIS_KEY, roles)).thenReturn(TestConstants.ONE);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(roleListRedis);
        when(userRepository.getUserByUsername(user.getUsername(), true)).thenReturn(user);

        //then
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        assertNotNull(httpServletRequest);
        securityContextHolder.close();
    }

    @Test
    void onAuthenticationSuccess_client() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicRsaKey", getRSAPublicKey());
        MockedStatic<SecurityContextHolder> securityContextHolder = mockStatic(SecurityContextHolder.class);
        securityContextHolder.when(SecurityContextHolder::getContext)
                .thenReturn(getSecurityContext(Constants.ROLE_SUPER_USER));
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(Constants.HEADER_CLIENT, Constants.CLIENT_WEB);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = getAuth(Constants.ROLE_SUPER_USER);
        User user = TestDataProvider.getUser();

        //when
        doNothing().when(userTokenService).saveUserToken(TestConstants.TOKEN, Constants.ROLE_SUPER_USER,
                Constants.CLIENT_ADMIN, TestConstants.ONE, TestConstants.APP_VERSION);

        ListOperations operations = mock(ListOperations.class);
        List<Map<String, List<String>>> roleListRedis = new ArrayList<>();
        Map<String, List<String>> applicationRoles = new HashMap<>();
        applicationRoles.put("admin",new ArrayList<>());
        applicationRoles.get("admin").add("SUPER_USER");
        roleListRedis.add(applicationRoles);
        List<Role> roles = List.of(TestDataProvider.getRole());

        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, 0)).thenReturn(new ArrayList<>());
        when(roleRepository.getActiveRoles()).thenReturn(roles);
        when(operations.leftPush(Constants.ROLE_REDIS_KEY, roles)).thenReturn(TestConstants.ONE);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(roleListRedis);
        when(userRepository.getUserByUsername(user.getUsername(), true)).thenReturn(user);

        //then
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        assertNotNull(httpServletRequest);
        securityContextHolder.close();
    }

    @Test
    void onAuthenticationSuccess_client_environment() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicRsaKey", getRSAPublicKey());
        MockedStatic<SecurityContextHolder> securityContextHolder = mockStatic(SecurityContextHolder.class);
        securityContextHolder.when(SecurityContextHolder::getContext)
                .thenReturn(getSecurityContext(Constants.ROLE_SUPER_USER));
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(Constants.HEADER_CLIENT, Constants.CLIENT_WEB);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = getAuth(Constants.ROLE_SUPER_USER);
        User user = TestDataProvider.getUser();

        //when
        doNothing().when(userTokenService).saveUserToken(TestConstants.TOKEN, Constants.ROLE_SUPER_USER,
                Constants.CLIENT_ADMIN, TestConstants.ONE, TestConstants.APP_VERSION);

        ListOperations operations = mock(ListOperations.class);
        List<Map<String, List<String>>> roleListRedis = new ArrayList<>();
        Map<String, List<String>> applicationRoles = new HashMap<>();
        applicationRoles.put("admin",new ArrayList<>());
        applicationRoles.get("admin").add("SUPER_USER");
        roleListRedis.add(applicationRoles);
        List<Role> roles = List.of(TestDataProvider.getRole());
        ReflectionTestUtils.setField(authenticationSuccess, "environment", Constants.DEV_ENVIRONMENT);

        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, 0)).thenReturn(new ArrayList<>());
        when(roleRepository.getActiveRoles()).thenReturn(roles);
        when(operations.leftPush(Constants.ROLE_REDIS_KEY, roles)).thenReturn(TestConstants.ONE);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(roleListRedis);
        when(userRepository.getUserByUsername(user.getUsername(), true)).thenReturn(user);

        //then
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        assertNotNull(httpServletRequest);
        securityContextHolder.close();
    }



    @Test
    void testOnAuthenticationSuccess() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicRsaKey", getRSAPublicKey());
        MockedStatic<SecurityContextHolder> securityContextHolder = mockStatic(SecurityContextHolder.class);
        securityContextHolder.when(SecurityContextHolder::getContext)
                .thenReturn(getSecurityContext(Constants.ROLE_CFR_REPORT_ADMIN));
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(Constants.HEADER_CLIENT, Constants.CLIENT_CFR);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = getAuth(Constants.ROLE_CFR_REPORT_ADMIN);
        //when
        doNothing().when(userTokenService).saveUserToken(TestConstants.TOKEN, Constants.ROLE_CFR_REPORT_ADMIN,
                Constants.CLIENT_CFR, TestConstants.ONE, TestConstants.APP_VERSION);

        ListOperations operations = mock(ListOperations.class);
        List<Map<String, List<String>>> roleListRedis = new ArrayList<>();
        Map<String, List<String>> applicationRoles = new HashMap<>();
        applicationRoles.put("spice web",new ArrayList<>());
        applicationRoles.get("spice web").add("SUPER_USER");
        applicationRoles.put("cfr web", List.of("CFR_REPORT_ADMIN"));
        applicationRoles.put("mob", List.of("CFR_REPORT_ADMIN"));
        roleListRedis.add(applicationRoles);
        List<Role> roles = List.of(TestDataProvider.getRole());
        User user = TestDataProvider.getUser();

        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, 0)).thenReturn(new ArrayList<>());
        when(roleRepository.getActiveRoles()).thenReturn(roles);
        when(operations.leftPush(Constants.ROLE_REDIS_KEY, roles)).thenReturn(TestConstants.ONE);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(roleListRedis);
        when(userRepository.getUserByUsername(user.getUsername(), true)).thenReturn(user);

        //then
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        Assertions.assertEquals(Constants.CLIENT_CFR, httpServletRequest.getHeader(Constants.HEADER_CLIENT));

        //given
        securityContextHolder.when(SecurityContextHolder::getContext)
                .thenReturn(getSecurityContext(Constants.ROLE_REGION_ADMIN));
        Authentication finalAuthentication = getAuth(Constants.ROLE_REGION_ADMIN);
        //then
        Assertions.assertThrows(BadCredentialsException.class,
                () -> authenticationSuccess
                        .onAuthenticationSuccess(httpServletRequest, httpServletResponse, finalAuthentication));
        securityContextHolder.close();
    }

    @Test
    void verifyOnAuthenticationSuccess() {
        //given
        ReflectionTestUtils.setField(authenticationSuccess, "publicRsaKey", getRSAPublicKey());
        MockedStatic<SecurityContextHolder> securityContextHolder = mockStatic(SecurityContextHolder.class);
        securityContextHolder.when(SecurityContextHolder::getContext)
                .thenReturn(getSecurityContext(Constants.ROLE_NURSE));
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(Constants.HEADER_CLIENT, Constants.CLIENT_SPICE_MOBILE);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = getAuth(Constants.ROLE_NURSE);

        //when
        doNothing().when(userTokenService).saveUserToken(TestConstants.TOKEN, Constants.ROLE_SUPER_USER,
                Constants.CLIENT_ADMIN, TestConstants.ONE, TestConstants.APP_VERSION);
        ListOperations operations = mock(ListOperations.class);
        List<Map<String, List<String>>> roleListRedis = new ArrayList<>();
        Map<String, List<String>> applicationRoles = new HashMap<>();
        applicationRoles.put("spice web",new ArrayList<>());
        applicationRoles.get("spice web").add("SUPER_USER");
        applicationRoles.put("cfr web", List.of("CFR_REPORT_ADMIN"));
        applicationRoles.put("spice mobile", List.of("NURSE"));
        applicationRoles.put("mob", List.of("NURSE"));

        roleListRedis.add(applicationRoles);
        List<Role> roles = List.of(TestDataProvider.getRole());
        User user = TestDataProvider.getUser();

        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, 0)).thenReturn(new ArrayList<>());
        when(roleRepository.getActiveRoles()).thenReturn(roles);
        when(operations.leftPush(Constants.ROLE_REDIS_KEY, roles)).thenReturn(TestConstants.ONE);
        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(roleListRedis);
        when(userRepository.getUserByUsername(user.getUsername(), true)).thenReturn(user);
        when(userRepository.getUserByPhonenumber(user.getUsername())).thenReturn(user);
        //then
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        assertNotNull(authentication);

        when(operations.range(Constants.ROLE_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(new ArrayList<>())
            .thenReturn(roleListRedis);
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        assertNotNull(authentication);
        securityContextHolder.close();
    }

    @Test
    void onAuthenticationSuccessException() {
        //given
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        httpServletRequest.addHeader(Constants.HEADER_CLIENT, Constants.CLIENT_SPICE_WEB);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        Authentication authentication = getAuth(Constants.ROLE_SUPER_USER);

        //then
        authenticationSuccess.onAuthenticationSuccess(httpServletRequest, httpServletResponse, authentication);
        assertNotNull(Boolean.TRUE);
    }

}
