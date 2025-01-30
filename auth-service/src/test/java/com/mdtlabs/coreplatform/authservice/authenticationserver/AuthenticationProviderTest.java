package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.test.util.ReflectionTestUtils;

import com.mdtlabs.coreplatform.authservice.common.TestCommonMethods;
import com.mdtlabs.coreplatform.authservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.authservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.authservice.repository.UserRepository;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.TimezoneDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;

/**
 * <p>
 * This class has the test methods for AuthenticationProvider class.
 * </p>
 *
 * @author Karthick M created on Jan 30, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AuthenticationProviderTest {

    @InjectMocks
    AuthenticationProvider authenticationProvider;

    @Mock
    private UserRepository userRepository;

    @Mock
    private RoleRepository roleRepository;

    @Test
    void authenticate() {
        //given
        Authentication authentication = mock(Authentication.class);
        Timezone timezone = new Timezone();
        timezone.setOffset("00:00:00");
        TimezoneDTO timezoneDTO = new TimezoneDTO();
        timezoneDTO.setOffset("00:00:00");
        List<Role> roles = List.of(TestDataProvider.getRole());
        User user = TestDataProvider.getUser();
        user.setTimezone(timezone);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setIsBlocked(Boolean.FALSE);
        user.setBlockedDate(null);
        Set<GrantedAuthority> authorityList = user.getAuthorities();
        Authentication expectedAuthentication = new UsernamePasswordAuthenticationToken(user, user.getPassword(), authorityList);

        //when
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);
        when(userRepository.save(user)).thenReturn(user);
        when(roleRepository.getActiveRoles()).thenReturn(roles);
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        Authentication actualAuthentication = authenticationProvider.authenticate(authentication);
        assertNotNull(actualAuthentication);
        assertEquals(expectedAuthentication, actualAuthentication);

        //when
        user.setIsBlocked(Boolean.TRUE);
        user.setPassword("NewPassword");
        LocalDate localDate = LocalDate.of(2023, 3, 8);
        Date invalidLoginDate = java.sql.Date.valueOf(localDate);
        user.setInvalidLoginTime(invalidLoginDate);
        ReflectionTestUtils.setField(authenticationProvider, "loginTimeLimitInMinutes", 2);
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));

        //when
        user.setPassword(null);
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);

        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));

        //when
        user.setIsBlocked(Boolean.FALSE);
        user.setPassword("NewPassword");
        user.setInvalidLoginTime(null);
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));

        //when
        user.setIsBlocked(Boolean.TRUE);
        user.setInvalidLoginTime(invalidLoginDate);
        ReflectionTestUtils.setField(authenticationProvider, "loginTimeLimitInMinutes", -1);
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);

        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));

        //when
        user.setIsBlocked(Boolean.FALSE);
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 1);

        //when
        user.setInvalidLoginAttempts(1);
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 2);
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);

        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));

        user.setInvalidLoginAttempts(1);
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 3);
        when(userRepository.getUserByUsername("superuser@test.com")).thenReturn(user);

        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));

    }

    @Test
    void isLoginWithinTimeLimit() {
        //given
        LocalDate localDate = LocalDate.of(2023, 3, 8);
        Date date = java.sql.Date.valueOf(localDate);
        ReflectionTestUtils.setField(authenticationProvider, "loginTimeLimitInMinutes", 2);
        User user = new User();
        user.setIsBlocked(Boolean.TRUE);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setBlockedDate(date);
        TestCommonMethods.init();

        //then
        TestCommonMethods.getStaticMock();
        boolean actualResponse = authenticationProvider.isLoginWithinTimeLimit(user);
        TestCommonMethods.cleanUp();
        assertTrue(actualResponse);
    }

    @Test
    void supports() {
        //given
        Class<String> authentication = String.class;

        //then
        boolean actual = authenticationProvider.supports(authentication);
        assertFalse(actual);
    }

    @Test
    void authenticateException() {
        //given
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 7);
        Authentication authentication = mock(Authentication.class);
        Timezone timezone = new Timezone();
        timezone.setOffset("00:00:00");
        TimezoneDTO timezoneDTO = new TimezoneDTO();
        timezoneDTO.setOffset("00:00:00");
        User user = TestDataProvider.getUser();
        user.setPassword(Constants.USER_DATA);
        user.setTimezone(null);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setIsBlocked(Boolean.FALSE);
        user.setBlockedDate(null);

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        when(userRepository.save(user)).thenReturn(user);
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
        TestCommonMethods.cleanUp();
    }

    @Test
    void throwBadCredentialsException() {
        //given
        Authentication authentication = mock(Authentication.class);
        Timezone timezone = new Timezone();
        timezone.setOffset("00:00:00");
        TimezoneDTO timezoneDTO = new TimezoneDTO();
        timezoneDTO.setOffset("00:00:00");
        Role role = TestDataProvider.getRole();
        role.setName(null);
        Set<Role> roles = Set.of(role);
        User user = TestDataProvider.getUser();
        user.setTimezone(timezone);
        user.setRoles(roles);
        user.setIsBlocked(false);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setIsBlocked(Boolean.FALSE);
        user.setBlockedDate(null);

        //when
        when(userRepository.save(user)).thenReturn(user);
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void usernamePasswordBlankException() {
        //given
        Authentication authentication = mock(Authentication.class);

        //when
        when(authentication.getCredentials()).thenReturn("");
        when(authentication.getPrincipal()).thenReturn("");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void usernameBlankException() {
        //given
        Authentication authentication = mock(Authentication.class);

        //when
        when(authentication.getCredentials()).thenReturn("   ");
        when(authentication.getPrincipal()).thenReturn("   ");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void nullPasswordException() {
        //given
        Authentication authentication = mock(Authentication.class);
        User user = TestDataProvider.getUser();
        user.setPassword(null);

        //then
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void loginWithinTimeLimitTrueException() {
        //given
        ReflectionTestUtils.setField(authenticationProvider, "loginTimeLimitInMinutes", 7);
        LocalDate localDate = LocalDate.of(2023, 3, 8);
        Date date = java.sql.Date.valueOf(localDate);
        Authentication authentication = mock(Authentication.class);
        Timezone timezone = new Timezone();
        timezone.setOffset("00:00:00");
        TimezoneDTO timezoneDTO = new TimezoneDTO();
        timezoneDTO.setOffset("00:00:00");
        User user = TestDataProvider.getUser();
        user.setTimezone(timezone);
        user.setIsBlocked(Boolean.TRUE);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setBlockedDate(date);
        TestCommonMethods.init();

        //when
        TestCommonMethods.getStaticMock();
        when(userRepository.save(user)).thenReturn(user);
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void updateAccBLockStatusTrueException() {
        //given
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 5);
        Authentication authentication = mock(Authentication.class);
        Timezone timezone = new Timezone();
        timezone.setOffset("00:00:00");
        TimezoneDTO timezoneDTO = new TimezoneDTO();
        timezoneDTO.setOffset("00:00:00");
        User user = TestDataProvider.getUser();
        user.setPassword(Constants.USER_DATA);
        user.setTimezone(timezone);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setIsBlocked(Boolean.FALSE);
        user.setBlockedDate(null);
        User updatedUser = TestDataProvider.getUser();
        updatedUser.setPassword(Constants.USER_DATA);
        updatedUser.setTimezone(timezone);
        updatedUser.setInvalidLoginAttempts(5);
        updatedUser.setIsBlocked(Boolean.FALSE);
        updatedUser.setBlockedDate(null);

        //when
        when(userRepository.save(updatedUser)).thenReturn(user);
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void updateAccBLockStatusException() {
        //given
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 3);
        Authentication authentication = mock(Authentication.class);
        Timezone timezone = new Timezone();
        timezone.setOffset("00:00:00");
        TimezoneDTO timezoneDTO = new TimezoneDTO();
        timezoneDTO.setOffset("00:00:00");
        User user = TestDataProvider.getUser();
        user.setPassword(Constants.USER_DATA);
        user.setTimezone(timezone);
        user.setInvalidLoginAttempts(Constants.FOUR);
        user.setIsBlocked(Boolean.FALSE);
        user.setBlockedDate(null);

        User updatedUser = TestDataProvider.getUser();
        updatedUser.setPassword(Constants.USER_DATA);
        updatedUser.setTimezone(timezone);
        updatedUser.setInvalidLoginAttempts(5);
        updatedUser.setIsBlocked(Boolean.FALSE);
        updatedUser.setBlockedDate(null);

        //when
        when(userRepository.save(updatedUser)).thenReturn(user);
        when(authentication.getCredentials()).thenReturn("spice123");
        when(authentication.getPrincipal()).thenReturn("superuser@test.com");

        //then
        assertThrows(BadCredentialsException.class, () -> authenticationProvider.authenticate(authentication));
    }

    @Test
    void isLoginLimitExceed() {
        //given
        User user = TestDataProvider.getUser();
        user.setInvalidLoginTime(new Date());
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 0);

        boolean response = authenticationProvider.isLoginLimitExceed(user);
        assertTrue(!response);
    }

    @Test
    void isLoginLimitExceed_returnFalse() {
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 0);
        boolean response = authenticationProvider.isLoginLimitExceed(null);
        assertFalse(response);
    }

    @Test
    void isLoginLimitExceed_else() {
        //given
        User user = TestDataProvider.getUser();
        user.setInvalidLoginTime(new Date());
        ReflectionTestUtils.setField(authenticationProvider, "loginTimeLimitInMinutes", 100);
        boolean response = authenticationProvider.isLoginLimitExceed(user);
        assertTrue(response);
    }

    @Test
    void isLoginLimitExceed_elseIf() {
        //given
        User user = TestDataProvider.getUser();
        user.setInvalidLoginTime(new Date());
        user.setInvalidLoginAttempts(Constants.ZERO);
        ReflectionTestUtils.setField(authenticationProvider, "loginTimeLimitInMinutes", 100);
        ReflectionTestUtils.setField(authenticationProvider, "loginCountLimit", 100);
        boolean response = authenticationProvider.isLoginLimitExceed(user);
        assertFalse(response);
    }
}
