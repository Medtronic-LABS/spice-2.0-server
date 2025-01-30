package com.mdtlabs.coreplatform.userservice.service;

import java.util.*;

import javax.xml.bind.DatatypeConverter;

import com.mdtlabs.coreplatform.commonservice.common.exception.InvalidPathException;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserPreferences;
import com.mdtlabs.coreplatform.commonservice.common.repository.UserTokenRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserPreferencesRepository;
import io.jsonwebtoken.JwtBuilder;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.Jwts.SIG;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.security.MacAlgorithm;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.userservice.apiinterface.ShortUrlInterface;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.springframework.data.domain.Sort.Direction.DESC;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.OrganizationUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.ForbiddenException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplate;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplateValue;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplateValues;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserSupervisor;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;
import com.mdtlabs.coreplatform.userservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.userservice.apiinterface.NotificationInterface;
import com.mdtlabs.coreplatform.userservice.mapper.UserMapper;
import com.mdtlabs.coreplatform.userservice.repository.UserRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserSupervisorRepository;
import com.mdtlabs.coreplatform.userservice.service.impl.OrganizationServiceImpl;
import com.mdtlabs.coreplatform.userservice.service.impl.UserServiceImpl;
import com.mdtlabs.coreplatform.userservice.util.TestConstants;
import com.mdtlabs.coreplatform.userservice.util.TestDataProvider;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserServiceTest {

    @InjectMocks
    UserServiceImpl userService;

    @Mock
    private UserRepository userRepository;

    @Mock
    private NotificationInterface notificationApiInterface;

    @Mock
    private OrganizationServiceImpl organizationService;

    @Mock
    private UserMapper userMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private RoleService roleService;

    @Mock
    private UserSupervisorRepository userSupervisorRepository;

    @Mock
    private OrganizationUtil organizationUtil;


    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private RedisTemplate<String, String> redisTemplate;

    @Mock
    private UserPreferencesRepository userPreferencesRepository;

    @Mock
    private UserTokenRepository userTokenRepository;

    @Mock
    private ShortUrlInterface shortUrlInterface;

    private static MockedStatic<UserContextHolder> userContextHolder;

    @Test
    void testAddUser() {
        User userRequest = TestDataProvider.getUser();
        User mockUser = TestDataProvider.getUser();

        userRequest.setUsername(null);
        assertThrows(BadRequestException.class, () -> userService.createUser(userRequest, true));

        userRequest.setUsername(TestConstants.USER_NAME);
        userRequest.setRoles(null);
        assertThrows(BadRequestException.class, () -> userService.createUser(userRequest, true));

        userRequest.setRoles(new HashSet<>());
        assertThrows(BadRequestException.class, () -> userService.createUser(userRequest, true));

        userRequest.setRoles(Set.of(TestDataProvider.getRole()));
        userRequest.setPhoneNumber(null);
        assertThrows(BadRequestException.class, () -> userService.createUser(userRequest, true));
        userRequest.setPhoneNumber(TestConstants.PHONE_NUMBER);
        userRequest.setCountryCode(TestConstants.COUNTRY_CODE);

        when(userRepository.getUserByUsernameOrPhoneNumber(userRequest.getUsername().toLowerCase(), userRequest.getPhoneNumber(), userRequest.getCountryCode())).thenReturn(mockUser);
        assertThrows(SpiceValidation.class, () -> userService.createUser(userRequest, true));

        when(userRepository.getUserByUsernameOrPhoneNumber(userRequest.getUsername().toLowerCase(), userRequest.getPhoneNumber(), userRequest.getCountryCode())).thenReturn(null);
        when(userRepository.save(userRequest)).thenReturn(mockUser);


        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        User userResponse = userService.createUser(userRequest, true);
        User userResponseNotValidating = userService.createUser(userRequest, false);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(userResponse);
        Assertions.assertNotNull(userResponseNotValidating);
    }

    @Test
    void testForgotPassword() {
        ReflectionTestUtils.setField(userService, "forgotPasswordtimeLimitInMinutes", 60);
        ReflectionTestUtils.setField(userService, "forgotPasswordCountLimit", 5);

        //given
        User user = TestDataProvider.getUser();

        //when
        when(userRepository.getUserByUsername(TestConstants.USER_NAME, Boolean.TRUE)).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> userService.forgetPassword(TestConstants.USER_NAME, Constants.SPICE, null, null));
        user.setForgetPasswordCount(6);
        user.setIsBlocked(true);
        when(userRepository.getUserByUsername(TestConstants.USER_NAME)).thenReturn(user);
        assertThrows(ForbiddenException.class, () -> userService.forgetPassword(TestConstants.USER_NAME, Constants.SPICE, null, null));

        EmailTemplate emailTemplate = new EmailTemplate();
        emailTemplate.setSubject("Welcome to our platform");
        emailTemplate.setBody("Dear [USERNAME], welcome!");
        EmailTemplateValue emailTemplateValue = new EmailTemplateValue();
        emailTemplateValue.setName("app_url_email");
        emailTemplate.setEmailTemplateValues(List.of(emailTemplateValue, new EmailTemplateValue()));
        ResponseEntity<EmailTemplate> emailTemplateResponse = null;
        user.setForgetPasswordCount(0);
        Boolean response = null;
        user.setIsBlocked(false);
        when(userRepository.getUserByUsername(TestConstants.USER_NAME, Boolean.TRUE)).thenReturn(user);
        emailTemplateResponse = ResponseEntity.ok(emailTemplate);
        when(notificationApiInterface.getEmailTemplate(any(), any())).thenReturn(emailTemplateResponse);
        response = userService.forgetPassword(TestConstants.USER_NAME, Constants.SPICE, null, null);
        Assertions.assertNotNull(response);

        user.setForgetPasswordCount(0);

        Map<String, Object> userInfo = new HashMap<>();
        userInfo.put(Constants.USERNAME, user.getUsername());
        when(userRepository.save(user)).thenReturn(user);
        response = userService.forgetPassword(TestConstants.USER_NAME, Constants.SPICE, null, null);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(Boolean.FALSE, response);

        // for sms send
        SMSTemplate template = new SMSTemplate();
        template.setBody("Welcome");
        SMSTemplateValues templateValue = new SMSTemplateValues();
        templateValue.setKey("url");
        template.setSmsTemplateValues(List.of(templateValue));
        ResponseEntity<SMSTemplate> smsTemplate = ResponseEntity.ok(template);
        ReflectionTestUtils.setField(userService, "appUrl", "localhost:80/");
        ReflectionTestUtils.setField(userService, "environment", "dev");
        ValueOperations operations = mock(ValueOperations.class);

        when(redisTemplate.opsForValue()).thenReturn(operations);
        when(notificationApiInterface.getSmsTemplateValues(Constants.FORGOT_PASSWORD_USER)).thenReturn(smsTemplate);

        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put(Constants.SHORT_URL, "http://shorturl");
        ResponseEntity<Map<String, Object>> responseEntity = new ResponseEntity<>(responseMap, HttpStatus.OK);
        when(shortUrlInterface.shortenURL(any())).thenReturn(responseEntity);
        user.setShortenUrl("shortenurl");
        response = userService.forgetPassword(TestConstants.USER_NAME, Constants.SPICE, Constants.SMS, "mob");
        Assertions.assertNotNull(response);
    }

    @Test
    void testSendEmailForUserCreation() {
        ReflectionTestUtils.setField(userService, "mailUser", "test@test.com");

        User user = new User();
        user.setUsername("testUser");
        boolean isFromCreation = true;
        String appType = "mockedAppType";

        EmailTemplate emailTemplate = new EmailTemplate();
        emailTemplate.setSubject("Welcome to our platform");
        emailTemplate.setBody("Dear [USERNAME], welcome!");
        EmailTemplateValue emailTemplateValue = new EmailTemplateValue();
        emailTemplateValue.setName("app_url_email");
        emailTemplate.setEmailTemplateValues(List.of(emailTemplateValue, new EmailTemplateValue()));
        Map<String, String> data = new HashMap<>();
        EmailDTO emailDto = new EmailDTO();
        EmailDTO expectedEmailDto = new EmailDTO();
        expectedEmailDto.setSubject("Welcome to our platform");

        ResponseEntity<EmailTemplate> emailTemplateResponse = null;
        assertThrows(SpiceValidation.class, () -> userService.sendEmail(user, true, appType));

        emailTemplateResponse = ResponseEntity.ok(null);
        when(notificationApiInterface.getEmailTemplate(any(), any())).thenReturn(emailTemplateResponse);
        assertThrows(SpiceValidation.class, () -> userService.sendEmail(user, true, appType));

        ResponseEntity<Boolean> isCreateOutbound = new ResponseEntity<Boolean>(true, HttpStatus.OK);
        emailTemplateResponse = ResponseEntity.ok(emailTemplate);
        when(notificationApiInterface.getEmailTemplate(any(), any())).thenReturn(emailTemplateResponse);


        data.put("app_url_email", "mockedJweToken");
        data.put("email", "testUser");
        when(userMapper.setUserCreationEmailTemplate(user, emailTemplate, emailDto, data)).thenReturn(expectedEmailDto);
        when(userMapper.setUserCreationEmailTemplate(any(), any(), any(), any())).thenReturn(expectedEmailDto);
        when(notificationApiInterface.createOutBoundEmail(any())).thenReturn(isCreateOutbound);
        userService.sendEmail(user, isFromCreation, appType);
        when(userMapper.setForgotPasswordEmailTemplate(any(), any(), any(), any(), any())).thenReturn(emailDto);
        userService.sendEmail(user, false, appType);
    }

    @Test
    void verifyJwtToken() {
        String requestToken = "yhgeqwfsdvbdfv";
        User user = TestDataProvider.getUser();

        when(userRepository.findByForgetPasswordToken(requestToken)).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> userService.verifyJwtToken(requestToken));

        MacAlgorithm signatureAlgorithm = SIG.HS256;
        byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(Constants.AES_KEY_TOKEN);
        Map<String, Object> userInfo = new HashMap<>();
        userInfo.put(Constants.USERNAME, user.getUsername());
        JwtBuilder jwt = Jwts.builder().claims(userInfo).
                signWith(Keys.hmacShaKeyFor(apiKeySecretBytes), signatureAlgorithm).id(user.getId().toString()).expiration(DateUtil.addMinutesToCurrentDate(60)).issuedAt(new Date()).issuer(Constants.ISSUER);
        String token = jwt.compact();
        user.setForgetPasswordToken(token);

        when(userRepository.findByForgetPasswordShortTokenAndIsDeletedFalseAndIsActiveTrue(requestToken)).thenReturn(user);

        User response = userService.verifyJwtToken(requestToken);
        Assertions.assertNotNull(response);

        jwt = Jwts.builder().claims(userInfo).
                signWith(Keys.hmacShaKeyFor(apiKeySecretBytes), signatureAlgorithm).id(user.getId().toString()).expiration(DateUtil.addMinutesToCurrentDate(-1)).issuedAt(new Date()).issuer(Constants.ISSUER);
        token = jwt.compact();
        user.setForgetPasswordToken(token);
        assertThrows(ForbiddenException.class, () -> userService.verifyJwtToken(requestToken));
    }

    @Test
    void testUpdatePassword() {
        String requestToken = "yhgeqwfsdvbdfv";
        Map<String, String> userInfo = new HashMap<>();
        User user = TestDataProvider.getUser();
        assertThrows(DataNotAcceptableException.class, () -> userService.updatePassword(requestToken, userInfo));

        when(userRepository.findByForgetPasswordToken(requestToken)).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> userService.verifyJwtToken(requestToken));

        MacAlgorithm signatureAlgorithm = SIG.HS256;
        byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(Constants.AES_KEY_TOKEN);
        Map<String, Object> userInfos = new HashMap<>();
        userInfo.put(Constants.PASSWORD, "GHJKNC6789");
        JwtBuilder jwt = Jwts.builder().claims(userInfos).
                signWith(Keys.hmacShaKeyFor(apiKeySecretBytes), signatureAlgorithm).id(user.getId().toString()).expiration(DateUtil.addMinutesToCurrentDate(60)).issuedAt(new Date()).issuer(Constants.ISSUER);
        String token = jwt.compact();
        user.setForgetPasswordToken(token);

        user.setPassword("GHJKNC6789");
        when(userRepository.findByForgetPasswordShortTokenAndIsDeletedFalseAndIsActiveTrue(requestToken)).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> userService.updatePassword(requestToken, userInfo));

        user.setPassword(null);
        when(userRepository.findByForgetPasswordToken(requestToken)).thenReturn(user);
        when(userRepository.save(user)).thenReturn(user);
        Boolean response = userService.updatePassword(requestToken, userInfo);

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Boolean.TRUE, response);
    }

    @Test
    void shouldChangeOldToNewPasswordSuccessfully() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setUserId(1L);
        requestDTO.setOldPassword("oldPassword");
        requestDTO.setNewPassword("newPassword");

        User user = new User();
        user.setId(1L);
        user.setPassword("oldPassword");

        UserDTO userDTO = new UserDTO();
        userDTO.setId(1L);

        when(userRepository.findById(1L)).thenReturn(Optional.of(user));

        userService.changeOldToNewPassword(requestDTO);

        verify(userRepository, times(1)).save(user);
        TestDataProvider.cleanUp();
    }

    @Test
    void shouldThrowExceptionWhenUserIdIsNull() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setOldPassword("oldPassword");
        requestDTO.setNewPassword("newPassword");

        assertThrows(DataNotAcceptableException.class, () -> userService.changeOldToNewPassword(requestDTO));
    }

    @Test
    void shouldThrowExceptionWhenOldPasswordIsNull() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setUserId(1L);
        requestDTO.setNewPassword("newPassword");

        assertThrows(DataNotAcceptableException.class, () -> userService.changeOldToNewPassword(requestDTO));
    }

    @Test
    void shouldThrowExceptionWhenNewPasswordIsNull() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setUserId(1L);
        requestDTO.setOldPassword("oldPassword");

        assertThrows(DataNotAcceptableException.class, () -> userService.changeOldToNewPassword(requestDTO));
    }

    @Test
    void shouldThrowExceptionWhenUserNotFound() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setUserId(1l);
        requestDTO.setOldPassword("oldPassword");
        requestDTO.setNewPassword("newPassword");

        when(userRepository.findById(1L)).thenReturn(Optional.empty());

        assertThrows(DataNotAcceptableException.class, () -> userService.changeOldToNewPassword(requestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void shouldThrowExceptionWhenOldPasswordDoesNotMatch() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setUserId(1L);
        requestDTO.setOldPassword("oldPassword");
        requestDTO.setNewPassword("newPassword");

        User user = new User();
        user.setId(1L);
        user.setPassword("differentOldPassword");

        when(userRepository.findById(1L)).thenReturn(Optional.of(user));

        assertThrows(DataConflictException.class, () -> userService.changeOldToNewPassword(requestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void shouldThrowExceptionWhenOldPasswordEqualsNewPassword() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setUserId(1L);
        requestDTO.setOldPassword("oldPassword");
        requestDTO.setNewPassword("oldPassword");

        User user = new User();
        user.setId(1l);
        user.setPassword("oldPassword");

        when(userRepository.findById(Long.valueOf(1L))).thenReturn(Optional.of(user));

        assertThrows(SpiceValidation.class, () -> userService.changeOldToNewPassword(requestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void testValidateUsers() {
        List<String> usernames = List.of("testuser");
        List<String> phoneNumbers = List.of("9858482492");
        UserRequestDTO userRequest = TestDataProvider.getUserRequestDTO();
        List<UserRequestDTO> users = List.of(userRequest);

        userRequest.setUsername(null);

        assertThrows(BadRequestException.class, () -> userService.validateUsers(users));

        userRequest.setUsername("testuser");
        userRequest.setPhoneNumber(null);
        assertThrows(BadRequestException.class, () -> userService.validateUsers(users));

        userRequest.setPhoneNumber("fkek");
        assertThrows(BadRequestException.class, () -> userService.validateUsers(users));

        userRequest.setPhoneNumber("9858482492");
        userRequest.setRoleIds(null);
        assertThrows(BadRequestException.class, () -> userService.validateUsers(users));

        userRequest.setRoleIds(new ArrayList<>());
        assertThrows(BadRequestException.class, () -> userService.validateUsers(users));
        userRequest.setRoleIds(List.of(1l));

        User user = TestDataProvider.getUser();
        user.setUsername("testuser");
        List<User> users2 = List.of(user);

        when(userRepository.getUsersByUsernameOrPhoneNumber(usernames, phoneNumbers)).thenReturn(users2);
        assertThrows(SpiceValidation.class, () -> userService.validateUsers(users));
    }

    @Test
    void testGetUsersByIds() {
        List<Long> ids = List.of(1l);
        List<User> users = List.of(TestDataProvider.getUser());

        when(userRepository.findByIdInAndIsDeletedAndIsActive(ids, false, true)).thenReturn(users);
        List<User> response = userService.getUsersByIds(ids);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(users, response);

        when(userRepository.findByIdInAndIsDeletedAndIsActive(ids, false, true)).thenReturn(null);
        response = userService.getUsersByIds(ids);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(new ArrayList<>(), response);
    }

    @Test
    void testGetUserById() {
        Long id = 1l;
        User user = TestDataProvider.getUser();
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> userService.getUserById(id));
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(user);

        User response = userService.getUserById(id);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(user, response);
    }

    @Test
    void testUpdateOrganizationUser() {
        UserRequestDTO request = TestDataProvider.getUserRequestDTO();
        assertThrows(DataNotAcceptableException.class, () -> userService.updateOrganizationUser(request));

    }

    @Test
    void testUnlockUser() {
        //given
        User userToUnlock = TestDataProvider.getUser();
        userToUnlock.setIsBlocked(Boolean.TRUE);
        User user = TestDataProvider.getUser();
        user.setIsBlocked(Constants.BOOLEAN_FALSE);
        user.setInvalidLoginAttempts(Constants.ZERO);
        user.setActive(Constants.BOOLEAN_TRUE);
        user.setBlockedDate(null);
        SearchRequestDTO commonRequestDTO = TestDataProvider.getSearchRequestDTO();

        //when
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(commonRequestDTO.getId())).thenReturn(userToUnlock);
        when(userRepository.save(userToUnlock)).thenReturn(user);

        //then
        Boolean isUserUnlocked = userService.unlockUser(commonRequestDTO);
        assertNotNull(isUserUnlocked);
    }

    @Test
    void unlockUserExceptions() {
        //given
        User userToUnlock = TestDataProvider.getUser();
        userToUnlock.setIsBlocked(Boolean.FALSE);
        SearchRequestDTO commonRequestDTO = TestDataProvider.getSearchRequestDTO();
        commonRequestDTO.setId(null);
        SearchRequestDTO secondCommonRequestDTO = TestDataProvider.getSearchRequestDTO();
        secondCommonRequestDTO.setId(TestConstants.TWO);
        SearchRequestDTO thirdCommonRequestDTO = TestDataProvider.getSearchRequestDTO();

        //when
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.TWO)).thenReturn(null);
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(thirdCommonRequestDTO.getId())).thenReturn(userToUnlock);

        //then
        assertThrows(DataNotAcceptableException.class, () -> userService.unlockUser(commonRequestDTO));
        assertThrows(DataNotFoundException.class, () -> userService.unlockUser(secondCommonRequestDTO));
        assertThrows(BadRequestException.class, () -> userService.unlockUser(thirdCommonRequestDTO));
    }

    @Test
    void toVerifySiteAdminValidateUser() {
        //given
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setEmail(TestConstants.USER_NAME);
        requestData.setChiefdomTenantId(10l);
        requestData.setHealthFacilityTenantId(TestConstants.FIVE);
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_CHP);
        user.getRoles().add(role);

        //when
        when(userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail())).thenReturn(user);
        when(userRepository.getUserByUsername(requestData.getEmail().toLowerCase()))
                .thenReturn(null);
        requestData.setTenantId(TestConstants.ONE);
        user.setTenantId(TestConstants.ONE);
        when(userRepository.getUserByUsername(requestData.getEmail().toLowerCase()))
                .thenReturn(user);

        //then
        assertThrows(DataConflictException.class, () -> userService.validateUser(requestData));
    }

    @Test
    void testUpdateSuperAdmin() {
        //given
        User user = TestDataProvider.getUser();
        UserSuperAdminDto userSuperAdminDto = TestDataProvider.getUserSuperAdminDto();

        //when
        when(userRepository.findById(userSuperAdminDto.getId())).thenReturn(Optional.of(user));
        when(userMapper.setSuperAdminUser(userSuperAdminDto, user)).thenReturn(user);
        when(userRepository.save(user)).thenReturn(user);

        //then
        userService.updateSuperAdmin(userSuperAdminDto);
        verify(userRepository, atLeastOnce()).findById(userSuperAdminDto.getId());
        verify(userMapper, atLeastOnce()).setSuperAdminUser(userSuperAdminDto, user);
        verify(userRepository, atLeastOnce()).save(user);
    }

    @Test
    void testCreateSuperAdmin() {
        //given
        User user = new User();
        user.setUsername(TestConstants.USER_NAME);
        Role role = TestDataProvider.getRole();
        user.setRoles(Set.of(role));
        user.setSuiteAccess(Set.of("Admin"));
        user.setForgetPasswordTime(new Date());

        List<UserRequestDTO> userRequestDTOs = List.of(TestDataProvider.getUserRequestDTO());

        List<User> userList = List.of(user);

        //when
        when(roleService.getRolesByIds(userRequestDTOs.get(0).getRoleIds())).thenReturn(Set.of(role));
        when(userRepository.saveAll(userList)).thenReturn(userList);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        //then
        userService.createSuperAdmin(userRequestDTOs);
        TestDataProvider.cleanUp();
        verify(roleService,atLeastOnce()).getRolesByIds(userRequestDTOs.get(0).getRoleIds());
    }

    @Test
    void saveAllUsers() {
        List<User> users = List.of(TestDataProvider.getUser());
        when(userRepository.saveAll(users)).thenReturn(users);
        userService.saveAllUsers(users);
        assertNotNull(users);
    }

    @Test
    void saveUser() {
        //given
        User user = TestDataProvider.getUser();

        //when
        when(userRepository.save(user)).thenReturn(user);

        //then
        User response = userService.saveUser(user);
        assertNotNull(response);
    }

    @Test
    void getUserByVillageIdsTest() {
        Set<Long> villageIds = Set.of(1l, 2l);
        List<User> user = List.of(TestDataProvider.getUser());
        when(userRepository.findAllByVillageIds(villageIds, TestConstants.ONE)).thenReturn(user);
        assertNotNull(userService.getUserByVillageIds(villageIds, TestConstants.ONE));
    }

    @Test
    void createUsers() {
        List<User> users = List.of(TestDataProvider.getUser());
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userRepository.saveAll(users)).thenReturn(users);
        List<User> response = userService.createUsers(users);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createUsersForExisting() {

        List<User> users = List.of(TestDataProvider.getUser());
        List<Long> existingIds = List.of(2l);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userRepository.saveAll(users)).thenReturn(users);
        List<User> response = userService.createUsers(users, existingIds);
        assertNotNull(response);

        existingIds = List.of(1l);
        response = userService.createUsers(users, existingIds);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void addOrganizationForUsers() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        List<User> users = List.of(TestDataProvider.getUser());
        Organization organization = TestDataProvider.getOrganization();
        List<Long> linkedSupervisorIds = List.of(2L);

        when(userRepository.saveAll(users)).thenReturn(users);
        when(userRepository.getUserByRoleOrId(Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT, List.of(organization.getId()), linkedSupervisorIds)).thenReturn(users);

        userService.addOrganizationForUsers(linkedSupervisorIds, organization, new ArrayList<>());

        linkedSupervisorIds = List.of(1L);
        userService.addOrganizationForUsers(linkedSupervisorIds, organization, new ArrayList<>());

        verify(userRepository, atLeastOnce()).saveAll(users);
        TestDataProvider.cleanUp();
    }

    @Test
    void getUserDetails() {
        Long id = 1l;
        User user = TestDataProvider.getUser();
        UserSupervisor userSupervisor = new UserSupervisor();

        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(user);

        UserResponseDTO response = userService.getUserDetails(id);
        assertNotNull(response);
        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(userSupervisor);

        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(userSupervisor.getSupervisorId())).thenReturn(user);

        response = userService.getUserDetails(id);
        assertNotNull(response);

    }

    @Test
    void updateOrganizationUser() {
        UserRequestDTO userRequest = TestDataProvider.getUserRequestDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        assertThrows(DataNotAcceptableException.class, () -> userService.updateOrganizationUser(userRequest));

        userRequest.setId(1l);
        User user = TestDataProvider.getUser();

        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(userRequest.getId())).thenReturn(user);
        doNothing().when(userMapper).setExistingUser(userRequest, user);
        user.setFhirId("23232");
        userRequest.setFhirId("23232");
        when(userRepository.save(user)).thenReturn(user);
        User response = userService.updateOrganizationUser(userRequest);
        when(fhirServiceApiInterface.updateUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), userRequest)).thenReturn(null);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getUsersByTenants() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        request.setTenantIds(List.of(TestConstants.ONE));
        request.setIsSiteUsers(Constants.BOOLEAN_TRUE);

        Page<User> userPage = new PageImpl<>(List.of(TestDataProvider.getUser()));
        Map<String, List<RoleResponseDTO>> roleGroup = Map.of(Constants.GROUP_NAME_SPICE, List.of(TestDataProvider.getRoleResponse(Constants.ROLE_REGION_ADMIN),
                TestDataProvider.getRoleResponse(Constants.ROLE_DISTRICT_ADMIN),
                TestDataProvider.getRoleResponse(Constants.ROLE_CHIEFDOM_ADMIN)));
        when(roleService.getRoleGroups(request)).thenReturn(roleGroup);
        when(userRepository.getUsers(any(), any(), any(), any(), any(), any())).thenReturn(userPage);

        ResponseListDTO<UserResponseDTO> response = userService.getUsersByTenants(request);
        assertNotNull(response);
        Sort.Order updatedAtOrder = new Sort.Order(DESC, Constants.U_UPDATED_AT);
        Sort.Order idOrder = new Sort.Order(DESC, Constants.USER_ID_PARAM);
        Sort sort = Sort.by(updatedAtOrder, idOrder);
        OrganizationDetailsDTO organizationDetailsDTO = new OrganizationDetailsDTO(TestConstants.ONE, Constants.FORM_NAME_CHIEFDOM, TestConstants.ONE, TestConstants.COUNTRY_NAME, TestConstants.ONE,
                TestConstants.ONE, TestConstants.ONE, TestConstants.USER_NAME, TestConstants.ONE, TestConstants.ONE, TestConstants.ONE, TestConstants.USER_NAME, TestConstants.ONE, TestConstants.ONE);
        organizationDetailsDTO.setFormName(Constants.FORM_NAME_CHIEFDOM);
        when(userRepository.getOrganizationsByUsers(List.of(1L), sort)).thenReturn(null);

        request.setUserBased(true);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        response = userService.getUsersByTenants(request);
        assertNotNull(response);

        request.setUserBased(false);
        request.setTenantBased(true);
        request.setTenantId(1l);
        response = userService.getUsersByTenants(request);
        assertNotNull(response);
        response = userService.getUsersByTenants(request);
        assertNotNull(response);
        response = userService.getUsersByTenants(request);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getUsersByTenants_commonUtil() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        ResponseListDTO<UserResponseDTO> response = userService.getUsersByTenants(request);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }


    @Test
    void userProfileUpdate() {
        UserRequestDTO requestDTO = TestDataProvider.getUserRequestDTO();
        assertThrows(DataNotAcceptableException.class, () -> userService.userProfileUpdate(requestDTO));
        requestDTO.setId(1L);
        User user = TestDataProvider.getUser();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(user);
        doNothing().when(userMapper).setExistingUser(requestDTO, user);
        when(userRepository.save(user)).thenReturn(user);

        User response = userService.userProfileUpdate(requestDTO);
        assertNotNull(response);

        user.setFhirId("1");
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(user);
        response = userService.userProfileUpdate(requestDTO);
        assertNotNull(response);

        TestDataProvider.cleanUp();
    }

    @Test
    void getUsersByIdsAsMap() {
        List<User> users = List.of(TestDataProvider.getUser());
        List<Long> ids = List.of(1l);

        when(userRepository.findByIdInAndIsDeletedAndIsActive(ids, false, true)).thenReturn(users);
        Map<Long, User> response = userService.getUsersByIdsAsMap(ids);
        assertNotNull(response);

        when(userRepository.findByIdInAndIsDeletedAndIsActive(ids, false, true)).thenReturn(null);
        response = userService.getUsersByIdsAsMap(ids);
        assertNotNull(response);
    }

    @Test
    void getRoleGroups() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        Map<String, List<RoleResponseDTO>> roles = new HashMap<>();
        when(roleService.getRoleGroups(request)).thenReturn(roles);

        Map<String, List<RoleResponseDTO>> response = userService.getRoleGroupes(request);
        assertNotNull(response);
    }

    @Test
    void getUserVillages() {
        Long id = 1l;
        User user = TestDataProvider.getUser();

        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(user);

        List<VillageDTO> response = userService.getUserVillages(id);
        assertNotNull(response);

        user.getVillages().add(TestDataProvider.getVillage());
        response = userService.getUserVillages(id);
        assertNotNull(response);
    }

    @Test
    void phoneNumberValidation() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        request.setPhoneNumber("9876554787");
        User user = null;
        when(userRepository.findByPhoneNumberAndIsDeletedFalse(request.getPhoneNumber())).thenReturn(user);
        assertDoesNotThrow(() -> userService.phoneNumberValidation(request));

    }

    @Test
    void phoneNumberValidationThrowAnExceptionWhenPhoneNumberIsNotValid() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        User user = TestDataProvider.getUser();
        request.setPhoneNumber(user.getPhoneNumber());
        when(userRepository.findByPhoneNumberAndCountryCodeAndIsDeletedFalse(request.getPhoneNumber(), request.getCountryCode())).thenReturn(user);
        assertThrows(DataConflictException.class, () -> userService.phoneNumberValidation(request));

        request.setPhoneNumber("HBW8");
        assertThrows(BadRequestException.class, () -> userService.phoneNumberValidation(request));
    }

    @Test
    void getPeerSupervisor() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        List<Long> tenantIds = List.of(1l);
        request.setTenantIds(tenantIds);
        Map<Long, List<Long>> tenantMap = Map.of(1l, List.of(2l, 3l));
        Set<Long> childTenantIds = Set.of(2l, 3l);
        List<User> users = List.of(TestDataProvider.getUser());
        when(userRepository.getUsersByRoleName(Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT, childTenantIds)).thenReturn(users);

        when(organizationUtil.getParentChildTenantMap()).thenReturn(tenantMap);
        List<UserResponseDTO> response = userService.getPeerSupervisor(request);
        assertNotNull(response);
    }

    @Test
    void testSetUserPassword() {
        //given
        Map<String, String> userInfo = new HashMap<>();

        assertThrows(DataNotAcceptableException.class, () -> {
            userService.resetUserPassword(TestConstants.TOKEN, userInfo);
        });

        assertThrows(DataNotAcceptableException.class, () -> {
            userService.resetUserPassword(TestConstants.TOKEN, userInfo);
        });

        userInfo.put(FieldConstants.PASSWORD, TestConstants.PASSWORD);

        //when
        when(userRepository.findByForgetPasswordToken(TestConstants.TOKEN)).thenReturn(null);

        //then
        assertThrows(DataNotFoundException.class, () -> {
            userService.setUserPassword(TestConstants.TOKEN, userInfo);
        });

        User user = TestDataProvider.getUser();
        userInfo.put(FieldConstants.PASSWORD, "spice123");
        MacAlgorithm signatureAlgorithm = SIG.HS256;
        byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(Constants.AES_KEY_TOKEN);
        userInfo.put(Constants.USERNAME, user.getUsername());
        JwtBuilder jwt = Jwts.builder().claims(userInfo).
                signWith(Keys.hmacShaKeyFor(apiKeySecretBytes), signatureAlgorithm).id(user.getId().toString()).expiration(DateUtil.addMinutesToCurrentDate(60)).issuedAt(new Date()).issuer(Constants.ISSUER);
        String token = jwt.compact();
        user.setForgetPasswordToken(token);
        user.setPassword(null);
        when(userRepository.findByForgetPasswordShortTokenAndIsDeletedFalseAndIsActiveTrue(TestConstants.TOKEN)).thenReturn(user);
        when(userService.verifyJwtToken(TestConstants.TOKEN)).thenReturn(user);

        Map<String, Object> response = userService.setUserPassword(TestConstants.TOKEN, userInfo);
        assertNotNull(response);
    }


    @Test
    void resetUserPassword() {
        User user = TestDataProvider.getUser();
        Map<String, String> userInfo = new HashMap<>();
        assertThrows(DataNotAcceptableException.class, () -> {
            userService.resetUserPassword(TestConstants.TOKEN, userInfo);
        });

        userInfo.put(FieldConstants.PASSWORD, "spice123");
        MacAlgorithm signatureAlgorithm = SIG.HS256;
        byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(Constants.AES_KEY_TOKEN);
        userInfo.put(Constants.USERNAME, user.getUsername());
        JwtBuilder jwt = Jwts.builder().claims(userInfo).
                signWith(Keys.hmacShaKeyFor(apiKeySecretBytes), signatureAlgorithm).id(user.getId().toString()).expiration(DateUtil.addMinutesToCurrentDate(60)).issuedAt(new Date()).issuer(Constants.ISSUER);
        String token = jwt.compact();
        user.setForgetPasswordToken(token);
        when(userRepository.findByForgetPasswordShortTokenAndIsDeletedFalseAndIsActiveTrue(TestConstants.TOKEN)).thenReturn(user);
        when(userService.verifyJwtToken(TestConstants.TOKEN)).thenReturn(user);

        Map<String, Object> response = userService.resetUserPassword(TestConstants.TOKEN, userInfo);
        assertNotNull(response);
    }

    @Test
    void deleteOrganizationUser() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setId(null);
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_CHW);
        user.setRoles(Set.of(role));
        user.setInsightUserOrganization(new ArrayList<>(List.of(TestDataProvider.getOrganization())));
        user.setReportUserOrganization(new ArrayList<>(List.of(TestDataProvider.getOrganization())));
        user.setInsightId(TestConstants.TEN);

        assertThrows(DataNotAcceptableException.class, () -> userService.deleteOrganizationUser(requestDTO));
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        requestDTO.setId(1l);
        requestDTO.setTenantIds(List.of(1l, 2l));
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(user);
        List<UserSupervisor> userSupervisor = new ArrayList<>(TestDataProvider.getUserSupervisor());
        when(userSupervisorRepository.findByUserIdAndIsDeletedAndIsActive(any(), anyBoolean(), anyBoolean())).thenReturn(userSupervisor);
        UserResponseDTO response = userService.deleteOrganizationUser(requestDTO);
        assertNotNull(response);

        requestDTO.setTenantIds(List.of(1l));
        Role roleName = TestDataProvider.getRole();
        roleName.setName(Constants.ROLE_SUPER_ADMIN);
        user.setRoles(Set.of(roleName));
        user.setTenantId(TestConstants.ONE);
        user.setOrganizations(TestDataProvider.getSetOrganizations());
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(user);
        when(userSupervisorRepository.findByUserIdAndIsDeletedAndIsActive(any(), anyBoolean(), anyBoolean())).thenReturn(userSupervisor);
        UserResponseDTO userResponse = userService.deleteOrganizationUser(requestDTO);
        assertNotNull(userResponse);

        role = TestDataProvider.getRole();
        role.setName(Constants.PEER_SUPERVISOR);
        user.setRoles(Set.of(role));
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(user);
        userSupervisor.get(0).setSupervisorId(TestConstants.TWO);
        when(userSupervisorRepository.findBySupervisorIdAndIsDeletedAndIsActive(any(), anyBoolean(), anyBoolean())).thenReturn(userSupervisor);
        userResponse = userService.deleteOrganizationUser(requestDTO);
        assertNotNull(userResponse);
        TestDataProvider.cleanUp();
    }

    @Test
    void shouldReturnAllMobileUsersWhenExist() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        List<User> mockUsers = new ArrayList<>();
        User mockUser = new User();
        mockUsers.add(mockUser);

        when(userRepository.findAllBySuiteAccess(any())).thenReturn(mockUsers);

        List<UserResponseDTO> response = userService.getAllMobileUsers();

        verify(userRepository, times(1)).findAllBySuiteAccess(any());
        TestDataProvider.cleanUp();
        assertNotNull(response);
        Assertions.assertEquals(1, response.size());
    }

    @Test
    void shouldReturnEmptyListWhenNoMobileUsersExist() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(userRepository.findAllBySuiteAccess(null)).thenReturn(Collections.emptyList());

        List<UserResponseDTO> response = userService.getAllMobileUsers();

        verify(userRepository, times(1)).findAllBySuiteAccess(null);
        TestDataProvider.cleanUp();
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void shouldReturnAllUsersByTenantIdWhenExist() {
        SearchRequestDTO mockRequest = new SearchRequestDTO();
        mockRequest.setTenantId(1L);
        List<User> mockUsers = new ArrayList<>();
        User mockUser = new User();
        mockUsers.add(mockUser);

        when(userRepository.findAllByTenantId(List.of(mockRequest.getTenantId()))).thenReturn(Collections.emptyList());

        List<UserResponseDTO> response = userService.getAllUsersByTenantId(mockRequest);

        verify(userRepository, times(1)).findAllByTenantId(List.of(mockRequest.getTenantId()));
        assertNotNull(response);
        assertEquals(0, response.size());
    }

    @Test
    void shouldReturnEmptyListWhenNoUsersByTenantIdExist() {
        SearchRequestDTO mockRequest = new SearchRequestDTO();
        mockRequest.setTenantId(1L);

        when(userRepository.findAllByTenantId(List.of(mockRequest.getTenantId()))).thenReturn(Collections.emptyList());

        List<UserResponseDTO> response = userService.getAllUsersByTenantId(mockRequest);

        verify(userRepository, times(1)).findAllByTenantId(List.of(mockRequest.getTenantId()));
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void shouldThrowDataNotFoundExceptionWhenTenantIdIsNull() {
        SearchRequestDTO mockRequest = new SearchRequestDTO();

        assertThrows(DataNotFoundException.class, () -> userService.getAllUsersByTenantId(mockRequest));
    }

    @Test
    void changeSiteUserPassword() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        assertThrows(DataNotAcceptableException.class, () -> userService.changeSiteUserPassword(request));

        request.setUserId(1L);
        request.setNewPassword("TestPassword");
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getUserId())).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> userService.changeSiteUserPassword(request));

        User user = TestDataProvider.getUser();
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getUserId())).thenReturn(user);
        userService.changeSiteUserPassword(request);
    }

    @Test
    void addPeerSupervisors() {
        List<Long> linkedSupervisorIds = List.of(1L);
        List<User> users = List.of(TestDataProvider.getUser());
        Organization organization = TestDataProvider.getOrganization();

        when(userRepository.findByIdIn(linkedSupervisorIds)).thenReturn(users);
        userService.addPeerSupervisors(linkedSupervisorIds, organization);

        organization.setId(2L);
        userService.addPeerSupervisors(linkedSupervisorIds, organization);
        verify(userRepository, atLeastOnce()).findByIdIn(linkedSupervisorIds);
    }

    @Test
    void createSuperAdmin() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        List<UserRequestDTO> requests = List.of(TestDataProvider.getUserRequestDTO());
        userService.createSuperAdmin(requests);
        assertDoesNotThrow(() -> userService.createSuperAdmin(requests));
        TestDataProvider.cleanUp();
    }

    @Test
    void removeSuperAdmin() {
        assertThrows(DataNotAcceptableException.class, () -> userService.removeSuperAdmin(null));

        User user = TestDataProvider.getUser();
        when(userRepository.findById(1L)).thenReturn(Optional.of(user));
        userService.removeSuperAdmin(1L);

        user.setDeleted(Boolean.TRUE);
        assertThrows(DataConflictException.class, () -> userService.removeSuperAdmin(1L));
        verify(userRepository, atLeastOnce()).findById(1L);
    }

    @Test
    void setUserPassword() {
        Map<String, String> userInfo = new HashMap<>();
        assertThrows(DataNotAcceptableException.class, () -> {
            userService.setUserPassword(TestConstants.TOKEN, userInfo);
        });
    }

    @Test
    void getSuperAdminUsers() {
        SearchRequestDTO searchRequest = TestDataProvider.getSearchRequestDTO();
        Pageable pageable = Pagination.setPagination(searchRequest.getSkip(), searchRequest.getLimit(), Constants.UPDATED_AT,
                Constants.BOOLEAN_FALSE);
        User user = TestDataProvider.getUser();
        Page<User> users = new PageImpl<>(List.of(user));

        when(userRepository.getSuperAdminUsers(Constants.ROLE_SUPER_ADMIN, searchRequest.getSearchTerm(), pageable)).thenReturn(users);

        ResponseListDTO<UserSuperAdminDto> response = userService.getSuperAdminUsers(searchRequest);
        assertNotNull(response);
    }

    @Test
    void deleteOrganizationUsers() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        List<User> users = List.of(TestDataProvider.getUser());

        when(userRepository.findByOrganizations_IdAndIsDeletedAndIsActive(request.getTenantId(), false, true)).thenReturn(users);

        userService.deleteOrganizationUsers(request);
        TestDataProvider.cleanUp();
        verify(userRepository, atLeastOnce()).findByOrganizations_IdAndIsDeletedAndIsActive(request.getTenantId(), false, true);
    }

    @Test
    void deleteOrganizationUsers_if() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        User user = TestDataProvider.getUser();
        user.setOrganizations(new HashSet<>());
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_CHP);
        List<User> users = List.of(user);

        when(userRepository.findByOrganizations_IdAndIsDeletedAndIsActive(request.getTenantId(), false, true)).thenReturn(users);

        userService.deleteOrganizationUsers(request);
        TestDataProvider.cleanUp();
        verify(userRepository, atLeastOnce()).findByOrganizations_IdAndIsDeletedAndIsActive(request.getTenantId(), false, true);
    }

    @Test
    void getUserProfile() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        User user = TestDataProvider.getUser();

        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(user.getId())).thenReturn(user);

        UserResponseDTO response = userService.getUserProfile();
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void validatePeerSupervisors() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        request.setIds(List.of(1L));
        List<UserSupervisor> userSupervisors = List.of(new UserSupervisor(1l, 2l));

        when(userSupervisorRepository
                .findBySupervisorIdInAndIsDeletedAndIsActive(request.getIds(),
                        false, true)).thenReturn(userSupervisors);

        userService.validatePeerSupervisors(request);
        verify(userSupervisorRepository,atLeastOnce()).findBySupervisorIdInAndIsDeletedAndIsActive(request.getIds(),false,true);
    }

    @Test
    void getPasswordResetUrl() {
        String url = "url";
        String originalUrl = "originalUrl";
        ValueOperations operations = mock(ValueOperations.class);
        operations.set(url, null);
        when(redisTemplate.opsForValue()).thenReturn(operations);
        assertThrows(DataNotFoundException.class, () -> userService.getPasswordResetUrl(url));

        when(operations.get(url)).thenReturn(originalUrl);
        Assertions.assertEquals(originalUrl, userService.getPasswordResetUrl(url));
    }


    @Test
    void testGetUserByRoleName() {
        //given
        List<User> users = TestDataProvider.getUsers();

        //when
        when(userRepository.getUsersByRole(TestConstants.ROLE_NAME)).thenReturn(users);

        //then
        List<User> userList = userService.getUserByRoleName(TestConstants.ROLE_NAME);
        Assertions.assertEquals(users.size(), userList.size());
    }

    @Test
    void toVerifySiteAdminValidateUsers() {
        //given
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setUserId(TestConstants.ONE);
        requestData.setEmail(TestConstants.USER_NAME);
        requestData.setNewPassword(TestConstants.PASSWORD);
        requestData.setParentOrganizationId(TestConstants.FIVE);
        requestData.setIgnoreTenantId(TestConstants.ZERO);
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_DISTRICT_ADMIN);
        Set<Role> roles = new HashSet<>();
        roles.add(role);
        UserResponseDTO userDTO = TestDataProvider.getUserResponseDTO();
        userDTO.setRoles(roles);
        UserContextDTO loggedUserDTO = new UserContextDTO();
        loggedUserDTO.setId(TestConstants.ONE);
        Role loggedUserRole = TestDataProvider.getRole();
        loggedUserRole.setName(Constants.ROLE_HEALTH_FACILITY_ADMIN);
        loggedUserDTO.setRoles(List.of(loggedUserRole));
        loggedUserDTO.setIsSuperUser(Boolean.FALSE);
        loggedUserDTO.setTenantId(TestConstants.ONE);
        MockedStatic<UserContextHolder> userContextHolderMockedStatic = mockStatic(UserContextHolder.class);

        //when
        userContextHolderMockedStatic.when(UserContextHolder::getUserDto).thenReturn(loggedUserDTO);
        when(userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail()))
                .thenReturn(user);
        doNothing().when(organizationService).validateParentOrganization(TestConstants.ONE, user);
        when(modelMapper.map(user, UserResponseDTO.class)).thenReturn(userDTO);
        //then
        UserResponseDTO userDTOResponse = userService.validateUser(requestData);
        Assertions.assertEquals(userDTO.getFirstName(), userDTOResponse.getFirstName());
        Assertions.assertEquals(user.getUsername(), userDTO.getUsername());
        assertNotNull(requestData);
        userContextHolderMockedStatic.close();
    }

   // @Test
    void toVerifySiteAdminValidateUserException() {
        //given
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setUserId(TestConstants.ONE);
        requestData.setEmail(TestConstants.USER_NAME);
        requestData.setNewPassword(TestConstants.PASSWORD);
        requestData.setParentOrganizationId(TestConstants.FIVE);
        requestData.setIgnoreTenantId(TestConstants.ZERO);
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_HEALTH_FACILITY_ADMIN);
        UserResponseDTO userDTO = TestDataProvider.getUserResponseDTO();
        UserContextDTO loggedUserDTO = new UserContextDTO();
        loggedUserDTO.setId(TestConstants.ONE);
        Role loggedUserRole = TestDataProvider.getRole();
        loggedUserRole.setName(Constants.ROLE_HEALTH_FACILITY_ADMIN);
        loggedUserDTO.setRoles(List.of(loggedUserRole));
        loggedUserDTO.setIsSuperUser(Boolean.FALSE);
        loggedUserDTO.setTenantId(TestConstants.ONE);
        MockedStatic<UserContextHolder> userContextHolderMockedStatic = mockStatic(UserContextHolder.class);

        //when
        userContextHolderMockedStatic.when(UserContextHolder::getUserDto).thenReturn(loggedUserDTO);
        when(userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail()))
                .thenReturn(user);
        doNothing().when(organizationService).validateParentOrganization(TestConstants.ONE, user);
        when(modelMapper.map(user, UserResponseDTO.class)).thenReturn(userDTO);
        //then
        assertThrows(InvalidPathException.class, () -> {
            userService.validateUser(requestData);
        });
        userContextHolderMockedStatic.close();
    }

    @Test
    void testValidateUser() {
        //given
        SearchRequestDTO requestData = TestDataProvider.getSearchRequestDTO();
        requestData.setIgnoreTenantId(TestConstants.ONE);
        User user = TestDataProvider.getUser();
        UserResponseDTO userDTO = TestDataProvider.getUserResponseDTO();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_DISTRICT_ADMIN);
        Set<Role> roles = new HashSet<>();
        roles.add(role);
        userDTO.setRoles(roles);
        UserContextDTO loggedUserDTO = new UserContextDTO();
        loggedUserDTO.setId(TestConstants.ONE);
        loggedUserDTO.setRoles(List.of(TestDataProvider.getRole()));
        loggedUserDTO.setIsSuperUser(Boolean.FALSE);
        loggedUserDTO.setTenantId(TestConstants.ONE);
        MockedStatic<UserContextHolder> userContextHolderMockedStatic;
        userContextHolderMockedStatic = mockStatic(UserContextHolder.class);

        //when
        userContextHolderMockedStatic.when(UserContextHolder::getUserDto).thenReturn(loggedUserDTO);
        when(userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail()))
                .thenReturn(user);
        when(modelMapper.map(user, UserResponseDTO.class)).thenReturn(userDTO);
        //then
        UserResponseDTO userDTOResponse = userService.validateUser(requestData);
        Assertions.assertEquals(userDTO.getFirstName(), userDTOResponse.getFirstName());
        Assertions.assertEquals(user.getUsername(), userDTO.getUsername());
        assertNotNull(user);

        //given
        requestData.setIgnoreTenantId(null);
        requestData.setParentOrganizationId(TestConstants.ONE);
        //then
        userDTOResponse = userService.validateUser(requestData);
        Assertions.assertEquals(userDTO.getFirstName(), userDTOResponse.getFirstName());
        Assertions.assertEquals(user.getUsername(), userDTO.getUsername());
        assertNotNull(user);
        userContextHolderMockedStatic.close();
    }


    @Test
    void testValidateUserWithNull() {
        //given
        SearchRequestDTO requestData = TestDataProvider.getSearchRequestDTO();
        User user = null;

        //when
        when(userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail()))
                .thenReturn(user);

        //then
        UserResponseDTO userDTO = userService.validateUser(requestData);
        Assertions.assertNull(userDTO);
    }

    @Test
    void toVerifyValidateUser() {
        //given
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setUserId(TestConstants.ONE);
        requestData.setEmail(TestConstants.USER_NAME);
        requestData.setNewPassword(TestConstants.PASSWORD);
        requestData.setParentOrganizationId(TestConstants.FIVE);
        requestData.setIgnoreTenantId(TestConstants.ZERO);
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_DISTRICT_ADMIN);
        Set<Role> roles = new HashSet<>();
        roles.add(role);
        UserResponseDTO userDTO = TestDataProvider.getUserResponseDTO();
        userDTO.setRoles(roles);
        UserContextDTO loggedUserDTO = new UserContextDTO();
        loggedUserDTO.setId(TestConstants.ONE);
        loggedUserDTO.setRoles(List.of(TestDataProvider.getRole()));
        loggedUserDTO.setIsSuperUser(Boolean.FALSE);
        loggedUserDTO.setTenantId(TestConstants.ONE);
        MockedStatic<UserContextHolder> userContextHolderMockedStatic = mockStatic(UserContextHolder.class);

        //when
        userContextHolderMockedStatic.when(UserContextHolder::getUserDto).thenReturn(loggedUserDTO);
        when(userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail()))
                .thenReturn(user);
        doNothing().when(organizationService).validateParentOrganization(TestConstants.ONE, user);
        when(modelMapper.map(user, UserResponseDTO.class)).thenReturn(userDTO);

        //then
        UserResponseDTO userDTOResponse = userService.validateUser(requestData);
        Assertions.assertEquals(userDTO.getFirstName(), userDTOResponse.getFirstName());
        Assertions.assertEquals(user.getUsername(), userDTO.getUsername());
        assertNotNull(requestData);
        userContextHolderMockedStatic.close();
    }

    @Test
    void checkDataConflictException() {
        //given
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setUserId(TestConstants.ONE);
        requestData.setEmail(TestConstants.USER_NAME);
        requestData.setNewPassword(TestConstants.PASSWORD);
        User user = TestDataProvider.getUser();
        UserDTO userDTO = TestDataProvider.getUserDTO();
        //when
        doReturn(user).when(userRepository).findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail());
        doReturn(userDTO).when(modelMapper).map(user, UserDTO.class);
        //then
        assertNotNull(requestData);
        assertThrows(DataConflictException.class, () -> userService.validateUser(requestData));

        //given
        requestData.setParentOrganizationId(5L);
        requestData.setIgnoreTenantId(TestConstants.TWO);
        //then
        assertNotNull(requestData);
        assertThrows(DataConflictException.class, () -> userService.validateUser(requestData));

    }


    @Test
    void getUserVillagesOfPeerSupervisorTest() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(userRepository.getUsersMappedToPeerSupervisor(UserContextHolder.getUserDto().getId())).thenReturn(null);
        User user = TestDataProvider.getUser();
        when(userRepository.getUsersMappedToPeerSupervisor(UserContextHolder.getUserDto().getId())).thenReturn(List.of(user));
        Assertions.assertNotNull(userService.getUserVillagesOfPeerSupervisor());

        TestDataProvider.cleanUp();
    }

    @Test
    void getUserVillagesOfPeerSupervisorWithPaginationTest() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        Pageable pageable = Pagination.setPagination(request.getSkip(), request.getLimit(),
                Constants.UPDATED_AT, Constants.BOOLEAN_FALSE);

        ObjectMapper objectMapper = new ObjectMapper();
        Map<String, Object> userMap = objectMapper.convertValue(TestDataProvider.getUser(), Map.class);
        Page<Map<String, Object>> users = new PageImpl<>(List.of(userMap));
        when(userRepository.getUsersMappedToPeerSupervisorWithPagination(request.getUserId(), request.getUserIds(), request.getVillageIds(), pageable)).thenReturn(users);

        List<UserVillageDTO> response = userService.getUserVillagesOfPeerSupervisorWithPagination(request);

        Assertions.assertNull(response);
    }

    @Test
    void saveUserPreferencesTest() {
        UserPreferencesDTO request = TestDataProvider.getUserPreferencesDTO();
        UserPreferences userPreferences = TestDataProvider.getUserPreferences();
        when(userPreferencesRepository.getPreferencesByType(request.getUserId(),
                Constants.PERFORMANCE_MONITORING)).thenReturn(userPreferences);
        when(userPreferencesRepository.save(userPreferences)).thenReturn(userPreferences);

        when(modelMapper.map(userPreferences, UserPreferencesDTO.class)).thenReturn(request);
        UserPreferencesDTO response = userService.saveUserPreferences(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getUserPreferencesByIdTest() {
        UserPreferences userPreferences = TestDataProvider.getUserPreferences();
        UserPreferencesDTO userPreferencesDTO = TestDataProvider.getUserPreferencesDTO();
        when(userPreferencesRepository.getPreferencesByType(1l,
                Constants.PERFORMANCE_MONITORING)).thenReturn(userPreferences);
        when(modelMapper.map(userPreferences, UserPreferencesDTO.class)).thenReturn(userPreferencesDTO);
        Assertions.assertNotNull(userService.getUserPreferencesById(1l));
    }

    @Test
    void updateUserTermsAndConditionDetailsById() {
        //given
        TestDataProvider.init();
        User user = TestDataProvider.getUser();
        User existingUser = TestDataProvider.getUser();
        existingUser.setIsTermsAndConditionsAccepted(Boolean.TRUE);

        //when
        TestDataProvider.getStaticMock();
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(user);
        when(userRepository.save(existingUser)).thenReturn(existingUser);

        //then
        userService.updateUserTermsAndConditionDetailsById();
        verify(userRepository, atLeastOnce()).findByIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateUserTermsAndConditionDetailsByIdException() {
        //given
        TestDataProvider.init();

        //when
        TestDataProvider.getStaticMock();
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(null);
        Assert.assertThrows(DataNotFoundException.class, () -> userService.updateUserTermsAndConditionDetailsById());

        //given
        User user = TestDataProvider.getUser();
        user.setIsTermsAndConditionsAccepted(Boolean.TRUE);

        //when
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(user);
        Assert.assertThrows(DataNotAcceptableException.class, () -> userService.updateUserTermsAndConditionDetailsById());
        TestDataProvider.cleanUp();
    }

    @Test
    void activateDeactivateUser() {
        TestDataProvider.init();
        List<Long> tenantIds = List.of(TestConstants.ONE);
        boolean isActive = Boolean.TRUE;
        List<User> users = TestDataProvider.getUsers();
        List<String> fhirUsers = List.of(TestConstants.USER_NAME);

        //when
        TestDataProvider.getStaticMock();
        when(userRepository.findUsersByTenantIds(tenantIds, !isActive)).thenReturn(users);
        when(userRepository.saveAll(users)).thenReturn(users);
        when(fhirServiceApiInterface.activateDeactivateUsers(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), fhirUsers, isActive)).thenReturn(new UserRequestDTO());

        //then
        Boolean response = userService.activateDeactivateUser(tenantIds, isActive);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getLockedUsers() {
        TestDataProvider.init();
        SearchRequestDTO requestObject = TestDataProvider.getSearchRequestDTO();
        requestObject.setSearchTerm(Constants.USER_SEARCH_TERM);
        //then
        Map<String, Object> result = userService.getLockedUsers(requestObject);
        Assertions.assertNotNull(result);
        TestDataProvider.cleanUp();
    }

    @Test
    void getLockedUsers_if() {
        SearchRequestDTO requestObject = TestDataProvider.getSearchRequestDTO();
        requestObject.setSearchTerm(Constants.USER_SEARCH_TERM);
        requestObject.setRoleType(Constants.ROLE_HEALTH_FACILITY_ADMIN);
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(123L);
        User user = new User();
        //then
        userContextHolder = mockStatic(UserContextHolder.class);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(123L)).thenReturn(user);

        Map<String, Object> result = userService.getLockedUsers(requestObject);
        Assertions.assertNotNull(result);
        userContextHolder.close();
    }

    @Test
    void getLockedUsers_elseIf() {
        SearchRequestDTO requestObject = TestDataProvider.getSearchRequestDTO();
        requestObject.setSearchTerm(Constants.USER_SEARCH_TERM);
        requestObject.setTenantId(null);
        requestObject.setRoleType(null);

        //then
        Map<String, Object> result = userService.getLockedUsers(requestObject);
        Assertions.assertNotNull(result);
    }

    @Test
    void getLockedUsers_else() {
        SearchRequestDTO requestObject = TestDataProvider.getSearchRequestDTO();
        requestObject.setSearchTerm(Constants.USER_SEARCH_TERM);
        Organization organization = TestDataProvider.getOrganization();
        User user = TestDataProvider.getUser();
        Page<User> lockedUsers =  new PageImpl<User>(List.of(user));
        Pageable pageable = Pagination.setPagination(requestObject.getSkip(), requestObject.getLimit(),
                Constants.UPDATED_AT, Constants.BOOLEAN_FALSE);
        when(organizationService.getOrganizationById(requestObject.getTenantId())).thenReturn(organization);
        when(userRepository.getLockedUsers("searchTerm", TestDataProvider.getSearchRequestDTO().getTenantIds(), pageable )).thenReturn(lockedUsers);
        //then
        Map<String, Object> result = userService.getLockedUsers(requestObject);
        Assertions.assertNotNull(result);
    }

    @Test
    void getUserListByRole() {
        TestDataProvider.init();
        CommonRequestDTO requestDto = new CommonRequestDTO();
        List<Long> roleIds = List.of(TestDataProvider.getRole().getId());
        List<User> users = TestDataProvider.getUsers();
        //when
        when(userRepository.findUsersByRoleIdS(roleIds, requestDto.getTenantId(), requestDto.getSearchTerm())).thenReturn(users);
        //then
        List<User> result = userService.getUserListByRole(requestDto);
        Assertions.assertNotNull(result);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateSuperAdmin_ThrowsException() {
        UserSuperAdminDto userDto = new UserSuperAdminDto();
        userDto.setId(null);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> userService.updateSuperAdmin(userDto));
    }

}
