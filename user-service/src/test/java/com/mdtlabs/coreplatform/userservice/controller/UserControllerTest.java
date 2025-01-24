package com.mdtlabs.coreplatform.userservice.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.userservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.userservice.service.UserService;
import com.mdtlabs.coreplatform.userservice.util.TestConstants;
import com.mdtlabs.coreplatform.userservice.util.TestDataProvider;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserControllerTest {

    @InjectMocks
    private UserController userController;

    @Mock
    private UserService userService;

    @Mock
    private ModelMapper modelMapper;

    private List<User> users = TestDataProvider.getUsers();

    @Test
    void getUserProfileById() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        SuccessResponse<UserResponseDTO> response = userController.getUserProfileById();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
        TestDataProvider.cleanUp();

        response = userController.getUserProfileById();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    //@Test
    void createUser() {
        UserDTO request = new UserDTO();
        User user = new User();
        when(userService.createUser(user, true)).thenReturn(null);
        SuccessResponse<UserDTO> response = userController.createUser(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void forgotPasswordValidation() {
        String username = "";
        String appType = "";

        when(userService.forgetPassword(username, appType, null, Constants.CLIENT_ADMIN)).thenReturn(true);
        SuccessResponse<String> response = userController.forgotPasswordValidation(username, appType, Constants.CLIENT_ADMIN);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());


        when(userService.forgetPassword(username, appType, null, null)).thenReturn(false);
        response = userController.forgotPasswordValidation(username, appType,Constants.CLIENT_ADMIN);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

        appType = Constants.CLIENT_SPICE_MOBILE;
        when(userService.forgetPassword(username, appType, null, Constants.CLIENT_ADMIN)).thenReturn(true);
        response = userController.forgotPasswordValidation(username, appType, Constants.CLIENT_ADMIN);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void verifyJwtToken() {
        User user = new User();
        String token = "";
        when(userService.verifyJwtToken(token)).thenReturn(user);
        SuccessResponse<Map<String, Object>> response = userController.verifyJwtToken(token);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void setUserPassword() {
        String token = "";
        Map<String, String> userInfo = new HashMap<>();
        when(userService.updatePassword(token, userInfo)).thenReturn(true);
        SuccessResponse<Boolean> response = userController.setUserPassword(token, userInfo);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getUserById() {
        Long id = 1L;
        UserResponseDTO user = new UserResponseDTO();
        when(userService.getUserDetails(id)).thenReturn(user);

        SuccessResponse<UserResponseDTO> response = userController.getUserById(id);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void validateUser() {
        SearchRequestDTO request = new SearchRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();
        when(userService.validateUser(request)).thenReturn(userResponse);

        SuccessResponse<UserResponseDTO> response = userController.validateUser(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void searchUser() {
        SearchRequestDTO request = new SearchRequestDTO();
        ResponseListDTO<UserResponseDTO> usersResponse = new ResponseListDTO<>();
        when(userService.getUsersByTenants(request)).thenReturn(usersResponse);

        SuccessResponse<UserResponseDTO> response = userController.searchUser(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

        usersResponse.setTotalCount(0L);
        response = userController.searchUser(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

        usersResponse.setTotalCount(1L);
        response = userController.searchUser(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void getSuperAdminUsers() {
        SearchRequestDTO request = new SearchRequestDTO();
        ResponseListDTO<UserSuperAdminDto> usersResponse = new ResponseListDTO<>();

        when(userService.getSuperAdminUsers(request)).thenReturn(usersResponse);
        SuccessResponse<UserSuperAdminDto> response = userController.getSuperAdminUsers(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void removeSuperAdminRoleForUser() {
        long id = 1L;
        doNothing().when(userService).removeSuperAdmin(id);

        SuccessResponse<String> response = userController.removeSuperAdminRoleForUser(id);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateSuperAdminUser() {
        UserSuperAdminDto request = new UserSuperAdminDto();
        doNothing().when(userService).updateSuperAdmin(request);

        SuccessResponse<String> response = userController.updateSuperAdminUser(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void unlockUser() {
        SearchRequestDTO requestDto = new SearchRequestDTO();

        when(userService.unlockUser(requestDto)).thenReturn(true);

        SuccessResponse<String> response = userController.unlockUser(requestDto);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createSuperAdminUser() {
        List<UserRequestDTO> user = new ArrayList<>();
        doNothing().when(userService).createSuperAdmin(user);

        SuccessResponse<String> response = userController.createSuperAdminUser(user);
        Assertions.assertNotNull(response);
    }

    @Test
    void resetUserPassword() {
        String token = "";
        Map<String, String> userInfo = new HashMap<>();
        Map<String, Object> map = new HashMap<>();

        when(userService.resetUserPassword(token, userInfo)).thenReturn(map);
        SuccessResponse<Map<String, Boolean>> response = userController.resetUserPassword(token, userInfo);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getRolesMeta() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        Map<String, List<RoleResponseDTO>> roles = new HashMap<>();
        when(userService.getRoleGroupes(request)).thenReturn(roles);

        SuccessResponse<Map<String, List<RoleResponseDTO>>> response = userController.getRolesMeta(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getUserVillages() {
        Long userId = 1L;
        User user = new User();
        when(userService.getUserById(userId)).thenReturn(user);

        ResponseEntity<UserResponseDTO> response = userController.getUserVillages(userId);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void validatePhoneNumberForUser() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        doNothing().when(userService).phoneNumberValidation(requestDTO);

        SuccessResponse<Boolean> response = userController.validatePhoneNumberForUser(requestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getPeerSupervisors() {
        SearchRequestDTO request = new SearchRequestDTO();
        List<UserResponseDTO> userResponses = new ArrayList<>();
        when(userService.getPeerSupervisor(request)).thenReturn(userResponses);

        SuccessResponse<UserResponseDTO> response = userController.getPeerSupervisors(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void getPeerSupervisorsByTenant() {
        SearchRequestDTO request = new SearchRequestDTO();
        List<UserResponseDTO> userResponses = new ArrayList<>();
        when(userService.getPeerSupervisorByTenants(request)).thenReturn(userResponses);

        ResponseEntity<List<UserResponseDTO>> response = userController.getPeerSupervisorsByTenant(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void shouldReturnAllMobileUsersWhenExist() {
        List<UserResponseDTO> mockUsers = new ArrayList<>();
        UserResponseDTO mockUser = new UserResponseDTO();
        mockUsers.add(mockUser);

        when(userService.getAllMobileUsers()).thenReturn(mockUsers);

        SuccessResponse<UserResponseDTO> response = userController.getAllMobileUsers();

        verify(userService, times(1)).getAllMobileUsers();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shouldReturnEmptyListWhenNoMobileUsersExist() {
        when(userService.getAllMobileUsers()).thenReturn(Collections.emptyList());

        SuccessResponse<UserResponseDTO> response = userController.getAllMobileUsers();

        verify(userService, times(1)).getAllMobileUsers();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shouldReturnAllMobileUsersByTenantIdWhenExist() {
        SearchRequestDTO mockRequest = new SearchRequestDTO();
        List<UserResponseDTO> mockUsers = new ArrayList<>();
        UserResponseDTO mockUser = new UserResponseDTO();
        mockUsers.add(mockUser);

        when(userService.getAllUsersByTenantId(mockRequest)).thenReturn(mockUsers);

        SuccessResponse<UserResponseDTO> response = userController.getAllUsersByTenantId(mockRequest);

        verify(userService, times(1)).getAllUsersByTenantId(mockRequest);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shouldReturnEmptyListWhenNoMobileUsersByTenantIdExist() {
        SearchRequestDTO mockRequest = new SearchRequestDTO();

        mockRequest.setTenantId(1L);
        when(userService.getAllUsersByTenantId(mockRequest)).thenReturn(Collections.emptyList());

        SuccessResponse<UserResponseDTO> response = userController.getAllUsersByTenantId(mockRequest);

        verify(userService, times(1)).getAllUsersByTenantId(mockRequest);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    
    @Test
    void userProfileUpdate() {
        UserRequestDTO userRequestDTO = new UserRequestDTO();
        User user = TestDataProvider.getUser();
        
        when(userService.userProfileUpdate(userRequestDTO)).thenReturn(user);
        
        SuccessResponse<String> response = userController.userProfileUpdate(userRequestDTO);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    
    @Test
    void changePassword() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        
        doNothing().when(userService).changeOldToNewPassword(requestDTO);
        
        SuccessResponse<Boolean> response = userController.changePassword(requestDTO);
        
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    
    @Test
    void changeSiteUserPassword() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        
        doNothing().when(userService).changeSiteUserPassword(requestDTO);
        
        SuccessResponse<Boolean> response = userController.changeSiteUserPassword(requestDTO);
        
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    
    @Test
    void validatePeerSupervisors() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        
        doNothing().when(userService).validatePeerSupervisors(requestDTO);
        
        ResponseEntity<Boolean> response = userController.validatePeerSupervisors(requestDTO);
        
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    
    @Test
    void forgotPasswordSmsValidation() {
        String userName = TestConstants.USER_NAME;
        String appType = "appType";
        
        when(userService.forgetPassword(userName, appType, Constants.SMS, null)).thenReturn(true);
        
        SuccessResponse<String> response = userController.forgotPasswordSmsValidation(userName, null);
        
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
        
    }
    

    @Test
    void testGetUserByRoleName() {
        //given
        List<UserResponseDTO> userResponseDTOs = TestDataProvider.getUserResponseDTOS();

        //when
        when(userService.getUserByRoleName(TestConstants.ROLE_NAME)).thenReturn(users);
        when(modelMapper.map(users, new TypeToken<List<UserResponseDTO>>() {
        }.getType())).thenReturn(userResponseDTOs);

        //then
        ResponseEntity<List<UserResponseDTO>> responseEntity = userController.getUserByRoleName(TestConstants.ROLE_NAME);
        Assertions.assertNotNull(users);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void toVerifyGetUserByRoleName() {
        //when
        when(userService.getUserByRoleName(TestConstants.ROLE_NAME)).thenReturn(null);

        //then
        ResponseEntity<List<UserResponseDTO>> responseEntity = userController.getUserByRoleName(TestConstants.ROLE_NAME);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetUserByRoleNameIsEmpty() {
        //given
        List<User> userList = new ArrayList<>();

        //when
        when(userService.getUserByRoleName(TestConstants.ROLE_NAME)).thenReturn(userList);

        //then
        ResponseEntity<List<UserResponseDTO>> responseEntity = userController.getUserByRoleName(TestConstants.ROLE_NAME);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    
    @Test
    void resetPasswordTest() {
        String url = TestConstants.URL;
        String originalUrl = TestConstants.ORIGINAL_URL;
        when(userService.getPasswordResetUrl(url)).thenReturn(originalUrl);

        ResponseEntity<Void> response = userController.resetPassword(url);

        Assertions.assertEquals(HttpStatus.FOUND,response.getStatusCode());

    }

    @Test
    void getPeerSupervisorLinkedUsersTest() {
        when(userService.getUserVillagesOfPeerSupervisor()).thenReturn(TestDataProvider.getUserVillageResponseDTO());

        SuccessResponse<UserVillageResponseDTO> response = userController.getPeerSupervisorLinkedUsers();

        Assertions.assertEquals(HttpStatus.OK,response.getStatusCode());
    }

    @Test
    void getPeerSupervisorLinkedUsersWithPaginationTest() {
        UserVillageDTO responseDto = TestDataProvider.getUserVillageDTO();
        when(userService.getUserVillagesOfPeerSupervisorWithPagination(TestDataProvider.getSearchRequestDTO())).thenReturn(List.of(responseDto));

        List<UserVillageDTO> response = userController.getPeerSupervisorLinkedUsersWithPagination(TestDataProvider.getSearchRequestDTO());

        Assertions.assertNotNull(response);
    }

    @Test
    void getUserPreferencesByIdTest() {
        SearchRequestDTO request =TestDataProvider.getSearchRequestDTO();
        when(userService.getUserPreferencesById(request.getUserId())).thenReturn(TestDataProvider.getUserPreferencesDTO());

        SuccessResponse<UserPreferencesDTO> response = userController.getUserPreferencesById(request);

        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void saveUserPreferencesTest() {
        UserPreferencesDTO request =TestDataProvider.getUserPreferencesDTO();
        when(userService.saveUserPreferences(request)).thenReturn(TestDataProvider.getUserPreferencesDTO());

        SuccessResponse<UserPreferencesDTO> response = userController.saveUserPreferences(request);

        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void updateTermsAndConditionValue() {
        //when
        doNothing().when(userService).updateUserTermsAndConditionDetailsById();

        //then
        SuccessResponse<UserDTO> response = userController.updateTermsAndConditionValue();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getUserListByRole() {
        //given
        CommonRequestDTO requestDTO = new CommonRequestDTO();
        List<User> response = List.of(TestDataProvider.getUser());

        //when
        when(userService.getUserListByRole(requestDTO)).thenReturn(response);
        //then
        SuccessResponse<User> result = userController.getUserListByRole(requestDTO);
        Assertions.assertEquals(HttpStatus.OK, result.getStatusCode());
    }

    @Test
    void getLockedUsers() {
        //given
        SearchRequestDTO requestObject = TestDataProvider.getSearchRequestDTO();
        Map<String, Object> response = Map.of("Test", TestDataProvider.getUser());

        //when
        when(userService.getLockedUsers(requestObject)).thenReturn(response);

        //then
        SuccessResponse<UserOrganizationDTO> result = userController.getLockedUsers(requestObject);
        Assertions.assertNotNull(result);
    }

    @Test
    void activateDeactivateUser() {
        //given
        List<Long> tenantIds = List.of(TestConstants.ONE);

        //then
        ResponseEntity<Boolean> response = userController.activateDeactivateUser(tenantIds,Boolean.TRUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getUserByTenants() {
        //given
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        List<UserResponseDTO> response = List.of(TestDataProvider.getUserResponseDTO());

        //when
        when(userService.getAllUsersByTenantId(request)).thenReturn(response);

        //then
        List<UserResponseDTO> result = userController.getUserByTenants(request);
        Assertions.assertNotNull(result);
    }
}
