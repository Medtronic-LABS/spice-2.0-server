package com.mdtlabs.coreplatform.userservice.controller;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserOrganizationDTO;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserPreferencesDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserSuperAdminDto;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.userservice.message.SuccessCode;
import com.mdtlabs.coreplatform.userservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.userservice.service.UserService;

/**
 * <p>
 * User Controller class that defines various REST API endpoints for managing users,
 * including updating, retrieving, creating, and deleting using user service.
 * </p>
 *
 * @author JeyahariniTA created on Feb 14, 2023
 */
@RestController
@RequestMapping("/user")
public class UserController {

    private final UserService userService;

    private final ModelMapper modelMapper = new ModelMapper();

    public UserController(UserService userService) {
        this.userService = userService;
    }

    /**
     * <p>
     * Fetches the user profile based on the user context.
     * This method retrieves the user profile from the UserContextHolder and uses the userService to fetch the user profile.
     * If the user profile is not found, it returns a response indicating that the user was not found.
     * </p>
     *
     * @return A SuccessResponsesResponse containing the UserResponseDTO of the fetched user profile and the HTTP status.
     */
    @PostMapping("/profile")
    public SuccessResponse<UserResponseDTO> getUserProfileById() {
        UserResponseDTO userDto = null;
        if (null != UserContextHolder.getUserDto()) {
            userDto = userService.getUserProfile();
            return new SuccessResponse<>(SuccessCode.GET_USER, userDto, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.USER_NOT_FOUND, userDto, HttpStatus.OK);
    }

    /**
     * <p>
     * Creates a new user.
     * This method takes a UserDTO as a parameter, creates the user using the userService, and returns a success response.
     * If the user creation fails, it returns a response indicating that the user was not saved.
     * </p>
     *
     * @param requestUserDto The UserDTO containing the details of the user to be created.
     * @return A SuccessResponse containing the created UserDTO and the HTTP status.
     */
    @PostMapping("/create")
    public SuccessResponse<UserDTO> createUser(@RequestBody UserDTO requestUserDto) {
        User addUser = userService.createUser(modelMapper.map(requestUserDto, User.class), true);
        if (null != addUser) {
            UserDTO userDto = modelMapper.map(addUser, UserDTO.class);
            return new SuccessResponse<>(SuccessCode.USER_SAVE, userDto, HttpStatus.CREATED);
        }
        return new SuccessResponse<>(SuccessCode.USER_NOT_SAVED, requestUserDto, HttpStatus.BAD_REQUEST);
    }

    /**
     * <p>
     * This method is used to handle a forgot password validation request .
     * </p>
     *
     * @param username {@link String} The username of the user who need to reset their password is given
     * @param appType  {@link String} The appType parameter is a path variable that is used to specify the
     *                 type of application for which the email template is being requested is given
     * @return {@link SuccessResponse<String>} Returns a message indicating that the password has been validated.
     */
    @PostMapping(value = "/forgot-password/{username}/{appType}", produces = "application/json")
    public SuccessResponse<String> forgotPasswordValidation(
            @PathVariable(FieldConstants.USERNAME) String username, @PathVariable(Constants.APP_TYPE) String appType,
            @RequestHeader(Constants.HEADER_CLIENT) String client) {
        Boolean response = userService.forgetPassword(username, appType, null, client);
        SuccessCode successCode;
        if (Boolean.TRUE.equals(response)) {
            if (Constants.CLIENT_SPICE_MOBILE.equals(appType)) {
                successCode = SuccessCode.SEND_SMS;
            } else {
                successCode = SuccessCode.SEND_EMAIL_USING_SMTP;
            }
        } else {
            successCode = SuccessCode.PASSWORD_UPDATED;
        }

        return new SuccessResponse<>(successCode, response, HttpStatus.OK);
    }

    /**
     * <p>
     * Verifies a JWT token.
     * This method takes a JWT token as a parameter, verifies the token using the userService, and returns a success response.
     * </p>
     *
     * @param token The JWT token to be verified.
     * @return A SuccessResponse containing a Map of String and Object indicating the verification result and the HTTP status.
     */
    @PostMapping("/verify-token/{token}")
    public SuccessResponse<Map<String, Object>> verifyJwtToken(@PathVariable(Constants.TOKEN) String token) {
        userService.verifyJwtToken(token);
        return new SuccessResponse<>(SuccessCode.VERIFY_TOKEN, HttpStatus.OK);
    }

    /**
     * <p>
     * Updates the password for a user.
     * This method takes a token and a Map of user information as parameters, uses the userService to update the user's password, and returns a success response.
     * </p>
     *
     * @param token    The token associated with the user.
     * @param userInfo A Map containing the user's information.
     * @return A SuccessResponse containing a Boolean value indicating the success of the password update operation and the HTTP status.
     */
    @PostMapping("/update-password/{token}")
    public SuccessResponse<Boolean> setUserPassword(@PathVariable(Constants.TOKEN) String token,
                                                    @RequestBody Map<String, String> userInfo) {
        return new SuccessResponse<>(SuccessCode.PASSWORD_UPDATED,
                userService.updatePassword(token, userInfo), HttpStatus.OK);
    }


    /**
     * <p>
     * Fetches a user by their ID.
     * This method takes a user ID as a parameter, uses the userService to fetch the user details, and returns a success response.
     * </p>
     *
     * @param userId The ID of the user to be fetched.
     * @return A SuccessResponse containing the UserResponseDTO of the fetched user and the HTTP status.
     */
    @PostMapping("/details/{id}")
    public SuccessResponse<UserResponseDTO> getUserById(@PathVariable(Constants.ID) Long userId) {
        return new SuccessResponse<>(SuccessCode.GET_USER, userService.getUserDetails(userId), HttpStatus.OK);
    }

    /**
     * <p>
     * Validates a user.
     * This method takes a SearchRequestDTO as a parameter, validates the user using the userService, and returns a success response.
     * </p>
     *
     * @param requestDto The SearchRequestDTO containing the details of the user to be validated.
     * @return A SuccessResponse containing the UserResponseDTO of the validated user and the HTTP status.
     */
    @PostMapping("/validate-user")
    public SuccessResponse<UserResponseDTO> validateUser(@RequestBody SearchRequestDTO requestDto) {
        return new SuccessResponse<>(SuccessCode.GET_USER, userService.validateUser(requestDto), HttpStatus.OK);
    }

    /**
     * Searches for users by their tenants.
     * This method takes a SearchRequestDTO as a parameter, uses the userService to fetch the users by their tenants, and returns a success response.
     * If no users are found, it returns a response with null total count.
     *
     * @param requestDTO The SearchRequestDTO containing the details of the tenants to be searched.
     * @return A SuccessResponse containing a list of UserResponseDTO of the fetched users, the total count of users, and the HTTP status.
     */
    @PostMapping("/admin-users")
    public SuccessResponse<UserResponseDTO> searchUser(@RequestBody SearchRequestDTO requestDTO) {
        ResponseListDTO<UserResponseDTO> response = userService.getUsersByTenants(requestDTO);
        if (Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount()) {
            return new SuccessResponse<>(SuccessCode.GET_USERS, response.getData(), null, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_USERS, response.getData(), response.getTotalCount(), HttpStatus.OK);
    }

    /**
     * <p>
     * Fetches all mobile users.
     * This method uses the userService to fetch all mobile users and returns a success response.
     * </p>
     *
     * @return A SuccessResponse containing a list of UserResponseDTO of the fetched mobile users, the total count of users, and the HTTP status.
     */
    @PostMapping("/mobile-users")
    public SuccessResponse<UserResponseDTO> getAllMobileUsers() {
        List<UserResponseDTO> users = userService.getAllMobileUsers();
        return new SuccessResponse<>(SuccessCode.GET_ALL_MOBILE_USERS, users,
                Long.valueOf(users.size()),
                HttpStatus.OK);
    }

    /**
     * <p>
     * Fetches all users by their tenant ID.
     * This method takes a SearchRequestDTO as a parameter, uses the userService to fetch all users by their tenant ID, and returns a success response.
     * </p>
     *
     * @param request The SearchRequestDTO containing the tenant ID.
     * @return A SuccessResponse containing a list of UserResponseDTO of the fetched users, the total count of users, and the HTTP status.
     */
    @PostMapping("/users-by-tenant-id")
    public SuccessResponse<UserResponseDTO> getAllUsersByTenantId(@RequestBody SearchRequestDTO request) {
        List<UserResponseDTO> users = userService.getAllUsersByTenantId(request);
        return new SuccessResponse<>(SuccessCode.GET_ALL_USERS,
                users,
                Long.valueOf(users.size()),
                HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to get the users by an tenants ids.
     * </p>
     *
     * @param request {@link SearchRequestDTO} The request contains necessary information
     *                   to activate the list of districts is given
     * @return {@link List<UserResponseDTO>} Return the list users details
     */
    @PostMapping("/get-by-tenants")
    public List<UserResponseDTO> getUserByTenants(@RequestBody SearchRequestDTO request) {
         return userService.getAllUsersByTenantId(request);
    }

    /**
     * <p>
     * Fetches all super-admin users.
     * This method takes a SearchRequestDTO as a parameter, uses the userService to fetch all super-admin users, and returns a success response.
     * </p>
     *
     * @param searchRequest The SearchRequestDTO containing the search parameters.
     * @return A SuccessResponse containing a list of UserSuperAdminDto of the fetched super-admin users, the total count of users, and the HTTP status.
     */
    @PostMapping("/super-admin/list")
    public SuccessResponse<UserSuperAdminDto> getSuperAdminUsers(@RequestBody SearchRequestDTO searchRequest) {
        ResponseListDTO<UserSuperAdminDto> response = userService.getSuperAdminUsers(searchRequest);
        return new SuccessResponse<>(SuccessCode.GET_SUPER_ADMIN_USER, response.getData(), response.getTotalCount(), HttpStatus.OK);
    }

    /**
     * <p>
     * Removes super-admin role for the provided user.
     * This method takes a user ID as a parameter, uses the userService to remove the super-admin role from the user, and returns a success response.
     * </p>
     *
     * @param id The ID of the user from whom the super-admin role is to be removed.
     * @return A SuccessResponse containing a success message and the HTTP status.
     */
    @PutMapping("/super-admin/remove/{id}")
    public SuccessResponse<String> removeSuperAdminRoleForUser(@PathVariable("id") long id) {
        userService.removeSuperAdmin(id);
        return new SuccessResponse<>(SuccessCode.REMOVE_SUPER_ADMIN_USER, HttpStatus.OK);
    }

    /**
     * <p>
     * Updates super admin user details.
     * This method takes a UserSuperAdminDto as a parameter, uses the userService to update the super admin user details, and returns a success response.
     * </p>
     *
     * @param userSuperAdminDto The UserSuperAdminDto containing the details of the super admin user to be updated.
     * @return A SuccessResponse containing a success message and the HTTP status.
     */
    @PostMapping("/super-admin/update")
    public SuccessResponse<String> updateSuperAdminUser(@RequestBody UserSuperAdminDto userSuperAdminDto) {
        userService.updateSuperAdmin(userSuperAdminDto);
        return new SuccessResponse<>(SuccessCode.UPDATE_SUPER_ADMIN_USER, HttpStatus.OK);

    }

    /**
     * <p>
     * Updates a user's profile.
     * This method takes a UserRequestDTO as a parameter, uses the userService to update the user's profile, and returns a success response.
     * </p>
     *
     * @param userRequestDTO The UserRequestDTO containing the details of the user to be updated.
     * @return A SuccessResponse containing a success message and the HTTP status.
     */
    @PostMapping("/update")
    public SuccessResponse<String> userProfileUpdate(@RequestBody UserRequestDTO userRequestDTO) {
        userService.userProfileUpdate(userRequestDTO);
        return new SuccessResponse<>(SuccessCode.USER_UPDATE, HttpStatus.OK);

    }

    /**
     * <p>
     * Unlocks a blocked user.
     * This method takes a SearchRequestDTO as a parameter, uses the userService to unlock the user, and returns a success response.
     * </p>
     *
     * @param requestDto The SearchRequestDTO containing the user Id to be unlocked.
     * @return A SuccessResponse containing a success message and the HTTP status.
     */
    @PostMapping("/unlock")
    public SuccessResponse<String> unlockUser(@RequestBody SearchRequestDTO requestDto) {
        userService.unlockUser(requestDto);
        return new SuccessResponse<>(SuccessCode.USER_UNLOCK, HttpStatus.OK);
    }

    /**
     * <p>
     * Creates super admin users.
     * This method takes a list of UserRequestDTO as a parameter, uses the userService to create super admin users, and returns a success response.
     * </p>
     *
     * @param usersList The list of UserRequestDTO containing the details of the super admin users to be created.
     * @return A SuccessResponse containing a success message and the HTTP status.
     */
    @PostMapping("/super-admin/create")
    public SuccessResponse<String> createSuperAdminUser(@RequestBody List<UserRequestDTO> usersList) {
        userService.createSuperAdmin(usersList);
        return new SuccessResponse<>(SuccessCode.CREATE_SUPER_ADMIN_USER, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Resets a user's password using a reset password link.
     * This method takes a token and a Map of user information as parameters, uses the userService to reset the user's password, and returns a success response.
     * </p>
     *
     * @param token    The token associated with the user.
     * @param userInfo A Map containing the user's information including the new password.
     * @return A SuccessResponse containing a Map of String and Boolean indicating the status of the password reset operation and the HTTP status.
     */
    @PostMapping("/reset-password/{token}")
    public SuccessResponse<Map<String, Boolean>> resetUserPassword(@PathVariable(Constants.TOKEN) String token,
                                                                   @RequestBody Map<String, String> userInfo) {
        return new SuccessResponse<>(SuccessCode.SET_PASSWORD,
                userService.resetUserPassword(token, userInfo), HttpStatus.OK);
    }

    /**
     * <p>
     * Changes a user's password.
     * This method takes a SearchRequestDTO as a parameter, which includes the user's ID, old password, and new password.
     * It uses the userService to change the user's password and returns a success response.
     * </p>
     *
     * @param requestDTO The SearchRequestDTO containing the user's ID, old password, and new password.
     * @return A SuccessResponse containing a Boolean value indicating the success of the password change operation and the HTTP status.
     */
    @PostMapping("/change-password")
    public SuccessResponse<Boolean> changePassword(@RequestBody SearchRequestDTO requestDTO) {
        userService.changeOldToNewPassword(requestDTO);
        return new SuccessResponse<>(SuccessCode.CHANGE_SITE_USER_PASSWORD, HttpStatus.OK);
    }

    /**
     * <p>
     * Changes another user's password.
     * This method takes a SearchRequestDTO as a parameter, which includes the user's ID and new password.
     * It uses the userService to change the user's password and returns a success response.
     * </p>
     *
     * @param requestDTO The SearchRequestDTO containing the user's ID and new password.
     * @return A SuccessResponse containing a Boolean value indicating the success of the password change operation and the HTTP status.
     */
    @PostMapping("/change-user-password")
    public SuccessResponse<Boolean> changeSiteUserPassword(@RequestBody SearchRequestDTO requestDTO) {
        userService.changeSiteUserPassword(requestDTO);
        return new SuccessResponse<>(SuccessCode.CHANGE_SITE_USER_PASSWORD, HttpStatus.OK);
    }

    /**
     * <p>
     * Fetches the roles metadata.
     * This method takes a SearchRequestDTO as a parameter, uses the userService to fetch the roles metadata, and returns a success response.
     * </p>
     *
     * @param request The SearchRequestDTO containing the request details.
     * @return A SuccessResponse containing a Map of String and List of RoleResponseDTO indicating the roles metadata and the HTTP status.
     */
    @PostMapping("/roles-list")
    public SuccessResponse<Map<String, List<RoleResponseDTO>>> getRolesMeta(@RequestBody SearchRequestDTO request) {
        Map<String, List<RoleResponseDTO>> roles = userService.getRoleGroupes(request);
        return new SuccessResponse<>(SuccessCode.GET_ROLE_GROUP, roles, HttpStatus.OK);
    }


    /**
     * <p>
     * Fetches a user's villages.
     * This method takes a user ID as a parameter, uses the userService to fetch the user's villages, and returns a ResponseEntity with UserResponseDTO and the HTTP status.
     * </p>
     *
     * @param userId The ID of the user whose villages are to be fetched.
     * @return A ResponseEntity containing the UserResponseDTO of the fetched user's villages and the HTTP status.
     */
    @PostMapping("/user-villages/{userId}")
    public ResponseEntity<UserResponseDTO> getUserVillages(@PathVariable("userId") Long userId) {
        UserResponseDTO userResponseDTO = modelMapper.map(userService.getUserById(userId), UserResponseDTO.class);
        return new ResponseEntity<>(userResponseDTO, HttpStatus.OK);
    }

    /**
     * <p>
     * Validates a user's phone number.
     * This method takes a SearchRequestDTO as a parameter, which includes the user's phone number.
     * It uses the userService to validate the phone number and returns a success response.
     * </p>
     *
     * @param request The SearchRequestDTO containing the user's phone number to be validated.
     * @return A SuccessResponse containing a Boolean value indicating the success of the phone number validation operation and the HTTP status.
     */
    @PostMapping("/validate-phonenumber")
    public SuccessResponse<Boolean> validatePhoneNumberForUser(@RequestBody SearchRequestDTO request) {
        userService.phoneNumberValidation(request);
        return new SuccessResponse<>(SuccessCode.GET_USER, HttpStatus.OK);
    }

    /**
     * <p>
     * Fetches peer supervisors based on tenantId.
     * This method takes a SearchRequestDTO as a parameter, which includes the tenantId.
     * It uses the userService to fetch the peer supervisors and returns a success response.
     * </p>
     *
     * @param request The SearchRequestDTO containing the tenantId to fetch the peer supervisors.
     * @return A SuccessResponse containing a UserResponseDTO of the fetched peer supervisors and the HTTP status.
     */
    @PostMapping("/peer-supervisors")
    public SuccessResponse<UserResponseDTO> getPeerSupervisors(@RequestBody SearchRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.GET_ALL_COMMUNITY_HEALTH_ASSISTANTS, userService.getPeerSupervisor(request),
                HttpStatus.OK);
    }

    /**
     * <p>
     * Fetches peer supervisors based on tenantId.
     * This method takes a SearchRequestDTO as a parameter, which includes the tenantId.
     * It uses the userService to fetch the peer supervisors and returns a ResponseEntity with a list of UserResponseDTO and the HTTP status.
     * </p>
     *
     * @param request The SearchRequestDTO containing the tenantId to fetch the peer supervisors.
     * @return A ResponseEntity containing a list of UserResponseDTO of the fetched peer supervisors and the HTTP status.
     */
    @PostMapping("/tenant-peer-supervisors")
    public ResponseEntity<List<UserResponseDTO>> getPeerSupervisorsByTenant(@RequestBody SearchRequestDTO request) {
        return new ResponseEntity<>(userService.getPeerSupervisorByTenants(request), HttpStatus.OK);
    }

    /**
     * <p>
     * Validates peer supervisors based on tenantId.
     * This method takes a SearchRequestDTO as a parameter, which includes the tenantId.
     * It uses the userService to validate the peer supervisors and returns a success response.
     * </p>
     *
     * @param requestDTO The SearchRequestDTO containing the tenantId to validate the peer supervisors.
     * @return A SuccessResponse containing a success message and the HTTP status.
     */
    @PostMapping("/validate-peer-supervisors")
    public ResponseEntity<Boolean> validatePeerSupervisors(@RequestBody SearchRequestDTO requestDTO) {
        userService.validatePeerSupervisors(requestDTO);
        return new ResponseEntity<>(Boolean.TRUE, HttpStatus.OK);
    }

    /**
     * <p>
     * Return false if user exceed the forget passowrd limit.
     * </p>
     *
     * @param username - username of the user.
     * @param appType  - application type.
     * @return User Entity
     * @throws Exception - exception
     */
    @PostMapping(value = "/forgot-password/sms/{username}/{appType}", produces = "application/json")
    public SuccessResponse<String> forgotPasswordSmsValidation(
            @PathVariable(FieldConstants.USERNAME) String username, @PathVariable(Constants.APP_TYPE) String appType) {
        Boolean response = userService.forgetPassword(username, appType, Constants.SMS, null);
        return new SuccessResponse<>(SuccessCode.SEND_SMS, response, HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to activate an user based on the provided request DTO.
     * </p>
     *
     * @param tenantIds {@link List<Long>} The request contains necessary information
     *                   to activate the list of users is given
     * @return {@link ResponseEntity} Returns a success message and status
     * after activating the user for the given ids
     */
    @PostMapping("/update-active-status")
    public ResponseEntity<Boolean> activateDeactivateUser(@RequestBody List<Long> tenantIds,
                                                          @RequestParam(Constants.IS_ACTIVE) boolean isActive) {
        return ResponseEntity.ok().body(userService.activateDeactivateUser(tenantIds, isActive));
    }

    /**
     * <p>
     * Redirect to original reset password url from encode short url
     * </p>
     *
     * @param url - Encoded short url
     * @return
     */
    @GetMapping("/reset-password/sms/{url}")
    public ResponseEntity<Void> resetPassword(@PathVariable(Constants.URL) String url) {
        String originalUrl = userService.getPasswordResetUrl(url);
        HttpHeaders headers = new HttpHeaders();
        headers.setLocation(URI.create(originalUrl));
        return new ResponseEntity<>(headers, HttpStatus.FOUND);
    }

    /**
     * <p>
     * get Peer supervisor linked all chw users
     * </p>
     *
     * @return list of chw role users
     */
    @GetMapping("/peer-supervisor/linked-chw")
    public SuccessResponse<UserVillageResponseDTO> getPeerSupervisorLinkedUsers() {
        return new SuccessResponse<>(SuccessCode.GET_USER, userService.getUserVillagesOfPeerSupervisor(), null,
                HttpStatus.OK);
    }

    /**
     * <p>
     * get Peer supervisor linked chw users with village
     * and pagination
     * </p>
     *
     * @param request page details
     * @return list of chw users
     */
    @PostMapping("/peer-supervisor/chw-list")
    List<UserVillageDTO> getPeerSupervisorLinkedUsersWithPagination(@RequestBody SearchRequestDTO request) {
        return userService.getUserVillagesOfPeerSupervisorWithPagination(request);
    }

    /**
     * Get user preferences details by user id.
     *
     * @param userId - Contains the user id.
     * @return User preferences detail is returned for the given user.
     */
    @PostMapping("/preferences")
    public SuccessResponse<UserPreferencesDTO> getUserPreferencesById(@RequestBody SearchRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.ADDED_USER_PREFERENCES,
                userService.getUserPreferencesById(request.getUserId()), HttpStatus.OK);
    }

    /**
     * Save user preferences detail.
     *
     * @param request - Contains the preference detail.
     * @return User preferences detail is returned that is being saved.
     */
    @PostMapping("/preferences/save")
    public SuccessResponse<UserPreferencesDTO> saveUserPreferences(@RequestBody UserPreferencesDTO request) {
        return new SuccessResponse<>(SuccessCode.GOT_USER_PREFERENCES, userService.saveUserPreferences(request), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to retrieve a list of locked users with respect
     * to the provided details.
     * </p>
     *
     * @param requestObject {@link SearchRequestDTO} The conditions to get the locked users is given
     * @return {@link SuccessResponse<UserOrganizationDTO>} The list of locked users is retrieved and
     * returned with the status
     */
    @UserTenantValidation
    @PostMapping("/locked-users")
    public SuccessResponse<UserOrganizationDTO> getLockedUsers(@RequestBody SearchRequestDTO requestObject) {
        Map<String, Object> response = userService.getLockedUsers(requestObject);
        List<UserOrganizationDTO> userList = response.containsKey(Constants.DATA)
                ? (List<UserOrganizationDTO>) response.get(Constants.DATA)
                : new ArrayList<>();
        Long totalCount = (response.containsKey(Constants.COUNT) && !Objects.isNull(response.get(Constants.COUNT)))
                ? Long.parseLong(response.get(Constants.COUNT).toString())
                : 0;
        return new SuccessResponse<>(SuccessCode.GET_USERS, userList, totalCount, HttpStatus.OK);
    }

    /**
     * <p>
     * To get users by role.
     * </p>
     *
     * @param roleName
     * @return List of UserDTO entity
     */
    @PostMapping("/get-by-rolename")
    public ResponseEntity<List<UserResponseDTO>> getUserByRoleName(@RequestBody String roleName) {
        List<UserResponseDTO> userDTOS = new ArrayList<>();
        List<User> users = userService.getUserByRoleName(roleName);
        if (!Objects.isNull(users) && !users.isEmpty()) {
            userDTOS = modelMapper.map(users, new TypeToken<List<UserResponseDTO>>() {
            }.getType());
        }
        return ResponseEntity.ok().body(userDTOS);
    }

    /**
     * <p>
     * Update Terms and Condition Value in User Table.
     * </p>
     *
     * @return {@link SuccessResponse} The success message is returned
     */
    @PostMapping("/terms-and-conditions/update")
    public SuccessResponse<UserDTO> updateTermsAndConditionValue() {
        userService.updateUserTermsAndConditionDetailsById();
        return new SuccessResponse<>(SuccessCode.TERMS_AND_CONDITIONS_SAVE, HttpStatus.OK);
    }

    /**
     * <p>
     * Handles the HTTP POST request to retrieve a list of users based on their role.
     * </p>
     *
     * @param requestDTO the request data transfer object containing the role criteria.
     * @return a SuccessResponse containing the list of users, a success code, the size of the list, and the HTTP status.
     */
    @PostMapping("/role-user-list")
    public SuccessResponse<User> getUserListByRole(@RequestBody CommonRequestDTO requestDTO) {
        List<User> response = userService.getUserListByRole(requestDTO);
        return new SuccessResponse<>(SuccessCode.GET_USERS, response, (long) response.size() , HttpStatus.OK);
    }

    /**
     * <p>
     * Update Culture of the User
     * </p>
     *
     * @param requestDto - user to be updated
     * @return a SuccessResponse containing a success code, and the HTTP status.
     */
    @PostMapping("/update-culture")
    public SuccessResponse<String> updateCulture(@RequestBody UserDTO requestDto) {
        userService.updateCulture(requestDto);
        return new SuccessResponse<>(SuccessCode.USER_UPDATE, HttpStatus.OK);
    }
}
