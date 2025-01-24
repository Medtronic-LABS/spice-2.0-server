package com.mdtlabs.coreplatform.userservice.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserPreferencesDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserSuperAdminDto;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;

/**
 * <p>
 * This an interface class for user module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public interface UserService {

    /**
     * <p>
     * Creates a new user.
     * This method is responsible for persisting a new {@link User} entity to the database. It can also perform
     * validation on the user entity before saving, based on the {@code isValidate} parameter.
     * </p>
     *
     * @param user       The {@link User} entity to be created and saved.
     * @param isValidate A boolean flag indicating whether to perform validation on the user entity before saving.
     *                   If {@code true}, the user data is validated; if {@code false}, the user is saved without validation.
     * @return The saved {@link User} entity, including any updates that occurred during the save process (e.g., generated ID).
     */
    User createUser(User user, boolean isValidate);

    /**
     * <p>
     * Initiates a password reset process for a user.
     * This method generates a token for users who have forgotten their password, allowing them to reset it.
     * The process involves verifying the user's identity and sending a reset link to their registered email.
     * </p>
     *
     * @param username The username of the user requesting a password reset.
     * @param appType  The type of application from which the request is made, to customize the reset process accordingly.
     * @return Boolean indicating whether the password reset token was successfully generated and sent.
     */
    Boolean forgetPassword(String username, String appType, String type, String client);

    /**
     * <p>
     * Changes the password of a site user.
     * This method allows a user to change their password, given they provide the correct old password.
     * It is typically used for routine password updates within a user's profile settings.
     * </p>
     *
     * @param requestDTO A SearchRequestDTO object containing the user's ID, old password, and new password.
     * @return Boolean indicating the success of the password change operation.
     */
    void changeSiteUserPassword(SearchRequestDTO requestDTO);

    /**
     * <p>
     * Updates the password for a user based on a token.
     * This method is part of the password reset flow, where the user, after receiving a reset token by email,
     * can set a new password. The token ensures the request's validity.
     * </p>
     *
     * @param token    The token that was sent to the user's email for password reset verification.
     * @param userInfo A map containing the new password (and potentially other user information).
     * @return Boolean indicating whether the password was successfully updated.
     */
    Boolean updatePassword(String token, Map<String, String> userInfo);

    /**
     * <p>
     * Verifies a JWT token.
     * This method is used to authenticate a user based on a JWT token they provide. It checks the token's validity,
     * ensuring it hasn't expired and matches a user in the database.
     * </p>
     *
     * @param token The JWT token to be verified.
     * @return User The user entity associated with the valid token, or null if verification fails.
     */
    User verifyJwtToken(String token);

    /**
     * <p>
     * Validates a list of users.
     * This method performs validation on a list of UserRequestDTO objects, typically to ensure that all required fields
     * are present and correct before processing user creation or updates.
     * </p>
     *
     * @param users A list of UserRequestDTO objects representing the users to be validated.
     */
    void validateUsers(List<UserRequestDTO> users);

    /**
     * <p>
     * Creates a list of users with a check against existing IDs.
     * This method creates and persists a list of {@link User} entities to the database, with an additional step to check
     * against a list of existing user IDs. This is useful to avoid duplicating users that are already present in the database.
     * </p>
     *
     * @param users       A list of {@link User} entities to be created and saved.
     * @param existingIds A list of user IDs that are already present in the database, to be checked against.
     * @return A list of {@link User} entities after they have been saved to the database.
     */
    List<User> createUsers(List<User> users, List<Long> existingIds);

    /**
     * <p>
     * Creates a list of users.
     * This method is responsible for creating and persisting a list of {@link User} entities to the database.
     * It can be used when there is a need to create multiple users at once, without any additional checks for existing user IDs.
     * </p>
     *
     * @param users A list of {@link User} entities to be created and saved.
     * @return A list of {@link User} entities after they have been saved to the database.
     */
    List<User> createUsers(List<User> users);

    /**
     * <p>
     * Updates a list of users.
     * This method updates and persists changes to a list of {@link User} entities in the database.
     * It can be used to apply bulk updates of users.
     * </p>
     *
     * @param users A list of {@link User} entities with updated information to be saved.
     * @return A list of {@link User} entities after the updates have been saved to the database.
     */
    List<User> saveAllUsers(List<User> users);

    /**
     * <p>
     * Updates a single user.
     * This method updates and persists changes to a single {@link User} entity in the database.
     * It is typically used for updating user details individually.
     * </p>
     *
     * @param user The {@link User} entity with updated information to be saved.
     * @return The {@link User} entity after the update has been saved to the database.
     */
    User saveUser(User user);

    /**
     * <p>
     * Retrieves users by village IDs.
     * This method fetches a list of {@link User} entities based on a set of village IDs. It is useful for identifying users
     * associated with specific villages, such as for sending localized notifications or updates.
     * </p>
     *
     * @param villageIds A set of village IDs for which to retrieve associated users.
     * @param userId     The ID of a user to exclude from the results, typically the current user making the request.
     * @return A list of {@link User} entities associated with the specified village IDs.
     */
    List<User> getUserByVillageIds(Set<Long> villageIds, Long userId);

    /**
     * <p>
     * Adds an organization to a list of users identified by their supervisor IDs. This method is useful for associating
     * users with a specific organization in bulk, typically for setting up hierarchical relationships.
     * </p>
     *
     * @param linkedSupervisorIds A list of supervisor IDs representing the users to whom the organization will be
     *                            added.
     * @param organization        The organization entity to be added to the users.
     * @param appTypes            The application type
     */
    void addOrganizationForUsers(List<Long> linkedSupervisorIds, Organization organization, List<String> appTypes);

    /**
     * <p>
     * Retrieves a list of users based on their unique identifiers.
     * This method is used to fetch user details in bulk by providing their IDs, optimizing for performance when multiple user details are needed.
     * </p>
     *
     * @param ids A list of user IDs for which details are to be retrieved.
     * @return A list of User entities corresponding to the provided IDs.
     */
    List<User> getUsersByIds(List<Long> ids);

    /**
     * <p>
     * Retrieves a user by their unique identifier.
     * This method is used to fetch a single {@link User} entity from the database based on its ID.
     * It is typically used in scenarios where user details are needed for display or further processing.
     * </p>
     *
     * @param id The unique identifier of the user to be retrieved.
     * @return User The {@link User} entity corresponding to the provided ID.
     */
    User getUserById(Long id);

    /**
     * <p>
     * Updates an organization user.
     * This method is responsible for updating the details of an existing {@link User} entity associated with an organization.
     * It can handle updates to various user attributes such as name, email, roles, etc., based on the provided {@link UserRequestDTO}.
     * </p>
     *
     * @param request The {@link UserRequestDTO} containing the updated user details.
     * @return UserResponseDTO The response object containing the updated user details.
     */
    User updateOrganizationUser(UserRequestDTO request);

    /**
     * <p>
     * Updates the profile of a user.
     * This method allows for the updating of a user's profile information. It accepts a {@link UserRequestDTO} containing
     * the new profile details and applies these updates to the specified user entity in the database.
     * </p>
     *
     * @param request The {@link UserRequestDTO} containing the new profile details to be updated.
     * @return User The updated {@link User} entity after the profile has been updated.
     */
    User userProfileUpdate(UserRequestDTO request);

    /**
     * <p>
     * Deletes user based on the criteria specified in {@link SearchRequestDTO}.
     * This method is responsible for removing a user associated with an organization from the database.
     * The criteria for selecting the user to be deleted are encapsulated within the {@link SearchRequestDTO}.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria for identifying the user to be deleted.
     * @return UserResponseDTO The response object containing details of the deleted user.
     */
    UserResponseDTO deleteOrganizationUser(SearchRequestDTO request);

    /**
     * <p>
     * Retrieves a mapping of user IDs to {@link User} entities.
     * This method is used to fetch user details in bulk by providing their IDs, optimizing for performance when multiple user details are needed.
     * The returned map has user IDs as keys and the corresponding {@link User} entities as values.
     * </p>
     *
     * @param ids A list of user IDs for which details are to be retrieved.
     * @return Map<Long, User> A map with user IDs as keys and {@link User} entities as values.
     */
    Map<Long, User> getUsersByIdsAsMap(List<Long> ids);

    /**
     * <p>
     * Retrieves a list of users based on tenant criteria specified in {@link SearchRequestDTO}.
     * This method fetches user details for users associated with specific tenants, as defined in the {@link SearchRequestDTO}.
     * It is useful for operations that require filtering users by tenant, such as generating tenant-specific reports or notifications.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria for filtering users by tenants.
     * @return ResponseListDTO<UserResponseDTO> A list of {@link UserResponseDTO} objects representing the users associated with the specified tenants.
     */
    ResponseListDTO<UserResponseDTO> getUsersByTenants(SearchRequestDTO request);

    /**
     * <p>
     * Removes a super administrator by their unique identifier.
     * This method is used to delete a super administrator's account from the database, typically as part of administrative user management.
     * The operation is irreversible and should be used with caution.
     * </p>
     *
     * @param id The unique identifier of the super administrator to be removed.
     */
    void removeSuperAdmin(Long id);

    /**
     * <p>
     * Updates the details of a super administrator.
     * This method is responsible for updating the information of a super administrator in the system based on the provided {@link UserSuperAdminDto}.
     * </p>
     *
     * @param userDto The {@link UserSuperAdminDto} containing the updated super admin details.
     */
    void updateSuperAdmin(UserSuperAdminDto userDto);

    /**
     * <p>
     * Validates a user's information.
     * This method performs validation on the user's data provided in {@link SearchRequestDTO}. It can be used to verify if the user's information meets
     * certain criteria before proceeding with further operations.
     * </p>
     *
     * @param requestData The {@link SearchRequestDTO} containing the user's information to be validated.
     * @return UserResponseDTO The response object containing the validation results and user's details.
     */
    UserResponseDTO validateUser(SearchRequestDTO requestData);

    /**
     * <p>
     * Retrieves a list of super administrators.
     * This method fetches a list of super administrators based on criteria specified in {@link SearchRequestDTO}.
     * </p>
     *
     * @param searchRequest The {@link SearchRequestDTO} containing the criteria for filtering super administrators.
     * @return ResponseListDTO<UserSuperAdminDto> A list of {@link UserSuperAdminDto} objects representing the super administrators.
     */
    ResponseListDTO<UserSuperAdminDto> getSuperAdminUsers(SearchRequestDTO searchRequest);

    /**
     * <p>
     * Unlocks a user account.
     * This method is used to unlock a user account that may have been locked due to reasons such as multiple failed login attempts.
     * The unlocking process is based on criteria specified in {@link SearchRequestDTO}.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria to identify the user account to be unlocked.
     * @return Boolean indicating whether the user account was successfully unlocked.
     */
    Boolean unlockUser(SearchRequestDTO request);

    /**
     * <p>
     * Creates a super administrator.
     * This method is responsible for creating a new super administrator in the system. It accepts a list of {@link UserRequestDTO} containing
     * the information of the super administrators to be created.
     * </p>
     *
     * @param usersList A list of {@link UserRequestDTO} representing the super administrators to be created.
     */
    void createSuperAdmin(List<UserRequestDTO> usersList);

    /**
     * <p>
     * Retrieves role Meta based on the criteria specified in {@link SearchRequestDTO}.
     * This method is used to fetch role Meta, which are collections of {@link RoleResponseDTO} objects,
     * based on the search criteria provided.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria for filtering role groups.
     * @return A map where each key is a string representing the role group name, and the value is a list of {@link RoleResponseDTO} objects belonging to that group.
     */
    Map<String, List<RoleResponseDTO>> getRoleGroupes(SearchRequestDTO request);

    /**
     * <p>
     * Retrieves detailed information about a user by their unique identifier.
     * This method fetches a single {@link UserResponseDTO} object from the database based on the user's ID.
     * It is typically used in scenarios where detailed user information is needed for display or further processing.
     * </p>
     *
     * @param userId The unique identifier of the user whose details are to be retrieved.
     * @return A {@link UserResponseDTO} object containing detailed information about the user.
     */
    UserResponseDTO getUserDetails(Long userId);

    /**
     * <p>
     * Resets a user's password using a reset password link.
     * This method allows users to reset their password by providing a token received via a reset password link
     * and their new password information.
     * </p>
     *
     * @param token    The reset password token provided to the user.
     * @param userInfo A map containing the new password and potentially other user information.
     * @return A map containing the status of the password update operation, with keys indicating status information.
     */
    public Map<String, Object> resetUserPassword(String token, Map<String, String> userInfo);

    /**
     * <p>
     * Changes a user's old password to a new password.
     * This method is used for updating a user's password, provided the correct old password is given.
     * It is a security measure that ensures only the rightful owner of the account can change the password.
     * </p>
     *
     * @param requestDTO A {@link SearchRequestDTO} object containing the user's ID, old password, and new password.
     */
    void changeOldToNewPassword(SearchRequestDTO requestDTO);

    /**
     * <p>
     * Retrieves village details associated with a user.
     * This method fetches a list of {@link VillageDTO} objects representing the villages associated with a user,
     * based on the user's unique identifier.
     * </p>
     *
     * @param userId The unique identifier of the user whose village details are to be retrieved.
     * @return A list of {@link VillageDTO} objects representing the villages associated with the user.
     */
    List<VillageDTO> getUserVillages(Long userId);

    /**
     * <p>
     * Validates the phone number already present for a user.
     * This method checks if the provided phone number in the {@link SearchRequestDTO} meets the system's validation criteria.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the user's phone number to be validated.
     */
    void phoneNumberValidation(SearchRequestDTO request);

    /**
     * <p>
     * Retrieves a list of peer supervisors based on a single tenant ID.
     * This method is used to fetch peer supervisors associated with a specific tenant.
     * </p>
     *
     * @param request The ID of the tenant for which peer supervisors are to be retrieved.
     * @return List<UserResponseDTO> A list of {@link UserResponseDTO} objects representing the peer supervisors.
     */
    List<UserResponseDTO> getPeerSupervisor(SearchRequestDTO request);

    /**
     * <p>
     * Retrieves a list of peer supervisors for multiple tenants.
     * This method fetches peer supervisors associated with a list of tenant IDs, allowing for bulk retrieval across
     * multiple tenants.
     * </p>
     *
     * @param request A list of tenant IDs for which peer supervisors are to be retrieved.
     * @return List<UserResponseDTO> A list of {@link UserResponseDTO} objects representing the peer supervisors for the specified tenants.
     */
    List<UserResponseDTO> getPeerSupervisorByTenants(SearchRequestDTO request);

    /**
     * <p>
     * Fetches all mobile users from the database.
     * This method is designed to retrieve all users classified as mobile users from the database.
     * It maps the User entity to UserResponseDTO and returns a list of UserResponseDTO.
     * </p>
     *
     * @return List<UserResponseDTO> A list of UserResponseDTO representing all mobile users.
     */
    List<UserResponseDTO> getAllMobileUsers();

    /**
     * <p>
     * Fetches all users from the database that belong to the specified tenant.
     * This method retrieves all users associated with a specific tenant ID, provided in the requestDTO.
     * It maps the User entity to UserResponseDTO, facilitating the return of a list of UserResponseDTO that belong to the specified tenant.
     * </p>
     *
     * @param requestDTO The DTO that contains the ID of the tenant to filter users by.
     * @return List<UserResponseDTO> A list of UserResponseDTO representing all users that belong to the specified tenant.
     */
    List<UserResponseDTO> getAllUsersByTenantId(SearchRequestDTO requestDTO);

    /**
     * <p>
     * Deletes organization users based on the criteria specified in the SearchRequestDTO.
     * This method is responsible for removing users associated with an organization from the database.
     * The criteria for selecting the users to be deleted are encapsulated within the SearchRequestDTO.
     * </p>
     *
     * @param request The SearchRequestDTO containing the criteria for identifying the users to be deleted.
     */
    void deleteOrganizationUsers(SearchRequestDTO request);

    /**
     * <p>
     * Retrieves the profile of the currently authenticated user.
     * This method fetches the profile details of the user making the request, typically used for displaying user profile information.
     * </p>
     *
     * @return UserResponseDTO The response object containing the details of the user's profile.
     */
    UserResponseDTO getUserProfile();

    /**
     * <p>
     * Adds an organization to a list of users identified by their supervisor IDs.
     * </p>
     *
     * @param linkedSupervisorIds A list of supervisor IDs representing the users to whom the organization will be added.
     * @param organization        The organization entity to be added to the users.
     */
    void addPeerSupervisors(List<Long> linkedSupervisorIds, Organization organization);

    /**
     * <p>
     * Validates peer supervisors based on tenant ID.
     * This method checks if the peer supervisors associated with a given tenant meet certain criteria.
     * It is typically used to ensure that the supervisors have the necessary attributes and permissions before being assigned specific roles or tasks.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the tenant ID and other criteria for validation.
     */
    void validatePeerSupervisors(SearchRequestDTO request);

    /**
     * Get the original password reset url from redisserver
     *
     * @param url - Contains short url.
     * @return - Returns orignal url from redisserver
     */
    String getPasswordResetUrl(String url);

    /**
     * Get users and village of the Peer Supervisor
     *
     * @return List of user villages
     */
    List<UserVillageResponseDTO> getUserVillagesOfPeerSupervisor();

    /**
     * Retrieves user preferences for a specific user by ID.
     *
     * @param userId A Long value representing the ID of the user to get preferences for.
     * @return A UserPreferencesDTO object containing the user's preferences.
     */
    UserPreferencesDTO getUserPreferencesById(Long userId);

    /**
     * Saves user preferences for a specific user.
     *
     * @param request A UserPreferencesDTO object containing the user preferences.
     * @return A UserPreferencesDTO object containing the saved user preferences.
     */
    UserPreferencesDTO saveUserPreferences(UserPreferencesDTO request);

    /**
     * Get users and village of the Peer Supervisor
     *
     * @return List of user villages
     */
    List<UserVillageDTO> getUserVillagesOfPeerSupervisorWithPagination(SearchRequestDTO request);

    /**
     * <p>
     * This method is used to activate or deactivate the users from fhir
     * </p>
     *
     * @param tenantIds  {@link List<Long>} The data required to activate or deactivate the user
     * @param isActive {@link Boolean} It contains the district active and inactive status
     */
    Boolean activateDeactivateUser(List<Long> tenantIds, boolean isActive);

    /**
     * <p>
     * This method is used to retrieve a list of locked users with respect
     * to the provided details.
     * </p>
     *
     * @param requestObject {@link SearchRequestDTO} The conditions to get the locked users is given
     * @return {@link Map} A map of total count and list of locked users is retrieved and returned
     */
    Map<String, Object> getLockedUsers(SearchRequestDTO requestObject);

    /**
     * <p>
     * Gets a list of users by role name
     * </p>
     *
     * @param roleName -- Role name
     * @return list of user
     */
    public List<User> getUserByRoleName(String roleName);

    /**
     * <p>
     * This method is used to update terms and condition details with respect to id.
     * </p>
     *
     */
    void updateUserTermsAndConditionDetailsById();

    /**
     * <p>
     * Handles the HTTP POST request to retrieve a list of users based on their role.
     * </p>
     *
     * @param requestDTO the request data transfer object containing the role criteria.
     * @return a SuccessResponse containing the list of users, a success code, the size of the list, and the HTTP status.
     */
    List<User> getUserListByRole(CommonRequestDTO requestDTO);
}
