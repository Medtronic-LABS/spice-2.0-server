package com.mdtlabs.coreplatform.userservice.repository;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDetailsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the role module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author VigneshKumar
 * @since Aug 26, 2022
 */
@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    String GET_USER_BY_USERNAME = "FROM User as user where user.username=:username and user.isDeleted=false and user.isActive=true";

    String GET_USER_BY_USERNAME_ISACTIVE = "FROM User as user where user.username=:username and user.isDeleted=false and user.isActive=:isActive";

    String SEARCH_TERM = " AND (COALESCE(:searchTerm) is null OR (lower(user.firstName) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%') OR lower(user.lastName) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%') OR lower(user.username) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%')))";
    
    String GET_USERS = "select distinct user from User as user join user.roles as role left join user.organizations as uo "
            + " WHERE (coalesce (:roles) is null or role.name in (:roles)) "
            +  "AND (:countryId is null or user.country.id = :countryId)"
            +  "AND (:countryId IS null OR role.name NOT IN ('SUPER_USER', 'JOB_USER'))"
            + " AND (COALESCE(:tenantIds) is null OR uo.id in (:tenantIds) or (user.tenantId is null and (coalesce (:cfrRoles) is null or role.name in (:cfrRoles)))) "
            + " AND user.isDeleted=false AND user.isActive=true"+SEARCH_TERM;

    String GET_USERS_WITH_SUPER_ADMINS = "select distinct user from User as user join user.roles as role left join user.organizations as uo "
            + "WHERE (coalesce (:roles) is null or role.name in (:roles)) "
            + "AND ((user.tenantId is null and user.username not in (:userName)) or ((COALESCE(:tenantIds) is null) OR (uo.id in (:tenantIds)))) "
            + "AND user.isDeleted = false AND user.isActive = true " + SEARCH_TERM;

    String GET_ALL_USERS_BY_ROLES = "select distinct user FROM User user" +
            " left join fetch user.roles as role " +
            " join user.organizations as org WHERE role.name in (:roleNames)" +
            " AND user.isDeleted=false AND user.isActive=true";

    String GET_ALL_USERS_BY_SUITE_ACCESS = "select distinct user FROM User user" +
            " left join fetch user.roles as role " +
            " join user.organizations as org WHERE role.suiteAccessName = :suiteAccessName" +
            " AND user.isDeleted=false AND user.isActive=true";

    String GET_ALL_USERS_BY_TENANT_ID_AND_ROLES = GET_ALL_USERS_BY_ROLES + " AND org.id in (:tenantIds)";

    String GET_ALL_USERS_BY_TENANT_ID = "select distinct user FROM User user" +
            " join user.organizations as org WHERE user.isDeleted=false AND user.isActive=true AND org.id in " +
            "(:tenantIds)";
    String GET_USERNAME_AND_PHONE_NUMBER = "FROM User as user where (user.username=:username OR user.phoneNumber = :phoneNumber) and user.countryCode = :countryCode and user.isDeleted=false and user.isActive=true";

    String GET_USERNAME_AND_PHONE_NUMBER_IN = "FROM User as user where (user.username in (:usernames) OR user.phoneNumber in (:phoneNumbers)) and user.isDeleted=false and user.isActive=true";

    String GET_USERS_BY_ROLE = "select user from User as user join user.roles as role"
            + " join user.organizations as org WHERE ((COALESCE(:tenantIds) is null OR org.id in (:tenantIds)) "
            + " AND role.name=:roleName AND user.isActive=true AND user.isDeleted=false) ";

    String GETS_BY_ROLE_OR_ID = GET_USERS_BY_ROLE + " OR user.id in (:userIds)";

    String SUPER_ADMIN_USERS = "select user from User as user join user.roles as role"
            + " WHERE role.name=:roleName AND user.isActive=true AND user.isDeleted=false "
            + " AND (COALESCE(:searchTerm) is null OR (lower(user.firstName) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%') OR lower(user.lastName) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%') OR lower(user.username) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%'))) ";

    String GET_PEER_SUPERVISOR_MAPPED_USERS =
            "select u.* from \"user\" as u join user_supervisor as us ON u.id =us" + ".user_id " +
                    "where us.supervisor_id = :userId AND u.is_active = true AND u.is_deleted = false AND us.is_active = true";

    String GET_PEER_SUPERVISOR_MAPPED_USERS_WITH_PAGINATION =
            "select distinct (u.id) as id, u.first_name as firstName, u.last_name as lastName, u.fhir_id as fhirId, " +
                    "uv.village_id as villageId, v.name as villageName " +
                    "from \"user\" as u join user_supervisor as us on u.id = us.user_id AND us.is_active = true join user_village uv on " +
                    "u.id = uv.user_id join village v on v.id = uv.village_id where " +
                    "(:userId is null or us.supervisor_id = :userId) and u.is_active = true and " +
                    "(COALESCE(:userIds) is null or u.id in (:userIds)) and " +
                    "(COALESCE(:villageIds) is null or v.id in (:villageIds)) and u.is_deleted = false";

    public static final String GET_USERS_BY_TENANT_IDS = "select user from User user "
            + "join user.organizations as org where org.id in (:tenantIds) AND user.isDeleted=false"
            + " and user.isActive=:isActive";

    public static final String SEARCH_TERM_CHECK = "AND (:searchTerm is null or lower(user.username) "
            + "LIKE CONCAT('%',lower(:searchTerm),'%')) ";

    public static final String GET_LOCKED_USERS = "select distinct user from User user join user.organizations "
            + "as org where org.id in (:tenantIds)  AND user.isBlocked = true AND user.isDeleted = false "
            + SEARCH_TERM_CHECK;

    public static final String GET_BLOCKED_USERS = "select user from User user where user.isBlocked = true"
            + " AND user.isDeleted = false "
            + SEARCH_TERM_CHECK;

    String GET_ORGANIZATIONS = "select new com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDetailsDTO(u.id as userId, o.formName as formName, " +
            " c_d.id as chiefdomDistrictId, c_d.name as chiefdomDistrictName, c_d.tenantId as chiefdomDistrictTenantId, c_d.countryId as chiefdomDistrictParentOrgId, " +
            " hf_d.id as healthFacilityDistrictId, hf_d.name as healthFacilityDistrictName, hf_d.tenantId as healthFacilityDistrictTenantId, hf_d.countryId as healthFacilityDistrictParentOrgId, " +
            " hf_c.id as healthFacilityChiefdomId, hf_c.name as healthFacilityChiefdomName, hf_c.tenantId as healthFacilityChiefdomTenantId, hf_c.districtId as healthFacilityChiefdomParentOrgId" +
            " ) from User u " +
            " LEFT JOIN Organization o ON o.id = u.tenantId " +
            " LEFT JOIN District d ON d.tenantId = o.id " +
            " LEFT JOIN Chiefdom c ON c.tenantId = o.id " +
            " LEFT JOIN HealthFacility hf ON hf.tenantId = o.id " +
            " LEFT JOIN District c_d ON c.districtId = c_d.id " +
            " LEFT JOIN Chiefdom hf_c ON hf.chiefdom.id = hf_c.id " +
            " LEFT JOIN District hf_d ON hf.district.id = hf_d.id where u.id in (:userId)";

    public static final String GET_USERS_BY_ROLENAME = "select user from User as user join user.roles "
            + "as role where role.name=:roleName and user.isActive=true";

    public static final String GET_USERS_BY_ROLE_IDS = "Select user from User user join user.roles as role "
            + "join user.organizations as org where org.id=:tenantId AND (:searchTerm is null or lower(user.username) "
            + "LIKE CONCAT('%',lower(:searchTerm),'%')) AND role.id in :roleIds AND user.isDeleted=false";

    /**
     * Gets User by username
     *
     * @param username
     * @return User
     */
    @Query(value = GET_USER_BY_USERNAME)
    User getUserByUsername(@Param("username") String username);

    /**
     * Gets a User id and isDeleted and isActive
     *
     * @param id
     * @return User
     */
    User findByIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     * Gets User by forgetpassword token
     *
     * @param token
     * @return User
     */
    User findByForgetPasswordToken(String token);

    /**
     * Finds a List of users by usernames and isDeleted and isActive
     *
     * @param list
     * @param isDeleted
     * @param isActive
     * @return List<User>
     */
    List<User> findByUsernameInAndIsDeletedAndIsActive(List<String> list, boolean isDeleted, boolean isActive);

    /**
     * Find a List of Users by ids
     *
     * @param linkedSupervisorIds
     * @return List<User>
     */
    List<User> findByIdIn(List<Long> linkedSupervisorIds);

    /**
     * Finds List of user by ids
     *
     * @param ids
     * @param isDeleted
     * @param isActive
     * @return List<User>
     */
    List<User> findByIdInAndIsDeletedAndIsActive(List<Long> ids, boolean isDeleted, boolean isActive);

    /**
     * Gets users by given tenant ids.
     *
     * @param tenantIds tenant ids for which the users need to be listed is given
     * @param searchTerm the search term for which the users need to be listed is given
     * @param pageable Pageable of for which the users need to be listed is given
     * @return Page<User> Page of users with super admins are retrieved
     */
    @Query(value = GET_USERS)
    Page<User> getUsers(@Param("tenantIds") Set<Long>tenantIds , @Param("searchTerm") String searchTerm,
                        @Param("countryId") Long countryId, @Param("roles") List<String> roles,  @Param("cfrRoles") List<String> cfrRoles, Pageable pageable);

    /**
     * Gets users by given tenant ids.
     *
     * @param tenantIds tenant ids for which the users need to be listed is given
     * @param searchTerm the search term for which the users need to be listed is given
     * @param pageable Pageable of for which the users need to be listed is given
     * @return Page<User> Page of users with super admins are retrieved
     */
    @Query(value = GET_USERS_WITH_SUPER_ADMINS)
    Page<User> getUsersWithSuperAdmins(@Param("tenantIds") Set<Long> tenantIds, @Param("searchTerm") String searchTerm,
            @Param("roles") List<String> roles, @Param("userName") String userName, Pageable pageable);

    /**
     * Gets a user by username and phonenumber.
     *
     * @param username
     * @param phoneNumber
     * @return User
     */
    @Query(value = GET_USERNAME_AND_PHONE_NUMBER)
    User getUserByUsernameOrPhoneNumber(@Param("username") String username, @Param("phoneNumber") String phoneNumber, @Param("countryCode") String countryCode);

    /**
     * Gets a users by usernames and phonenumbers.
     *
     * @param username
     * @param phoneNumber
     * @return List<User>
     */
    @Query(value = GET_USERNAME_AND_PHONE_NUMBER_IN)
    List<User> getUsersByUsernameOrPhoneNumber(@Param("usernames") List<String> username, @Param("phoneNumbers") List<String> phoneNumber);

    /**
     * Gets a list super admins
     *
     * @param roleSuperAdmin
     * @param searchTerm
     * @param pageable
     * @return Page<User>
     */
    @Query(value = SUPER_ADMIN_USERS)
    Page<User> getSuperAdminUsers(@Param("roleName") String roleSuperAdmin, @Param("searchTerm") String searchTerm, Pageable pageable);

    /**
     * Gets User by username
     *
     * @param username
     * @return User
     */
    @Query(value = GET_USER_BY_USERNAME_ISACTIVE)
    User getUserByUsername(@Param("username") String username, @Param("isActive") boolean isActive);


    /**
     * Finds a user by phone number.
     *
     * @param phoneNumber
     * @return User
     */
    User findByPhoneNumberAndIsDeletedFalse(String phoneNumber);

    /**
     * Finds a user by phone number and country code.
     *
     * @param phoneNumber
     * @param countryCode
     * @return User
     */
    User findByPhoneNumberAndCountryCodeAndIsDeletedFalse(String phoneNumber, String countryCode);

    /**
     * Gets a user by roleName.
     *
     * @param roleName
     * @param tenantIds
     * @return List<User>
     */
    @Query(value = GET_USERS_BY_ROLE)
    List<User> getUsersByRoleName(@Param("roleName") String roleName, @Param("tenantIds") Set<Long> tenantIds);


    /**
     * Fetches all users from the database that have any of the roles specified.
     *
     * @param roleNames A list of role names to filter users by.
     * @return List<User> - Returns a list of User entities that have any of the specified roles.
     */
    @Query(value = GET_ALL_USERS_BY_ROLES)
    List<User> findAllByRoleNames(@Param("roleNames") List<String> roleNames);

    String GET_ALL_USERS_BY_VILLAGES = "select distinct user FROM User user" +
            " join user.villages as village WHERE village.id in (:villageIds)" +
            " AND user.isDeleted=false AND user.isActive=true AND user.id != :id";

    /**
     * Fetches all users from the database that have any of the roles specified.
     *
     * @param villageIds A list of role names to filter users by.
     * @return List<User> - Returns a list of User entities that have any of the specified roles.
     */
    @Query(value = GET_ALL_USERS_BY_VILLAGES)
    List<User> findAllByVillageIds(@Param("villageIds") Set<Long> villageIds, @Param("id") Long id);

    /**
     * Fetches all users from the database that have the specified suite access.
     *
     * @param suiteAccessName The name of the suite access to filter users by.
     * @return List<User> - Returns a list of User entities that have the specified suite access.
     */
    @Query(value = GET_ALL_USERS_BY_SUITE_ACCESS)
    List<User> findAllBySuiteAccess(@Param("suiteAccessName") String suiteAccessName);

    /**
     * Fetches all users from the database that have any of the roles specified and belong to the specified tenant.
     *
     * @param roleNames A list of role names to filter users by.
     * @param tenantIds The ID of the tenant to filter users by.
     * @return List<User> - Returns a list of User entities that have any of the specified roles and belong to the specified tenant.
     */
    @Query(value = GET_ALL_USERS_BY_TENANT_ID_AND_ROLES)
    List<User> findAllByRoleNamesAndTenantId(@Param("roleNames") List<String> roleNames, @Param("tenantIds") List<Long> tenantIds);

    /**
     * Fetches all users from the database that have any of the roles specified and belong to the specified tenant.
     *
     * @param tenantIds The ID of the tenant to filter users by.
     * @return List<User> - Returns a list of User entities that have any of the specified roles and belong to the specified tenant.
     */
    @Query(value = GET_ALL_USERS_BY_TENANT_ID)
    List<User> findAllByTenantId(@Param("tenantIds") List<Long> tenantIds);

    /**
     * Finds user by organization  id.
     *
     * @param tenantId
     * @param b
     * @param c
     * @return
     */
    List<User> findByOrganizations_IdAndIsDeletedAndIsActive(Long tenantId, boolean b, boolean c);

    /**
     * Gets users by role names and id.
     *
     * @param roleName
     * @param tenantIds
     * @param userIds
     * @return
     */
    @Query(value = GETS_BY_ROLE_OR_ID)
    List<User> getUserByRoleOrId(@Param("roleName") String roleName, @Param("tenantIds") List<Long> tenantIds, @Param("userIds") List<Long> userIds);

    /**
     * Get mapped user to PeerSupervisor
     * @param l supervisor id
     * @return List of users
     */
    @Query(value = GET_PEER_SUPERVISOR_MAPPED_USERS, nativeQuery = true)
    List<User> getUsersMappedToPeerSupervisor(@Param("userId")long l);

    /**
     * Get mapped user to PeerSupervisor
     * @param l supervisor id
     * @return List of users
     */
    @Query(value = GET_PEER_SUPERVISOR_MAPPED_USERS_WITH_PAGINATION, nativeQuery = true)
    Page<Map<String, Object>> getUsersMappedToPeerSupervisorWithPagination(@Param("userId") Long l,
                                                                           @Param("userIds") Set<Long> userIds,
                                                                           @Param("villageIds") Set<Long> villageIds,
                                                                           Pageable pageable);

    /**
     * <p>
     * This method find active user with Forget password short token.
     * </p>
     *
     * @param token {@link String} - Forgot password Short token
     * @return {@link User} - User Entity
     */
    User findByForgetPasswordShortTokenAndIsDeletedFalseAndIsActiveTrue(String token);

    /**
     * <p>
     * This method is used to identify whether a user already exists with the given short forgot password token.
     * It returns a boolean value
     * </p>
     *
     * @param shortToken {@link String} - Short Forgot password token
     * @param isActive {@link Boolean} - isActive value
     * @param isDeleted {@link Boolean} - isDeleted value
     * @return a {@link Boolean} value based on the existence of the User based on given values.
     */
    boolean existsByForgetPasswordShortTokenAndIsActiveAndIsDeleted(String shortToken, boolean isActive, boolean isDeleted);

    /**
     * <p>
     * This method is used to get the users by an tenants ids.
     * </p>
     *
     * @param tenantIds {@link List<Long>} The request contains necessary information
     *                 to get the users
     * @return {@link List<User>} Return the list users details
     */
    @Query(value = GET_USERS_BY_TENANT_IDS)
    public List<User> findUsersByTenantIds(@Param(Constants.TENANT_IDS) List<Long> tenantIds,
                                           @Param(Constants.IS_ACTIVE) boolean isActive);

    /**
     * <p>
     * This method is used to get the list of distinct users of given search term
     * and list of tenant ids with pageable from the database who are all
     * blocked and not deleted.
     * </p>
     *
     * @param tenantIds  {@link List<Long>} The list of tenantIds that belongs to the users who are locked is given
     * @param searchTerm {@link String} The term or keyword that is being searched for in the query to filter the
     *                   results is given
     * @param pageable   {@link Pageable} The pagination information that contains information such as the page number,
     *                   page size, sorting criteria, and more is given
     * @return {@link List<User>} The list of locked users for given search term, list of tenant ids and pagination
     * is retrieved and returned from database
     */
    @Query(value = GET_LOCKED_USERS)
    public Page<User> getLockedUsers(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                                     @Param(Constants.TENANT_IDS) List<Long> tenantIds, Pageable pageable);

    /**
     * <p>
     * This method is used to get the list of users of given search term with pageable
     * from the database who are all blocked and not deleted.
     * </p>
     *
     * @param searchTerm {@link String} The term or keyword that is being searched for in the query to filter the results is given
     * @param pageable   {@link Pageable} The pagination information that contains information such as the page number,
     *                   page size, sorting criteria, and more is given
     * @return {@link List<User>} The list of blocked users for given search term and pagination is retrieved and
     * returned from database
     */
    @Query(value = GET_BLOCKED_USERS)
    public Page<User> getBlockedUsers(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm, Pageable pageable);

    /**
     * <p>
     * Retrieves a list of organizations for the users
     * </p>
     *
     * @param userIds - Ids in the organization table
     * @return a list of organizations that match the specified form name
     */
    @Query(value = GET_ORGANIZATIONS)
    List<OrganizationDetailsDTO> getOrganizationsByUsers(@Param(Constants.USER_ID_PARAM) List<Long> userIds, Sort sort);
    /**
     * Gets a user by roleName.
     *
     * @param roleName
     * @return List<User>
     */
    @Query(value = GET_USERS_BY_ROLENAME)
    List<User> getUsersByRole(@Param("roleName") String roleName);
    
    /**
     * <p>
     * Validates a user by its user name and return it if exists.
     * </p>
     *
     * @param email user name
     * @return User entity.
     */
    public User findByUsernameIgnoreCaseAndIsDeletedFalse(String email);

    /**
     * <p>
     * Retrieves a list of users based on the specified role IDs, tenant ID, and an optional search term.
     * </p>
     *
     * @param roleIds the list of role IDs to filter users by.
     * @param tenantId the tenant ID to filter users by.
     * @param searchTerm an optional search term to further filter users.
     * @return a list of users matching the specified criteria.
     */
    @Query(value = GET_USERS_BY_ROLE_IDS)
    public List<User> findUsersByRoleIdS(@Param("roleIds") List<Long> roleIds, @Param("tenantId") Long tenantId,
                                         @Param("searchTerm") String searchTerm);
}
