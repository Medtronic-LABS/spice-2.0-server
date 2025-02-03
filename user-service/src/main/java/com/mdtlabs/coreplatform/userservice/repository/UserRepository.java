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

    public static final String GET_USERS_BY_ROLENAME = "select user from User as user join user.roles "
            + "as role where role.name=:roleName and user.isActive=true";

    public static final String GET_USERS_BY_ROLE_IDS = "Select user from User user join user.roles as role "
            + "join user.organizations as org where org.id=:tenantId AND (:searchTerm is null or lower(user.username) "
            + "LIKE CONCAT('%',lower(:searchTerm),'%')) AND role.id in :roleIds AND user.isDeleted=false";

    String PHONE_NUMBER_SEARCH = " AND (COALESCE(:phoneNumber) is null or user.phoneNumber LIKE CONCAT('%', :phoneNumber,'%'))";

    String GET_ALL_USERS = "select distinct user from User as user join user.roles as role left join user.organizations as uo "
            + " WHERE (coalesce (:roles) is null or role.name in (:roles)) "
            + " AND ((COALESCE(:tenantIds) IS NULL AND (user.country.id IS NULL OR user.country.id = :countryId)) OR (COALESCE(:tenantIds) IS NOT NULL AND user.country.id = :countryId))"
            + " AND (:countryId IS null OR role.name NOT IN ('SUPER_USER', 'JOB_USER'))"
            + " AND (COALESCE(:tenantIds) is null OR uo.id in (:tenantIds)) "
            + " AND user.isDeleted=false " + SEARCH_TERM + PHONE_NUMBER_SEARCH;

    public static final String GET_USERS_BASE = "SELECT " +
            " \"result\".role_string as roles, " +
            " \"result\".LastName as LastName, " +
            " \"result\".gender as gender, " +
            " \"result\".phoneNumber as phoneNumber, " +
            " \"result\".id as id, " +
            " \"result\".tenantId as tenantId, " +
            " \"result\".username as username, " +
            " \"result\".countryId as countryId, " +
            " \"result\".fhirId as fhirId, " +
            " \"result\".countryCode as countryCode, " +
            " \"result\".suiteAccess as suiteAccess, " +
            " \"result\".firstName as firstName, " +
            " \"result\".designationId as designationId, " +
            " \"result\".countryName as countryName, " +
            " \"result\".phoneNumberCode as phoneNumberCode, " +
            " \"result\".unitMeasurement as unitMeasurement, " +
            " \"result\".regionCode as regionCode, " +
            " \"result\".countryTenantId as countryTenantId, " +
            " \"result\".appTypes as appTypes, " +
            " \"result\".cultureName as cultureName, " +
            " \"result\".cultureCode as cultureCode, " +
            " \"result\".designationName as designationName, " +
            " \"result\".cultureId as cultureId, " +
            " \"result\".timezoneId as timezoneId, " +
            " \"result\".offset as offset, " +
            " \"result\".description as description, " +
            " \"result\".isDeleted as isDeleted, " +
            " \"result\".isActive as isActive, " +
            " \"result\".formName as formName, " +
            " \"result\".districtId as districtId, " +
            " \"result\".districtName as districtName, " +
            " \"result\".districtTenantId as districtTenantId, " +
            " \"result\".districtParentOrgId as districtParentOrgId, " +
            " \"result\".healthFacilityId as healthFacilityId, " +
            " \"result\".healthFacilityName as healthFacilityName, " +
            " \"result\".healthFacilityTenantId as healthFacilityTenantId, " +
            " \"result\".healthFacilityParentOrgId as healthFacilityParentOrgId, " +
            " \"result\".chiefdomId as chiefdomId, " +
            " \"result\".chiefdomName as chiefdomName, " +
            " \"result\".chiefdomTenantId as chiefdomTenantId, " +
            " \"result\".chiefdomParentOrgId as chiefdomParentOrgId, " +
            " \"result\".chiefdomDistrictId as chiefdomDistrictId, " +
            " \"result\".chiefdomDistrictName as chiefdomDistrictName, " +
            " \"result\".chiefdomDistrictTenantId as chiefdomDistrictTenantId, " +
            " \"result\".chiefdomDistrictParentOrgId as chiefdomDistrictParentOrgId, " +
            " \"result\".healthFacilityDistrictId as healthFacilityDistrictId, " +
            " \"result\".healthFacilityDistrictName as healthFacilityDistrictName, " +
            " \"result\".healthFacilityDistrictTenantId as healthFacilityDistrictTenantId, " +
            " \"result\".healthFacilityDistrictParentOrgId as healthFacilityDistrictParentOrgId, " +
            " \"result\".healthFacilityChiefdomId as healthFacilityChiefdomId, " +
            " \"result\".healthFacilityChiefdomName as healthFacilityChiefdomName, " +
            " \"result\".healthFacilityChiefdomTenantId as healthFacilityChiefdomTenantId, " +
            " \"result\".healthFacilityChiefdomParentOrgId as healthFacilityChiefdomParentOrgId, " +
            " \"result\".insightUserOrganizationId as insightUserOrganizationId, " +
            " \"result\".insightUserOrganizationName as insightUserOrganizationName, " +
            " \"result\".insightUserOrganizationFormDataId as insightUserOrganizationFormDataId, " +
            " \"result\".insightUserOrganizationParentOrganizationId as insightUserOrganizationParentOrganizationId, " +
            " \"result\".insightUserOrganizationFormName as insightUserOrganizationFormName, " +
            " \"result\".reportUserOrganizationId as reportUserOrganizationId, " +
            " \"result\".reportUserOrganizationName as reportUserOrganizationName, " +
            " \"result\".reportUserOrganizationFormDataId as reportUserOrganizationFormDataId, " +
            " \"result\".reportUserOrganizationParentOrganizationId as reportUserOrganizationParentOrganizationId, " +
            " \"result\".reportUserOrganizationFormName as reportUserOrganizationFormName " +
            " FROM (" +
            " SELECT " +
            " distinct on (u.id) u.id as id, " +
            " u.country_id as countryId, " +
            " u.is_deleted as isDeleted, " +
            " u.is_active as isActive, " +
            " u.username as username, " +
            " u.first_name as firstName, " +
            " u.tenant_id as tenantId, " +
            " u.last_name as LastName, " +
            " u.gender as gender, " +
            " u.phone_number as phoneNumber, " +
            " u.country_code as countryCode, " +
            " u.fhir_id as fhirId, " +
            " u.created_by as created_by, " +
            " u.culture_id as cultureId, " +
            " u.timezone_id as timezoneId, " +
            " u.updated_at as updatedAt, " +
            " u.designation_id as designationId, " +
            " array_to_string(u.suite_access, ',') as suiteAccess, " +
            " array_to_string(ARRAY_AGG(role.name), ',') as role_string, " +
            " ARRAY_AGG(role.name) as roleNames, " +
            " ARRAY_AGG(uo.organization_id) AS organizationId, " +
            " ARRAY_AGG(ruo.organization_id) AS reportOrganizationId, " +
            " country.name as countryName, " +
            " country.phone_number_code as phoneNumberCode, " +
            " country.unit_measurement as unitMeasurement, " +
            " country.region_code as regionCode, " +
            " country.tenant_id as countryTenantId, " +
            " array_to_string(country.app_types, ',') as appTypes, " +
            " cul.code as cultureCode, " +
            " cul.name as cultureName, " +
            " t.offset as offset, " +
            " t.description as description, " +
            " des.name as designationName, " +
            " o.form_name as formName, " +
            " d.id as districtId, " +
            " d.name as districtName, " +
            " d.tenant_id as districtTenantId, " +
            " d.country_id as districtParentOrgId, " +
            " hf.id as healthFacilityId, " +
            " hf.name as healthFacilityName, " +
            " hf.tenant_id as healthFacilityTenantId, " +
            " hf.country_id as healthFacilityParentOrgId, " +
            " c.id as chiefdomId, " +
            " c.name as chiefdomName, " +
            " c.tenant_id as chiefdomTenantId, " +
            " c.country_id as chiefdomParentOrgId, " +
            " c_d.id as chiefdomDistrictId, " +
            " c_d.name as chiefdomDistrictName, " +
            " c_d.tenant_id as chiefdomDistrictTenantId, " +
            " c_d.country_id as chiefdomDistrictParentOrgId, " +
            " hf_d.id as healthFacilityDistrictId, " +
            " hf_d.name as healthFacilityDistrictName, " +
            " hf_d.tenant_id as healthFacilityDistrictTenantId, " +
            " hf_d.country_id as healthFacilityDistrictParentOrgId, " +
            " hf_c.id as healthFacilityChiefdomId, " +
            " hf_c.name as healthFacilityChiefdomName, " +
            " hf_c.tenant_id as healthFacilityChiefdomTenantId, " +
            " hf_c.district_id as healthFacilityChiefdomParentOrgId, " +
            " ruo_organization.id AS reportUserOrganizationId, " +
            " ruo_organization.name AS reportUserOrganizationName, " +
            " ruo_organization.form_data_id AS reportUserOrganizationFormDataId, " +
            " ruo_organization.parent_organization_id AS reportUserOrganizationParentOrganizationId, " +
            " ruo_organization.form_name AS reportUserOrganizationFormName, " +
            " iuo_organization.id AS insightUserOrganizationId, " +
            " iuo_organization.name AS insightUserOrganizationName, " +
            " iuo_organization.form_data_id AS insightUserOrganizationFormDataId, " +
            " iuo_organization.parent_organization_id AS insightUserOrganizationParentOrganizationId, " +
            " iuo_organization.form_name AS insightUserOrganizationFormName " +
            " FROM  " +
            " \"user\" AS u " +
            " LEFT JOIN " +
            " user_role ur ON ur.user_id = u.id " +
            " LEFT JOIN " +
            " role ON role.id = ur.role_id " +
            " LEFT JOIN " +
            " timezone t ON t.id = u.timezone_id " +
            " LEFT JOIN " +
            " user_organization uo ON uo.user_id = u.id " +
            " LEFT JOIN " +
            " country as country on country.id = u.country_id " +
            " LEFT JOIN " +
            " culture cul on cul.id = u.culture_id " +
            " LEFT JOIN " +
            " designation des on des.id = u.designation_id " +
            " LEFT JOIN " +
            " organization o ON o.id = u.tenant_id " +
            " LEFT JOIN " +
            " district d ON d.tenant_id = o.id " +
            " LEFT JOIN " +
            " chiefdom c ON c.tenant_id = o.id " +
            " LEFT JOIN " +
            " health_facility hf ON hf.tenant_id = o.id " +
            " LEFT JOIN " +
            " district c_d ON c.district_id = c_d.id " +
            " LEFT JOIN " +
            " chiefdom hf_c ON hf.chiefdom_id = hf_c.id " +
            " LEFT JOIN " +
            " district hf_d ON hf.district_id = hf_d.id " +
            " LEFT JOIN " +
            " report_user_organization ruo ON ruo.user_id = u.id " +
            " LEFT JOIN " +
            " insight_user_organization iuo ON iuo.user_id = u.id " +
            " LEFT JOIN " +
            " organization ruo_organization ON ruo.organization_id = ruo_organization.id " +
            " LEFT JOIN " +
            " organization iuo_organization on iuo.organization_id = iuo_organization.id " +
            " GROUP BY " +
            " u.id, " +
            " country.name, " +
            " country.phone_number_code, " +
            " country.unit_measurement, " +
            " country.region_code, " +
            " country.tenant_id, " +
            " country.app_types, " +
            " cul.name, " +
            " cul.code, " +
            " t.offset, " +
            " t.description, " +
            " des.name, " +
            " o.form_name, " +
            " hf_d.id, " +
            " hf_c.id, " +
            " c_d.id, " +
            " c.id, " +
            " d.id, " +
            " hf.id, " +
            " ruo_organization.id, " +
            " ruo_organization.name, " +
            " ruo_organization.form_data_id, " +
            " ruo_organization.parent_organization_id, " +
            " ruo_organization.form_name, " +
            " iuo_organization.id, " +
            " iuo_organization.name, " +
            " iuo_organization.form_data_id, " +
            " iuo_organization.parent_organization_id, " +
            " iuo_organization.form_name " +
            " ) as \"result\" " ;
    
    public static final String SEARCH_TERM_BASE = " AND result.isDeleted = false " +
            " AND result.isActive = true " +
            " AND (COALESCE(:searchTerm) is null OR " +
            " (lower(result.firstName) LIKE CONCAT('%', lower(CAST(:searchTerm AS text)), '%') " +
            " OR lower(result.lastName) LIKE CONCAT('%', lower(CAST(:searchTerm AS text)), '%') " +
            " OR lower(result.username) LIKE CONCAT('%', lower(CAST(:searchTerm AS text)), '%'))) ";

    public static final String GET_USERS_WITH_SUPER_ADMINS_NATIVE = GET_USERS_BASE +
            " WHERE " +
            " (CAST(:roles AS VARCHAR[]) is null OR result.roleNames && CAST(:roles as varchar[])) " +
            " AND  (CAST(:notInRoles AS VARCHAR[]) is null OR NOT (result.roleNames && CAST(:notInRoles as varchar[]))) " +
            " AND ((result.tenantId is null and result.username not in (:userName)) or ((COALESCE(:tenantIds) is null) OR (result.organizationId && CAST(:tenantIds AS bigint[])))) " +
            SEARCH_TERM_BASE;


    public static final String GET_USERS_NATIVE = GET_USERS_BASE +
            " WHERE " +
            " (CAST(:roles AS VARCHAR[]) is null OR result.roleNames && CAST(:roles as varchar[])) " +
            " AND (CAST(:notInRoles AS VARCHAR[]) is null OR NOT (result.roleNames && CAST(:notInRoles as varchar[]))) " +
            " AND (:countryId is null OR result.countryId = :countryId) " +
            " AND (:countryId IS null OR result.role_string NOT IN ('SUPER_USER', 'JOB_USER')) " +
            " AND (COALESCE(:tenantIds) IS NULL OR result.organizationId && CAST(:tenantIds AS bigint[]) " +
            " or ((:isFacilityUsersOnly = false and result.tenantId is null) and (CAST(:cfrRoles as varchar[]) is null or result.roleNames && CAST(:cfrRoles as varchar[])))" +
            " or result.reportOrganizationId && CAST(:tenantIds AS bigint[])) " +
            SEARCH_TERM_BASE;

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
     * <p>
     * Executes a native SQL query to retrieve a paginated list of users, including super admins, based on various filtering criteria.
     * The query fetches user data and returns results in a pageable format, considering tenant IDs, roles, and search terms.
     * </p>
     *
     * @param tenantIds   a Set of tenant IDs to filter the users by their associated tenant(s).
     * @param searchTerm  a String term to search within user data (e.g., username, name, etc.).
     * @param roles       an array of Strings representing the roles to filter users by their assigned roles.
     * @param userName    a String representing the username to filter users by their username.
     * @param pageable    a Pageable object containing pagination information (e.g., page number, page size, sorting).
     *
     * @return a Page object containing a paginated result of Map<String, Object> entries,
     *         where each Map represents a row of user data retrieved from the query.
     *
     * This method uses a native SQL query (specified by the GET_USERS_WITH_SUPER_ADMINS_NATIVE constant)
     * to retrieve user data, including super admin users, filtered by the provided criteria such as
     * tenant IDs, roles, search term, and username. The results are returned in a paginated format
     * based on the provided Pageable parameter.
     *
     */
    @Query(value = GET_USERS_WITH_SUPER_ADMINS_NATIVE, nativeQuery = true)
    Page<Map<String, Object>> getUsersWithSuperAdminsNative(@Param("tenantIds") Long[] tenantIds, @Param("searchTerm") String searchTerm,
                                             @Param("roles") String[] roles, @Param("notInRoles") String[] notInRoles, @Param("userName") String userName, Pageable pageable);

    /**
     * <p>
     * Executes a native SQL query to retrieve a paginated list of users based on various filtering criteria.
     * The query fetches user data, including roles, tenant IDs, and country information, and returns the result
     * in a pageable format.
     * </p>
     *
     * @param tenantIds   a Set of tenant IDs to filter the users by their associated tenant(s).
     * @param searchTerm  a String term to search within the user data (e.g., username, name).
     * @param countryId   a Long representing the country ID to filter users by their associated country.
     * @param roles       an array of Strings representing roles to filter users by their assigned roles.
     * @param cfrRoles    an array of Strings representing specific CFR roles to filter users.
     * @param pageable    a Pageable object containing pagination information (e.g., page number, size).
     *
     * @return a Page object containing a paginated result of Map<String, Object> entries,
     *         where each map represents a row of user data retrieved from the query.
     *
     * This method uses a native SQL query (specified by the GET_USERS_NATIVE constant) to fetch user data
     * based on the provided filtering criteria, such as tenant IDs, search terms, country ID, roles, and CFR roles.
     * It returns the results in a paginated format using the Pageable parameter, which determines the page size
     * and sorting behavior.
     *
     */
    @Query(value = GET_USERS_NATIVE, nativeQuery = true)
    Page<Map<String, Object>> getUsersnative(@Param("tenantIds") Long[] tenantIds, @Param("searchTerm") String searchTerm, @Param("countryId") Long countryId,
                                             @Param("roles") String[] roles, @Param("notInRoles") String[] notInRoles, @Param("cfrRoles") String[] cfrRoles,
                                             @Param("isFacilityUsersOnly") boolean isFacilityUsersOnly, Pageable pageable);

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

    /**
     * Gets All users by given tenant ids.
     *
     * @param tenantIds  tenant ids for which the users need to be listed is given
     * @param searchTerm the search term for which the users need to be listed is given
     * @param pageable   Pageable of for which the users need to be listed is given
     * @return Page<User> Page of users with super admins are retrieved
     */
    @Query(value = GET_ALL_USERS)
    Page<User> getAllUsers(@Param(Constants.TENANT_IDS) Set<Long> tenantIds, @Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                           @Param(Constants.COUNTRY_ID) Long countryId, @Param(Constants.ROLE_REDIS_KEY) List<String> roles,
                           Pageable pageable, @Param(Constants.PARAM_PHONE_NUMBER) Long phoneNumber);


}
