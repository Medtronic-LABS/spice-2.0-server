package com.mdtlabs.coreplatform.userservice.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the role module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE.
 * </p>
 *
 * @author Prabu created on Sep 16, 2022
 */
@Repository
public interface RoleRepository extends JpaRepository<Role, Long> {

    public static final String GET_ALL_ROLES = "select role from Role as role where role.isActive =:status AND role.groupName is not null";

    public static final String GET_ROLE_BY_ID = "select role from Role as role where role.id =:roleId and role.isActive=true and role.isDeleted = false";

    public static final String GET_ROLE_BY_NAME = "select role from Role as role"
            + " where role.name =:name and role.isActive=true and role.isDeleted = false";

    public static final String GET_ROLES = "SELECT role from Role as role left join role.countryRoles as countryRoles where role.level >=:level " +
            "and countryRoles.countryId = :countryId OR role.isActive=true ";

    /**
     * <p>
     * This method is used to get the list of roles from the database which
     * are all either active or not active from the database.
     * </p>
     *
     * @param status It is used as a filter the list of roles that have the specified active status
     * @return {@link List<Role>} The list of roles is retrieved based on the given active status
     */
    @Query(value = GET_ALL_ROLES)
    public List<Role> getAllRoles(@Param(FieldConstants.STATUS) boolean status);

    /**
     * <p>
     * This method is used to get the role of the given id from the database.
     * </p>
     *
     * @param roleId The id of the role that need to retrieve from the database is given
     * @return {@link Role} The role is retrieved which has the provided role id
     */
    @Query(value = GET_ROLE_BY_ID)
    public Role getRoleById(@Param(Constants.ROLE_ID_PARAM) long roleId);

    /**
     * <p>
     * This method used to get the role of the given name from the database.
     * </p>
     *
     * @param name {@link String} The name of the role that need to retrieve from the database is given
     * @return {@link Role} The role is retrieved which has the provided role name
     */
    @Query(value = GET_ROLE_BY_NAME)
    public Role getRoleByName(@Param(FieldConstants.NAME) String name);

    /**
     * <p>
     * This method used to get list of roles of the given list of role names from the database.
     * </p>
     *
     * @param roleNames {@link List<String>} The list of role names that need to retrieve from the database is given
     * @return {@link Set<Role>} The set of roles with the given names that are neither
     * deleted nor inactive has been retrieved
     */
    public Set<Role> findByIsDeletedFalseAndIsActiveTrueAndNameIn(List<String> roleNames);

    /**
     * <p>
     * This method used to get list of roles of the given list of ids from the database.
     * </p>
     *
     * @param roleIds {@link List<Long>} The list of role IDs that need to retrieve from the database is given
     * @return {@link Set<Role>} The set of roles with the given ids that are neither
     * deleted nor inactive has been retrieved
     */
    public Set<Role> findByIsDeletedFalseAndIsActiveTrueAndIdIn(List<Long> roleIds);

    /**
     * <p>
     * This method is used to retrieve a list of roles from the database based on the provided list of IDs.
     * </p>
     *
     * @param ids The list of IDs for which roles need to be retrieved.
     * @return List<Role> A list of roles that match the given IDs.
     */
    public List<Role> findByIdIn(List<Long> ids);

    /**
     * <p>
     * This method is used to retrieve a list of roles from the database based on the provided country ID.
     * </p>
     *
     * @param countryId The ID of the country for which roles need to be retrieved.
     * @return List<Role> A list of roles that are associated with the given country ID.
     */
    @Query(value = GET_ROLES)
    public List<Role> getRolesByCountry(@Param("countryId") Long countryId, @Param("level") Long level);

}
