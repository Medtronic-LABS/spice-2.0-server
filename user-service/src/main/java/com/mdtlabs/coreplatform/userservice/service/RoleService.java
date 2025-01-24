package com.mdtlabs.coreplatform.userservice.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;

/**
 * <p>
 * RoleService interface is an interface for role module that can be implemented
 * in any class.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */

public interface RoleService {

    /**
     * <p>
     * This method is used to add a new role.
     * </p>
     *
     * @param role {@link Role} The role to be created and added to the database is given
     * @return {@link Role} The created role is returned
     */
    Role addRole(Role role);

    /**
     * <p>
     * This method is used to get the list of roles.
     * </p>
     *
     * @return {@link List<Role>} The list of roles retrieved is returned
     */
    List<Role> getAllRoles();

    /**
     * <p>
     * This method is used to update the role using the given role details.
     * </p>
     *
     * @param role {@link Role} The role which is to be updated with details is given
     * @return {@link Role} The role is updated and returned
     */
    Role updateRole(Role role);

    /**
     * <p>
     * This method is used to soft delete the role for the given id.
     * </p>
     *
     * @param roleId The id of the role which is to be deleted is given
     * @return The int value is returned if the role is successfully deleted for the given id
     */
    int deleteRoleById(long roleId);

    /**
     * <p>
     * This method is used to get the role of the given id.
     * </p>
     *
     * @param roleId The id of the role that need to retrieve is given
     * @return {@link Role} The role is retrieved which has the provided role id
     */
    Role getRoleById(long roleId);

    /**
     * <p>
     * This method used to get the role of the given name.
     * </p>
     *
     * @param name {@link String} The name of the role that need to retrieve is given
     * @return {@link Role} The role is retrieved which has the provided role name
     */
    Role getRoleByName(String name);

    /**
     * <p>
     * This method used to get list of roles of the given list of role names.
     * </p>
     *
     * @param roles {@link List<String>} The list of role names that need to retrieve is given
     * @return {@link Set<Role>} A set of roles that match the names provided in the name list is returned
     */
    Set<Role> getRolesByName(List<String> roles);

    /**
     * <p>
     * This method is used to get a set of roles of given list of role IDs.
     * </p>
     *
     * @param roleIds {@link List<Long>} A list of IDs of the roles that need to be retrieved is given
     * @return {@link Set<Role>} A set of roles identified by the list of role IDs is returned
     */
    Set<Role> getRolesByIds(List<Long> roleIds);

    /**
     * <p>
     * Retrieves a map of roles based on a list of role IDs.
     * This method is intended to efficiently fetch roles by their IDs and return them in a map where
     * the key is the role ID and the value is the corresponding {@link Role} object.
     * </p>
     *
     * @param ids A list of role IDs for which roles need to be retrieved.
     * @return A map with role IDs as keys and {@link Role} objects as values.
     */
    Map<Long, Role> getRoleMap(List<Long> ids);

    /**
     * <p>
     * Retrieves a mapping of role groups based on the criteria specified in {@link SearchRequestDTO}.
     * This method is designed to categorize roles into groups based on certain criteria (e.g., role type, permissions)
     * specified in the {@link SearchRequestDTO} and return a map where the key is a group identifier (e.g., role type)
     * and the value is a list of {@link RoleResponseDTO} objects belonging to that group.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria for grouping roles.
     * @return A map where each key represents a group identifier and each value is a list of {@link RoleResponseDTO} objects belonging to that group.
     */
	Map<String, List<RoleResponseDTO>> getRoleGroups(SearchRequestDTO request);
}
