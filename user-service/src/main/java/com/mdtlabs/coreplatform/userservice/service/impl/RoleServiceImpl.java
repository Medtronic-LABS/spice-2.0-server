package com.mdtlabs.coreplatform.userservice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.userservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.userservice.service.RoleService;

/**
 * <p>
 * This service class contain all the business logic for role module and perform
 * all the role operation here.
 * </p>
 *
 * @author VigneshKumar created on Jun 30, 2022
 */
@Service
public class RoleServiceImpl implements RoleService  {

    private final RoleRepository roleRepository;

    @Autowired
    public RoleServiceImpl(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    /**
     * {@inheritDoc}
     */
    public Role addRole(Role role) {
        return roleRepository.save(role);
    }

    /**
     * {@inheritDoc}
     */
    public List<Role> getAllRoles() {
        return roleRepository.getAllRoles(Boolean.TRUE);
    }

    /**
     * {@inheritDoc}
     */
    public Role updateRole(Role role) {
        return roleRepository.save(role);
    }

    /**
     * {@inheritDoc}
     */
    public int deleteRoleById(long roleId) {
        Role role = roleRepository.findById(roleId).orElseThrow(() -> new DataNotFoundException(1010));
        role.setActive(Boolean.FALSE);
        roleRepository.save(role);
        return Constants.ONE;
    }

    /**
     * {@inheritDoc}
     */
    public Role getRoleById(long roleId) {
        return roleRepository.getRoleById(roleId);
    }

    /**
     * {@inheritDoc}
     */
    public Role getRoleByName(String name) {
        return roleRepository.getRoleByName(name);
    }

    /**
     * {@inheritDoc}
     */
    public Set<Role> getRolesByName(List<String> roleNames) {
        return roleRepository.findByIsDeletedFalseAndIsActiveTrueAndNameIn(roleNames);
    }

    /**
     * {@inheritDoc}
     */
    public Set<Role> getRolesByIds(List<Long> roleIds) {
        return roleRepository.findByIsDeletedFalseAndIsActiveTrueAndIdIn(roleIds);
    }

    /**
     * {@inheritDoc}
     */
    public Map<Long, Role> getRoleMap(List<Long> ids) {
        Map<Long, Role> rolesMap = new HashMap<>();
        List<Role> roles = roleRepository.findByIdIn(ids);
        for (Role role : roles) {
            rolesMap.put(role.getId(), role);
        }
        return rolesMap;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, List<RoleResponseDTO>> getRoleGroups(SearchRequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        RoleDTO defaultRole = getLoggedUserDefaultRole();
        List<Role> roles = roleRepository.getRolesByCountry(request.getCountryId(), defaultRole.getLevel());
        Map<String, List<RoleResponseDTO>> roleMap = new HashMap<>();
        roles.forEach(role -> {
            if (!roleMap.containsKey(role.getGroupName())) {
                roleMap.put(role.getGroupName(), new ArrayList<>());
            }
            if ((!Constants.ROLE_SUPER_USER.equals(role.getName()) && !Constants.ROLE_JOB_USER.equals(role.getName()) && !Constants.ROLE_REPORT_SUPER_ADMIN.equals(role.getName()))
                    && (Objects.isNull(role.getLevel()) || Objects.isNull(defaultRole.getLevel())
                    || role.getLevel() > defaultRole.getLevel())) {
                roleMap.get(role.getGroupName()).add(mapper.map(role, RoleResponseDTO.class));
            }
            if (defaultRole.getName().equals(Constants.ROLE_SUPER_ADMIN) && (!Objects.isNull(role.getLevel())
                    && !Objects.isNull(defaultRole.getLevel()) && defaultRole.getLevel().equals(role.getLevel()))) {
                roleMap.get(role.getGroupName()).add(mapper.map(role, RoleResponseDTO.class));
            }
        });
        return roleMap;
    }

    /**
     * <p>
     * Retrieves the default role of the logged-in user.
     * </p>
     *
     * @return the default RoleDTO of the logged-in user
     * @throws DataNotFoundException if the roleDTO or its level is null
     */
    private RoleDTO getLoggedUserDefaultRole() {
            RoleDTO roleDTO = UserContextHolder.getUserDto().getRoles().stream().filter(role ->
                    Constants.CLIENT_ADMIN.equals(role.getSuiteAccessName())).findFirst().orElse(null);
        if (Objects.isNull(roleDTO)
        ) {
            throw new DataNotFoundException(2025);
        }
        return roleDTO;
    }
}
