package com.mdtlabs.coreplatform.userservice.service;


import static org.junit.jupiter.api.Assertions.assertEquals;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleResponseDTO;
import org.junit.jupiter.api.DisplayName;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.userservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.userservice.service.impl.RoleServiceImpl;
import com.mdtlabs.coreplatform.userservice.util.TestDataProvider;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RoleServiceTest {

    @InjectMocks
    RoleServiceImpl roleService;

    @Mock
    RoleRepository roleRepository;

    @Test
    void addRole() {
        Role role = TestDataProvider.getRole();
        when(roleRepository.save(role)).thenReturn(role);

        Role response = roleService.addRole(role);
        assertNotNull(response);
    }

    @Test
    void getAllRoles() {
        List<Role> roles = List.of(TestDataProvider.getRole());
        when(roleRepository.getAllRoles(Boolean.TRUE)).thenReturn(roles);

        List<Role> response = roleService.getAllRoles();
        assertNotNull(response);
    }

    @Test
    void updateRole() {
        Role role = TestDataProvider.getRole();
        when(roleRepository.save(role)).thenReturn(role);

        Role response = roleService.updateRole(role);
        assertNotNull(response);
    }

    @Test
    void getRoleById() {
        Long roleId = 1l;
        Role role = TestDataProvider.getRole();

        when(roleRepository.getRoleById(roleId)).thenReturn(role);
        Role response = roleService.getRoleById(roleId);
        assertNotNull(response);
    }

    @Test
    void getRolesByName() {
        List<String> name = new ArrayList<>();
        Set<Role> role = Set.of(TestDataProvider.getRole());

        when(roleRepository.findByIsDeletedFalseAndIsActiveTrueAndNameIn(name)).thenReturn(role);
        Set<Role> response = roleService.getRolesByName(name);

        assertNotNull(response);
    }

    @Test
    void getRoleByNameTest() {
        Role role = TestDataProvider.getRole();
        when(roleRepository.getRoleByName(Constants.ROLE_SUPER_ADMIN)).thenReturn(role);

        Role response = roleService.getRoleByName(Constants.ROLE_SUPER_ADMIN);
        assertNotNull(response);
    }

    @Test
    void getRolesByIds() {
        List<Long> ids = new ArrayList<>();
        Set<Role> role = Set.of(TestDataProvider.getRole());

        when(roleRepository.findByIsDeletedFalseAndIsActiveTrueAndIdIn(ids)).thenReturn(role);
        Set<Role> response = roleService.getRolesByIds(ids);
        assertNotNull(response);
    }

    @Test
    void getRoleGroups() {
        //given
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        Role role = TestDataProvider.getRole();
        role.setGroupName("");
        role.setLevel(2L);
        List<Role> roles = List.of(role);
        TestDataProvider.init();

        //when
        TestDataProvider.getStaticMocks();
        when(roleRepository.getAllRoles(Boolean.TRUE)).thenReturn(roles);
        when(roleRepository.getRolesByCountry(request.getCountryId(), 2L)).thenReturn(roles);


        //then
        Map<String, List<RoleResponseDTO>> response = roleService.getRoleGroups(request);
        TestDataProvider.cleanUp();
        assertNotNull(response);
    }


    @Test
    void getRoleGroupsThrowAnException() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        UserContextDTO userDto = mock(UserContextDTO.class);
        when(userDto.getRoles()).thenReturn(List.of());
        UserContextHolder.setUserDto(userDto);
        assertThrows(DataNotFoundException.class,()-> roleService.getRoleGroups(request));
    }

    @Test
    void deleteRoleById() {
        Role role = TestDataProvider.getRole();
        role.setActive(false);
        assertThrows(DataNotFoundException.class, () -> roleService.deleteRoleById(1L));
        when(roleRepository.findById(1L)).thenReturn(Optional.of(role));
        int response = roleService.deleteRoleById(1L);
        assertEquals(1L, response);
    }

    @Test
    void getRoleMap() {
        List<Role> roles = List.of(TestDataProvider.getRole());

        when(roleRepository.findByIdIn(List.of(1L))).thenReturn(roles);

        assertNotNull(roleService.getRoleMap(List.of(1L)));
    }

    @Test
    @DisplayName("Get role when the requested name is valid")
    void getRoleByNameReturnRoleWhenRoleNameIsValid() {
        Role role = TestDataProvider.getRole();
        when(roleRepository.getRoleByName(any())).thenReturn(role);
        Role roleByName = roleService.getRoleByName(role.getName());
        assertNotNull(roleByName);
        assertEquals(role.getName(), roleByName.getName(), "Should be same name");
    }

}
