package com.mdtlabs.coreplatform.userservice.service;

import java.util.*;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.userservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.userservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.userservice.mapper.UserMapper;
import com.mdtlabs.coreplatform.userservice.repository.OrganizationRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserSupervisorRepository;
import com.mdtlabs.coreplatform.userservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.userservice.service.impl.OrganizationServiceImpl;
import com.mdtlabs.coreplatform.userservice.util.TestConstants;
import com.mdtlabs.coreplatform.userservice.util.TestDataProvider;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class OrganizationServiceTest {

    @InjectMocks
    OrganizationServiceImpl organizationService;

    @Mock
    UserService userService;

    @Mock
    OrganizationRepository organizationRepository;

    @Mock
    AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    RoleService roleService;

    @Mock
    UserMapper userMapper;

    @Mock
    VillageRepository villageRepository;

    @Mock
    RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;

    @Mock
    FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    UserSupervisorRepository userSupervisorRepository;

    @Mock
    UserRepository userRepository;

    @Mock
    ModelMapper modelMapper;

    @Test
    void testCreateHealthFacility() {
        Organization organization = TestDataProvider.getOrganization();
        HealthFacilityRequestDTO request = TestDataProvider.getHealthFacilityRequestDTOWithInsightID();
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(request.getName(), Constants.FORM_NAME_HEALTH_FACILITY, false, true)).thenReturn(organization);
        assertThrows(DataConflictException.class, () -> organizationService.createHealthFacility(request));
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(request.getName(), Constants.FORM_NAME_HEALTH_FACILITY, false, true)).thenReturn(null);
        doNothing().when(userService).validateUsers(request.getUsers());
        organization = new Organization(Constants.FORM_NAME_HEALTH_FACILITY, request.getName(), request.getTenantId());
        when(organizationRepository.save(organization)).thenReturn(organization);

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        ResponseEntity<HealthFacility> responseEntity = new ResponseEntity<HealthFacility>(healthFacility, HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(adminServiceApiInterface.createHealthFacility(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), request)).thenReturn(responseEntity);
        List<Long> roleIds = List.of(1l, 1l);
        Map<Long, Role> roles = Map.of(1l, TestDataProvider.getRole());
        when(roleService.getRoleMap(roleIds)).thenReturn(roles);
        doNothing().when(userService).addOrganizationForUsers(request.getLinkedSupervisorIds(), organization,
                request.getAppTypes());
        when(fhirServiceApiInterface.createOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                CommonUtil.getClient(), request)).thenReturn(request);
        when(fhirServiceApiInterface.createOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), request)).thenReturn(request);
        when(adminServiceApiInterface.updateHealthFacility(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), request)).thenReturn(responseEntity);


        organizationService.createHealthFacility(request);

        request.getUsers().get(0).setId(1L);
        List<Long> existingUserIDS = List.of(1L);
        List<UserSupervisor> userSupervisors = null;
        User existingUser = TestDataProvider.getUser();
        Map<Long, User> existingUsersMap = Map.of(1L, existingUser);
        // List<User>

        when(userService.getUsersByIdsAsMap(existingUserIDS)).thenReturn(existingUsersMap);
        when(userSupervisorRepository.findByUserIdInAndIsDeletedFalseAndIsActiveTrue(existingUserIDS)).thenReturn(userSupervisors);
        doNothing().when(userMapper).setExistingUser(request.getUsers().get(0), existingUser);
        organizationService.createHealthFacility(request);

        userSupervisors = new ArrayList<>();
        when(userSupervisorRepository.findByUserIdInAndIsDeletedFalseAndIsActiveTrue(existingUserIDS)).thenReturn(userSupervisors);
        organizationService.createHealthFacility(request);


        request.getUsers().get(0).setSupervisorId(2l);
        organizationService.createHealthFacility(request);

        userSupervisors = List.of(new UserSupervisor(1l, 3l));
        when(userSupervisorRepository.findByUserIdInAndIsDeletedFalseAndIsActiveTrue(existingUserIDS)).thenReturn(userSupervisors);

        request.getUsers().get(0).setSupervisorId(3l);
        HealthFacility response = organizationService.createHealthFacility(request);
        TestDataProvider.cleanUp();
        assertNotNull(responseEntity, "Should not be return null");
        assertEquals(healthFacility, response, "The created health facility should match the expected one");

    }


    @Test
    void createRegionThrowDataConflictException() {
        Organization organization = TestDataProvider.getOrganization();
        CountryRequestDTO request = TestDataProvider.getCountryRequestDTO();
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(request.getName(), Constants.FORM_NAME_COUNTRY, false, true)).thenReturn(organization);
        assertThrows(DataConflictException.class, () -> organizationService.createRegion(request));
    }


    @Test
    void testCreateRegion() {
        CountryRequestDTO request = TestDataProvider.getCountryRequestDTO();
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(request.getName(), Constants.FORM_NAME_COUNTRY, false, true)).thenReturn(null);
        List<Long> roleIds = List.of(1l);
        Map<Long, Role> roles = Map.of(1l, TestDataProvider.getRole());
        when(roleService.getRoleMap(roleIds)).thenReturn(roles);
        doNothing().when(userService).validateUsers(request.getUsers());
        Role role = TestDataProvider.getRole();
        when(roleService.getRoleByName(Constants.ROLE_REGION_ADMIN)).thenReturn(role);
        Organization organization = new Organization(Constants.FORM_NAME_COUNTRY, request.getName(), request.getTenantId());
        organization.setSequence(Constants.LONG_ZERO);
        when(organizationRepository.save(organization)).thenReturn(organization);
        Country country = TestDataProvider.getCountry();
        UserRequestDTO userFhirDTO = request.getUsers().get(0);
        userFhirDTO.setFhirId(Constants.STRING_FIVE);

        ResponseEntity<Country> responseEntity = new ResponseEntity<Country>(country, HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(adminServiceApiInterface.createCountry(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(),
                request)).thenReturn(responseEntity);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                CommonUtil.getClient(),
                userFhirDTO)).thenReturn(userFhirDTO);

        Country response = organizationService.createRegion(request);
        TestDataProvider.cleanUp();
        assertEquals(response, country);
    }

    @Test
    void testAddAdmin() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        UserRequestDTO request = TestDataProvider.getUserRequestDTO();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_CHP);
        Set<Role> roles = Set.of(role);

        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);

        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(null);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        List<Long> roleIds = List.of(1l);
        Role chwRole = TestDataProvider.getRole();
        chwRole.setName(Constants.ROLE_CHP);
        roles = Set.of(chwRole);
        request.setVillageIds(List.of(1l));
        when(roleService.getRolesByIds(roleIds)).thenReturn(roles);
        User user = new ModelMapper().map(request, User.class);
        user.getRoles().addAll(roles);
        user.setOrganizations(Set.of(TestDataProvider.getOrganization()));
        user.getSuiteAccess().add("Admin");
        when(userService.createUser(user, true)).thenReturn(user);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(request);
        when(userService.saveUser(user)).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        //when
        UserRequestDTO userFhirDTO = TestDataProvider.getUserRequestDTO();
        userFhirDTO.setFhirId("1");
        user.setFhirId(userFhirDTO.getFhirId());
        user.setVillages(List.of());
        request.setSupervisorId(1L);
        Set<Organization> organizations = new HashSet<Organization>();
        organization.setId(1L);
        organizations.add(organization);
        user.setOrganizations(organizations);
        when(userService.createUser(user, true)).thenReturn(user);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(userFhirDTO);
        when(userService.getUserById(request.getSupervisorId())).thenReturn(user);
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getSupervisorId())).thenReturn(user);
        UserResponseDTO response = organizationService.addAdmin(request);
        assertNotNull(response);

        request.setVillageIds(List.of(1L));
        List<Village> villages = List.of(TestDataProvider.getVillage());
        when(villageRepository.findByIdInAndIsDeletedAndIsActive(request.getVillageIds(), false, true)).thenReturn(villages);
        user.setVillages(villages);

        response = organizationService.addAdmin(request);
        assertNotNull(response);

        UserSupervisor userSupervisor = new UserSupervisor(null, 2l);
        when(userSupervisorRepository.save(userSupervisor)).thenReturn(userSupervisor);
        response = organizationService.addAdmin(request);
        assertNotNull(response);

        User existinguser = TestDataProvider.getUser();
        existinguser.setId(1l);
        request.setId(1l);
        when(userService.getUserById(request.getId())).thenReturn(existinguser);
        when(userService.createUser(existinguser, false)).thenReturn(existinguser);
        doNothing().when(userMapper).setExistingUser(request, existinguser, organization);
        when(userService.saveUser(existinguser)).thenReturn(existinguser);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(request);
        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(request.getId())).thenReturn(userSupervisor);

        response = organizationService.addAdmin(request);
        assertNotNull(response);

        response = organizationService.addAdmin(request);
        assertNotNull(response);
        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(request.getId())).thenReturn(userSupervisor);
        response = organizationService.addAdmin(request);
        assertNotNull(response);

        Assertions.assertNotNull(response);


        //when
        request.setId(3L);
        when(userService.getUserById(request.getId())).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        //when
        organization.setId(2L);
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        when(userService.getUserById(request.getId())).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        userFhirDTO.setFhirId("1");
        user.setFhirId(userFhirDTO.getFhirId());
        user.setVillages(List.of());
        request.setSupervisorId(1L);
        Organization userOrganization = TestDataProvider.getOrganization();
        Set<Organization> userOrganizations = new HashSet<>();
        userOrganization.setId(4L);
        userOrganizations.add(userOrganization);
        user.setOrganizations(userOrganizations);
        when(userService.createUser(user, false)).thenReturn(user);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(userFhirDTO);
        when(userService.getUserById(request.getSupervisorId())).thenReturn(user);
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getSupervisorId())).thenReturn(user);
        request.setId(3L);
        when(userService.getUserById(request.getId())).thenReturn(user);
        userSupervisor = new UserSupervisor(1l, 6l);
        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(user.getId()))
                .thenReturn(userSupervisor);
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        UserResponseDTO userResponse = organizationService.addAdmin(request);
        Assertions.assertNotNull(userResponse);

        Role userRole = TestDataProvider.getRole();
        userRole.setName(Constants.ROLE_SUPER_ADMIN);
        Set<Role> userRoles = Set.of(userRole);
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(userRoles);
        Assertions.assertNotNull(organizationService.addAdmin(request));
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);

        TestDataProvider.cleanUp();

    }

    @Test
    void testAddAdmin_if() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        UserRequestDTO request = TestDataProvider.getUserRequestDTO();
        request.setInsightUserOrganizationIds(List.of(TestConstants.ONE));
        request.setReportUserOrganizationIds(List.of(TestConstants.ONE));
        request.setCommunityUnitId(TestConstants.ONE);
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_SUPER_ADMIN);
        role.setSuiteAccessName(Constants.CLIENT_INSIGHTS);
        Set<Role> roles = Set.of(role);
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);

        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(null);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        List<Long> roleIds = List.of(1l);
        Role chwRole = TestDataProvider.getRole();
        chwRole.setName(Constants.ROLE_CHP);
        chwRole.setGroupName(Constants.INSIGHTS);
        roles = Set.of(chwRole);
        request.setVillageIds(List.of(1l));
        when(roleService.getRolesByIds(roleIds)).thenReturn(roles);
        User user = new ModelMapper().map(request, User.class);
        user.getRoles().addAll(roles);
        user.setOrganizations(Set.of(TestDataProvider.getOrganization()));
        user.getSuiteAccess().add("Admin");
        when(userService.createUser(user, true)).thenReturn(user);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(request);
        when(userService.saveUser(user)).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        //when
        UserRequestDTO userFhirDTO = TestDataProvider.getUserRequestDTO();
        userFhirDTO.setFhirId("1");
        user.setFhirId(userFhirDTO.getFhirId());
        user.setVillages(List.of());
        request.setSupervisorId(1L);
        Set<Organization> organizations = new HashSet<Organization>();
        organization.setId(1L);
        organizations.add(organization);
        user.setOrganizations(organizations);
        when(userService.createUser(user, true)).thenReturn(user);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(userFhirDTO);
        when(userService.getUserById(request.getSupervisorId())).thenReturn(user);
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getSupervisorId())).thenReturn(user);
        UserResponseDTO response = organizationService.addAdmin(request);
        assertNotNull(response);

        request.setVillageIds(List.of(1L));
        List<Village> villages = List.of(TestDataProvider.getVillage());
        when(villageRepository.findByIdInAndIsDeletedAndIsActive(request.getVillageIds(), false, true)).thenReturn(villages);
        user.setVillages(villages);

        response = organizationService.addAdmin(request);
        assertNotNull(response);

        UserSupervisor userSupervisor = new UserSupervisor(null, 2l);
        when(userSupervisorRepository.save(userSupervisor)).thenReturn(userSupervisor);
        response = organizationService.addAdmin(request);
        assertNotNull(response);

        User existinguser = TestDataProvider.getUser();
        existinguser.setId(1l);
        request.setId(1l);
        when(userService.getUserById(request.getId())).thenReturn(existinguser);
        when(userService.createUser(existinguser, false)).thenReturn(existinguser);
        doNothing().when(userMapper).setExistingUser(request, existinguser, organization);
        when(userService.saveUser(existinguser)).thenReturn(existinguser);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(request);
        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(request.getId())).thenReturn(userSupervisor);

        response = organizationService.addAdmin(request);
        assertNotNull(response);

        response = organizationService.addAdmin(request);
        assertNotNull(response);

        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(request.getId())).thenReturn(userSupervisor);
        response = organizationService.addAdmin(request);
        assertNotNull(response);
        Assertions.assertNotNull(response);


        //when
        request.setId(3L);
        when(userService.getUserById(request.getId())).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        //when
        organization.setId(2L);
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        when(userService.getUserById(request.getId())).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.addAdmin(request));

        userFhirDTO.setFhirId("1");
        user.setFhirId(userFhirDTO.getFhirId());
        user.setVillages(List.of());
        Organization userOrganization = TestDataProvider.getOrganization();
        Set<Organization> userOrganizations = new HashSet<>();
        userOrganization.setId(4L);
        userOrganizations.add(userOrganization);
        user.setOrganizations(userOrganizations);
        when(userService.createUser(user, false)).thenReturn(user);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request)).thenReturn(userFhirDTO);
        when(userService.getUserById(request.getSupervisorId())).thenReturn(user);
        when(userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getSupervisorId())).thenReturn(user);
        request.setId(3L);
        when(userService.getUserById(request.getId())).thenReturn(user);
        userSupervisor = new UserSupervisor(1l, 6l);
        when(userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(user.getId()))
                .thenReturn(userSupervisor);
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        UserResponseDTO userResponse = organizationService.addAdmin(request);
        Assertions.assertNotNull(userResponse);
        Role userRole = TestDataProvider.getRole();
        userRole.setName(Constants.ROLE_SUPER_ADMIN);
        userRole.setGroupName(Constants.INSIGHTS);
        userRole.setSuiteAccessName(Constants.CLIENT_INSIGHTS);
        Set<Role> userRoles = Set.of(userRole);
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(userRoles);
        Assertions.assertNotNull(organizationService.addAdmin(request));
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);

        TestDataProvider.cleanUp();

    }

    @Test
    void testRemoveAdmin() {
        TestDataProvider.init();
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        request.setTenantIds(List.of(1l));
        List<Organization> organization = List.of(TestDataProvider.getOrganization());

        when(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getTenantIds())).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> organizationService.removeAdmin(request));
        when(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getTenantIds())).thenReturn(organization);
        UserResponseDTO response = new UserResponseDTO();
        when(userService.deleteOrganizationUser(request)).thenReturn(response);
        UserResponseDTO mockResponse = organizationService.removeAdmin(request);

        Assertions.assertNotNull(mockResponse);

        request.setTenantIds(null);
        assertThrows(DataNotFoundException.class, () -> organizationService.removeAdmin(request));

        Role role = TestDataProvider.getRole();
        User user = TestDataProvider.getUser();
        user.setRoles(Set.of(role));
        user.setInsightId(TestConstants.TEN);
        user.setInsightUserOrganization(new ArrayList<>(List.of(TestDataProvider.getOrganization())));
        user.setReportUserOrganization(new ArrayList<>(List.of(TestDataProvider.getOrganization())));
        user.setFhirId(TestConstants.PARENT_ID);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(userService.saveUser(user)).thenReturn(user);
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.deleteUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(),
                user.getFhirId())).thenReturn(null);

        mockResponse = organizationService.removeAdmin(request);
        Assertions.assertNotNull(mockResponse);

        when(userService.getUserById(request.getId())).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> organizationService.removeAdmin(request));
        TestDataProvider.cleanUp();
    }

   // @Test
    void testRemoveAdmin_throwsException() {
        TestDataProvider.init();
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        request.setTenantIds(List.of(1l));
        List<Organization> organization = List.of(TestDataProvider.getOrganization());

        when(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getTenantIds())).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> organizationService.removeAdmin(request));
        when(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getTenantIds())).thenReturn(organization);
        UserResponseDTO response = new UserResponseDTO();
        when(userService.deleteOrganizationUser(request)).thenReturn(response);
        UserResponseDTO mockResponse = organizationService.removeAdmin(request);

        Assertions.assertNotNull(mockResponse);

        request.setTenantIds(null);
        assertThrows(DataNotFoundException.class, () -> organizationService.removeAdmin(request));

        Role role = TestDataProvider.getRole();
        User user = TestDataProvider.getUser();
        user.setRoles(Set.of(role));
        user.setInsightId(TestConstants.TEN);
        user.setInsightUserOrganization(new ArrayList<>(List.of(TestDataProvider.getOrganization())));
        user.setReportUserOrganization(new ArrayList<>(List.of(TestDataProvider.getOrganization())));
        user.setFhirId(TestConstants.PARENT_ID);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(userService.saveUser(user)).thenReturn(user);
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.deleteUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(),
                user.getFhirId())).thenReturn(null);

        assertThrows(SpiceValidation.class, () -> organizationService.removeAdmin(request));

        mockResponse = organizationService.removeAdmin(request);
        Assertions.assertNotNull(mockResponse);

        when(userService.getUserById(request.getId())).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> organizationService.removeAdmin(request));
        TestDataProvider.cleanUp();
    }



    @Test
    void testUpdateAdmin() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        UserRequestDTO request = TestDataProvider.getUserRequestDTO();
        Organization organization = TestDataProvider.getOrganization();
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_CHP);
        Set<Role> roles = Set.of(role);

        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(null);
        assertThrows(SpiceValidation.class, () -> organizationService.updateAdmin(request));
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        request.setVillageIds(List.of(1l));
        List<Village> villages = List.of(TestDataProvider.getVillage());
        when(villageRepository.findByIdInAndIsDeletedAndIsActive(request.getVillageIds(), false, true)).thenReturn(villages);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(userService.createUser(user, false)).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.updateAdmin(request));

        request.setSupervisorId(1L);
        user.setFhirId("1");
        when(userService.createUser(user, false)).thenReturn(user);
        User supervisor = TestDataProvider.getUser();
        when(userService.getUserById(TestConstants.ONE)).thenReturn(supervisor);
        UserResponseDTO mockResponse = organizationService.updateAdmin(request);
        Assertions.assertNotNull(mockResponse);

        role.setName(Constants.ROLE_SUPER_ADMIN);
        roles = Set.of(role);

        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);

        mockResponse = organizationService.updateAdmin(request);
        Assertions.assertNotNull(mockResponse);

        Role chwRole = TestDataProvider.getRole();
        chwRole.setName(Constants.ROLE_CHP);
        user.getRoles().add(chwRole);
        user.setVillages(villages);
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(userService.getUserByVillageIds(Set.of(1l), user.getId())).thenReturn(List.of(user));

        mockResponse = organizationService.updateAdmin(request);
        Assertions.assertNotNull(mockResponse);

        TestDataProvider.cleanUp();
    }

    @Test
    void testUpdateAdmin_If() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        UserRequestDTO request = TestDataProvider.getUserRequestDTO();
        Organization organization = TestDataProvider.getOrganization();
        User user = TestDataProvider.getUser();
        Role role = TestDataProvider.getRole();
        role.setName(Constants.ROLE_CHP);
        Set<Role> roles = Set.of(role);
        request.setInsightUserOrganizationIds(List.of(TestConstants.ONE));
        request.setReportUserOrganizationIds(List.of(TestConstants.ONE));

        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(null);
        assertThrows(SpiceValidation.class, () -> organizationService.updateAdmin(request));

        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        request.setVillageIds(List.of(1l));
        List<Village> villages = List.of(TestDataProvider.getVillage());
        when(villageRepository.findByIdInAndIsDeletedAndIsActive(request.getVillageIds(), false, true)).thenReturn(villages);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(userService.createUser(user, false)).thenReturn(user);
        assertThrows(SpiceValidation.class, () -> organizationService.updateAdmin(request));

        request.setSupervisorId(1L);
        user.setFhirId("1");
        User supervisor = TestDataProvider.getUser();
        when(userService.getUserById(TestConstants.ONE)).thenReturn(supervisor);
        when(userService.createUser(user, false)).thenReturn(user);
        UserResponseDTO mockResponse = organizationService.updateAdmin(request);
        Assertions.assertNotNull(mockResponse);

        role.setName(Constants.ROLE_SUPER_ADMIN);
        roles = Set.of(role);
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);
        Assertions.assertNotNull(mockResponse);
        organizationService.updateAdmin(request);

        Role chwRole = TestDataProvider.getRole();
        chwRole.setName(Constants.ROLE_CHP);
        user.getRoles().add(chwRole);
        user.setVillages(villages);
        when(roleService.getRolesByIds(request.getRoleIds())).thenReturn(roles);
        when(userService.getUserById(request.getId())).thenReturn(user);
        when(userService.getUserByVillageIds(Set.of(1l), user.getId())).thenReturn(List.of(user));

        mockResponse = organizationService.updateAdmin(request);
        Assertions.assertNotNull(mockResponse);

        TestDataProvider.cleanUp();
    }

    @Test
    void updateOrganization() {
        OrganizationDTO organizationDTO = TestDataProvider.getOrganizationDTO();
        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.findByIdAndIsDeletedFalse(organizationDTO.getId())).thenReturn(organization);
        when(organizationRepository.save(organization)).thenReturn(organization);

        Long organizationId = organizationService.updateOrganization(organizationDTO);
        assertNotNull(organizationId);
    }

    @Test
    void getOrganization() {
        Long id = 1l;
        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.findByIdAndIsDeletedFalse(id)).thenReturn(organization);

        Organization response = organizationService.getOrganization(id);
        assertNotNull(response);

        organization = null;
        when(organizationRepository.findByIdAndIsDeletedFalse(id)).thenReturn(organization);
        assertThrows(DataNotFoundException.class, () -> organizationService.getOrganization(id));

    }

    @Test
    void getChildOrganization() {
        Long id = 1l;
        List<Organization> organizations = List.of(TestDataProvider.getOrganization());

        when(organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(id)).thenReturn(organizations);
        List<Organization> response = organizationService.getChildOrganization(id);
        assertNotNull(response);
        organizations = null;

        when(organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(id)).thenReturn(organizations);
        assertThrows(DataNotFoundException.class, () -> organizationService.getChildOrganization(id));

    }

    @Test
    void deleteOrganization() {
        Long id = 1l;
        Organization organization = TestDataProvider.getOrganization();
        List<Organization> organizations = new ArrayList<>();
        organizations.add(TestDataProvider.getOrganization());
        List<Organization> childOrganization = new ArrayList<>();
        childOrganization.add(TestDataProvider.getOrganization());
        childOrganization.add(TestDataProvider.getOrganization());
        when(organizationRepository.findByIdAndIsDeletedFalse(id)).thenReturn(organization);
        when(organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(id)).thenReturn(organizations);
        when(organizationRepository.saveAll(childOrganization)).thenReturn(childOrganization);
        organizationService.deleteOrganization(id);
        verify(organizationRepository,atLeastOnce()).findByIdAndIsDeletedFalse(id);
    }

    @Test
    void deleteHealthFacility() {
        SearchRequestDTO request = new SearchRequestDTO();
        Organization organization = TestDataProvider.getOrganization();
        assertThrows(BadRequestException.class, () -> organizationService.deleteHealthFacility(request));
        request.setId(1l);
        assertThrows(BadRequestException.class, () -> organizationService.deleteHealthFacility(request));
        request.setTenantId(2l);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(organization);
        when(organizationRepository.save(organization)).thenReturn(organization);
        ResponseEntity<String> responseEntity = new ResponseEntity<String>("1l", HttpStatus.CREATED);
        doReturn(responseEntity).when(adminServiceApiInterface).deleteHealthFacility(any(), any(), any(), eq(request));
        doNothing().when(userService).deleteOrganizationUsers(request);
        organizationService.deleteHealthFacility(request);
        TestDataProvider.cleanUp();
        assertEquals(Boolean.FALSE, organization.isActive(), "Organization active status should be false");
        assertEquals(Boolean.TRUE, organization.isDeleted(), "Organization deleted status should be true");
    }

    @Test
    void deleteHealthFacilityThrowExceptionWhenOrganizationNotPresent() {
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        when(organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId())).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> organizationService.deleteHealthFacility(request));
    }

    @Test
    void updateHealthFacility() {
        HealthFacilityRequestDTO request = TestDataProvider.getHealthFacilityRequestDTO();
        request.setTenantId(1L);
        request.setLinkedSupervisorIds(List.of(1L));
        Organization organization = TestDataProvider.getOrganization();

        when(organizationRepository.findById(request.getTenantId())).thenReturn(Optional.of(organization));

        organizationService.updateHealthFacility(request);

        request.setParentTenantId(1L);
        when(organizationRepository.findById(request.getTenantId())).thenReturn(Optional.of(organization));
        organizationService.updateHealthFacility(request);
        verify(organizationRepository,atLeastOnce()).findById(request.getTenantId());
    }

    @Test
    void getOrganizations() {
        //Given
        String formName = Constants.FORM_NAME;
        List<Organization> expectedOrganizations = List.of(new Organization(), new Organization());

        //when
        when(organizationRepository.getOrganizationsByFormName(formName)).thenReturn(expectedOrganizations);

        //then
        List<Organization> actualOrganizations = organizationService.getOrganizations(formName);

        assertNotNull(actualOrganizations);
        assertEquals(expectedOrganizations.size(), actualOrganizations.size());
        assertEquals(expectedOrganizations, actualOrganizations);
    }

    @Test
    void toVerifyValidateParentOrganization() {
        //given
        User user = TestDataProvider.getUser();

        //then
        assertThrows(DataNotAcceptableException.class, () -> {
            organizationService
                    .validateParentOrganization(TestConstants.ONE, user);
        });
    }

    @Test
    void testValidateParentOrganization() {
        //given
        User user = TestDataProvider.getUser();

        //then
        organizationService.validateParentOrganization(TestConstants.FIVE, user);
    }

    @Test
    @DisplayName("Throw data conflict exception when name is already exist")
    void validateOrganizationThrowDataConflictExceptionWhenNameIsDuplicate() {
        Organization org = TestDataProvider.getOrganization();
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(org.getName(), org.getFormName(), false, true)).thenReturn(org);
        assertThrows(DataConflictException.class, () -> organizationService.validateOrganization("kenya", "country"));
    }

    @Test
    @DisplayName("Get the organization when requested id is valid")
    void getOrganizationByIdReturnOrganizationWhenIdIsValid() {
        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.getOrganizationById(organization.getId())).thenReturn(organization);
        Organization response = organizationService.getOrganizationById(organization.getId());
        assertNotNull(response, "Organization should not return null");
    }

    @Test
    @DisplayName("Throw data conflict exception when chiefdom name is already exist")
    void createChiefdomThrowDataConflictExceptionWhenNameIsDuplicate() {
        Organization org = TestDataProvider.getOrganization();
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(chiefdom.getName(), Constants.FORM_NAME_CHIEFDOM, false, true)).thenReturn(org);
        assertThrows(DataConflictException.class, () -> organizationService.validateOrganization("chiefdom", Constants.FORM_NAME_CHIEFDOM));
    }

    @Test
    void createChiefdomWhenRequestIsValid() {
        ChiefdomRequestDTO request = TestDataProvider.getChiefdomRequestDto();
        when(organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(request.getName(), Constants.FORM_NAME_CHIEFDOM, false, true)).thenReturn(null);
        List<UserRequestDTO> users = request.getUsers();
        doNothing().when(userService).validateUsers(users);
        Role role = TestDataProvider.getRole();
        when(roleService.getRoleByName(Constants.ROLE_REGION_ADMIN)).thenReturn(role);
        Organization organization = new Organization(Constants.FORM_NAME_CHIEFDOM, request.getName(), request.getTenantId());
        organization.setSequence(Constants.LONG_ZERO);
        when(organizationRepository.save(organization)).thenReturn(organization);
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        chiefdom.setId(1L);
        UserRequestDTO userFhirDTO = request.getUsers().get(0);
        userFhirDTO.setFhirId(Constants.STRING_FIVE);

        ResponseEntity<Chiefdom> responseEntity = new ResponseEntity<>(chiefdom, HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(adminServiceApiInterface.createChiefdom(CommonUtil.getAuthToken(),
                UserContextHolder.getUserDto().getClient()
                , request)).thenReturn(responseEntity);
        when(organizationRepository.save(any())).thenReturn(organization);
        List<Long> roleIds = List.of(1l);
        Map<Long, Role> roles = Map.of(1l, role);
        when(roleService.getRoleMap(roleIds)).thenReturn(roles);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                CommonUtil.getClient(),
                userFhirDTO)).thenReturn(userFhirDTO);
        organizationService.createChiefdom(request);
        TestDataProvider.cleanUp();
        verify(userService).validateUsers(request.getUsers());
        verify(userService).createUsers(anyList());
    }


    @Test
    void createDistrictWhenRequestIsValid() {
        DistrictRequestDTO request = TestDataProvider.getDistrictRequestDTO();
        when(organizationRepository.findByNameIgnoreCaseAndFormName(request.getName(), Constants.FORM_NAME_DISTRICT)).thenReturn(null);

        List<UserRequestDTO> users = request.getUsers();
        doNothing().when(userService).validateUsers(users);
        Role role = TestDataProvider.getRole();
        when(roleService.getRoleByName(Constants.ROLE_REGION_ADMIN)).thenReturn(role);
        Organization organization = new Organization(Constants.FORM_NAME_DISTRICT, request.getName(), request.getTenantId());
        organization.setSequence(Constants.LONG_ZERO);
        when(organizationRepository.save(organization)).thenReturn(organization);
        District district = TestDataProvider.getDistrict();
        district.setId(1L);
        UserRequestDTO userFhirDTO = request.getUsers().get(0);
        userFhirDTO.setFhirId(Constants.STRING_FIVE);

        ResponseEntity<District> responseEntity = new ResponseEntity<>(district, HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(adminServiceApiInterface.createDistrict(CommonUtil.getAuthToken(),
                UserContextHolder.getUserDto().getClient()
                , request)).thenReturn(responseEntity);
        when(organizationRepository.save(any())).thenReturn(organization);
        List<Long> roleIds = List.of(1l);
        Map<Long, Role> roles = Map.of(1l, role);
        when(roleService.getRoleMap(roleIds)).thenReturn(roles);
        when(fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                CommonUtil.getClient(),
                userFhirDTO)).thenReturn(userFhirDTO);
        organizationService.createDistrict(request);
        TestDataProvider.cleanUp();
        verify(userService).validateUsers(request.getUsers());
        verify(userService).createUsers(anyList());
    }

    @Test
    void createDistrict_throwsException() {
        DistrictRequestDTO districtRequestDTO = TestDataProvider.getDistrictRequestDTO();
        Organization org = TestDataProvider.getOrganization();
                when(organizationRepository.findByNameIgnoreCaseAndFormName(districtRequestDTO.getName(), Constants.FORM_NAME_DISTRICT)).thenReturn(org);
        assertThrows(DataConflictException.class, () -> organizationService.createDistrict(districtRequestDTO));
    }

    @Test
    void activateOrDeactivateOrganization() {
        List<Long> formdataIdList = Arrays.asList(1L, 2L);
        boolean doActivate = false;
        List<String> fhirIds = Arrays.asList("fhirId1", "fhirId2");
        Organization org1 = TestDataProvider.getOrganization();
        org1.setId(1L);
        org1.setActive(true);

        Organization org2 = TestDataProvider.getOrganization();
        org2.setId(2L);
        org2.setActive(true);

        Set<Organization> existingOrganizations = new HashSet<>(Arrays.asList(org1, org2));
        List<Organization> organizations = new ArrayList<>(existingOrganizations);
        when(organizationRepository.findByIsDeletedFalseAndIsActiveAndIdIn(true, formdataIdList))
                .thenReturn(existingOrganizations);
        when(organizationRepository.saveAll(any())).thenReturn(organizations);

        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        doNothing().when(fhirServiceApiInterface).activateDeactivateOrganization(anyString(), anyString(), anyList(), anyBoolean());
        Boolean result = organizationService.activateOrDeactivateOrganization(formdataIdList, doActivate, fhirIds);
        TestDataProvider.cleanUp();
        assertTrue(result);
        verify(organizationRepository).findByIsDeletedFalseAndIsActiveAndIdIn(true, formdataIdList);
    }

    @Test
    void activateOrDeactivateOrganization_false() {
        List<Long> formdataIdList = null;
        boolean doActivate = false;
        List<String> fhirIds = Arrays.asList("fhirId1", "fhirId2");
        Boolean result = organizationService.activateOrDeactivateOrganization(formdataIdList, doActivate, fhirIds);
        Assertions.assertFalse(result);
    }

    @Test
    void getChildOrganizations() {
        Long tenantId = TestConstants.TWO;
        String formName = Constants.COUNTRY;
        List<Organization> childOrgs = List.of(TestDataProvider.getOrganization());

        //when
        when(organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(tenantId)).thenReturn(childOrgs);

        //then
        Map<String, List<Long>> response = organizationService.getChildOrganizations(tenantId, formName);
        assertNotNull(response);
    }

    @Test
    void getChildOrganizations_District() {
        Long tenantId = TestConstants.TWO;
        String formName = Constants.DISTRICT;
        List<Organization> childOrgs = List.of(TestDataProvider.getOrganization());

        //when
        when(organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(tenantId)).thenReturn(childOrgs);

        //then
        Map<String, List<Long>> response = organizationService.getChildOrganizations(tenantId, formName);
        assertNotNull(response);
    }

    @Test
    void getChildOrganizations_CHIEFDOM() {
        Long tenantId = TestConstants.TWO;
        String formName = Constants.CHIEFDOM;
        List<Organization> childOrgs = List.of(TestDataProvider.getOrganization());

        //when
        when(organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(tenantId)).thenReturn(childOrgs);

        //then
        Map<String, List<Long>> response = organizationService.getChildOrganizations(tenantId, formName);
        assertNotNull(response);
    }

}
