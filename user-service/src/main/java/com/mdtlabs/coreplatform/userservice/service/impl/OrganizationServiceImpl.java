package com.mdtlabs.coreplatform.userservice.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserSupervisor;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.userservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.userservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.userservice.mapper.UserMapper;
import com.mdtlabs.coreplatform.userservice.repository.OrganizationRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserSupervisorRepository;
import com.mdtlabs.coreplatform.userservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.userservice.service.OrganizationService;
import com.mdtlabs.coreplatform.userservice.service.RoleService;
import com.mdtlabs.coreplatform.userservice.service.UserService;

/**
 * <p>
 * This service class contain all the business logic for organization module and
 * perform all the organization operation here.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
@Service
public class OrganizationServiceImpl implements OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final UserService userService;
    private final AdminServiceApiInterface adminServiceApiInterface;
    private final RoleService roleService;
    private final UserMapper userMapper;
    private final VillageRepository villageRepository;
    private final UserSupervisorRepository userSupervisorRepository;
    private final RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;
    private final FhirServiceApiInterface fhirServiceApiInterface;

    public OrganizationServiceImpl(OrganizationRepository organizationRepository, UserService userService,
            AdminServiceApiInterface adminServiceApiInterface, RoleService roleService,
            UserMapper userMapper, VillageRepository villageRepository,
            UserSupervisorRepository userSupervisorRepository,
            RedisTemplate<String, Map<Long, List<Long>>> redisTemplate, FhirServiceApiInterface fhirServiceApiInterface) {
        this.organizationRepository = organizationRepository;
        this.userService = userService;
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.roleService = roleService;
        this.userMapper = userMapper;
        this.villageRepository = villageRepository;
        this.userSupervisorRepository = userSupervisorRepository;
        this.redisTemplate = redisTemplate;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public Organization createOrganization(Organization organization) {
        organization = organizationRepository.save(organization);
        redisTemplate.delete(Constants.ORGANIZATION_REDIS_KEY);
        return organization;
    }

    /**
     * {@inheritDoc}
     */
    public Long updateOrganization(OrganizationDTO organiation) {
        Organization updatingOrg = organizationRepository.findByIdAndIsDeletedFalse(organiation.getId());
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        mapper.map(organiation, updatingOrg);
        return createOrganization(updatingOrg).getId();
    }

    /**
     * {@inheritDoc}
     */
    public Organization getOrganization(Long id) {
        Organization organization = organizationRepository.findByIdAndIsDeletedFalse(id);
        if (Objects.isNull(organization)) {
            throw new DataNotFoundException(1001);
        }
        return organization;
    }

    /**
     * {@inheritDoc}
     */
    public List<Organization> getChildOrganization(Long id) {
        List<Organization> organization = organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(id);
        if (Objects.isNull(organization)) {
            throw new DataNotFoundException(1001);
        }
        return organization;
    }

    /**
     * {@inheritDoc}
     */
    public void deleteOrganization(Long id) {
        Organization organization = getOrganization(id);
        organization.setDeleted(Boolean.TRUE);
        List<Organization> childOrganization = getChildOrganization(id);
        childOrganization.forEach(org ->
            org.setDeleted(Boolean.TRUE)
        );
        childOrganization.add(organization);
        organizationRepository.saveAll(childOrganization);
    }

    /**
     * {@inheritDoc}
     */
    public void validateOrganization(String name, String formName) {
        Organization org = organizationRepository.findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(name, formName,
                false, true);
        if (!Objects.isNull(org)) {
            throw new DataConflictException(1002, name);
        }
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacility createHealthFacility(HealthFacilityRequestDTO request) {
        validateOrganization(request.getName(), Constants.FORM_NAME_HEALTH_FACILITY);
        userService.validateUsers(request.getUsers());
        Organization organization = new Organization(Constants.FORM_NAME_HEALTH_FACILITY, request.getName(),
                request.getParentTenantId());
        organization = createOrganization(organization);
        request.setTenantId(organization.getId());
        HealthFacilityRequestDTO fhirResponse = fhirServiceApiInterface.createOrganization(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(),
                CommonUtil.getClient(), request);
        request.setFhirId(fhirResponse.getFhirId());
        HealthFacility healthFacility = adminServiceApiInterface.createHealthFacility(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(),
                request).getBody();
        request.setId(healthFacility.getId());
        organization.setFormDataId(healthFacility.getId());
        organization = createOrganization(organization);
        List<User> users = new ArrayList<>();

        List<UserRequestDTO> existingUser = new ArrayList<>();
        List<Long> existingUserIds = new ArrayList<>();
        Map<String, Long> usernameSupervisorMap = new HashMap<>();

        List<Long> roleIds = new ArrayList<>();
        request.getUsers().forEach(user -> roleIds.addAll(user.getRoleIds()));
        Map<Long, Role> rolesMap = roleService.getRoleMap(roleIds);
        List<UserSupervisor> userSupervisors = new ArrayList<>();

        for (UserRequestDTO userRequestDTO : request.getUsers()) {
            if (Objects.isNull(userRequestDTO.getId())) {
                User user = mapUser(userRequestDTO,
                        organization,
                        userRequestDTO.getRoleIds().stream().map(rolesMap::get).collect(Collectors.toSet()));
                if (!Objects.isNull(userRequestDTO.getInsightUserOrganizationIds())
                        && !userRequestDTO.getInsightUserOrganizationIds().isEmpty()) {
                    user.setInsightUserOrganization(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(
                            userRequestDTO.getInsightUserOrganizationIds()));
                }
                if (!Objects.isNull(userRequestDTO.getReportUserOrganizationIds())
                        && !userRequestDTO.getReportUserOrganizationIds().isEmpty()) {
                    user.setReportUserOrganization(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(
                            userRequestDTO.getReportUserOrganizationIds()));
                }
                users.add(user);
            } else {
                existingUser.add(userRequestDTO);
                existingUserIds.add(userRequestDTO.getId());
            }
            usernameSupervisorMap.put(userRequestDTO.getUsername(), userRequestDTO.getSupervisorId());
        }
        if (!existingUser.isEmpty()) {
            users.addAll(mapExistingUsers(existingUser, organization, rolesMap, userSupervisors));
        }
        Map<String, String> userFhirIdMap = new HashMap<>();
        for (UserRequestDTO userRequestDTO : fhirResponse.getUsers()) {
            userFhirIdMap.put(userRequestDTO.getUsername(), userRequestDTO.getFhirId());
        }
        for (User user : users) {
            user.setFhirId(userFhirIdMap.get(user.getUsername()));
        }
        users = userService.createUsers(users, existingUserIds);

        for (User user : users) {
            if (!existingUserIds.contains(user.getId())
                    && !Objects.isNull(usernameSupervisorMap.get(user.getUsername()))) {
                userSupervisors.add(new UserSupervisor(user.getId(), usernameSupervisorMap.get(user.getUsername())));
            }
        }
        if (!userSupervisors.isEmpty()) {
            userSupervisorRepository.saveAll(userSupervisors);
        }
        if (!Objects.isNull(request.getLinkedSupervisorIds()) && !request.getLinkedSupervisorIds().isEmpty()) {
            userService.addPeerSupervisors(request.getLinkedSupervisorIds(), organization);
        }
        return healthFacility;
    }



    /**
         * {@inheritDoc}
         */
    @Transactional
    public Country createRegion(CountryRequestDTO request) {
        validateOrganization(request.getName(), Constants.FORM_NAME_COUNTRY);
        List<Long> roleIds = new ArrayList<>();
        request.getUsers().forEach(user -> {
            if (Objects.nonNull(user.getRoleIds()) && !user.getRoleIds().isEmpty()) {
                roleIds.addAll(user.getRoleIds());
            }
        });
        Map<Long, Role> rolesMap = roleService.getRoleMap(roleIds);
        userService.validateUsers(request.getUsers());
        Organization organization = new Organization(Constants.FORM_NAME_COUNTRY, request.getName(),
                request.getTenantId());
        organization.setSequence(Constants.LONG_ZERO);
        organization = createOrganization(organization);
        request.setTenantId(organization.getId());
        Country country = adminServiceApiInterface.createCountry(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), request).getBody();
        assert country != null;
        organization.setFormDataId(country.getId());
        organization = createOrganization(organization);
        request.getUsers().forEach(user -> user.setCountry(country));
        List<User> users = mapUsers(request.getUsers(), organization, rolesMap);
        userService.createUsers(users);
        return country;
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO addAdmin(UserRequestDTO request) {
        Set<Role> roles = roleService.getRolesByIds(request.getRoleIds());
        if (roles.stream().allMatch(role -> Objects.equals(Constants.ROLE_SUPER_ADMIN, role.getName())
                || Objects.equals(Constants.ROLE_REPORT_ADMIN, role.getName()))) {
            if (!Objects.isNull(request.getInsightUserOrganizationIds())
                    && !request.getInsightUserOrganizationIds().isEmpty()) {
                List<Organization> organizationList = organizationRepository
                        .findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getInsightUserOrganizationIds());
                request.setInsightUserOrganization(organizationList);
            }
            if (!Objects.isNull(request.getReportUserOrganizationIds())
                    && !request.getReportUserOrganizationIds().isEmpty()) {
                List<Organization> organizationList = organizationRepository
                        .findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getReportUserOrganizationIds());
                request.setReportUserOrganization(organizationList);
            }
            return addSuperAdmin(request, roles);
        }
        Organization organization = organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId());
        if (!Objects.isNull(request.getVillageIds())) {
            request.setVillages(villageRepository.findByIdInAndIsDeletedAndIsActive(request.getVillageIds(),
                    false,
                    true));
        }
        User user = null;

        if (!Objects.isNull(request.getId())) {
            user = mapExistingUser(request, organization, roles, false);
        } else {
            user = mapUser(request, organization, roles);
            UserRequestDTO userFhirDTO = fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(),
                    CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(),
                    request);
            if (Objects.isNull(userFhirDTO) || Objects.isNull(userFhirDTO.getFhirId())) {
                throw new SpiceValidation(2020, user.getUsername());
            }
            user.setFhirId(userFhirDTO.getFhirId());
        }
        if (Objects.nonNull(request.getCommunityUnitId())) {
            user.setCommunityUnitId(request.getCommunityUnitId());
        }

        if (!Objects.isNull(request.getInsightUserOrganizationIds()) && !request.getInsightUserOrganizationIds().isEmpty()) {
            user.setInsightUserOrganization(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getInsightUserOrganizationIds()));
        }
        if (!Objects.isNull(request.getReportUserOrganizationIds()) && !request.getReportUserOrganizationIds().isEmpty()) {
            user.setReportUserOrganization(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getReportUserOrganizationIds()));
        }
        user = userService.createUser(user, Objects.isNull(request.getId()));
        List<String> roleNames = roles.stream().map(Role::getName).toList();

        if (Objects.isNull(request.getSupervisorId()) && roleNames.contains(Constants.ROLE_CHP)
                && roleNames.contains(Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT)) {
            request.setSupervisorId(user.getId());
        }

        if (!Objects.isNull(request.getSupervisorId())) {
            User supervisor = userService.getUserById(request.getSupervisorId());
            supervisor.getOrganizations().add(organization);
            userService.saveUser(supervisor);
            if (Objects.isNull(request.getId())) {
                UserSupervisor userSupervisor = new UserSupervisor(user.getId(), request.getSupervisorId());
                userSupervisorRepository.save(userSupervisor);
            } else {
                updateSupervisorForUser(user.getId(), request.getSupervisorId());
            }
        }
        return new ModelMapper().map(user, UserResponseDTO.class);
    }

    /**
     * <p>
     * Adds a super admin user to the system.
     * This method maps the provided UserRequestDTO to a User entity, assigns the
     * specified roles,
     * and sets the user's suite access based on these roles. It then persists the
     * new super admin user to the database.
     * A UserResponseDTO is returned, but it is not populated with the created
     * user's details in this implementation.
     * </p>
     *
     * @param request The UserRequestDTO object containing the data for the new
     *                user.
     * @param roles   The set of roles to be assigned to the new super admin user.
     * @return UserResponseDTO An empty UserResponseDTO object.
     */
    private UserResponseDTO addSuperAdmin(UserRequestDTO request, Set<Role> roles) {
        User user = mapUser(request, null, roles);
        UserRequestDTO userFhirDTO = fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                CommonUtil.getClient(),
                request);
        if (Objects.isNull(userFhirDTO) || Objects.isNull(userFhirDTO.getFhirId())) {
            throw new SpiceValidation(2020, user.getUsername());
        }
        user.setFhirId(userFhirDTO.getFhirId());
        setUserRoleAndSuiteAccess(user, null, roles);
        userService.createUser(user, Boolean.TRUE);
        return new UserResponseDTO();
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO updateAdmin(UserRequestDTO request) {
        Set<Role> roles = roleService.getRolesByIds(request.getRoleIds());
        if (roles.stream().allMatch(role -> Objects.equals(Constants.ROLE_SUPER_ADMIN, role.getName())
                || Objects.equals(Constants.ROLE_REPORT_ADMIN, role.getName()))) {
            if (!Objects.isNull(request.getInsightUserOrganizationIds())
                    && !request.getInsightUserOrganizationIds().isEmpty()) {
                List<Organization> organizationList = organizationRepository
                        .findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getInsightUserOrganizationIds());
                request.setInsightUserOrganization(organizationList);
            }
            if (!Objects.isNull(request.getReportUserOrganizationIds())
                    && !request.getReportUserOrganizationIds().isEmpty()) {
                List<Organization> organizationList = organizationRepository
                        .findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getReportUserOrganizationIds());
                request.setReportUserOrganization(organizationList);
            }
            return updateSuperAdmin(request, roles);
        }
        Organization organization = organizationRepository.findByIdAndIsDeletedFalse(request.getTenantId());
        if (!Objects.isNull(request.getVillageIds())) {
            request.setVillages(
                    villageRepository.findByIdInAndIsDeletedAndIsActive(request.getVillageIds(), false, true));
        }
        User user ;
        User existingUser = userService.getUserById(request.getId());
        if (existingUser.getOrganizations().isEmpty() || Constants.ZERO >= existingUser.getOrganizations().size()) {
            user = mapExistingUser(request, organization, roles, false);
        } else {
            user = mapExistingUser(request, organization, roles, true);
        }

        if (!Objects.isNull(request.getInsightUserOrganizationIds())
                && !request.getInsightUserOrganizationIds().isEmpty()) {
            user.setInsightUserOrganization(organizationRepository
                    .findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getInsightUserOrganizationIds()));
        }
        if (!Objects.isNull(request.getReportUserOrganizationIds())
                && !request.getReportUserOrganizationIds().isEmpty()) {
            user.setReportUserOrganization(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(
                    request.getReportUserOrganizationIds()));
        }
        if (!Objects.isNull(request.getSupervisorId())) {
            updateSupervisorForUser(user.getId(), request.getSupervisorId());
        }
        if (Objects.nonNull(user.getFhirId())) {
            request.setFhirId(user.getFhirId());
            fhirServiceApiInterface.updateUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(), request);
        } else {
            UserRequestDTO userFhirDTO = fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(),
                    CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(),
                    request);
            if (Objects.isNull(userFhirDTO) || Objects.isNull(userFhirDTO.getFhirId())) {
                throw new SpiceValidation(2020, user.getUsername());
            }
            user.setFhirId(userFhirDTO.getFhirId());
        }
        user = userService.createUser(user, false);
        return new ModelMapper().map(user, UserResponseDTO.class);
    }

    /**
     * <p>
     * Updates a super admin user's details.
     * This method is responsible for updating the details of an existing super
     * admin user based on the provided UserRequestDTO.
     * It performs the update by mapping the request DTO to a new User entity,
     * setting the appropriate roles and suite access,
     * and then applying these changes to the existing user entity found by ID. It
     * also validates the phone number of the user
     * to ensure it meets the system's requirements before saving the updated user
     * entity.
     * </p>
     *
     * @param request The UserRequestDTO object containing the updated data for the
     *                super admin user.
     * @param roles   The set of roles to be assigned to the super admin user.
     * @return UserResponseDTO An empty UserResponseDTO object. This implementation
     * does not populate the response DTO with the updated user's details.
     */
    private UserResponseDTO updateSuperAdmin(UserRequestDTO request, Set<Role> roles) {
        ModelMapper mapper = new ModelMapper();
        SearchRequestDTO searchRequest = new SearchRequestDTO();
        User newUser = mapper.map(request, User.class);
        setUserRoleAndSuiteAccess(newUser, null, roles);
        User existingUser = userService.getUserById(request.getId());
        validateRoles(existingUser.getRoles().stream().map(Role::getName).collect(Collectors.toSet()),
                roles.stream().map(Role::getName).collect(Collectors.toSet()), existingUser);
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        mapper.map(newUser, existingUser);
        if (!Objects.isNull(request.getInsightUserOrganization())
                && !request.getInsightUserOrganization().isEmpty()) {
            existingUser.setInsightUserOrganization(request.getInsightUserOrganization());
        }
        if (!Objects.isNull(request.getReportUserOrganizationIds())
                && !request.getReportUserOrganizationIds().isEmpty()) {
            existingUser.setReportUserOrganization(organizationRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(
                    request.getReportUserOrganizationIds()));
        }
        if (Objects.nonNull(existingUser.getFhirId())) {
            request.setFhirId(existingUser.getFhirId());
            fhirServiceApiInterface.updateUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(), request);
        } else {
            UserRequestDTO userFhirDTO = fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(),
                    request);
            if (Objects.isNull(userFhirDTO) || Objects.isNull(userFhirDTO.getFhirId())) {
                throw new SpiceValidation(2020, existingUser.getUsername());
            }
            existingUser.setFhirId(userFhirDTO.getFhirId());
        }
        searchRequest.setPhoneNumber(request.getPhoneNumber());
        searchRequest.setId(request.getId());
        userService.phoneNumberValidation(searchRequest);
        userService.saveUser(existingUser);
        return new UserResponseDTO();
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO removeAdmin(SearchRequestDTO request) {
        if (Objects.isNull(request.getTenantIds()) || request.getTenantIds().isEmpty()) {
            return removeSuperAdmin(request);
        }
        List<Organization> organizations = organizationRepository
                .findByIdInAndIsDeletedFalseAndIsActiveTrue(request.getTenantIds());
        if (Objects.isNull(organizations) || request.getTenantIds().size() != organizations.size()) {
            throw new DataNotFoundException(1001);
        }
        return userService.deleteOrganizationUser(request);
    }

    /**
     * <p>
     * Removes a super admin user.
     * This method deactivates and marks a super admin user as deleted based on the
     * provided SearchRequestDTO.
     * It first retrieves the user by ID, checks if the user exists and if they have
     * the super admin role.
     * If the user does not exist or does not have the super admin role, it throws
     * an exception.
     * Otherwise, it sets the user's active status to false, marks them as deleted,
     * saves the changes,
     * and returns a UserResponseDTO containing the user's details.
     * </p>
     *
     * @param request The SearchRequestDTO object containing the identification
     *                details of the super admin to be removed.
     * @return UserResponseDTO The response object containing the details of the
     * removed super admin.
     * @throws DataNotFoundException if the user does not exist.
     * @throws SpiceValidation       if the user does not have the super admin role.
     */
    private UserResponseDTO removeSuperAdmin(SearchRequestDTO request) {
        User user = userService.getUserById(request.getId());
        if (Objects.isNull(user)) {
            throw new DataNotFoundException(2003);
        }
        if (!Objects.isNull(user.getInsightUserOrganization()) && !user.getInsightUserOrganization().isEmpty()) {
            user.getInsightUserOrganization().clear();
        }
        if (!Objects.isNull(user.getReportUserOrganization()) && !user.getReportUserOrganization().isEmpty()) {
            user.getReportUserOrganization().clear();
        }
        user.setActive(false);
        user.setDeleted(true);
        if (!Objects.isNull(user.getFhirId())) {
            fhirServiceApiInterface.deleteUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(),
                    user.getFhirId());
        }
        user = userService.saveUser(user);
        return new ModelMapper().map(user, UserResponseDTO.class);
    }

    /**
     * <p>
     * Maps a list of UserRequestDTOs to User entities.
     * This method iterates over each UserRequestDTO, converts it to a User entity
     * by mapping the provided roles
     * and organization details, and then collects these User entities into a list.
     * </p>
     *
     * @param userRequests A list of UserRequestDTO objects to be mapped to User
     *                     entities.
     * @param organization The Organization entity to which the users belong.
     * @param rolesMap     A map of role IDs to Role entities, used to assign roles
     *                     to the users.
     * @return A list of User entities mapped from the provided UserRequestDTOs.
     */
    private List<User> mapUsers(List<UserRequestDTO> userRequests, Organization organization,
                                Map<Long, Role> rolesMap) {
        List<User> users = new ArrayList<>();

        for (UserRequestDTO userRequestDTO : userRequests) {
            User user = mapUser(userRequestDTO, organization,
                    userRequestDTO.getRoleIds().stream().map(rolesMap::get).collect(Collectors.toSet()));
            users.add(user);
            UserRequestDTO userFhirDTO = fhirServiceApiInterface.createUser(CommonUtil.getAuthToken(),
                    CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(),
                    userRequestDTO);
            if (Objects.isNull(userFhirDTO) || Objects.isNull(userFhirDTO.getFhirId())) {
                throw new SpiceValidation(2020, user.getUsername());
            }
            user.setFhirId(userFhirDTO.getFhirId());
        }
        return users;
    }

    /**
     * <p>
     * Maps a {@link UserRequestDTO} to a {@link User} entity.
     * This method utilizes {@link ModelMapper} to map properties from
     * {@link UserRequestDTO} to {@link User},
     * then sets the user's roles, suite access, and organization based on the
     * provided parameters.
     * If the user is assigned the role of SUPER_ADMIN, their organization list is
     * set to null,
     * otherwise, the user is added to the provided organization.
     * </p>
     *
     * @param userRequestDTO The DTO containing user data to be mapped.
     * @param organization   The organization to which the user will be added.
     * @param roles          The set of roles to be assigned to the user.
     * @return The mapped {@link User} entity with roles and organization set.
     */
    private User mapUser(UserRequestDTO userRequestDTO, Organization organization, Set<Role> roles) {
        ModelMapper mapper = new ModelMapper();
        User user = mapper.map(userRequestDTO, User.class);
        setUserRoleAndSuiteAccess(user, organization, roles);
        user.setTenantId(!Objects.isNull(organization) ? organization.getId() : null);
        return user;
    }

    /**
     * <p>
     * Sets the roles and suite access for a {@link User} entity.
     * This method assigns the provided set of roles to the user and determines the
     * suite access based on these roles.
     * If the user has the SUPER_ADMIN role, their organization list is cleared (set
     * to null).
     * Otherwise, the user is added to the specified organization.
     * </p>
     *
     * @param user         The user entity whose roles and suite access are to be
     *                     set.
     * @param organization The organization to which the user will be added, unless
     *                     they are a SUPER_ADMIN.
     * @param roles        The set of roles to be assigned to the user.
     */
    private void setUserRoleAndSuiteAccess(User user, Organization organization, Set<Role> roles) {
        user.setRoles(roles);
        user.setSuiteAccess(roles.stream().map(Role::getSuiteAccessName).collect(Collectors.toSet()));
        if (user.getRoles().stream().allMatch(role -> Objects.equals(Constants.ROLE_SUPER_ADMIN, role.getName())
                || Objects.equals(Constants.ROLE_REPORT_ADMIN, role.getName()))) {
            user.setOrganizations(null);
        } else if (!Objects.isNull(organization)) {
            user.getOrganizations().add(organization);
        }
    }

    /**
     * <p>
     * Maps existing users from a list of UserRequestDTOs to User entities and
     * updates their supervisor relationships.
     * This method processes each UserRequestDTO, retrieves the corresponding User
     * entity from a pre-fetched map,
     * updates its properties, roles, and supervisor as per the provided data, and
     * collects these users into a list.
     * It also handles the creation or update of UserSupervisor entities to reflect
     * any changes in supervisor assignments.
     * </p>
     *
     * @param userRequests    A list of UserRequestDTO objects representing the
     *                        users to be updated.
     * @param organization    The organization to which the users belong.
     * @param rolesMap        A map of role IDs to Role entities, used for assigning
     *                        roles to the users.
     * @param userSupervisors A list of UserSupervisor entities to be updated or
     *                        created based on supervisor changes.
     * @return A list of updated User entities.
     */
    private List<User> mapExistingUsers(List<UserRequestDTO> userRequests, Organization organization,
                                        Map<Long, Role> rolesMap, List<UserSupervisor> userSupervisors) {
        List<Long> existingIds = userRequests.stream().map(UserRequestDTO::getId).toList();
        List<User> users = new ArrayList<>();
        Map<Long, User> usersMap = userService.getUsersByIdsAsMap(existingIds);
        Map<Long, UserSupervisor> userSuperVisorMap = getUserSupervisorAsMap(existingIds);
        for (UserRequestDTO userRequest : userRequests) {
            User user = usersMap.get(userRequest.getId());
            userMapper.setExistingUser(userRequest, user);
            setUserRoleAndSuiteAccess(user, organization,
                    userRequest.getRoleIds().stream().map(rolesMap::get).collect(Collectors.toSet()));
            if (!Objects.isNull(userRequest.getInsightUserOrganizationIds())
                    && !userRequest.getInsightUserOrganizationIds().isEmpty()) {
                user.setInsightUserOrganization(organizationRepository
                        .findByIdInAndIsDeletedFalseAndIsActiveTrue(userRequest.getInsightUserOrganizationIds()));
            }
            if (!Objects.isNull(userRequest.getReportUserOrganizationIds())
                    && !userRequest.getReportUserOrganizationIds().isEmpty()) {
                user.setReportUserOrganization(organizationRepository
                        .findByIdInAndIsDeletedFalseAndIsActiveTrue(userRequest.getReportUserOrganizationIds()));
            }
            users.add(user);
            if (!Objects.isNull(userRequest.getSupervisorId())) {
                UserSupervisor userSupervisor = userSuperVisorMap.get(userRequest.getId());
                if (!Objects.isNull(userSupervisor)) {
                    if (!Objects.equals(userSupervisor.getSupervisorId(), userRequest.getSupervisorId())) {
                        userSupervisor.setActive(false);
                        userSupervisor.setDeleted(true);
                        userSupervisors.add(userSupervisor);
                        userSupervisors.add(new UserSupervisor(user.getId(), userRequest.getSupervisorId()));
                    }
                } else {
                    userSupervisors.add(new UserSupervisor(user.getId(), userRequest.getSupervisorId()));
                }
            }
        }
        return users;
    }

    /**
     * <p>
     * Maps a single UserRequestDTO to an existing User entity.
     * This method retrieves an existing User by ID and updates its properties,
     * roles, and organization based on the provided DTO.
     * It also performs a check to ensure that the user is not being re-added to an
     * organization they are already part of, if it's not an update operation.
     * </p>
     *
     * @param userRequest  The UserRequestDTO containing the updated information for
     *                     the user.
     * @param organization The organization to which the user will be added or
     *                     updated.
     * @param roles        The set of roles to be assigned to the user.
     * @param isUpdate     A boolean flag indicating whether this is an update
     *                     operation.
     * @return The updated User entity.
     * @throws SpiceValidation if the user is being re-added to an organization they
     *                         are already part of and it's not an update operation.
     */
    private User mapExistingUser(UserRequestDTO userRequest, Organization organization, Set<Role> roles,
                                 boolean isUpdate) {
        User user = userService.getUserById(userRequest.getId());
        if (!isUpdate && user.getOrganizations().stream()
                .anyMatch(org -> Objects.equals(organization.getId(), org.getId()))) {
            throw new SpiceValidation(2021);
        }
        userMapper.setExistingUser(userRequest, user);
        validateRoles(user.getRoles().stream().map(Role::getName).collect(Collectors.toSet()),
                roles.stream().map(Role::getName).collect(Collectors.toSet()), user);
        setUserRoleAndSuiteAccess(user, organization, roles);
        return user;
    }

    /**
     * <p>
     * Retrieves a mapping of UserSupervisor entities for a given list of user IDs.
     * This method fetches all active and non-deleted UserSupervisor entities for
     * the specified user IDs and returns a map
     * where the key is the user ID and the value is the corresponding
     * UserSupervisor entity.
     * </p>
     *
     * @param ids A list of user IDs for which to retrieve UserSupervisor entities.
     * @return A map of user IDs to their corresponding UserSupervisor entities.
     */
    private Map<Long, UserSupervisor> getUserSupervisorAsMap(List<Long> ids) {
        List<UserSupervisor> userSupervisors = userSupervisorRepository
                .findByUserIdInAndIsDeletedFalseAndIsActiveTrue(ids);
        Map<Long, UserSupervisor> userSupervisorsMap = new HashMap<>();
        if (!Objects.isNull(userSupervisors) && !userSupervisors.isEmpty()) {
            userSupervisors.forEach(userSupervisor ->
                userSupervisorsMap.put(userSupervisor.getUserId(), userSupervisor)
            );
        }
        return userSupervisorsMap;
    }

    /**
     * <p>
     * Updates or creates a supervisor relationship for a user.
     * This method checks if an existing UserSupervisor entity is present for the
     * user. If so, it updates the supervisor ID if it has changed,
     * marking the old relationship as inactive and deleted. If no existing
     * relationship is found, a new UserSupervisor entity is created.
     * </p>
     *
     * @param userId       The ID of the user for whom the supervisor relationship
     *                     is to be updated or created.
     * @param supervisorId The ID of the supervisor to be assigned to the user.
     */
    private void updateSupervisorForUser(Long userId, Long supervisorId) {
        UserSupervisor userSupervisor = userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(userId);
        if (!Objects.isNull(userSupervisor)) {
            if (!Objects.equals(userSupervisor.getSupervisorId(), supervisorId)) {
                userSupervisor.setActive(false);
                userSupervisor.setDeleted(true);
                userSupervisorRepository.save(userSupervisor);
                userSupervisorRepository.save(new UserSupervisor(userId, supervisorId));
            }
        } else {
            userSupervisorRepository.save(new UserSupervisor(userId, supervisorId));
        }
    }

    /**
     * {@inheritDoc}
     */
    public void deleteHealthFacility(SearchRequestDTO request) {
        if (Objects.isNull(request.getId()) || Objects.isNull(request.getTenantId())) {
            throw new BadRequestException(20004);
        }
        Organization organization = getOrganization(request.getTenantId());
        organization.setActive(false);
        organization.setDeleted(true);
        createOrganization(organization);
        ResponseEntity<String> responseEntity = adminServiceApiInterface.deleteHealthFacility(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), request);
        fhirServiceApiInterface.deleteOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                CommonUtil.getClient(),
                responseEntity.getBody());
        userService.deleteOrganizationUsers(request);
    }

    /**
     * {@inheritDoc}
     */
    public void updateHealthFacility(HealthFacilityRequestDTO request) {
        Organization organization = organizationRepository.findById(request.getTenantId())
                .orElse(new Organization());
        if ((!Objects.isNull(request.getName()) && !organization.getName().equals(request.getName()))) {
            organization.setName(request.getName());
        }
        if (!Objects.isNull(request.getParentTenantId())
                && !request.getParentTenantId().equals(organization.getParentOrganizationId())) {
            organization.setParentOrganizationId(request.getParentTenantId());
        }
        organizationRepository.save(organization);
        if (!Objects.isNull(request.getLinkedSupervisorIds())) {
            userService.addOrganizationForUsers(request.getLinkedSupervisorIds(), organization, request.getAppTypes());
        }
    }

    /**
     * <p>
     * This method is used to validate the roles of a user. It checks the existing
     * roles of the user and the new roles that are to be assigned.
     * If there are any inconsistencies, it modifies the user's properties or throws
     * an exception.
     * </p>
     *
     * @param existingRoles The set of roles the user currently has.
     * @param newRoles      The set of roles to be assigned to the user.
     * @param user          The user entity whose roles are being validated and
     *                      updated.
     * @throws SpiceValidation if the validation fails due to role-specific
     *                         constraints.
     */
    private void validateRoles(Set<String> existingRoles, Set<String> newRoles, User user) {
        if (existingRoles.contains(Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT)
                && !newRoles.contains(Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT)) {
            throw new SpiceValidation(2023);
        }

        if (existingRoles.contains(Constants.ROLE_CHP) && !newRoles.contains(Constants.ROLE_CHP)) {
            Set<Long> villageIds = user.getVillages().stream().map(Village::getId).collect(Collectors.toSet());
            List<User> userList = userService.getUserByVillageIds(villageIds, user.getId());
            Set<Long> userVillageIds = new HashSet<>();
            userList.forEach(userObject -> userVillageIds
                    .addAll(userObject.getVillages().stream().map(BaseEntity::getId).toList()));
            if (!userVillageIds.containsAll(villageIds)) {
                throw new SpiceValidation(2024);
            }
            user.setVillages(null);
        }
        if (newRoles.contains(Constants.ROLE_SUPER_ADMIN)) {
            user.setOrganizations(null);
            user.setVillages(null);
            user.setTenantId(null);
        }
    }

    /**
     * {@inheritDoc}
     */
    public Organization getOrganizationById(long organizationId) {
        return organizationRepository.getOrganizationById(organizationId);
    }

    /**
     * {@inheritDoc}
     */
    @Transactional
    public void createDistrict(DistrictRequestDTO districtRequestDTO) {
        Organization org = organizationRepository.findByNameIgnoreCaseAndFormName(districtRequestDTO.getName(), Constants.FORM_NAME_DISTRICT);
        if (!Objects.isNull(org)) {
            throw new DataConflictException(1002, districtRequestDTO.getName());
        }
        userService.validateUsers(districtRequestDTO.getUsers());
        Organization organization = new Organization(Constants.FORM_NAME_DISTRICT,
                districtRequestDTO.getName(), districtRequestDTO.getTenantId());
        organization = createOrganization(organization);
        districtRequestDTO.setTenantId(organization.getId());
        District district = adminServiceApiInterface.createDistrict(CommonUtil.getAuthToken(),
                UserContextHolder.getUserDto().getClient()
                , districtRequestDTO).getBody();
        if (Objects.nonNull(district)) {
            organization.setFormDataId(district.getId());
        }
        organization = createOrganization(organization);
        List<Long> roleIds = new ArrayList<>();
        districtRequestDTO.getUsers().forEach(user -> roleIds.addAll(user.getRoleIds()));
        Map<Long, Role> rolesMap = roleService.getRoleMap(roleIds);
        List<User> users = mapUsers(districtRequestDTO.getUsers(), organization, rolesMap);
        userService.createUsers(users);
    }

    /**
     * {@inheritDoc}
     */
    public Boolean activateOrDeactivateOrganization(List<Long> formdataIdList, boolean doActivate, List<String> fhirIds) {
        if (!Objects.isNull(formdataIdList)) {
            Set<Organization> organizations = organizationRepository.findByIsDeletedFalseAndIsActiveAndIdIn(!doActivate,
                    formdataIdList);
            if (!Objects.isNull(organizations)) {
                organizations.forEach(organization -> organization.setActive(doActivate));
                fhirServiceApiInterface.activateDeactivateOrganization(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(),
                        fhirIds, doActivate);
            }
            return true;
        }
        return Boolean.FALSE;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, List<Long>> getChildOrganizations(long tenantId, String formName) {
        Map<String, List<Long>> childIds = new HashMap<>();
        List<Organization> childOrgs;
        List<Long> childOrgIds = new ArrayList<>();
        List<Long> childOrgIdsToDelete = new ArrayList<>();
        if (formName.equalsIgnoreCase(Constants.COUNTRY)) {
            childOrgs = organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(tenantId);
            childOrgIds = childOrgs.stream().map(BaseEntity::getId).toList();
            childIds.put(Constants.DISTRICT_IDS, childOrgIds);
            childOrgIdsToDelete.addAll(childOrgIds);
        }
        if (formName.equalsIgnoreCase(Constants.COUNTRY) || formName.equalsIgnoreCase(Constants.DISTRICT)) {
            if (formName.equalsIgnoreCase(Constants.DISTRICT)) {
                childOrgs = organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(tenantId);
            } else {
                childOrgs = organizationRepository.findByParentOrganizationIdIn(childOrgIds);
            }
            childOrgIds = childOrgs.stream().map(BaseEntity::getId).toList();
            childIds.put(Constants.CHIEFDOM_IDS, childOrgIds);
            childOrgIdsToDelete.addAll(childOrgIds);
        }
        if (formName.equalsIgnoreCase(Constants.COUNTRY) || formName.equalsIgnoreCase(Constants.DISTRICT)
                || formName.equalsIgnoreCase(Constants.CHIEFDOM)) {
            if (formName.equalsIgnoreCase(Constants.CHIEFDOM)) {
                childOrgs = organizationRepository.findByParentOrganizationIdAndIsDeletedFalse(tenantId);
            } else {
                childOrgs = organizationRepository.findByParentOrganizationIdIn(childOrgIds);
            }
            childOrgIds = childOrgs.stream().map(BaseEntity::getId).toList();
            childIds.put(Constants.HEALTH_FACILITY_IDS, childOrgIds);
            childOrgIdsToDelete.addAll(childOrgIds);
        }
        return childIds;
    }


    @Transactional
    public void createChiefdom(ChiefdomRequestDTO chiefdomRequestDTO) {
        validateOrganization(chiefdomRequestDTO.getName(), Constants.FORM_NAME_CHIEFDOM);
        userService.validateUsers(chiefdomRequestDTO.getUsers());
        Organization organization = new Organization(Constants.FORM_NAME_CHIEFDOM,
                chiefdomRequestDTO.getName(), chiefdomRequestDTO.getTenantId());
        organization = createOrganization(organization);
        chiefdomRequestDTO.setTenantId(organization.getId());
        Chiefdom chiefdom = adminServiceApiInterface.createChiefdom(CommonUtil.getAuthToken(),
                UserContextHolder.getUserDto().getClient()
                , chiefdomRequestDTO).getBody();
        assert chiefdom != null;
        organization.setFormDataId(chiefdom.getId());
        organization = createOrganization(organization);
        List<Long> roleIds = new ArrayList<>();
        chiefdomRequestDTO.getUsers().forEach(user -> roleIds.addAll(user.getRoleIds()));
        Map<Long, Role> rolesMap = roleService.getRoleMap(roleIds);
        List<User> users = mapUsers(chiefdomRequestDTO.getUsers(), organization, rolesMap);
        userService.createUsers(users);
    }

    /**
     * {@inheritDoc}
     */
    public List<Organization> getOrganizations(String formName) {
        return organizationRepository.getOrganizationsByFormName(formName);
    }

    /**
     * {@inheritDoc}
     */
    public void validateParentOrganization(Long parentOrganizationId, User user) {
        List<Long> parentOrganizationIds = user.getOrganizations().stream().map(Organization::getParentOrganizationId)
                .toList();
        if (!parentOrganizationIds.contains(parentOrganizationId)) {
            throw new DataNotAcceptableException(50002);
        }
    }
}
