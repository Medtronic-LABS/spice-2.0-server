package com.mdtlabs.coreplatform.userservice.util;

import static org.mockito.Mockito.mockStatic;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserPreferences;
import org.mockito.MockedStatic;
import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserSupervisor;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserToken;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

public class TestDataProvider {

    public static ModelMapper modelMapper = new ModelMapper();
    private static MockedStatic<CommonUtil> commonUtil;
    private static MockedStatic<UserSelectedTenantContextHolder> userSelectedTenantContextHolder;
    private static MockedStatic<UserTenantsContextHolder> userTenantsContextHolder;
    private static MockedStatic<UserContextHolder> userContextHolder;
    private static MockedStatic<MessageFormat> messageFormat;

    public static void init() {
        commonUtil = mockStatic(CommonUtil.class);
        userSelectedTenantContextHolder = mockStatic(UserSelectedTenantContextHolder.class);
        userTenantsContextHolder = mockStatic(UserTenantsContextHolder.class);
        userContextHolder = mockStatic(UserContextHolder.class);
        messageFormat = mockStatic(MessageFormat.class);
    }

    public static void getStaticMock() {
        UserDTO userDTO = TestDataProvider.getUserDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(Boolean.FALSE);
        userDTO.setTenantId(TestConstants.ONE);
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn("BearerTest");
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
        userTenantsContextHolder.when(UserTenantsContextHolder::get).thenReturn(List.of(1L));
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
    }

    public static void getUserDtoMock(boolean isSuperUser) {
        UserDTO userDTO = TestDataProvider.getUserDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(isSuperUser);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);
    }

    public static void getMessageValidatorMock() {
        messageFormat.when(() -> MessageFormat.format("Invalid token: {0}, message: {1}", TestConstants.ARGUMENT, TestConstants.MESSAGE)).thenReturn("message");
        messageFormat.when(() -> MessageFormat.format("Invalid token: {0}", TestConstants.ARGUMENT)).thenReturn("message");
    }

    public static void cleanUp() {
        commonUtil.close();
        userSelectedTenantContextHolder.close();
        userTenantsContextHolder.close();
        userContextHolder.close();
        messageFormat.close();
    }

    public static UserDTO getUserDTO() {
        return modelMapper.map(getUser(), UserDTO.class);
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setPassword(TestConstants.PASSWORD);
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordTime(new Date());
        user.setTenantId(TestConstants.FIVE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.getRoles().add(getRole());
        user.setOrganizations(getSetOrganizations());
        user.getSuiteAccess().add("Admin");
        user.setTimezone(getTimezone());
        return user;
    }

    public static Role getRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName(Constants.ROLE_SUPER_ADMIN);
        role.setLevel(1000l);
        role.setSuiteAccessName("Admin");
        return role;
    }

    public static Set<Organization> getSetOrganizations() {
        Set<Organization> setOrganizations = new HashSet<>();
        setOrganizations.add(getOrganization());
        return setOrganizations;
    }

    public static Organization getOrganization() {
        Organization organization = new Organization();
        organization.setId(TestConstants.TWO);
        organization.setFormName(TestConstants.FORM_NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(TestConstants.ORGANIZATION_NAME);
        organization.setSequence(TestConstants.ZERO);
        organization.setParentOrganizationId(TestConstants.FIVE);
        organization.setFormDataId(TestConstants.ONE);
        organization.setActive(Boolean.TRUE);
        return organization;
    }

    public static Timezone getTimezone() {
        Timezone timeZone = new Timezone();
        timeZone.setAbbreviation(TestConstants.ABBREVIATION);
        timeZone.setDescription(TestConstants.DESCRIPTION);
        timeZone.setOffset(TestConstants.OFFSET);
        return timeZone;
    }

    public static UserRequestDTO getUserRequestDTO() {
        UserRequestDTO user = new UserRequestDTO();
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setTenantId(TestConstants.FIVE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.setRoleIds(List.of(1l));
        user.setCountry(getCountry());
        user.setTimezone(getTimezone());
        return user;
    }

    public static UserRequestDTO getUserRequestDTO_insightID() {
        UserRequestDTO user = new UserRequestDTO();
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setTenantId(TestConstants.FIVE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.setRoleIds(List.of(1l));
        user.setCountry(getCountry());
        user.setTimezone(getTimezone());
        user.setInsightUserOrganizationIds(List.of(1L));
        user.setReportUserOrganizationIds(List.of(1L));
        return user;
    }

    public static SearchRequestDTO getSearchRequestDTO() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setId(5L);
        requestDTO.setTenantId(15L);
        requestDTO.setTenantId(TestConstants.ONE);
        requestDTO.setEmail(TestConstants.EMAIL);
        return requestDTO;
    }

    public static HealthFacilityRequestDTO getHealthFacilityRequestDTO() {
        HealthFacilityRequestDTO request = new HealthFacilityRequestDTO();

        request.setAddress(null);
        request.setChiefdom(new Chiefdom());
        request.setDistrict(new District());
        request.setLanguage(null);
        request.setLatitude(null);
        request.setLongitude(null);
        request.setName("abc");
        request.setTenantId(null);
        request.setUsers(List.of(getUserRequestDTO(), getUserRequestDTO()));
        return request;
    }

    public static HealthFacilityRequestDTO getHealthFacilityRequestDTOWithInsightID() {
        HealthFacilityRequestDTO request = new HealthFacilityRequestDTO();

        request.setAddress(null);
        request.setChiefdom(new Chiefdom());
        request.setDistrict(new District());
        request.setLanguage(null);
        request.setLatitude(null);
        request.setLongitude(null);
        request.setName("abc");
        request.setTenantId(null);
        request.setUsers(List.of(getUserRequestDTO_insightID(), getUserRequestDTO_insightID()));
        return request;
    }

    public static HealthFacility getHealthFacility() {
        HealthFacility request = new HealthFacility();

        request.setAddress(null);
        request.setChiefdom(new Chiefdom());
        request.setDistrict(new District());
        request.setLanguage(null);
        request.setLatitude(null);
        request.setLongitude(null);
        request.setName("abc");
        request.setTenantId(null);
        return request;
    }

    public static CountryRequestDTO getCountryRequestDTO() {
        CountryRequestDTO request = new CountryRequestDTO();
        request.setName("SL");
        request.setUnitMeasurement("+23");
        request.setUsers(List.of(getUserRequestDTO()));
        return request;
    }

    public static Country getCountry() {
        Country country = new Country();
        country.setName("SL");
        country.setUnitMeasurement("+23");
        return country;
    }

    public static Village getVillage() {
        Village village = new Village();
        village.setId(1l);
        village.setName("village 1");
        return village;
    }

    public static UserSuperAdminDto getUserSuperAdminDto() {
        UserSuperAdminDto userSuperAdminDto = new UserSuperAdminDto();
        userSuperAdminDto.setId(TestConstants.ONE);
        userSuperAdminDto.setFirstName(TestConstants.FIRST_NAME);
        return userSuperAdminDto;
    }

    public static OrganizationDTO getOrganizationDTO() {
        OrganizationDTO organization = new OrganizationDTO();
        organization.setId(TestConstants.ONE);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(TestConstants.ORGANIZATION_NAME);
        organization.setSequence(TestConstants.ZERO);
        organization.setParentOrganizationId(TestConstants.FIVE);
        organization.setFormDataId(TestConstants.ONE);
        return organization;
    }

    public static UserPreferencesDTO getUserPreferencesDTO() {
        UserPreferencesDTO userPreferencesDTO = new UserPreferencesDTO();
        userPreferencesDTO.setUserId(1L);
        userPreferencesDTO.setActive(true);
        return userPreferencesDTO;
    }

    public static UserPreferences getUserPreferences() {
        UserPreferences userPreferences = new UserPreferences();
        userPreferences.setUserId(1L);
        userPreferences.setActive(true);
        userPreferences.setDeleted(false);
        return userPreferences;
    }
    
    public static DistrictRequestDTO getDistrictRequestDTO() {
        DistrictRequestDTO districtRequestDTO = new DistrictRequestDTO();
        List<UserRequestDTO> users = new ArrayList<>();
        users.add(getUserRequestDTO());
        districtRequestDTO.setId(TestConstants.ONE);
        districtRequestDTO.setName(TestConstants.NAME);
        districtRequestDTO.setCountryId(TestConstants.ONE);
        districtRequestDTO.setUsers(users);
        return districtRequestDTO;
    }

    public static District getDistrict() {
        District district = new District();
        district.setName(TestConstants.NAME);
        return district;
    }

    /**
     * Used to get list of UserResponseDTOs
     * @return
     */
    public static List<UserResponseDTO> getUserResponseDTOS() {
        UserResponseDTO userResponseDTO = getUserResponseDTO();
        userResponseDTO.setId(TestConstants.ONE);
        UserResponseDTO secondUserResponseDTO = getUserResponseDTO();
        secondUserResponseDTO.setId(TestConstants.TWO);
        return List.of(userResponseDTO, secondUserResponseDTO);
    }

    /**
     * Used to get UserResponseDTO
     * @return
     */
    public static UserResponseDTO getUserResponseDTO() {
        UserResponseDTO userResponseDTO = new UserResponseDTO();
        userResponseDTO.setFirstName(TestConstants.FIRST_NAME);
        userResponseDTO.setUsername(TestConstants.USER_NAME);
        userResponseDTO.setDefaultRoleName(Constants.ROLE_HEALTH_FACILITY_ADMIN);
        return userResponseDTO;
    }

    /**
     * Used to get list of users
     * @return
     */
    public static List<User> getUsers() {
        List<User> users = new ArrayList<>();
        users.add(getUser());
        return users;
    }

    public static RoleResponseDTO getRoleResponse(String roleName) {
        RoleResponseDTO role = new RoleResponseDTO();
        role.setId(TestConstants.ONE);
        role.setName(roleName);
        role.setSuiteAccessName(Constants.CLIENT_ADMIN);
        return role;
    }

    public static List<UserVillageResponseDTO> getUserVillageResponseDTO() {
        UserVillageResponseDTO response = new UserVillageResponseDTO();

        response.setId(1L);
        response.setName(TestConstants.NAME);
        response.setUsername(TestConstants.NAME);
        response.setFirstName(TestConstants.FIRST_NAME);
        response.setLastName(TestConstants.LAST_NAME);
        return List.of(response);
    }

    public static UserVillageDTO getUserVillageDTO() {
        UserVillageDTO response = new UserVillageDTO();

        response.setId(1L);
        response.setFirstName(TestConstants.FIRST_NAME);
        response.setLastName(TestConstants.LAST_NAME);
        response.setVillageId(TestConstants.FIVE);
        return response;
    }

    public static List<UserSupervisor> getUserSupervisor() {
        UserSupervisor userSupervisor = new UserSupervisor();
        userSupervisor.setUserId(TestConstants.ONE);
        userSupervisor.setId(TestConstants.TWO);
        return List.of(userSupervisor);
    }

    public static List<UserToken> getUserToken() {
        UserToken userToken = new UserToken();
        userToken.setUserId(TestConstants.ONE);
        userToken.setAuthToken("A4353sd");
        return List.of(userToken);
    }

    public static void getStaticMocks() {
        UserDTO userDTO = TestDataProvider.getUserDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(Boolean.FALSE);
        userDTO.setTenantId(TestConstants.ONE);

        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);

        RoleDTO roleDTO = new RoleDTO();
        roleDTO.setSuiteAccessName(Constants.CLIENT_WEB);
        roleDTO.setLevel(2L);

        Role role = new Role();
        role.setSuiteAccessName(roleDTO.getSuiteAccessName());
        role.setLevel(roleDTO.getLevel());
        role.setGroupName("SPICE");
        role.setName("SUPER_ADMIN");
        role.setSuiteAccessName("admin");

        Set<Role> roles = new HashSet<>();
        roles.add(role);

        // Set the roles in userDTO
        userDTO.setRoles(roles);
        List<Role> rolesList = new ArrayList<>();
        rolesList.add(role);
        userContextDTO.setRoles(rolesList);

        commonUtil.when(CommonUtil::getAuthToken).thenReturn("BearerTest");
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
        userTenantsContextHolder.when(UserTenantsContextHolder::get).thenReturn(List.of(1L));
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
    }


    public static Chiefdom getChiefdom() {
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setName("chiefdom");
        return chiefdom;
    }

    public static ChiefdomRequestDTO getChiefdomRequestDto() {
        ChiefdomRequestDTO request = new ChiefdomRequestDTO();
        request.setName("west china");
        request.setTenantId(TestConstants.ONE);
        request.setUsers(List.of(getUserRequestDTO()));
        return request;
    }
}
