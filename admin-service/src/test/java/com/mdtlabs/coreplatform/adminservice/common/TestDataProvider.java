package com.mdtlabs.coreplatform.adminservice.common;

import java.text.MessageFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import org.mockito.MockedStatic;
import org.modelmapper.ModelMapper;

import static org.mockito.Mockito.mockStatic;

import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.AppTypesContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
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
    private static MockedStatic<AppTypesContextHolder> appTypesContextHolder;
    private static final Date date = new Date();

    public static Country getCountry() {
        Country country = new Country();
        country.setId(1L);
        country.setName("Sierra Leone");
        return country;
    }

    public static List<Country> getCountries() {
        return List.of(getCountry());
    }

    public static District getDistrict() {
        District district = new District();
        district.setId(1L);
        district.setName("northern region");
        district.setCode("1");
        return district;
    }

    public static Chiefdom getChiefdom() {
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setId(1L);
        chiefdom.setName("bombali");
        chiefdom.setCode("1");
        return chiefdom;
    }

    public static HealthFacility getHealthFacility() {
        HealthFacility healthFacility = new HealthFacility();
        healthFacility.setId(1L);
        healthFacility.setName("afya kijijini");
        return healthFacility;
    }

    public static Village getVillage() {
        Village village = new Village();
        village.setId(1L);
        village.setName("bolgatanga Regional Hospital");
        return village;
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordTime(new Date());
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.setRoles(Set.of(getRole()));
        return user;
    }

    public static Role getRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName(Constants.ROLE_SUPER_ADMIN);
        return role;
    }

    public static Designation getDesignation() {
        Designation designation = new Designation();
        designation.setCountryId(TestConstants.ONE);
        designation.setName(Constants.ROLE_PHYSICIAN_PRESCRIBER);
        return designation;
    }

    public static DesignationListResponseDTO getDesignationListResponseDTO() {
        DesignationListResponseDTO designationListResponseDTO = new DesignationListResponseDTO();
        designationListResponseDTO.setCountryId(TestConstants.ONE);
        designationListResponseDTO.setName(Constants.ROLE_PHYSICIAN_PRESCRIBER);
        return designationListResponseDTO;
    }

    public static VillageDTO getVillageDTO() {
        VillageDTO villageDTO = new VillageDTO();
        villageDTO.setId(1L);
        villageDTO.setName("bolgatanga Regional Hospital");
        villageDTO.setCode("1234");
        return villageDTO;
    }

    public static void init() {
        userSelectedTenantContextHolder = mockStatic(UserSelectedTenantContextHolder.class);
        commonUtil = mockStatic(CommonUtil.class);
        userTenantsContextHolder = mockStatic(UserTenantsContextHolder.class);
        userContextHolder = mockStatic(UserContextHolder.class);
        messageFormat = mockStatic(MessageFormat.class);
        appTypesContextHolder = mockStatic(AppTypesContextHolder.class);
    }

    public static void getStaticMock() {
        UserDTO userDTO = TestDataProvider.getUserDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(Boolean.FALSE);
        userDTO.setTenantId(TestConstants.ONE);
        userDTO.setCountry(getCountry());
        userDTO.setClient("mob");
        Role role = TestDataProvider.getRole();
        role.setLevel(1L);
        role.setSuiteAccessName(Constants.CLIENT_ADMIN);
        userDTO.setRoles(Set.of(role));
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);
        userContextDTO.setCountry(getCountry());
        userContextDTO.setClient("mob");
        userContextDTO.setRoles(List.of(role));
        CultureDTO culture = new CultureDTO();
        culture.setId(1L);
        userDTO.setCulture(culture);
        userContextDTO.setCulture(culture);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn("BearerTest");
        commonUtil.when(CommonUtil::getClient).thenReturn("mob");
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
        userTenantsContextHolder.when(UserTenantsContextHolder::get).thenReturn(List.of(1L));
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        appTypesContextHolder.when(AppTypesContextHolder::get).thenReturn(List.of());
    }

    public static void cleanUp() {
        commonUtil.close();
        userSelectedTenantContextHolder.close();
        userTenantsContextHolder.close();
        userContextHolder.close();
        messageFormat.close();
        appTypesContextHolder.close();
    }

    public static UserDTO getUserDTO() {
        UserDTO userDTO = modelMapper.map(getUsers(), UserDTO.class);
        userDTO.setClient("mob");
        return userDTO;
    }

    public static User getUsers() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordTime(date);
        user.setTenantId(TestConstants.ONE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.getRoles().add(getUserRole());
        user.setOrganizations(getSetOrganizations());
        user.getSuiteAccess().add("Admin");
        user.setTimezone(getTimezone());
        return user;
    }

    public static Role getUserRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName("SUPER_ADMIN");
        role.setLevel(1000l);
        role.setSuiteAccessName("admin");
        return role;
    }

    public static Role getMobileRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName("PROVIDER");
        role.setLevel(1000l);
        role.setSuiteAccessName("mobile");
        return role;
    }

    public static Set<Organization> getSetOrganizations() {
        Set<Organization> setOrganizations = new HashSet<>();
        setOrganizations.add(getOrganization());
        return setOrganizations;
    }

    public static Organization getOrganization() {
        Organization organization = new Organization();
        organization.setId(TestConstants.ONE);
        organization.setFormName(TestConstants.FORM_NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(TestConstants.ORGANIZATION_NAME);
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
}
