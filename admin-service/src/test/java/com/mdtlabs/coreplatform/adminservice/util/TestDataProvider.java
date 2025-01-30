package com.mdtlabs.coreplatform.adminservice.util;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.Mockito.mockStatic;

import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityFilterDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import org.mockito.MockedStatic;
import org.modelmapper.ModelMapper;
import com.mdtlabs.coreplatform.adminservice.model.dto.RequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.Code;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.MedicationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTestCustomization;
import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CommunityUnit;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;
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
        userDTO.setCountry(getCountry());
        userDTO.setRoles(Set.of(new Role(TestConstants.ONE, Constants.ROLE_HEALTH_FACILITY_ADMIN, Constants.LONG_ONE, Constants.CLIENT_WEB, List.of(Constants.APP_TYPE_COMMUNITY))));
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);
        userContextDTO.setCountry(getCountry());
        userContextDTO.setRoles(List.of(new Role(TestConstants.ONE, Constants.ROLE_HEALTH_FACILITY_ADMIN, Constants.LONG_ONE, Constants.CLIENT_WEB, List.of(Constants.APP_TYPE_COMMUNITY))));
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
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordTime(new Date());
        user.setTenantId(TestConstants.ONE);
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
        organization.setId(2l);
        organization.setFormName(TestConstants.FORM_NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(TestConstants.ORGANIZATION_NAME);
        organization.setParentOrganizationId(1l);
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

    public static LabTest getLabTest() {
        LabTest labTest = new LabTest();
        labTest.setName(TestConstants.LAB_TEST_NAME);
        labTest.setTenantId(1l);
        labTest.setCountryId(1l);
        return labTest;
    }

    public static LabTestDTO getLabTestDTO() {
        LabTestDTO labTest = new LabTestDTO();
        labTest.setName(TestConstants.LAB_TEST_NAME);
        labTest.setTenantId(1l);
        labTest.setCountryId(1l);
        return labTest;
    }

    public static SearchRequestDTO getSearchRequestDTO() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setId(5L);
        requestDTO.setTenantId(15L);
        return requestDTO;
    }

    public static List<Medication> getMedications() {
        List<Medication> medications = new ArrayList<>();
        medications.add(getMedication());
        return medications;
    }

    public static Medication getMedication() {
        Medication medication = new Medication();
        MedicationDTO medicationDTO = getMedicationDTO();



        medication.setId(medicationDTO.getId());
        medication.setBrandId(medicationDTO.getBrandId());
        medication.setClassificationId(medicationDTO.getClassificationId());
        medication.setName(medicationDTO.getName());
        medication.setDosageFormId(medicationDTO.getDosageFormId());
        medication.setCountryId(medicationDTO.getCountryId());
        medication.setDosageFormName(medicationDTO.getDosageFormName());
        medication.setBrandName(medicationDTO.getBrandName());
        medication.setClassificationName(medicationDTO.getClassificationName());
        Medication.Code code = new Medication.Code();
        code.setCode(TestConstants.STRING_ONE);
        code.setSystem(TestConstants.STRING_ONE);
        code.setDisplay("Paracetamol");
        medication.setCodes(List.of(code));
        return medication;
    }

    public static List<MedicationDTO> getMedicationDTOs() {
        List<MedicationDTO> medications = new ArrayList<>();
        medications.add(getMedicationDTO());
        return medications;
    }

    public static MedicationDTO getMedicationDTO() {
        MedicationDTO medication = new MedicationDTO();
        medication.setClassificationId(TestConstants.ONE);
        medication.setTenantId(TestConstants.ONE);
        medication.setBrandId(TestConstants.ONE);
        medication.setCountryId(TestConstants.ONE);
        medication.setDosageFormId(TestConstants.ONE);
        medication.setCodeDetails(getCode());
        medication.setName(TestConstants.MEDICATION_NAME);
        return medication;
    }

    public static Code getCode() {
        Code code = new Code();
        code.setCode(TestConstants.STRING_ONE);
        code.setUrl(TestConstants.STRING_ONE);
        return code;
    }

    public static HealthFacilityRequestDTO getHealthFacilityRequestDTO() {
        HealthFacilityRequestDTO requestDTO = new HealthFacilityRequestDTO();
        requestDTO.setName("ABC");
        requestDTO.setChiefdom(getChiefdom());
        requestDTO.setTenantId(TestConstants.ONE);
        requestDTO.setDistrict(getDistrict());
        requestDTO.setCountry(getCountry());
        requestDTO.setUsers(List.of(getUserRequestDTO()));
        requestDTO.setParentTenantId(3l);
        return requestDTO;
    }
    
    public static UserRequestDTO getUserRequestDTO() {
        UserRequestDTO user = new UserRequestDTO();
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setTenantId(TestConstants.ONE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.setRoleIds(List.of(1l));
        user.setCountry(getCountry());
        return user;
    }

    public static Country getCountry() {
        Country country = new Country();
        country.setName("SL");
        country.setUnitMeasurement("+23");
        return country;
    }

    public static Culture getCulture() {
        Culture culture = new Culture();
        culture.setName(TestConstants.FIRST_NAME);
        culture.setCode(TestConstants.STRING_ONE);
        return culture;
    }

    public static HealthFacility getHealthFacility() {
        HealthFacility healthFacility = new HealthFacility();
        healthFacility.setName("ABC");
        healthFacility.setChiefdom(getChiefdom());
        healthFacility.setTenantId(TestConstants.ONE);
        healthFacility.setDistrict(getDistrict());
        healthFacility.setCountry(getCountry());
        healthFacility.setLinkedVillages(List.of(getVillage()));
        healthFacility.setFhirId("1");
        return healthFacility;
    }

    public static Village getVillage() {
        Village village = new Village();
        village.setId(1l);
        village.setName("village 1");
        return village;
    }

    public static HealthFacilityDTO getHealthFacilityDTO() {
        HealthFacilityDTO healthFacility = new HealthFacilityDTO();
        healthFacility.setName("ABC");
        healthFacility.setChiefdom(new ChiefdomDTO());
        healthFacility.setTenantId(TestConstants.ONE);
        healthFacility.setDistrict(new DistrictDTO());
        healthFacility.setLinkedVillages(List.of(new VillageDTO()));
        return healthFacility;
    }

    public static Chiefdom getChiefdom() {
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setName("Chiefdom");
        return chiefdom;
    }

    public static District getDistrict() {
        District district = new District();
        district.setName("District");
        return district;
    }

    public static ClinicalWorkflow getClinicalWorkflow() {
        ClinicalWorkflow clinicalWorkflow = new ClinicalWorkflow();
        clinicalWorkflow.setName("TB");
        return clinicalWorkflow;
    }

    public static UserResponseDTO getUserResponseDTO() {
        return new ModelMapper().map(getUserDTO(), UserResponseDTO.class);
    }

    public static CountryRequestDTO getCountryRequestDTO() {
        CountryRequestDTO request = new CountryRequestDTO();
        request.setName("SL");
        request.setUnitMeasurement("+23");
        request.setUsers(List.of(getUserRequestDTO()));
        return request;
    }

    public static LabTestCustomizationDTO getLabTestCustomizationDTO() {
        LabTestCustomizationDTO labTestCustomizationDTO = new LabTestCustomizationDTO();
        labTestCustomizationDTO.setCountryId(1l);
        labTestCustomizationDTO.setTestName(TestConstants.NAME);
        labTestCustomizationDTO.setUniqueName(TestConstants.NAME);
        labTestCustomizationDTO.setFormInput(Constants.FORM_LAYOUT);
        return labTestCustomizationDTO;
    }

    public static LabTestCustomization getLabTestCustomization() {
        LabTestCustomization labTestCustomization = new LabTestCustomization();
        labTestCustomization.setTestName(TestConstants.NAME);
        labTestCustomization.setUniqueName(TestConstants.NAME);
        labTestCustomization.setCountryId(1l);
        labTestCustomization.setFormInput("");
        return labTestCustomization;
    }

    public static DistrictRequestDTO getDistrictRequestDTO() {
        DistrictRequestDTO districtRequestDTO = new DistrictRequestDTO();
        districtRequestDTO.setName("test");
        districtRequestDTO.setTenantId(1L);
        districtRequestDTO.setCountryId(1L);
        return districtRequestDTO;
    }

    public static SearchRequestDTO getSearchRequestDto(String searchTerm, int skip, int limit) {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        searchRequestDTO.setSearchTerm(searchTerm);
        searchRequestDTO.setSkip(skip);
        searchRequestDTO.setLimit(limit);
        return searchRequestDTO;
    }

    public static List<District> getDistricts() {
        District district = getDistrict();
        district.setId(TestConstants.ONE);
        district.setTenantId(Constants.THREE);
        district.setCountryId(TestConstants.ONE);
        District secondDistrict = getDistrict();
        secondDistrict.setId(TestConstants.TWO);
        secondDistrict.setTenantId(TestConstants.ONE);
        secondDistrict.setCountryId(TestConstants.ONE);
        return List.of(district, secondDistrict);
    }

    public static DistrictListDTO getDistrictListDTO(long id, String name, long tenantId) {
        return new DistrictListDTO(id, name, tenantId);
    }

    public static SearchRequestDTO getRequestDtoForPagination(String searchTerm, int skip, int limit) {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setLimit(limit);
        requestDTO.setSkip(skip);
        requestDTO.setSearchTerm(searchTerm);
        return requestDTO;
    }

    public static List<Country> getCountries() {
        Country country = new Country();
        country.setId(TestConstants.ONE);
        country.setName(TestConstants.COUNTRY_NAME);
        country.setTenantId(TestConstants.ONE);
        Country secondCountry = new Country();
        secondCountry.setId(TestConstants.TWO);
        secondCountry.setName(TestConstants.SECOND_COUNTRY_NAME);
        secondCountry.setTenantId(TestConstants.ONE);
        return List.of(country, secondCountry);
    }

    public static CountryListDTO getCountryListDTO() {
        CountryListDTO countryListDTO = new CountryListDTO();
        countryListDTO.setId(TestConstants.ONE);
        countryListDTO.setTenantId(TestConstants.ONE);
        countryListDTO.setName(TestConstants.COUNTRY_NAME);
        return countryListDTO;
    }

    public static List<CountryListDTO> getCountryListDTOs() {
        CountryListDTO countryListDTO = getCountryListDTO();
        countryListDTO.setId(TestConstants.ONE);
        countryListDTO.setChiefdomCount(Constants.ONE);
        countryListDTO.setHealthFacilityCount(Constants.ONE);
        countryListDTO.setDistrictCount(Constants.ONE);
        CountryListDTO secondCountryListDto = getCountryListDTO();
        secondCountryListDto.setId(TestConstants.TWO);
        secondCountryListDto.setName(TestConstants.SECOND_COUNTRY_NAME);
        secondCountryListDto.setHealthFacilityCount(Constants.ONE);
        secondCountryListDto.setChiefdomCount(Constants.ONE);
        secondCountryListDto.setDistrictCount(Constants.ONE);
        List<CountryListDTO> countryListDTOs = new ArrayList<>();
        countryListDTOs.add(countryListDTO);
        countryListDTOs.add(secondCountryListDto);
        return countryListDTOs;
    }

    public static CommunityUnit getCommunityUnit() {
        CommunityUnit communityUnit = new CommunityUnit();
        communityUnit.setCountryId(TestConstants.ONE);
        return communityUnit;
    }

    public static List<Map<String, Object>> getChiefdoms() {
        Map<String, Object> chiefdom = new HashMap<String, Object>();
        chiefdom.put("name", "test");
        chiefdom.put("id", TestConstants.ONE);
        chiefdom.put("tenantId", TestConstants.ONE);
        Map<String, Object> secondChiefdom = new HashMap<String, Object>();
        secondChiefdom.put("name", "test");
        secondChiefdom.put("id", TestConstants.TWO);
        secondChiefdom.put("tenantId", TestConstants.TWO);
        return List.of(chiefdom, secondChiefdom);
    }

    public static List<ChiefdomDTO> getChiefdomDTOs() {
        ChiefdomDTO chiefdomDTO = new ChiefdomDTO();
        chiefdomDTO.setId(TestConstants.ONE);
        chiefdomDTO.setTenantId(TestConstants.ONE);
        ChiefdomDTO secondChiefdomDTO = new ChiefdomDTO();
        secondChiefdomDTO.setId(TestConstants.TWO);
        secondChiefdomDTO.setTenantId(TestConstants.TWO);
        return List.of(chiefdomDTO, secondChiefdomDTO);
    }

    public static List<Chiefdom> getChiefdomList() {
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        chiefdom.setId(TestConstants.ONE);
        chiefdom.setTenantId(TestConstants.ONE);
        Chiefdom secondChiefdom= TestDataProvider.getChiefdom();
        secondChiefdom.setId(TestConstants.TWO);
        secondChiefdom.setTenantId(TestConstants.TWO);
        return List.of(chiefdom, secondChiefdom);
    }

    public static HealthFacilityFilterDTO getHealthFacilityFilterDTO() {
        return modelMapper.map(getHealthFacility(), HealthFacilityFilterDTO.class);
    }

    public static ChiefdomRequestDTO getChiefdomRequestDTO() {
        ChiefdomRequestDTO chiefdomRequestDTO = new ChiefdomRequestDTO();
        chiefdomRequestDTO.setName("test");
        chiefdomRequestDTO.setTenantId(TestConstants.ONE);
        chiefdomRequestDTO.setCountryId(TestConstants.ONE);
        return chiefdomRequestDTO;
    }
    
    public static RequestDTO getRequestDTO() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setFhirId("12344");
        return  requestDTO;
    }

    public static UserTermsAndConditions getUserTermsAndCondition() {
        UserTermsAndConditions userTermsAndConditions = new UserTermsAndConditions();
        userTermsAndConditions.setCountryId(TestConstants.ONE);
        userTermsAndConditions.setActive(Boolean.TRUE);
        return userTermsAndConditions;
    }
}
