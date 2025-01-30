package com.mdtlabs.coreplatform.spiceservice.common;

import java.text.MessageFormat;
import java.util.*;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.MockedStatic;
import org.modelmapper.ModelMapper;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.AppTypesContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.*;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO.ChiefdomDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.CallStatus;
import com.mdtlabs.coreplatform.spiceservice.common.model.RiskAlgorithm;
import com.mdtlabs.coreplatform.spiceservice.common.model.*;

public class TestDataProvider {
    public static ModelMapper modelMapper = new ModelMapper();
    private static MockedStatic<CommonUtil> commonUtil;
    private static MockedStatic<UserSelectedTenantContextHolder> userSelectedTenantContextHolder;
    private static MockedStatic<UserTenantsContextHolder> userTenantsContextHolder;
    private static MockedStatic<UserContextHolder> userContextHolder;
    private static MockedStatic<MessageFormat> messageFormat;
    private static MockedStatic<AppTypesContextHolder> appTypesContextHolder;
    private static MockedStatic<DateUtil> dateUtil;
    private static final Date date = new Date();

    public static void init() {
        userSelectedTenantContextHolder = mockStatic(UserSelectedTenantContextHolder.class);
        commonUtil = mockStatic(CommonUtil.class);
        userTenantsContextHolder = mockStatic(UserTenantsContextHolder.class);
        userContextHolder = mockStatic(UserContextHolder.class);
        messageFormat = mockStatic(MessageFormat.class);
        appTypesContextHolder = mockStatic(AppTypesContextHolder.class);
    }

    public static void initDateUtil() {
        dateUtil = mockStatic(DateUtil.class);
    }

    public static void getStaticMock() {
        UserDTO userDTO = TestDataProvider.getUserDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(Boolean.FALSE);
        userDTO.setTenantId(TestConstants.ONE);
        userDTO.setCountry(getCountry());
        userDTO.setClient("mob");
        userDTO.setCultureId(TestConstants.ONE);
        CultureDTO culture = new CultureDTO();
        culture.setId(TestConstants.ONE);
        culture.setName(TestConstants.NAME);
        userDTO.setCulture(culture);
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);
        userContextDTO.setCountry(getCountry());
        userContextDTO.setClient("mob");
        userContextDTO.setCulture(culture);
        userContextDTO.setCultureId(TestConstants.ONE);
        userContextDTO.setTimezone(TestDataProvider.getTimezone());
        Long organizationId = TestConstants.TWO;
        userContextDTO.setOrganizationIds(Set.of(organizationId));
        userContextDTO.setRoles(List.of(TestDataProvider.getRole()));
        userContextDTO.getRoles().get(0).setName(TestConstants.NAME);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn("BearerTest");
        commonUtil.when(CommonUtil::getClient).thenReturn("mob");
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
        userTenantsContextHolder.when(UserTenantsContextHolder::get).thenReturn(List.of(1L));
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        appTypesContextHolder.when(AppTypesContextHolder::get).thenReturn(List.of());
    }

    public static void getDateUtilStaticMock() {
        dateUtil.when(() -> DateUtil.getStartDayOfWeekByUserTimeZone(any())).thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.formatDate(anyString(), anyString())).thenReturn(new Date(TestConstants.TEST_DATE));
        dateUtil.when(() -> DateUtil.addDateWithTimezone(any(), eq(Constants.SEVEN), any()))
                .thenReturn(new Date(TestConstants.TEST_DATE));
        dateUtil.when(() -> DateUtil.convertDateToString(any())).thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(Constants.ZERO), eq(Constants.BOOLEAN_FALSE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(Constants.ZERO), eq(Constants.BOOLEAN_TRUE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(Constants.ONE), eq(Constants.BOOLEAN_FALSE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(Constants.ONE), eq(Constants.BOOLEAN_TRUE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getStartDayOfMonthByUserTimeZone(any())).thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getISOString(any())).thenReturn(TestConstants.TEST_DATE);
    }

    public static Country getCountry() {
        Country country = new Country();
        country.setName("SL");
        country.setId(1L);
        country.setUnitMeasurement("+23");
        return country;
    }

    public static void getUserDtoMock(boolean isSuperUser) {
        UserDTO userDTO = TestDataProvider.getUserDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(isSuperUser);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);
    }

    public static void getMessageValidatorMock() {
        messageFormat.when(() -> MessageFormat.format("Invalid token: {0}, message: {1}", TestConstants.ARGUMENT, TestConstants.MESSAGE)).thenReturn("message");
        messageFormat.when(() -> MessageFormat.format("Invalid token: {0} ", TestConstants.ARGUMENT)).thenReturn("message");
    }

    public static void cleanUp() {
        commonUtil.close();
        userSelectedTenantContextHolder.close();
        userTenantsContextHolder.close();
        userContextHolder.close();
        messageFormat.close();
        appTypesContextHolder.close();
    }

    public static void dateUtilCleanUp() {
        dateUtil.close();
    }

    public static UserDTO getUserDTO() {
        UserDTO userDTO = modelMapper.map(getUser(), UserDTO.class);
        userDTO.setClient("mob");
        return userDTO;
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordTime(date);
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
        role.setName(TestConstants.ROLE_PROVIDER);
        role.setLevel(1000L);
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

    public static StaticUserDataResponseDTO getStaticUserDataResponseDTO() {
        StaticUserDataResponseDTO response = new StaticUserDataResponseDTO();
        response.setDefaultHealthFacility(getHealthFacilityDTO());
        response.setNearestHealthFacilities(List.of(getHealthFacilityDTO()));
        response.setMenu(new Menu());
        response.setUserProfile(getUserResponseDTO());
        response.setVillages(List.of(getVillage()));
        return response;
    }

    public static UserResponseDTO getUserResponseDTO() {
        UserResponseDTO userResponseDTO = new ModelMapper().map(getUserDTO(), UserResponseDTO.class);
        userResponseDTO.setVillages(List.of(new VillageDTO()));
        return userResponseDTO;
    }

    public static VillageDTO getVillage() {
        VillageDTO village = new VillageDTO();

        village.setChiefdomId(1l);
        village.setCountryId(1l);
        village.setDistrictId(1l);
        village.setId(1l);
        village.setName("village1");
        return village;
    }

    public static HealthFacilityDTO getHealthFacilityDTO() {
        HealthFacilityDTO healthFacility = new HealthFacilityDTO();
        healthFacility.setName("ABC");
        healthFacility.setTenantId(TestConstants.ONE);
        ChiefdomDTO chiefdomDTO = new ChiefdomDTO();
        healthFacility.setChiefdom(chiefdomDTO);
        ClinicalWorkflow workflow = new ClinicalWorkflow();
        workflow.setWorkflowName("iccm");
        workflow.setId(TestConstants.ONE);
        healthFacility.setClinicalWorkflows(List.of(workflow));
        healthFacility.setCustomizedWorkflows(List.of(workflow));
        healthFacility.setLinkedVillages(List.of(new VillageDTO()));
        return healthFacility;
    }

    public static Menu getMenu() {
        Menu menu = new Menu();
        menu.setId(null);
        menu.setMenus(List.of(Map.of("name", "HOUESHOLD", "order", 1)));
        return menu;
    }

    public static MetaDataDTO getMetaDataDTO() {
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId(TestConstants.ONE);
        metaDataDTO.setCategory(TestConstants.CATEGORY);
        metaDataDTO.setType(TestConstants.TYPE);
        metaDataDTO.setName(TestConstants.TYPE);
        metaDataDTO.setDisplayOrder(TestConstants.TEN);
        metaDataDTO.setCountryId(1L);
        metaDataDTO.setDiseaseCondition(List.of(getDiseaseConditionDTO()));
        return metaDataDTO;
    }

    /**
     * Used to get DiseaseConditionDTO
     *
     * @return DiseaseConditionDTO data
     */
    public static com.mdtlabs.coreplatform.commonservice.common.model.dto.DiseaseConditionDTO getDiseaseConditionDTO() {
        com.mdtlabs.coreplatform.commonservice.common.model.dto.DiseaseConditionDTO diseaseConditionDTO = new com.mdtlabs.coreplatform.commonservice.common.model.dto.DiseaseConditionDTO();
        diseaseConditionDTO.setId(TestConstants.ONE);
        diseaseConditionDTO.setName(TestConstants.TYPE);
        diseaseConditionDTO.setValue(TestConstants.TYPE);
        diseaseConditionDTO.setDiseaseId(TestConstants.ONE);
        diseaseConditionDTO.setDisplayOrder(TestConstants.TEN);
        return diseaseConditionDTO;
    }

    /**
     * Get FollowUpDTO for TestData
     *
     * @return FollowUpDTO data
     */
    public static FollowUpDTO getFollowUp() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setAttempts(TestConstants.INT_ZERO);
        followUpDTO.setCurrentPatientStatus(TestConstants.REFERRED);
        followUpDTO.setFollowUpDetails(getFollowUpDetailDTO());
        followUpDTO.setEncounterDate(date);
        followUpDTO.setEncounterId(TestConstants.STRING_ONE);
        followUpDTO.setEncounterType(TestConstants.ICCM);
        followUpDTO.setHouseholdId(TestConstants.STRING_ONE);
        followUpDTO.setId(TestConstants.ONE);
        followUpDTO.setIsCompleted(Boolean.FALSE);
        followUpDTO.setMemberId(TestConstants.STRING_ONE);
        followUpDTO.setNextVisitDate(date);
        followUpDTO.setPatientId(TestConstants.STRING_ONE);
        followUpDTO.setProvenance(getProvenance());
        followUpDTO.setPatientStatus(TestConstants.ON_TREATMENT);
        followUpDTO.setReason(TestConstants.MALARIA);
        followUpDTO.setReferredSiteId(TestConstants.STRING_ONE);
        followUpDTO.setSuccessfulAttempts(TestConstants.TEN);
        followUpDTO.setType(AppointmentType.HH_VISIT);
        followUpDTO.setVillageId(TestConstants.STRING_ONE);
        return followUpDTO;
    }

    /**
     * Get FollowUpDetailDTO List  for TestData
     *
     * @return FollowUpDetailDTO list data
     */
    public static List<FollowUpDetailDTO> getFollowUpDetailDTO() {
        List<FollowUpDetailDTO> followUpDetailDTOS = new ArrayList<>();
        FollowUpDetailDTO followUpDetailDTO = new FollowUpDetailDTO();
        followUpDetailDTO.setAttempts(TestConstants.ONE);
        followUpDetailDTO.setCallDate(date);
        followUpDetailDTO.setPatientStatus(TestConstants.REFERRED);
        followUpDetailDTO.setStatus(CallStatus.SUCCESSFUL);
        followUpDetailDTOS.add(followUpDetailDTO);
        return followUpDetailDTOS;
    }

    /**
     * Get ProvenanceDTO for TestData
     *
     * @return ProvenanceDTO data
     */
    public static ProvenanceDTO getProvenance() {
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setModifiedDate(date);
        provenanceDTO.setUserId(TestConstants.STRING_ONE);
        provenanceDTO.setOrganizationId(TestConstants.STRING_ONE);
        return provenanceDTO;
    }

    /**
     * Used to get RequestDO
     *
     * @return RequestDTO Data
     */
    public static RequestDTO getRequestDTO() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setVillageIds(List.of(TestConstants.STRING_ONE));
        requestDTO.setPatientStatus(TestConstants.REFERRED);
        requestDTO.setTicketType(TestConstants.MEDICAL_REVIEW);
        requestDTO.setCategory(TestConstants.CATEGORY);
        requestDTO.setReason(TestConstants.MALARIA);
        requestDTO.setMemberId(TestConstants.STRING_ONE);
        requestDTO.setCloseReferralTicket(Boolean.TRUE);
        requestDTO.setProvenance(getProvenance());
        return requestDTO;
    }

    /**
     * Get FollowUpCriteria for TestData
     *
     * @return FollowUpCriteria data
     */
    public static FollowUpCriteria getFollowUpCriteria() {
        FollowUpCriteria followUpCriteria = new FollowUpCriteria();
        followUpCriteria.setMalaria(TestConstants.INT_ONE);
        followUpCriteria.setPneumonia(TestConstants.INT_ONE);
        followUpCriteria.setDiarrhea(TestConstants.INT_ONE);
        followUpCriteria.setMuac(TestConstants.INT_ONE);
        followUpCriteria.setEscalation(TestConstants.INT_ONE);
        followUpCriteria.setAncVisit(TestConstants.INT_ONE);
        followUpCriteria.setReferral(TestConstants.INT_ONE);
        followUpCriteria.setPncVisit(TestConstants.INT_ONE);
        followUpCriteria.setChildVisit(TestConstants.INT_ONE);
        followUpCriteria.setSuccessfulAttempts(TestConstants.INT_ONE);
        followUpCriteria.setUnsuccessfulAttempts(TestConstants.INT_ZERO);
        return followUpCriteria;
    }

    /**
     * Used to get CallRegister
     *
     * @return CallRegister data
     */
    public static CallRegister getCallRegister() {
        CallRegister callRegister = new CallRegister();
        callRegister.setAttempts(TestConstants.INT_ZERO);
        callRegister.setEncounterDate(date);
        callRegister.setEncounterId(TestConstants.STRING_ONE);
        callRegister.setEncounterType(TestConstants.ICCM);
        callRegister.setHouseholdId(TestConstants.STRING_ONE);
        callRegister.setId(TestConstants.ONE);
        callRegister.setIsCompleted(Boolean.FALSE);
        callRegister.setMemberId(TestConstants.STRING_ONE);
        callRegister.setNextVisitDate(date);
        callRegister.setPatientId(TestConstants.STRING_ONE);
        callRegister.setPatientStatus(TestConstants.ON_TREATMENT);
        callRegister.setReason(TestConstants.MALARIA);
        callRegister.setReferredSiteId(TestConstants.STRING_ONE);
        callRegister.setType(AppointmentType.HH_VISIT);
        callRegister.setVillageId(TestConstants.STRING_ONE);
        callRegister.setCallRegisterDetail(List.of(getCallRegisterDetail()));
        return callRegister;
    }

    /**
     * Used to get CallRegisterDetail
     *
     * @return CallRegisterDetail data
     */
    public static CallRegisterDetail getCallRegisterDetail() {
        CallRegisterDetail callRegisterDetail = new CallRegisterDetail();
        callRegisterDetail.setId(TestConstants.ONE);
        callRegisterDetail.setCallRegisterId(TestConstants.ONE);
        callRegisterDetail.setStatus(CallStatus.SUCCESSFUL);
        callRegisterDetail.setCallDate(date);
        callRegisterDetail.setDuration(TestConstants.INT_ONE);
        callRegisterDetail.setAttempts(TestConstants.INT_ZERO);
        callRegisterDetail.setReason(Constants.WRONG_NUMBER);
        return callRegisterDetail;
    }

    /**
     * Used to get ReferralDetailsDTO
     *
     * @return ReferralDetailsDTO data
     */
    public static ReferralDetailsDTO getReferralDetailsDTO() {
        ReferralDetailsDTO referralDetailsDTO = new ReferralDetailsDTO();
        referralDetailsDTO.setPatientStatus(TestConstants.REFERRED);
        referralDetailsDTO.setReferredReason(TestConstants.MALARIA);
        referralDetailsDTO.setMemberId(TestConstants.STRING_ONE);
        referralDetailsDTO.setEncounterId(TestConstants.STRING_ONE);
        referralDetailsDTO.setCurrentPatientStatus(TestConstants.REFERRED);
        referralDetailsDTO.setProvenance(getProvenance());
        referralDetailsDTO.setCategory(TestConstants.TYPE);
        referralDetailsDTO.setNextVisitDate(date);
        referralDetailsDTO.setPatientId(TestConstants.STRING_ONE);
        referralDetailsDTO.setReferred(Boolean.TRUE);
        referralDetailsDTO.setReferredSiteId(TestConstants.STRING_ONE);
        referralDetailsDTO.setType(TestConstants.MEDICAL_REVIEW);
        return referralDetailsDTO;
    }

    /**
     * Get Household Object For Test Data
     *
     * @return HouseholdDTO Object
     */
    public static HouseholdDTO getHouseholdData() {
        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setHouseholdNo(TestConstants.ONE);
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setReferenceId(TestConstants.ONE.toString());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);
        householdDTO.setHouseholdMembers(List.of(getHouseHoldMember()));
        return householdDTO;
    }

    /**
     * Get Household Member for TestData
     *
     * @return HouseholdMember data
     */
    public static HouseholdMemberDTO getHouseHoldMember() {
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setGender(TestConstants.GENDER);
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setReferenceId(TestConstants.ONE.toString());
        householdMemberDTO.setHouseholdHeadRelationship(TestConstants.NAME);
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        return householdMemberDTO;
    }

    /**
     * Used to get HouseholdSequenceDTO
     *
     * @return HouseholdSequenceDTO data
     */
    public static HouseholdSequenceDTO getHouseholdSequenceDTO() {
        HouseholdSequenceDTO householdSequenceDTO = new HouseholdSequenceDTO();
        householdSequenceDTO.setSequence(TestConstants.ONE);
        householdSequenceDTO.setVillageId(TestConstants.STRING_ONE);
        return householdSequenceDTO;
    }

    /**
     * Used to get HouseholdMemberSequenceDTO
     *
     * @return HouseholdMemberSequenceDTO data
     */
    public static HouseholdMemberSequenceDTO getHouseholdMemberSequenceDTO() {
        HouseholdMemberSequenceDTO householdMemberSequenceDTO = new HouseholdMemberSequenceDTO();
        householdMemberSequenceDTO.setSequence(TestConstants.ONE);
        householdMemberSequenceDTO.setUserId(TestConstants.STRING_ONE);
        householdMemberSequenceDTO.setChiefdomCode(TestConstants.STRING_ONE);
        householdMemberSequenceDTO.setVillageCode(TestConstants.STRING_ONE);
        return householdMemberSequenceDTO;
    }

    /**
     * Used to get FormMetaDTO
     *
     * @return FormMetaDTO data
     */
    public static FormMetaDTO getFormMetaDTO() {
        FormMetaDTO formMetaDTO = new FormMetaDTO();
        formMetaDTO.setFormType(TestConstants.TYPE);
        formMetaDTO.setFormInput(TestConstants.TYPE);
        formMetaDTO.setWorkflowName(TestConstants.NAME);
        formMetaDTO.setClinicalWorkflowId(TestConstants.ONE);
        return formMetaDTO;
    }

    /**
     * Used to get MetaDTO
     *
     * @return MetaDTO data
     */
    public static MetaDTO getMetaDTO() {
        MetaDTO metaDTO = new MetaDTO();
        metaDTO.setSymptoms(List.of(getSymptom()));
        metaDTO.setClinicalTools(List.of(getClinicalWorkflow()));
        metaDTO.setFormData(List.of(getFormMetaDTO()));
        return metaDTO;
    }

    /**
     * Used to get Symptom
     *
     * @return Symptom data
     */
    public static Symptom getSymptom() {
        Symptom symptom = new Symptom();
        symptom.setId(TestConstants.ONE);
        symptom.setName(TestConstants.NAME);
        symptom.setValue(TestConstants.NAME);
        symptom.setCategory(TestConstants.CATEGORY);
        symptom.setDisplayOrder(TestConstants.ONE);
        symptom.setType(TestConstants.TYPE);
        return symptom;
    }

    /**
     * Creates and returns a new instance of SymptomDTO with predefined values for testing or initialization purposes.
     * This method populates a SymptomDTO object with default values that can be used in tests or as a standard
     * reference for symptom-related data.
     *
     * @return a SymptomDTO object populated with default values:
     * This object can be utilized in various parts of the application that require symptom data.
     */
    public static SymptomDTO getSymptomDTO() {
        SymptomDTO symptom = new SymptomDTO();
        symptom.setId(TestConstants.ONE);
        symptom.setName(TestConstants.NAME);
        symptom.setOtherSymptom(TestConstants.NAME);
        return symptom;
    }

    /**
     * Used to get ClinicalWorkflow
     *
     * @return ClinicalWorkflow data
     */
    public static ClinicalWorkflow getClinicalWorkflow() {
        ClinicalWorkflow clinicalWorkflow = new ClinicalWorkflow();
        clinicalWorkflow.setName(TestConstants.NAME);
        clinicalWorkflow.setModuleType(TestConstants.TYPE);
        clinicalWorkflow.setWorkflowName(TestConstants.NAME);
        clinicalWorkflow.setCountryId(TestConstants.ONE);
        clinicalWorkflow.setDisplayOrder(TestConstants.ONE);
        return clinicalWorkflow;
    }

    /**
     * Used to get StaticMetaDataResponseDTO
     *
     * @return StaticMetaDataResponseDTO data
     */
    public static StaticMetaDataResponseDTO getStaticMetaDataResponseDTO() {
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = new StaticMetaDataResponseDTO();
        staticMetaDataResponseDTO.setSymptoms(List.of(new MetaDataDTO()));
        staticMetaDataResponseDTO.setCost(List.of(getMetaDataDTO()));
        staticMetaDataResponseDTO.setMedicalSupplies(List.of(getMetaDataDTO()));
        staticMetaDataResponseDTO.setPatientStatus(List.of(getMetaDataDTO()));
        staticMetaDataResponseDTO.setCounselledOn(List.of(getMetaDataDTO()));
        staticMetaDataResponseDTO.setBloodGroup(List.of(getMetaDataDTO()));
        staticMetaDataResponseDTO.setDeliveryAt(List.of(getMetaDataDTO()));
        return staticMetaDataResponseDTO;
    }

    /**
     * Used to get DiseaseConditionDTO
     *
     * @return DiseaseConditionDTO data
     */
    public static com.mdtlabs.coreplatform.spiceservice.common.dto.DiseaseConditionDTO getSpiceDiseaseCondition() {
        com.mdtlabs.coreplatform.spiceservice.common.dto.DiseaseConditionDTO diseaseConditionDTO = new com.mdtlabs.coreplatform.spiceservice.common.dto.DiseaseConditionDTO();
        diseaseConditionDTO.setId(TestConstants.ONE);
        diseaseConditionDTO.setName(TestConstants.TYPE);
        diseaseConditionDTO.setValue(TestConstants.TYPE);
        diseaseConditionDTO.setDiseaseId(TestConstants.ONE);
        diseaseConditionDTO.setDisplayOrder(TestConstants.TEN);
        return diseaseConditionDTO;
    }

    /**
     * Get EncounterDetailsDTO for TestData
     *
     * @return EncounterDetailsDTO data
     */
    public static EncounterDetailsDTO getEncounterDetailsDTO() {
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setHouseholdId(TestConstants.HOUSEHOLD_ID);
        encounterDetailsDTO.setMemberId(TestConstants.MEMBER_ID);
        encounterDetailsDTO.setPatientId(TestConstants.PATIENT_ID);
        encounterDetailsDTO.setProvenance(getProvenance());
        encounterDetailsDTO.setReferred(Boolean.TRUE);
        return encounterDetailsDTO;
    }

    /**
     * Get Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentData() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setEncounter(getEncounterDetailsDTO());
        assessmentDTO.getEncounter().setHouseholdId(TestConstants.HOUSEHOLD_ID);
        assessmentDTO.getEncounter().setMemberId(TestConstants.MEMBER_ID);
        assessmentDTO.getEncounter().setPatientId(TestConstants.PATIENT_ID);
        assessmentDTO.setAssessmentType(TestConstants.ICCM);
        assessmentDTO.setAssessmentDetails(getAssessmentDetailsDTO(TestConstants.ICCM));
        assessmentDTO.setSummary(getSummary());
        assessmentDTO.setReferredReasons(TestConstants.MALARIA);
        return assessmentDTO;
    }

    public static SummaryDTO getSummary() {
        SummaryDTO summaryDTO = new SummaryDTO();
        summaryDTO.setReferredSiteId(TestConstants.STRING_ONE);
        summaryDTO.setReferredSite(TestConstants.NAME);
        summaryDTO.setNextVisitDate(date);
        return summaryDTO;
    }

    public static AssessmentDetailsDTO getAssessmentDetailsDTO(String type) {
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        switch (type) {
            case TestConstants.ICCM:
                assessmentDetailsDTO.setIccm(getICCMDTODetails());
                break;
            case Constants.ANC:
                assessmentDetailsDTO.setAnc(getAncDTODetails());
                break;
            case Constants.PNC_MOTHER:
                assessmentDetailsDTO.setPncMother(getPncMotherDTODetails());
                break;
            case Constants.CHILDHOOD_VISIT:
                assessmentDetailsDTO.setPncChild(getPncChildDTODetails());
                break;
            case Constants.PNC_CHILD_REVIEW:
                assessmentDetailsDTO.setPncNeonatal(getPncNeonatalDTODetails());
                break;
            default : break;
        }
        return assessmentDetailsDTO;
    }

    public static DiseaseCategoryDTO getDiseaseCategoryDTO() {
        DiseaseCategoryDTO diseaseCategoryDTO = new DiseaseCategoryDTO();
        diseaseCategoryDTO.setDiseaseCondition(List.of(getSpiceDiseaseCondition()));
        diseaseCategoryDTO.setId(TestConstants.ONE);
        diseaseCategoryDTO.setValue(TestConstants.NAME);
        diseaseCategoryDTO.setName(TestConstants.NAME);
        diseaseCategoryDTO.setType(TestConstants.TYPE);
        diseaseCategoryDTO.setDisplayOrder(TestConstants.INT_ONE);
        return diseaseCategoryDTO;
    }

    public static DiseaseCondition getDiseaseCondition() {
        DiseaseCondition diseaseCondition = new DiseaseCondition();
        diseaseCondition.setId(TestConstants.ONE);
        diseaseCondition.setName(TestConstants.NAME);
        diseaseCondition.setDiseaseId(TestConstants.ONE);
        diseaseCondition.setValue(TestConstants.TYPE);
        diseaseCondition.setDisplayOrder(TestConstants.INT_ONE);
        return diseaseCondition;
    }

    public static DiseaseCategory getDiseaseCategory() {
        DiseaseCategory diseaseCategory = new DiseaseCategory();
        diseaseCategory.setId(TestConstants.ONE);
        diseaseCategory.setName(TestConstants.NAME);
        diseaseCategory.setValue(TestConstants.TYPE);
        diseaseCategory.setValue(TestConstants.TYPE);
        diseaseCategory.setDiseaseCondition(List.of(getDiseaseCondition()));
        return diseaseCategory;
    }

    public static GeneralMedicalReviewDTO getGeneralMedicalReviewDTO() {
        GeneralMedicalReviewDTO generalMedicalReviewDTO = new GeneralMedicalReviewDTO();
        generalMedicalReviewDTO.setId(TestConstants.STRING_ONE);
        generalMedicalReviewDTO.setEncounter(getEncounterDetailsDTO());
        generalMedicalReviewDTO.setClinicalNotes(TestConstants.NAME);
        generalMedicalReviewDTO.setSystemicExaminationsNotes(TestConstants.TYPE);
        generalMedicalReviewDTO.setPresentingComplaintsNotes(TestConstants.TYPE);
        generalMedicalReviewDTO.setSystemicExaminations(List.of(TestConstants.TYPE));
        generalMedicalReviewDTO.setPresentingComplaints(List.of(TestConstants.TYPE));
        return generalMedicalReviewDTO;
    }

    public static PncMotherDTO getPncMotherDTODetails() {
        PncMotherDTO pncMotherDTO = new PncMotherDTO();
        pncMotherDTO.setFatherPresent(Boolean.TRUE);
        pncMotherDTO.setExclusivelyBreastfeeding(Boolean.TRUE);
        pncMotherDTO.setChlorhexidine(Boolean.TRUE);
        pncMotherDTO.setVisitNo(1);
        pncMotherDTO.setNoOfNeonates("1");
        pncMotherDTO.setPncMotherSigns(Arrays.asList("Headache"));
        pncMotherDTO.setDateOfDelivery(date);
        pncMotherDTO.setSleepsUnderBedNet(Boolean.TRUE);
        return pncMotherDTO;
    }

    public static PncNeonatalDTO getPncNeonatalDTODetails() {
        PncNeonatalDTO pncNeonatalDTO = new PncNeonatalDTO();
        pncNeonatalDTO.setDeathOfNewborn(Boolean.FALSE);
        pncNeonatalDTO.setLowBirthWeight(Boolean.TRUE);
        pncNeonatalDTO.setNewbornReferredToSBCU(Boolean.TRUE);
        pncNeonatalDTO.setPncNeonatalSigns(Arrays.asList("Headache"));
        return pncNeonatalDTO;
    }

    public static AncDTO getAncDTODetails() {
        AncDTO ancDTO = new AncDTO();
        ancDTO.setLastMenstrualPeriod(date);
        ancDTO.setEstimatedDeliveryDate(date);
        ancDTO.setIsMalePartnerPresent(Boolean.TRUE);
        ancDTO.setSleepsUnderBedNet(Boolean.TRUE);
        ancDTO.setEatsMoreThanBefore(Boolean.TRUE);
        ancDTO.setTakesFancidarTablets(Boolean.TRUE);
        ancDTO.setTakesIronFloatTablets(Boolean.TRUE);
        ancDTO.setPriorityPregnancy(Boolean.TRUE);
        ancDTO.setBirthPlanMade(Boolean.TRUE);
        ancDTO.setMiscarriage(Boolean.FALSE);
        ancDTO.setPlaceOfDelivery("Unity Care");
        ancDTO.setGestationalAge("2 Weeks");
        ancDTO.setEats4GroupIronVitARichFoods(Boolean.TRUE);
        ancDTO.setNextVisitDate(date);
        ancDTO.setAncSigns(Arrays.asList("Headache"));
        return ancDTO;
    }

    public static ICCMDTO getICCMDTODetails() {
        ICCMDTO iccmdto = new ICCMDTO();
        iccmdto.setGeneralDangerSigns(getGeneralDangerSignsDTODetails());
        iccmdto.setCough(getCoughDTODetails());
        iccmdto.setDiarrhoea(getDiarrhoeaDTODetails());
        iccmdto.setFever(getFeverDetails());
        iccmdto.setNutritionalStatusDetails(getNutritionalStatusDetails());
        return iccmdto;
    }

    public static OtherSymptomsDTO getOtherSymptomsDTODetails() {
        OtherSymptomsDTO otherSymptomsDTO = new OtherSymptomsDTO();
        otherSymptomsDTO.setFever(getFeverDetails());
        otherSymptomsDTO.setSignsAndSymptoms(getSignsAndSymptomsDetails());
        return otherSymptomsDTO;
    }

    public static PncChildDTO getPncChildDTODetails() {
        PncChildDTO pncChildDTO = new PncChildDTO();
        pncChildDTO.setPncChildSigns(Arrays.asList("Headache"));
        pncChildDTO.setExclusivelyBreastfeeding(Boolean.TRUE);
        pncChildDTO.setFatherPresent(Boolean.TRUE);
        pncChildDTO.setTakingMinimumMealsPerDay(Boolean.TRUE);
        pncChildDTO.setFedFrom4FoodGroups(Boolean.TRUE);
        pncChildDTO.setDeathOfBaby(Boolean.FALSE);
        pncChildDTO.setMotherOrPartnerUsingFamilyPlanning(Boolean.TRUE);
        pncChildDTO.setMuac("Red");
        return pncChildDTO;
    }

    public static FeverDTO getFeverDetails() {
        FeverDTO feverDTO = new FeverDTO();
        feverDTO.setRdtTest("+Ve");
        feverDTO.setAct("Dispensed");
        feverDTO.setHasFever(Boolean.TRUE);
        feverDTO.setTemperature(33.0);
        feverDTO.setNoOfDaysOfFever(33L);
        return feverDTO;
    }

    public static SignsAndSymptomsDTO getSignsAndSymptomsDetails() {
        SignsAndSymptomsDTO signsAndSymptomsDTO = new SignsAndSymptomsDTO();
        signsAndSymptomsDTO.setSymptoms(List.of("No Symptoms"));
        return signsAndSymptomsDTO;
    }

    public static GeneralDangerSignsDTO getGeneralDangerSignsDTODetails() {
        GeneralDangerSignsDTO generalDangerSignsDTO = new GeneralDangerSignsDTO();
        generalDangerSignsDTO.setIsBreastfeed(Boolean.TRUE);
        generalDangerSignsDTO.setIsUnusualSleepy(Boolean.TRUE);
        generalDangerSignsDTO.setIsConvulsionPastFewDays(Boolean.TRUE);
        generalDangerSignsDTO.setIsVomiting(Boolean.TRUE);
        return generalDangerSignsDTO;
    }

    public static CoughDTO getCoughDTODetails() {
        CoughDTO coughDTO = new CoughDTO();
        coughDTO.setHasCough(Boolean.TRUE);
        coughDTO.setChestInDrawing(Boolean.TRUE);
        coughDTO.setNoOfDaysOfCough(33L);
        coughDTO.setBreathPerMinute(33L);
        coughDTO.setAmoxicillin("NA");
        return coughDTO;
    }

    public static DiarrhoeaDTO getDiarrhoeaDTODetails() {
        DiarrhoeaDTO diarrhoeaDTO = new DiarrhoeaDTO();
        diarrhoeaDTO.setHasDiarrhoea(Boolean.TRUE);
        diarrhoeaDTO.setIsBloodyDiarrhoea(Boolean.TRUE);
        diarrhoeaDTO.setNumberOfDaysDiarrhoea(30L);
        diarrhoeaDTO.setOrsDispensedStatus("NA");
        diarrhoeaDTO.setZincDispensedStatus("NA");
        diarrhoeaDTO.setDiarrhoeaSigns(Arrays.asList("Unable to or drinking poorly"));
        return diarrhoeaDTO;
    }

    public static NutritionalStatusDTO getNutritionalStatusDetails() {
        NutritionalStatusDTO nutritionalStatusDTO = new NutritionalStatusDTO();
        nutritionalStatusDTO.setMuacCode("Green");
        nutritionalStatusDTO.setHasOedemaOfBothFeet(Boolean.TRUE);
        return nutritionalStatusDTO;
    }

    public static PrescriptionRequestDTO getPrescriptionRequestDTO() {
        PrescriptionRequestDTO prescriptionRequestDTO = new PrescriptionRequestDTO();
        prescriptionRequestDTO.setEncounter(getEncounterDetailsDTO());

        prescriptionRequestDTO.setPrescriptions(List.of(getPrescriptionDTO()));
        prescriptionRequestDTO.setPrescriptionId(TestConstants.STRING_ONE);
        prescriptionRequestDTO.setDiscontinuedReason(TestConstants.MALARIA);
        prescriptionRequestDTO.setProvenance(getProvenance());
        return prescriptionRequestDTO;
    }

    public static PrescriptionDTO getPrescriptionDTO() {
        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();
        prescriptionDTO.setPrescriptionId(TestConstants.STRING_ONE);
        prescriptionDTO.setPrescribedDays(TestConstants.ONE);
        prescriptionDTO.setMedicationName(TestConstants.NAME);
        prescriptionDTO.setMedicationId(TestConstants.STRING_ONE);
        prescriptionDTO.setFrequency(2);
        prescriptionDTO.setPrescribedSince(date);
        prescriptionDTO.setEndDate(date); // 30 days later
        prescriptionDTO.setDiscontinuedReason(TestConstants.MALARIA);
        prescriptionDTO.setDiscontinuedDate(date);
        prescriptionDTO.setEncounterId(TestConstants.STRING_ONE);
        prescriptionDTO.setIsActive(Boolean.TRUE);
        return prescriptionDTO;
    }

    public static GeneralMedicalReviewSummaryDetailsDTO getGeneralMedicalReviewSummaryDetailsDTO() {
        GeneralMedicalReviewSummaryDetailsDTO generalMedicalReviewSummaryDetailsDTO = new GeneralMedicalReviewSummaryDetailsDTO();
        generalMedicalReviewSummaryDetailsDTO.setId(TestConstants.STRING_ONE);
        generalMedicalReviewSummaryDetailsDTO.setPrescriptions(List.of(getPrescriptionDTO()));
        generalMedicalReviewSummaryDetailsDTO.setPresentingComplaints(List.of(TestConstants.TYPE));
        generalMedicalReviewSummaryDetailsDTO.setPatientStatus(TestConstants.REFERRED);
        generalMedicalReviewSummaryDetailsDTO.setPresentingComplaintsNotes(TestConstants.TYPE);
        generalMedicalReviewSummaryDetailsDTO.setSystemicExaminations(List.of(TestConstants.TYPE));
        return generalMedicalReviewSummaryDetailsDTO;
    }

    public static GeneralMedicalReviewSummaryDTO getGeneralMedicalReviewSummaryDTO() {
        GeneralMedicalReviewSummaryDTO generalMedicalReviewSummaryDTO = new GeneralMedicalReviewSummaryDTO();
        generalMedicalReviewSummaryDTO.setMedicalSupplies(List.of(TestConstants.NAME));
        generalMedicalReviewSummaryDTO.setMemberId(TestConstants.MEMBER_ID);
        generalMedicalReviewSummaryDTO.setHouseholdId(TestConstants.HOUSEHOLD_ID);
        generalMedicalReviewSummaryDTO.setPatientId(TestConstants.PATIENT_ID);
        generalMedicalReviewSummaryDTO.setVillageId(TestConstants.STRING_ONE);
        generalMedicalReviewSummaryDTO.setEncounterType(TestConstants.NAME);
        generalMedicalReviewSummaryDTO.setProvenance(getProvenance());
        generalMedicalReviewSummaryDTO.setCost(TestConstants.TEST);
        generalMedicalReviewSummaryDTO.setNextVisitDate(date);
        generalMedicalReviewSummaryDTO.setPatientStatus(TestConstants.REFERRED);
        generalMedicalReviewSummaryDTO.setPatientReference(TestConstants.REFERRED);
        generalMedicalReviewSummaryDTO.setCategory(TestConstants.TYPE);
        return generalMedicalReviewSummaryDTO;
    }

    public static MedicalReviewPregnancyDTO getMedicalReviewPregnancyDTO() {
        MedicalReviewPregnancyDTO medicalReviewPregnancyDTO = new MedicalReviewPregnancyDTO();
        medicalReviewPregnancyDTO.setPresentingComplaints(List.of(TestConstants.TYPE));
        medicalReviewPregnancyDTO.setPresentingComplaintsNotes(TestConstants.NOTES);
        medicalReviewPregnancyDTO.setObstetricExaminations(List.of(TestConstants.TYPE));
        medicalReviewPregnancyDTO.setObstetricExaminationNotes(TestConstants.NOTES);
        medicalReviewPregnancyDTO.setClinicalNotes(TestConstants.NOTES);
        medicalReviewPregnancyDTO.setPatientReference(TestConstants.TEST);
        medicalReviewPregnancyDTO.setPatientId(TestConstants.PATIENT_ID);
        medicalReviewPregnancyDTO.setFundalHeight(TestConstants.DOUBLE_ONE);
        medicalReviewPregnancyDTO.setFetalHeartRate(TestConstants.DOUBLE_ONE);
        medicalReviewPregnancyDTO.setVisitNumber(TestConstants.ONE);
        medicalReviewPregnancyDTO.setPregnancyHistory(List.of(TestConstants.TEST));
        medicalReviewPregnancyDTO.setEncounter(getEncounterDetailsDTO());
        return medicalReviewPregnancyDTO;
    }

    public static MotherNeonateDTO getMotherNeonateDTO() {
        MotherNeonateDTO motherNeonateDTO = new MotherNeonateDTO();
        motherNeonateDTO.setNeonateDTO(new NeonateDTO());
        motherNeonateDTO.setMotherDTO(new MotherDTO());
        motherNeonateDTO.setChild(new HouseholdMemberDTO());
        return motherNeonateDTO;
    }

    public static UnderFiveIccmDTO getUnderFiveIccmDTO() {
        UnderFiveIccmDTO underFiveIccmDTO = new UnderFiveIccmDTO();
        underFiveIccmDTO.setId(TestConstants.STRING_ONE);
        underFiveIccmDTO.setClinicalNotes(TestConstants.NOTES);
        underFiveIccmDTO.setClinicalSummaryAndSigns(new ClinicalSummaryAndSignsDTO());
        underFiveIccmDTO.setExamination(new UnderFiveIccmDTO.Examination());
        underFiveIccmDTO.setPatientId(TestConstants.PATIENT_ID);
        underFiveIccmDTO.setSystemicExamination(List.of(TestConstants.TYPE));
        return underFiveIccmDTO;
    }

    public static ObservationDTO getObservationDTO() {
        ObservationDTO observationDTO = new ObservationDTO();
        observationDTO.setEncounter(getEncounterDetailsDTO());
        observationDTO.setPatientReference(TestConstants.REFERRED);
        observationDTO.setWeight(TestConstants.DOUBLE_ONE);
        observationDTO.setSystolic(TestConstants.DOUBLE_ONE);
        observationDTO.setPulse(TestConstants.DOUBLE_ONE);
        observationDTO.setDiastolic(TestConstants.DOUBLE_ONE);
        observationDTO.setType(TestConstants.TYPE);
        observationDTO.setNumberValue(TestConstants.INT_ONE);
        observationDTO.setDateValue(date);
        return observationDTO;
    }

    public static MedicalReviewHistoryDTO getMedicalReviewHistoryDTO() {
        MedicalReviewHistoryDTO medicalReviewHistoryDTO = new MedicalReviewHistoryDTO();
        medicalReviewHistoryDTO.setId(TestConstants.STRING_ONE);
        medicalReviewHistoryDTO.setPatientId(TestConstants.PATIENT_ID);
        medicalReviewHistoryDTO.setPatientReference(TestConstants.REFERRED);
        medicalReviewHistoryDTO.setPatientStatus(TestConstants.REFERRED);
        medicalReviewHistoryDTO.setDateOfReview(date);
        medicalReviewHistoryDTO.setNextVisitDate(date);
        medicalReviewHistoryDTO.setType(TestConstants.TYPE);
        medicalReviewHistoryDTO.setVisitNumber(TestConstants.STRING_ONE);
        return medicalReviewHistoryDTO;
    }

    public static MedicalReviewRequestDTO getMedicalReviewRequestDTO() {
        MedicalReviewRequestDTO medicalReviewRequestDTO = new MedicalReviewRequestDTO();
        medicalReviewRequestDTO.setLatestRequired(Boolean.TRUE);
        medicalReviewRequestDTO.setPatientReference(TestConstants.TEST);
        medicalReviewRequestDTO.setPatientId(TestConstants.PATIENT_ID);
        medicalReviewRequestDTO.setType(TestConstants.TYPE);
        medicalReviewRequestDTO.setAssessmentName(TestConstants.NAME);
        medicalReviewRequestDTO.setChildId(TestConstants.STRING_ONE);
        medicalReviewRequestDTO.setMotherId(TestConstants.STRING_ONE);
        medicalReviewRequestDTO.setEncounterId(TestConstants.STRING_ONE);
        return medicalReviewRequestDTO;
    }

    public static PncMedicalReviewDTO getPncMedicalReviewDTO() {
        PncMedicalReviewDTO pncMedicalReviewDTO = new PncMedicalReviewDTO();
        pncMedicalReviewDTO.setPncChild(new PncChildMedicalReviewDTO());
        pncMedicalReviewDTO.setPncMother(new PncMotherMedicalReviewDTO());
        return pncMedicalReviewDTO;
    }

    public static HealthFacility getHealthFacility() {
        HealthFacility healthFacility = new HealthFacility();
        healthFacility.setName("ABC");
        healthFacility.setTenantId(TestConstants.ONE);
        Chiefdom chiefdom = new Chiefdom();
        healthFacility.setChiefdom(chiefdom);
        District district = new District();
        district.setId(1l);
        healthFacility.setDistrict(district);
        ClinicalWorkflow workflow = new ClinicalWorkflow();
        workflow.setWorkflowName("iccm");
        healthFacility.setClinicalWorkflows(List.of(workflow));
        healthFacility.setLinkedVillages(List.of(new Village()));
        return healthFacility;
    }

    public static List<CountryCustomization> getCountryCustomizations() {
        List<CountryCustomization> countryCustomizations = new ArrayList<>();
        CountryCustomization countryCustomization = new CountryCustomization();
        countryCustomization.setId(1l);
        countryCustomization.setFormInput("{formLayout:[{view:input}]}");
        countryCustomization.setCategory("Input_form");
        countryCustomization.setType("Screening");
        countryCustomizations.add(countryCustomization);
        countryCustomization = new CountryCustomization();

        countryCustomization.setId(2l);
        countryCustomization.setFormInput("forms");
        countryCustomization.setCategory("Consent_form");
        countryCustomization.setType(Constants.ENROLLMENT);
        countryCustomizations.add(countryCustomization);

        countryCustomization = new CountryCustomization();

        countryCustomization.setId(3l);
        countryCustomization.setFormInput("forms");
        countryCustomization.setCategory("Consent_form");
        countryCustomization.setType(Constants.ASSESSMENT);
        countryCustomizations.add(countryCustomization);
        return countryCustomizations;
    }

    public static List<ClinicalWorkflow> getCustomizedWorkflows() {
        ClinicalWorkflow customizedWorkflow = new ClinicalWorkflow();
        customizedWorkflow.setId(2L);
        customizedWorkflow.setName("SDOH");
        customizedWorkflow.setCountryId(1l);
        customizedWorkflow.setModuleType("customized");
        customizedWorkflow.setViewScreens(List.of("Screening"));
        return List.of(customizedWorkflow);
    }

    public static List<WorkflowCustomization> getWorkflowCustomizations() {
        WorkflowCustomization workflowCustomization = new WorkflowCustomization();
        workflowCustomization.setClinicalWorkflowId(2l);
        workflowCustomization.setType("Screening");
        workflowCustomization.setCategory("Consent_form");
        workflowCustomization.setCountryId(1l);

        WorkflowCustomization workflowCustomization2 = new WorkflowCustomization();
        workflowCustomization.setClinicalWorkflowId(2l);
        workflowCustomization.setType("Module");
        workflowCustomization.setCategory("Input_form");
        workflowCustomization.setDistrictId(1l);

        return List.of(workflowCustomization, workflowCustomization2);
    }

    public static Map<String, List<MetaDataDTO>> getSaticDataCache() {
        Map<String, List<MetaDataDTO>> valuesMap = new HashMap<>();
        valuesMap.put(TestConstants.META_COMORBIDITIES, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_COMPLAINTS, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_COMPLICATIONS, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_CURRENT_MEDICATION, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_DIAGNOSIS, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_DOSAGE_FORM, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_DOSAGE_FREQUENCY, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_FREQUENCY, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_FREQUENCY_TYPE, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_NUTRITION_LIFESTYLE, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_UNIT, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_SYMPTOMS, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_PHYSICAL_EXAMINATION, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_MEDICAL_COMPLIANCES, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_MESSAGE, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_RISK_ALGORITHM, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_MODEL_QUESTIONS, List.of(new MetaDataDTO()));
        valuesMap.put(TestConstants.META_LIFESTYLE, List.of(new MetaDataDTO()));
        return valuesMap;
    }

    public static Comorbidity getComorbidity() {
        Comorbidity comorbidity = new Comorbidity();
        comorbidity.setId(1L);
        return comorbidity;
    }

    public static Complaints getComplaints() {
        Complaints complaints = new Complaints();
        complaints.setId(1L);
        return complaints;
    }

    public static Complication getComplication() {
        Complication complication = new Complication();
        complication.setId(1L);
        return complication;
    }

    public static CurrentMedication getCurrentMedication() {
        CurrentMedication currentMedication = new CurrentMedication();
        currentMedication.setId(1L);
        return currentMedication;
    }

    public static Diagnosis getDiagnosis() {
        Diagnosis diagnosis = new Diagnosis();
        diagnosis.setId(1L);
        return diagnosis;
    }

    public static DosageForm getDosageForm() {
        DosageForm dosageForm = new DosageForm();
        dosageForm.setId(1L);
        dosageForm.setName("testName");
        return dosageForm;
    }

    public static Lifestyle getLifestyle() {
        Lifestyle lifestyle = new Lifestyle();
        lifestyle.setId(1L);
        lifestyle.setName(Constants.NAME);
        lifestyle.setType("diabetes");
        lifestyle.setAnswerDependent(Constants.BOOLEAN_TRUE);
        return lifestyle;
    }

    public static MedicalCompliance getMedicalCompliance() {
        MedicalCompliance medicalCompliance = new MedicalCompliance();
        medicalCompliance.setId(1L);
        return medicalCompliance;
    }

    public static ModelQuestions getModelQuestions() {
        ModelQuestions modelQuestions = new ModelQuestions();
        modelQuestions.setId(1L);
        return modelQuestions;
    }

    public static PhysicalExamination getPhysicalExamination() {
        PhysicalExamination physicalExamination = new PhysicalExamination();
        physicalExamination.setId(1L);
        return physicalExamination;
    }

    public static Reason getReason() {
        Reason reason = new Reason();
        reason.setId(1L);
        return reason;
    }

    public static RiskAlgorithm getMetaRiskAlgorithm() {
        RiskAlgorithm riskAlgorithm = new RiskAlgorithm();
        riskAlgorithm.setId(1L);
        riskAlgorithm.setCountryId(1L);
        return riskAlgorithm;
    }

    public static Frequency getFrequency() {
        Frequency frequency = new Frequency();
        frequency.setId(1l);
        frequency.setName(Constants.NAME);
        frequency.setType("Blood Pressure Check Frequency");
        frequency.setPeriod("03");
        frequency.setDuration(Constants.TWO);
        frequency.setRiskLevel("Medium");
        frequency.setDisplayOrder(Constants.FOUR);
        return frequency;
    }

    public static FrequencyType getFrequencyType() {
        FrequencyType frequencyType = new FrequencyType();
        frequencyType.setName("BP Check");
        return frequencyType;
    }

    public static NutritionLifestyle getNutritionLifestyle() {
        NutritionLifestyle nutritionLifestyle = new NutritionLifestyle();
        nutritionLifestyle.setName("Stress Management");
        nutritionLifestyle.setDisplayOrder(1);
        nutritionLifestyle.setId(1l);
        return nutritionLifestyle;
    }

    public static DosageFrequency getDosageFrequency() {
        DosageFrequency dosageFrequency = new DosageFrequency();
        dosageFrequency.setId(1l);
        dosageFrequency.setName("testDosageFrequency");
        return dosageFrequency;
    }

    public static SearchRequestDTO getSearchRequestDTO() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setId(5L);
        requestDTO.setTenantId(15L);
        return requestDTO;
    }

    /**
     * Used to get smsDTO
     *
     * @return
     */
    public static SmsDTO getSmsDTO() {
        SmsDTO user = new SmsDTO();
        user.setTenantId(TestConstants.ONE);
        user.setUserName(Constants.NAME);
        user.setNotificationId(Constants.LONG_ONE);
        user.setFormDataId(Constants.STRING_ONE);
        user.setFormName(TestConstants.FORM_NAME);
        user.setBody(Constants.TEMPLATE_TYPE_RED_RISK);
        return user;
    }

    /**
     * Used to get sms template
     *
     * @return
     */
    public static SMSTemplate getSmsTemplate() {
        SMSTemplate smsTemplate = new SMSTemplate();
        smsTemplate.setId(Constants.LONG_ONE);
        smsTemplate.setBody(Constants.NAME);
        smsTemplate.setType(Constants.TYPE);
        return smsTemplate;
    }

    /**
     * Used to get red risk notification
     *
     * @return
     */
    public static RedRiskNotification getRedRiskNotification() {
        RedRiskNotification notification = new RedRiskNotification();
        notification.setStatus(Constants.NEW);
        notification.setEncounterId(Constants.STRING_ONE);
        notification.setMemberId(Constants.STRING_ONE);
        notification.setPatientId(Constants.STRING_ONE);
        notification.setId(Constants.LONG_ONE);
        notification.setTenantId(Constants.LONG_ONE);
        return notification;
    }

    /**
     * Used to get mental health DTO
     *
     * @return
     */
    public static MentalHealthDTO getMentalHealthRequest() {
        MentalHealthDTO mentalHealthDTO = new MentalHealthDTO();
        mentalHealthDTO.setScore(TestConstants.INT_ONE);
        mentalHealthDTO.setRiskLevel(TestConstants.RISK_LEVEL);
        mentalHealthDTO.setMentalHealthDetails(List.of(getMentalHealthDetailsDTO()));
        return mentalHealthDTO;
    }

    /**
     * Used to get mental health details
     *
     * @return
     */
    public static MentalHealthDetailsDTO getMentalHealthDetailsDTO() {
        MentalHealthDetailsDTO mentalHealthDetailsDTO = new MentalHealthDetailsDTO();
        mentalHealthDetailsDTO.setScore(TestConstants.INT_ONE);
        mentalHealthDetailsDTO.setQuestionId(TestConstants.ONE);
        mentalHealthDetailsDTO.setQuestion("Little interest or pleasure in doing things?");
        mentalHealthDetailsDTO.setAnswer("Nearly every day");
        mentalHealthDetailsDTO.setAnswerId(TestConstants.ONE);
        return mentalHealthDetailsDTO;
    }

    public static MenuDTO getMenuDTO() {
        return modelMapper.map(getMenu(), MenuDTO.class);
    }

    public static LabTestDTO getLabTestDTO() {
        LabTestDTO labTestDTO = new LabTestDTO();
        labTestDTO.setId(TestConstants.STRING_ONE);
        labTestDTO.setTestName(TestConstants.NAME);
        labTestDTO.setTestedOn(date);
        labTestDTO.setLabTestResults(List.of(getLabTestResultDTO()));
        labTestDTO.setPatientId(TestConstants.PATIENT_ID);
        return labTestDTO;

    }

    public static LabTestResultDTO getLabTestResultDTO() {
        LabTestResultDTO labTestResultDTO = new LabTestResultDTO();
        labTestResultDTO.setTestedOn(new Date());
        labTestResultDTO.setId("1");
        labTestResultDTO.setResource("String");
        labTestResultDTO.setValue("123");
        return labTestResultDTO;
    }

    public static LabTestRequestDTO getLabTestRequestDTO() {
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        labTestRequestDTO.setLabTests(List.of(getLabTestDTO()));
        labTestRequestDTO.setEncounter(getEncounterDetailsDTO());
        return labTestRequestDTO;

    }

    public static LabTestHistoryDTO getLabTestHistoryDTO() {
        LabTestHistoryDTO labTestHistoryDTO = new LabTestHistoryDTO();
        labTestHistoryDTO.setInvestigations(List.of(getLabTestDTO()));
        labTestHistoryDTO.setEncounterId("123");
        labTestHistoryDTO.setPatientReference("123");
        return labTestHistoryDTO;
    }

    public static PregnancyInfo getPregnancyInfo() {
        PregnancyInfo pregnancyInfo = new PregnancyInfo();
        pregnancyInfo.setAncVisitNo(Constants.ONE);
        return pregnancyInfo;
    }

    public static ConfirmDiagnosisDTO getConfirmDiagnosisDTO() {
        ConfirmDiagnosisDTO confirmDiagnosisDTO = new ConfirmDiagnosisDTO();
        confirmDiagnosisDTO.setPatientReference(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE);
        return confirmDiagnosisDTO;
    }

    /**
     * <p>
     * This method creates and returns a new {@link PatientTransferRequestDTO} object
     * with preset values for transfer details, including transfer location, patient reference,
     * transfer reason, transfer site, and old site.
     * </p>
     *
     * @return {@link PatientTransferRequestDTO} The PatientTransferRequestDTO containing
     * preset values for initializing a patient transfer request.
     */
    public static PatientTransferRequestDTO getPatientTransferRequestDTO() {
        PatientTransferRequestDTO patientTransferRequestDTO = new PatientTransferRequestDTO();
        patientTransferRequestDTO.setTransferTo(Constants.LONG_ONE);
        patientTransferRequestDTO.setPatientReference(Constants.STRING_ONE);
        patientTransferRequestDTO.setTransferReason(String.valueOf(PatientTransferStatus.ACCEPTED));
        patientTransferRequestDTO.setTransferSite(Constants.LONG_ONE);
        patientTransferRequestDTO.setOldSite(Constants.LONG_ONE);
        return patientTransferRequestDTO;
    }

    /**
     * <p>
     * This method creates and returns a new {@link PatientTransfer} object
     * with preset values for a patient transfer. It sets the user to whom the patient
     * will be transferred, the patient's FHIR ID, the reason for the transfer, the transfer site,
     * and the original site, using predefined constants and helper methods.
     * </p>
     *
     * @return {@link PatientTransfer} The PatientTransfer object containing initialized
     * values for user, patient ID, transfer reason, transfer site, and old site.
     */
    public static PatientTransfer getPatientTransfer() {
        PatientTransfer patientTransfer = new PatientTransfer();
        patientTransfer.setTransferTo(getUser());
        patientTransfer.setPatientFhirId(Constants.STRING_ONE);
        patientTransfer.setTransferReason(String.valueOf(PatientTransferStatus.ACCEPTED));
        patientTransfer.setTransferStatus(PatientTransferStatus.ACCEPTED);
        patientTransfer.setTransferSite(getHealthFacility());
        patientTransfer.setOldSite(getHealthFacility());
        patientTransfer.setTransferBy(getUser());
        return patientTransfer;
    }

    /**
     * <p>
     * This method creates and returns a new {@link PatientTransferUpdateRequestDTO} object
     * with preset values for updating a patient transfer request. It sets the transfer ID and
     * the transfer status to predefined constants.
     * </p>
     *
     * @return {@link PatientTransferUpdateRequestDTO} The PatientTransferUpdateRequestDTO containing
     * the preset ID and transfer status for updating a patient transfer.
     */
    public static PatientTransferUpdateRequestDTO getPatientTransferUpdateRequestDTO() {
        PatientTransferUpdateRequestDTO patientTransferUpdateRequestDTO = new PatientTransferUpdateRequestDTO();
        patientTransferUpdateRequestDTO.setId(Constants.LONG_ONE);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.ACCEPTED);
        patientTransferUpdateRequestDTO.setProvenance(getProvenance());
        return patientTransferUpdateRequestDTO;
    }

    /**
     * <p>
     * This method creates and returns a new {@link PatientDTO} object with preset values
     * for a patient's details. It sets the patient ID, name, birth date, and phone number
     * using predefined constants and the current date for the birth date.
     * </p>
     *
     * @return {@link PatientDTO} The PatientDTO object containing initialized values for
     * patient ID, name, birth date, and phone number.
     */
    public static PatientDTO getPatientDTO() {
        PatientDTO patientDTO = new PatientDTO();
        patientDTO.setId(TestConstants.STRING_ONE);
        patientDTO.setName(TestConstants.NAME);
        patientDTO.setBirthDate(new Date());
        patientDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        return patientDTO;
    }

    public static CustomizedModule getCustomizedModule() {
        CustomizedModule customizedModule = new CustomizedModule();
        customizedModule.setId(Constants.LONG_ONE);
        customizedModule.setMemberId(Constants.STRING_ONE);
        customizedModule.setTenantId(Constants.LONG_ONE);
        customizedModule.setClinicalWorkflowId(Constants.LONG_ONE);
        customizedModule.setScreenType(Constants.TYPE);
        customizedModule.setPatientId(Constants.STRING_ONE);
        return customizedModule;
    }

    public static PatientRequestDTO getPatientRequestDTO() {
        PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
        patientRequestDTO.setName("John Doe");
        patientRequestDTO.setPhoneNumber("1234567890");
        patientRequestDTO.setId("exampleId");
        patientRequestDTO.setIdSystem("exampleIdSystem");
        patientRequestDTO.setIdCode("exampleIdCode");
        patientRequestDTO.setSiteId("exampleSiteId");
        patientRequestDTO.setStatus("active");
        patientRequestDTO.setVillageId("exampleVillageId");
        patientRequestDTO.setCount(10);
        patientRequestDTO.setVillageNames(Arrays.asList("Village 1", "Village 2"));
        patientRequestDTO.setSearchText("search text");
        patientRequestDTO.setSkip(0);
        patientRequestDTO.setLimit(10);
        patientRequestDTO.setVillageIds(Arrays.asList(1L, 2L));
        patientRequestDTO.setDistrictId(1L);
        patientRequestDTO.setReferencePatientId("exampleReferencePatientId");
        patientRequestDTO.setFilter(getPatientFilterDTO()); // create a PatientFilterDTO object and set its properties
        patientRequestDTO.setSort(getPatientSortDTO()); // create a PatientSortDTO object and set its properties
        patientRequestDTO.setType(Constants.SCREENED);
        patientRequestDTO.setCountryId(1L);
        patientRequestDTO.setTenantId(1L);
        patientRequestDTO.setMemberReference("exampleMemberReference");
        patientRequestDTO.setRemainingAttempts(Arrays.asList(1, 2, 3));
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_WEEKLY);
        patientRequestDTO.setCustomDate(Map.of(Constants.START_DATE, new Date(TestConstants.TEST_DATE),
                Constants.END_DATE, new Date(TestConstants.TEST_DATE)));
        patientRequestDTO.setCurrentSyncTime(new Date());
        patientRequestDTO.setLastSyncTime(new Date());
        patientRequestDTO.setReferredReasonsRequired(true);
        return patientRequestDTO;
    }

    public static PatientFilterDTO getPatientFilterDTO() {
        PatientFilterDTO patientFilter = new PatientFilterDTO();
        patientFilter.setPatientStatus(Arrays.asList("Active", "Inactive"));
        patientFilter.setVisitDate(Arrays.asList("2023-01-01", "2023-01-15"));
        patientFilter.setEnrollmentStatus("Enrolled");
        patientFilter.setMedicalReviewDate("2023-01-10");
        patientFilter.setAssessmentDate("2023-01-12");
        patientFilter.setIsRedRiskPatient(true);
        patientFilter.setCvdRiskLevel("High");
        patientFilter.setLabTestReferredOn("2023-01-05");
        patientFilter.setPrescriptionReferredOn("2023-01-07");
        return patientFilter;
    }

    public static PatientSortDTO getPatientSortDTO() {
        PatientSortDTO patientSort = new PatientSortDTO();
        patientSort.setIsRedRisk(true);
        patientSort.setIsLatestAssessment(false);
        patientSort.setIsMedicalReviewDueDate(true);
        patientSort.setIsHighLowBp(false);
        patientSort.setIsHighLowBg(true);
        patientSort.setIsAssessmentDueDate(false);
        patientSort.setIsScreeningDueDate(true);
        return patientSort;
    }

    public static PatientDetailsDTO getPatientDetailsDTO() {
        PatientDetailsDTO patientDetails = new PatientDetailsDTO();
        patientDetails.setId("1");
        patientDetails.setName("John Doe");
        patientDetails.setBirthDate(new Date(90, 0, 1));
        patientDetails.setPatientId("P12345");
        patientDetails.setNationalId("N123456789");
        patientDetails.setGender("Male");
        patientDetails.setVillage("Village A");
        patientDetails.setVillageId("V001");
        patientDetails.setLandmark("Near the river");
        patientDetails.setPhoneNumber("123-456-7890");
        patientDetails.setMemberId("M001");
        patientDetails.setChwName("Community Health Worker");
        patientDetails.setChildPatientId("C123");
        patientDetails.setChwPhoneNumber("098-765-4321");
        patientDetails.setHouseHoldId("H001");
        patientDetails.setRelationship("Son");
        patientDetails.setHouseHoldNumber(1L);
        patientDetails.setIsPregnant(false);
        patientDetails.setPhoneNumberCategory("Mobile");
        patientDetails.setPregnancyStatus("Not Applicable");
        patientDetails.setIsActive(true);
        patientDetails.setDateOfBirth(new Date(90, 0, 1));
        patientDetails.setAge(33);
        patientDetails.setHeight(175.0);
        patientDetails.setWeight(70.0);
        patientDetails.setBmi(22.9);
        patientDetails.setBmiCategory("Normal");
        patientDetails.setAvgSystolic(120.0);
        patientDetails.setAvgDiastolic(80.0);
        patientDetails.setAvgBloodPressure("120/80");
        patientDetails.setAvgPulse(72.0);
        patientDetails.setIsRegularSmoker(false);
        patientDetails.setIdentityType("National ID");
        patientDetails.setIdentityValue("N123456789");
        patientDetails.setFirstName("John");
        patientDetails.setMiddleName("A.");
        patientDetails.setLastName("Doe");
        patientDetails.setEnrollmentAt("Health Center A");
        patientDetails.setOccupation("Engineer");
        patientDetails.setPatientStatus("Active");
        patientDetails.setProgramId("Program A");
        patientDetails.setCreatedBy("Admin");
        patientDetails.setUpdatedBy("Admin");
        patientDetails.setCageAid("CAGE");
        patientDetails.setRiskMessage("Low Risk");
        patientDetails.setCvdRiskLevel("Low");
        patientDetails.setCvdRiskScoreDisplay("0-1");
        patientDetails.setCvdRiskScore("0");
        patientDetails.setPhq4RiskLevel("Low");
        patientDetails.setPhq4score("2");
        patientDetails.setPhq4FirstScore("1");
        patientDetails.setPhq4SecondScore("1");
        patientDetails.setGad7Score("0");
        patientDetails.setGad7RiskLevel("Low");
        patientDetails.setPhq9Score("0");
        patientDetails.setPhq9RiskLevel("Low");
        patientDetails.setSuicidalIdeation("No");
        patientDetails.setGlucoseUnit("mg/dL");
        patientDetails.setGlucoseValue(90.0);
        patientDetails.setGlucoseType("Random");
        patientDetails.setGlucoseDateTime(new Date());
        patientDetails.setDiabetesOtherSymptoms("None");
        patientDetails.setLastMealTime(new Date());
        patientDetails.setHba1c(5.5);
        patientDetails.setHba1cUnit("%");
        patientDetails.setHba1cDateTime(new Date());
        patientDetails.setIsPhq9(true);
        patientDetails.setRedRiskPatient(false);
        return patientDetails;
    }

    public static FollowUpDTO getFollowUpDTO() {
        FollowUpDTO followUpDTO = new FollowUpDTO("M001", AppointmentType.SCREENED);
        followUpDTO.setId(1L);
        followUpDTO.setHouseholdId("H001");
        followUpDTO.setPatientId("patientId1");
        followUpDTO.setEncounterId("E001");
        followUpDTO.setEncounterName("Routine Checkup");
        followUpDTO.setEncounterType("Screening");
        followUpDTO.setPatientStatus("Active");
        followUpDTO.setReason("Follow-up on health status");
        followUpDTO.setAttempts(3);
        followUpDTO.setVisits(2);
        followUpDTO.setSuccessfulAttempts(1);
        followUpDTO.setUnsuccessfulAttempts(2);
        followUpDTO.setCurrentPatientStatus("Under Observation");
        followUpDTO.setType(AppointmentType.SCREENED);
        followUpDTO.setNextVisitDate(new Date());
        followUpDTO.setEncounterDate(new Date());
        followUpDTO.setReferredSiteId("Site A");
        followUpDTO.setVillageId("V001");
        followUpDTO.setCalledAt(System.currentTimeMillis());
        followUpDTO.setProvenance(new ProvenanceDTO()); // Assuming you have a constructor or setters in ProvenanceDTO
        followUpDTO.setAppType("Mobile");
        followUpDTO.setName("John Doe");
        followUpDTO.setGender("Male");
        followUpDTO.setDateOfBirth(new Date(90, 0, 1)); // January 1, 1990
        followUpDTO.setAge(33); // Set age directly or calculate it
        followUpDTO.setPhoneNumber("123-456-7890");
        followUpDTO.setCountyName("County A");
        followUpDTO.setSubCountyName("SubCounty B");
        followUpDTO.setCommunityHealthUnitName("Health Unit A");
        followUpDTO.setVillageName("Village A");
        followUpDTO.setLandmark("Near the river");
        followUpDTO.setReferAssessment(true);
        followUpDTO.setCallCompleted(false);
        followUpDTO.setCallInitiated(true);
        followUpDTO.setReferredDateSince(System.currentTimeMillis());
        followUpDTO.setCreatedAt(System.currentTimeMillis());
        followUpDTO.setRetryAttempts(2);
        followUpDTO.setScreeningDateTime(new Date());
        followUpDTO.setAssessmentDate(new Date());
        followUpDTO.setNextMedicalReviewDate(new Date());
        followUpDTO.setNextBpAssessmentDate(new Date());
        followUpDTO.setNextBgAssessmentDate(new Date());
        followUpDTO.setOverDueCategories(Arrays.asList("Category 1", "Category 2"));
        followUpDTO.setDueDate(new Date());
        followUpDTO.setCallRegisterId(1001L);
        followUpDTO.setReferredReasons(Arrays.asList("Reason 1", "Reason 2"));
        followUpDTO.setIdentityType("National ID");
        followUpDTO.setIdentityValue("N123456789");
        followUpDTO.setActive(true);
        followUpDTO.setDeleted(false);
        followUpDTO.setUpdatedAt(System.currentTimeMillis());
        return followUpDTO;
    }

    public static Map<String, Object> getCallRegisterDetails() {
        FollowUpDTO followUpDTO = getFollowUpDTO();
        Map<String, Object> callRegister = new HashMap<>();
        callRegister.put("id", followUpDTO.getId());
        callRegister.put("patient_id", followUpDTO.getPatientId());
        callRegister.put("reason", followUpDTO.getReason());
        callRegister.put("attempts", followUpDTO.getAttempts());
        callRegister.put("visits", followUpDTO.getVisits());
        callRegister.put("is_completed", followUpDTO.getIsCompleted());
        callRegister.put("type", followUpDTO.getType());
        callRegister.put("is_wrong_number", followUpDTO.getIsWrongNumber());
        callRegister.put("referred_site_id", followUpDTO.getReferredSiteId());
        callRegister.put("village_id", followUpDTO.getVillageId());
        callRegister.put("called_at", followUpDTO.getCalledAt());
        callRegister.put("provenance", followUpDTO.getProvenance());
        callRegister.put("is_initiated", followUpDTO.getIsInitiated());
        callRegister.put("name", followUpDTO.getName());
        callRegister.put("gender", followUpDTO.getGender());
        callRegister.put("date_of_birth", followUpDTO.getDateOfBirth());
        callRegister.put("age", followUpDTO.getAge());
        callRegister.put("phone_number", followUpDTO.getPhoneNumber());
        callRegister.put("county_name", followUpDTO.getCountyName());
        callRegister.put("sub_county_name", followUpDTO.getSubCountyName());
        callRegister.put("community_health_unit_name", followUpDTO.getCommunityHealthUnitName());
        callRegister.put("village_name", followUpDTO.getVillageName());
        callRegister.put("landmark", followUpDTO.getLandmark());
        callRegister.put("referred_date_since", followUpDTO.getReferredDateSince());
        callRegister.put("created_at", followUpDTO.getCreatedAt());
        callRegister.put("retry_attempts", followUpDTO.getRetryAttempts());
        callRegister.put("due_date", followUpDTO.getDueDate());
        callRegister.put("call_register_id", followUpDTO.getCallRegisterId());
        callRegister.put("identity_type", followUpDTO.getIdentityType());
        callRegister.put("identity_value", followUpDTO.getIdentityValue());
        callRegister.put("is_active", followUpDTO.isActive());
        callRegister.put("is_deleted", followUpDTO.isDeleted());
        callRegister.put("updated_at", followUpDTO.getUpdatedAt());
        return callRegister;
    }

    public static PrescriptionPredictionDTO getPrescriptionPrediction() {
        PrescriptionPredictionDTO prescriptionPrediction = new PrescriptionPredictionDTO();
        List<PrescriptionDTO> prescriptionResults = new ArrayList<>();
        prescriptionResults.add(TestDataProvider.getPrescriptionDTO());  // Assuming getPrescriptionDTO() returns a single PrescriptionDTO
        prescriptionPrediction.setPrescriptionResults(prescriptionResults);

        List<GlucoseLogDTO> glucoseLogs = getGlucoseLogDTOS();
        prescriptionPrediction.setRecentBGLogs(glucoseLogs);
        return prescriptionPrediction;
    }

    private static List<GlucoseLogDTO> getGlucoseLogDTOS() {
        List<GlucoseLogDTO> glucoseLogs = new ArrayList<>();
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        glucoseLogDTO.setType("fbs");
        glucoseLogDTO.setGlucoseValue(5.6);
        glucoseLogDTO.setBioData(new BioDataDTO());
        glucoseLogs.add(glucoseLogDTO);
        return glucoseLogs;
    }

    public static LifestyleResponseDTO getPatientLifestyleDetails() {
        try {
            return new ObjectMapper().readValue(getPatientLifestyleDetailsJsonString(),
                    LifestyleResponseDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    private static String getPatientLifestyleDetailsJsonString() {
        return """
                {
                   "entity": [
                     {
                       "comments": null,
                       "lifestyle": "Have you ever consumed alcohol?",
                       "lifestyleType": "ALCOHOL",
                       "lifestyleAnswer": "No",
                       "value": "haveYouEverConsumedAlcohol?"
                     },
                     {
                       "comments": "Good",
                       "lifestyle": "How would you describe your diet and nutrition?",
                       "lifestyleType": "NUT",
                       "lifestyleAnswer": "Healthy",
                       "value": "howWouldYouDescribeYourDietAndNutrition?"
                     },
                     {
                       "comments": null,
                       "lifestyle": "About how many hours per week do you spend doing physical labor, walking, and/or exercising?",
                       "lifestyleType": "OTHER",
                       "lifestyleAnswer": ">15",
                       "value": "aboutHowManyHoursPerWeekDoYouSpendDoingPhysicalLabor,Walking,And/OrExercising?"
                     },
                     {
                       "comments": null,
                       "lifestyle": "Have you ever smoked cigarettes or used other tobacco products?",
                       "lifestyleType": "SMOKE",
                       "lifestyleAnswer": "No",
                       "value": "haveYouEverSmokedCigarettesOrUsedOtherTobaccoProducts?"
                     }
                   ],
                   "status": true,
                   "entityList": null,
                   "responseCode": 200,
                   "totalCount": null
                 }
                """;
    }

    public static EnrollmentRequestDTO getEnrollmentRequestDto() {
        try {
            return new ObjectMapper().readValue(getEnrollmentRequestDtoJsonString(),
                    EnrollmentRequestDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    private static String getEnrollmentRequestDtoJsonString() {
        return """
                {
                  "healthFacilityFhirId": "30",
                  "healthFacilityId": 1,
                  "provenance": {
                    "modifiedDate": "2024-08-19T13:53:33.444Z",
                    "organizationId": "53867",
                    "spiceUserId": 11,
                    "userId": "53866"
                  },
                  "tenantId": "225",
                  "bioMetrics": {
                    "gender": "male",
                    "isRegularSmoker": true,
                    "dateOfBirth": "1988-09-14T00:00:00+00:00"
                  },
                  "bioData": {
                    "firstName": "januu",
                    "lastName": "venu",
                    "country": {
                      "name": "Kenya",
                      "id": 2
                    },
                    "levelOfEducation": "Secondary School Completed",
                    "phoneNumber": "665528557",
                    "occupation": "Self Employed",
                    "district": {
                      "name": "Mombasa",
                      "id": 1
                    },
                    "chiefdom": {
                      "name": "Changamwe-3",
                      "id": 2
                    },
                     "village": {
                      "name": "Changamwe-3",
                      "id": 2
                    },
                    "identityValue": "Cg1",
                    "phoneNumberCategory": "Personal",
                    "landmark": "Gg"
                  },
                  "customizedWorkflows": [
                      {
                        "workflowType": "Enrollment",
                        "status": "Completed",
                        "details": {
                          "step": 1,
                          "description": "Initial Registration"
                        }
                      },
                      {
                        "workflowType": "Assessment",
                        "status": "Pending",
                        "details": {
                          "step": 2,
                          "description": "Medical Assessment"
                        }
                      }
                    ]
                }
                """;
    }

    public static AssessmentDTO getAssessmentDTO() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setReferralTicketType("Referred");
        assessmentDTO.setPhq4(getMentalHealthPHQ4());
        assessmentDTO.setPhq9(getMentalHealthPHQ9());
        assessmentDTO.setGad7(getMentalHealthGad7());

        assessmentDTO.setMemberReference("1234567");
        assessmentDTO.setPatientReference("12345");
        assessmentDTO.setPatientId("123");
        assessmentDTO.setVillageId("123");
        assessmentDTO.setNextBgAssessmentDate(new Date());
        assessmentDTO.setNextBpAssessmentDate(new Date());

        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        glucoseLogDTO.setGlucoseType("");
        glucoseLogDTO.setGlucoseValue(1D);
        assessmentDTO.setGlucoseLog(glucoseLogDTO);
        assessmentDTO.setNcdSymptoms(List.of(TestDataProvider.getSymptomDTO()));
        BpLogDTO bpLogDTO = new BpLogDTO();
        bpLogDTO.setAvgDiastolic(32D);
        bpLogDTO.setAvgDiastolic(77D);
        assessmentDTO.setBpLog(bpLogDTO);
        Symptom symptom = new Symptom();
        symptom.setId(1L);
        assessmentDTO.setSymptoms(List.of(symptom));
        return assessmentDTO;
    }
    public static MentalHealthDTO getMentalHealthPHQ4(){
        MentalHealthDTO mentalHealthPHQ4 = new MentalHealthDTO();
        mentalHealthPHQ4.setScore(7);
        return mentalHealthPHQ4;
    }
    public static MentalHealthDTO getMentalHealthPHQ9(){
        MentalHealthDTO mentalHealthPHQ9 = new MentalHealthDTO();
        mentalHealthPHQ9.setScore(12);
        return mentalHealthPHQ9;
    }

    public static MentalHealthDTO getMentalHealthGad7() {
        MentalHealthDTO mentalHealthGad7 = new MentalHealthDTO();
        mentalHealthGad7.setScore(7);
        return mentalHealthGad7;
    }
}


