package com.mdtlabs.coreplatform.fhirmapper.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ca.uhn.fhir.context.FhirContext;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CarePlan;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Timing;
import org.jetbrains.annotations.NotNull;
import org.mockito.MockedStatic;
import org.modelmapper.ModelMapper;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AncDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO.DataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ClinicalSummaryAndSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.Code;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ComplianceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.CoughDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiarrhoeaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FeverDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FrequencyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralDangerSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ICCMDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherAndNeonateSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NutritionalStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.OtherSymptomsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientFilterDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncChildDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncChildMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMotherDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMotherMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncNeonatalDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyAncDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RiskDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.SignsAndSymptomsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.SummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.SymptomDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.AnaemiaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.BreastfeedingProblemDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.EarProblemDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.GeneralDangerSigns;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.HIVInfectionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.HivRdtTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.JaundiceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.NonBreastfeedingProblemDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.VerySevereDiseaseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;

/**
 * This class used to set testData for unit Testcases
 *
 * @author Nandhakumar karthikeyan
 * @since Mar 4, 2024
 */
public class TestDataProvider {

    public static ModelMapper modelMapper = new ModelMapper();
    private static MockedStatic<CommonUtil> commonUtil;
    private static MockedStatic<UserContextHolder> userContextHolder;
    private static MockedStatic<UserSelectedTenantContextHolder> userSelectedTenantContextHolder;
    private static MockedStatic<UserTenantsContextHolder> userTenantsContextHolder;
    private static MockedStatic<DateUtil> dateUtil;

    /**
     * Static Mock data initiate
     */
    public static void init() {
        commonUtil = mockStatic(CommonUtil.class);
        userSelectedTenantContextHolder = mockStatic(UserSelectedTenantContextHolder.class);
        userContextHolder = mockStatic(UserContextHolder.class);
        userTenantsContextHolder = mockStatic(UserTenantsContextHolder.class);
    }

    public static void dateUtilInit() {
        dateUtil = mockStatic(DateUtil.class);
    }

    public static void cleanUp() {
        commonUtil.close();
        userSelectedTenantContextHolder.close();
        userTenantsContextHolder.close();
        userContextHolder.close();
    }

    public static UserDTO getUserDTO() {
        return modelMapper.map(getUser(), UserDTO.class);
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setFirstName(TestConstants.NAME);
        user.setLastName(TestConstants.NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordTime(new Date());
        user.setTenantId(TestConstants.ONE);
        user.setCountryCode(TestConstants.NAME);
        user.getRoles().add(getRole());
        user.setOrganizations(getSetOrganizations());
        user.getSuiteAccess().add(TestConstants.ADMIN);
        user.setTimezone(getTimezone());
        return user;
    }

    /**
     * Set Role Object For Test Data
     *
     * @return Role Object
     */
    public static Role getRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName(Constants.ROLE_SUPER_ADMIN);
        role.setLevel(1000L);
        role.setSuiteAccessName(TestConstants.ADMIN);
        return role;
    }

    /**
     * Set Organization Object For Test Data
     *
     * @return Organization Object
     */
    public static Set<Organization> getSetOrganizations() {
        Set<Organization> setOrganizations = new HashSet<>();
        setOrganizations.add(getOrganization());
        return setOrganizations;
    }

    /**
     * Set Organization Object For Test Data
     *
     * @return Organization Object
     */
    public static Organization getOrganization() {
        Organization organization = new Organization();
        organization.setId(TestConstants.ONE);
        organization.setFormName(TestConstants.NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(TestConstants.NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setActive(Boolean.TRUE);
        return organization;
    }

    /**
     * Set Timezone Object For Test Data
     *
     * @return Timezone Object
     */
    public static Timezone getTimezone() {
        Timezone timeZone = new Timezone();
        timeZone.setAbbreviation(TestConstants.NAME);
        timeZone.setDescription(TestConstants.NAME);
        timeZone.setOffset(TestConstants.NAME);
        return timeZone;
    }

    /**
     * Set Static Mock details
     */
    public static void getStaticMock() {
        Country country = new Country();
        country.setId(TestConstants.ONE);
        UserDTO userDTO = TestDataProvider.getUserDTO();
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);
        userContextDTO.setCountry(country);
        userContextDTO.setTimezone(getTimezone());
        userDTO.setId(TestConstants.ONE);
        userDTO.setIsSuperUser(Boolean.FALSE);
        userDTO.setTenantId(TestConstants.ONE);
        userDTO.setCountry(country);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.BEARER_TEST);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.ADMIN);
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
    }

    public static void getDateUtilStaticMock() {
        dateUtil.when(() -> DateUtil.getStartDayOfWeekByUserTimeZone(any())).thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.formatDate(anyString(), anyString())).thenReturn(new Date());
        dateUtil.when(() -> DateUtil.addDateWithTimezone(any(), eq(7), any()))
                .thenReturn(new Date());
        dateUtil.when(() -> DateUtil.convertDateToString(any())).thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(TestConstants.INT_ZERO), eq(TestConstants.BOOLEAN_FALSE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(TestConstants.INT_ZERO), eq(TestConstants.BOOLEAN_TRUE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(TestConstants.INT_ONE), eq(TestConstants.BOOLEAN_FALSE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(TestConstants.INT_ONE), eq(TestConstants.BOOLEAN_TRUE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(TestConstants.NEGATIVE_ONE), eq(TestConstants.BOOLEAN_TRUE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getUserTimezoneTime(any(), eq(TestConstants.NEGATIVE_ONE), eq(TestConstants.BOOLEAN_FALSE)))
                .thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getStartDayOfMonthByUserTimeZone(any())).thenReturn(TestConstants.TEST_DATE);
        dateUtil.when(() -> DateUtil.getISOString(any())).thenReturn(TestConstants.TEST_DATE);
    }

    public static void dateUtilCleanUp() {
        dateUtil.close();
    }


    /**
     * Set Household Object For Test Data
     *
     * @return HouseholdDTO Object
     */
    public static HouseholdDTO getHouseholdData() {
        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setHouseholdNo(TestConstants.FIVE);
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);
        householdDTO.setVillage(TestConstants.VILLAGE);
        householdDTO.setLandmark(TestConstants.LANDMARK);
        householdDTO.setNoOfPeople(TestConstants.TEN);
        householdDTO.setOwnedHandWashingFacilityWithSoap(true);
        householdDTO.setOwnedTreatedBedNet(true);
        householdDTO.setOwnedAnImprovedLatrine(false);
        householdDTO.setBedNetCount(TestConstants.TEN);
        householdDTO.setLatitude(13.347876786892543);
        householdDTO.setLongitude(17.803379331688296);
        householdDTO.setHouseholdMembers(List.of(getHouseHoldMember()));
        householdDTO.setHeadPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        return householdDTO;
    }

    public static AncDTO getAncData() {
        AncDTO ancDTO = new AncDTO();
        ancDTO.setLastMenstrualPeriod(new Date());
        ancDTO.setEstimatedDeliveryDate(new Date());
        ancDTO.setIsMalePartnerPresent(true);
        ancDTO.setSleepsUnderBedNet(true);
        ancDTO.setEatsMoreThanBefore(true);
        ancDTO.setTakesIronFloatTablets(true);
        ancDTO.setTakesFancidarTablets(true);
        ancDTO.setPriorityPregnancy(true);
        ancDTO.setMiscarriage(false);
        ancDTO.setBirthPlanMade(true);
        ancDTO.setPlaceOfDelivery(TestConstants.NAME);
        ancDTO.setAncSigns(Arrays.asList(TestConstants.NAME, TestConstants.NAME));
        ancDTO.setOtherSigns(TestConstants.STRING_VALUE);
        ancDTO.setGestationalAge(TestConstants.ONE_DOUBLE.intValue());
        ancDTO.setEats4GroupIronVitARichFoods(true);
        ancDTO.setNextVisitDate(new Date());
        ancDTO.setVisitNo(1);
        return ancDTO;
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentDTO() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setAssessmentType(com.mdtlabs.coreplatform.fhirmapper.common.Constants.ICCM);
        assessmentDTO.setEncounter(new EncounterDetailsDTO());
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        assessmentDTO.getEncounter().setLatitude(TestConstants.ONE_DOUBLE);
        assessmentDTO.getEncounter().setLongitude(TestConstants.ONE_DOUBLE);
        assessmentDTO.getEncounter().setProvenance(getProvenance());
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        ICCMDTO iccmdto = new ICCMDTO();
        iccmdto.setFever(getFeverDTO());
        iccmdto.setCough(getCoughDTO());
        iccmdto.setDiarrhoea(getDiarrhoeaDTO());
        iccmdto.setGeneralDangerSigns(new GeneralDangerSignsDTO());
        iccmdto.setNutritionalStatusDetails(new NutritionalStatusDTO());
        assessmentDTO.setSummary(new SummaryDTO());
        assessmentDetailsDTO.setIccm(iccmdto);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        return assessmentDTO;
    }

    /**
     * Set Screening log request Object For Test Data
     *
     * @return ScreeningLogRequestDTO Object
     */
    public static ScreeningLogRequestDTO getScreeningRequest() {
        ScreeningLogRequestDTO screeningLogRequestDTO = new ScreeningLogRequestDTO();
        screeningLogRequestDTO.setBioData(getBioData());
        screeningLogRequestDTO.setBioMetrics(getBioMetricsDTO());
        screeningLogRequestDTO.setSuicideScreener(getSuicideScreenerDetails());
        screeningLogRequestDTO.setSubstanceAbuse(getSubstanceAbuseDetails());
        screeningLogRequestDTO.setPhq4(getPhq4Request());
        screeningLogRequestDTO.setPregnancyAnc(getPregnancyAncRequest());
        screeningLogRequestDTO.setIsReferAssessment(Boolean.TRUE);
        screeningLogRequestDTO.setCvdRiskScore(TestConstants.ONE_DOUBLE);
        screeningLogRequestDTO.setCvdRiskLevel(TestConstants.TEXT);
        screeningLogRequestDTO.setCvdRiskScoreDisplay(TestConstants.TEXT);
        screeningLogRequestDTO.setLatitude(TestConstants.ONE_TWO_THREE);
        screeningLogRequestDTO.setLongitude(TestConstants.ONE_TWO_THREE);
        screeningLogRequestDTO.setBpLog(getBpLogRequest());
        screeningLogRequestDTO.setGlucoseLog(getGlucoseLogRequest());
        screeningLogRequestDTO.setCageAid(TestConstants.ONE_DOUBLE);
        screeningLogRequestDTO.setSuicidalIdeation(TestConstants.YES);
        screeningLogRequestDTO.setScreeningDateTime(new Date());
        screeningLogRequestDTO.setType(TestConstants.INPATIENT);
        return new ScreeningLogRequestDTO();
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static CodeableConcept getCodeableConceptTest() {
        CodeableConcept codeableConcept = new CodeableConcept();
       Coding coding = new Coding();
       coding.setSystem(TestConstants.SYSTEM);
       coding.setCode(TestConstants.CODE);
       coding.setDisplay(TestConstants.DISPLAY);
       codeableConcept.addCoding(coding);
       return codeableConcept;
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static GlucoseLogDTO  getDiabetesSymptoms() {
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        glucoseLogDTO.setHba1c(TestConstants.ONE.doubleValue());
        glucoseLogDTO.setGlucoseUnit(TestConstants.TEXT);
        glucoseLogDTO.setGlucoseType(TestConstants.TEXT);
        glucoseLogDTO.setGlucoseDateTime(new Date());
        glucoseLogDTO.setLastMealTime(new Date());
        glucoseLogDTO.setIsBeforeDiabetesDiagnosis(Boolean.TRUE);
        glucoseLogDTO.getDiabetes();
        return glucoseLogDTO;
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentDTOOther() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setAssessmentType(com.mdtlabs.coreplatform.fhirmapper.common.Constants.ICCM);
        assessmentDTO.setEncounter(new EncounterDetailsDTO());
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        assessmentDTO.getEncounter().setLatitude(TestConstants.ONE_DOUBLE);
        assessmentDTO.getEncounter().setLongitude(TestConstants.ONE_DOUBLE);
        assessmentDTO.getEncounter().setProvenance(getProvenance());
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        OtherSymptomsDTO otherSymptomsDTO = new OtherSymptomsDTO();
        SignsAndSymptomsDTO signsAndSymptomsDTO = new SignsAndSymptomsDTO();
        signsAndSymptomsDTO.setSymptoms(List.of(TestConstants.PATIENT_ID));
        signsAndSymptomsDTO.setOtherConcerningSymptoms(TestConstants.FHIR_SERVER_URL);
        otherSymptomsDTO.setSignsAndSymptoms(new SignsAndSymptomsDTO());
        assessmentDetailsDTO.setOtherSymptoms(otherSymptomsDTO);
        otherSymptomsDTO.setFever(getFeverDTO());
        assessmentDTO.setSummary(new SummaryDTO());
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        return assessmentDTO;
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentDTOAnc() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setAssessmentType(com.mdtlabs.coreplatform.fhirmapper.common.Constants.ANC);
        assessmentDTO.setEncounter(new EncounterDetailsDTO());
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        assessmentDTO.getEncounter().setProvenance(getProvenance());
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        AncDTO ancDTO = getAncData();
        assessmentDetailsDTO.setAnc(ancDTO);
        assessmentDTO.setSummary(new SummaryDTO());
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        return assessmentDTO;
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentDTOPnc() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setAssessmentType(com.mdtlabs.coreplatform.fhirmapper.common.Constants.ANC);
        assessmentDTO.setEncounter(new EncounterDetailsDTO());
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        assessmentDTO.getEncounter().setProvenance(getProvenance());
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        assessmentDetailsDTO.setPncMother(getPncMotherData());
        assessmentDetailsDTO.setPncNeonatal(getPncNeonatalData());
        assessmentDTO.setSummary(new SummaryDTO());
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        return assessmentDTO;
    }

    public static PncNeonatalDTO getPncNeonatalData() {
        PncNeonatalDTO pncNeonatalDTO = new PncNeonatalDTO();
        pncNeonatalDTO.setLowBirthWeight(false);
        pncNeonatalDTO.setDeathOfNewborn(false);
        pncNeonatalDTO.setNewbornReferredToSBCU(false);
        pncNeonatalDTO.setPncNeonatalSigns(Arrays.asList(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE));
        pncNeonatalDTO.setOtherSigns(TestConstants.STRING_VALUE);
        return pncNeonatalDTO;
    }

    /**
     * Set Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentDTOPncChild() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setAssessmentType(com.mdtlabs.coreplatform.fhirmapper.common.Constants.ANC);
        assessmentDTO.setEncounter(new EncounterDetailsDTO());
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        assessmentDTO.getEncounter().setProvenance(getProvenance());
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        assessmentDTO.setSummary(new SummaryDTO());
        assessmentDetailsDTO.setPncChild(getPncChildData());
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        return assessmentDTO;
    }

    public static PncChildDTO getPncChildData() {
        PncChildDTO pncChildDTO = new PncChildDTO();
        pncChildDTO.setExclusivelyBreastfeeding(true);
        pncChildDTO.setFatherPresent(true);
        pncChildDTO.setMuac(TestConstants.STRING_VALUE);
        pncChildDTO.setSleepsUnderBedNet(true);
        pncChildDTO.setTakingMinimumMealsPerDay(true);
        pncChildDTO.setFedFrom4FoodGroups(true);
        pncChildDTO.setMotherOrPartnerUsingFamilyPlanning(true);
        pncChildDTO.setDeathOfBaby(false);
        pncChildDTO.setPncChildSigns(Arrays.asList(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE));
        pncChildDTO.setOtherSigns(TestConstants.STRING_VALUE);
        return pncChildDTO;
    }

    public static PncMotherDTO getPncMotherData() {
        PncMotherDTO pncMotherDTO = new PncMotherDTO();
        pncMotherDTO.setPncMotherSigns(Arrays.asList(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE));
        pncMotherDTO.setOtherSigns(TestConstants.STRING_VALUE);
        pncMotherDTO.setDateOfDelivery(new Date());
        pncMotherDTO.setNoOfNeonates(TestConstants.ONE.intValue());
        pncMotherDTO.setFatherPresent(true);
        pncMotherDTO.setExclusivelyBreastfeeding(true);
        pncMotherDTO.setSleepsUnderBedNet(true);
        pncMotherDTO.setChlorhexidine(true);
        pncMotherDTO.setVisitNo(1);
        return pncMotherDTO;
    }

    /**
     * Set Provenance Object For Test Data
     *
     * @return ProvenanceDTO Object
     */
    public static ProvenanceDTO getProvenance() {
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setModifiedDate(new Date());
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);
        return provenanceDTO;
    }

    /**
     * Set Fever Object For Test Data
     *
     * @return FeverDTO Object
     */
    public static FeverDTO getFeverDTO() {
        FeverDTO feverDTO = new FeverDTO();
        feverDTO.setHasFever(Boolean.TRUE);
        feverDTO.setTemperature(Double.valueOf(TestConstants.ONE));
        feverDTO.setNoOfDaysOfFever(TestConstants.ONE);
        feverDTO.setRdtTest(String.valueOf(ResourceType.DiagnosticReport));
        feverDTO.setAct(com.mdtlabs.coreplatform.fhirmapper.common.Constants.DISPENSED);
        return feverDTO;
    }

    /**
     * Set Cough Object For Test Data
     *
     * @return CoughDTO Object
     */
    public static CoughDTO getCoughDTO() {
        CoughDTO coughDTO = new CoughDTO();
        coughDTO.setHasCough(Boolean.TRUE);
        coughDTO.setChestInDrawing(Boolean.TRUE);
        coughDTO.setNoOfDaysOfCough(TestConstants.ONE);
        coughDTO.setAmoxicillin(com.mdtlabs.coreplatform.fhirmapper.common.Constants.DISPENSED);
        coughDTO.setBreathPerMinute(TestConstants.ONE);
        return coughDTO;
    }

    /**
     * Set Diarrhoea Object For Test Data
     *
     * @return Diarrhoea Object
     */
    public static DiarrhoeaDTO getDiarrhoeaDTO() {
        DiarrhoeaDTO diarrhoeaDTO = new DiarrhoeaDTO();
        diarrhoeaDTO.setHasDiarrhoea(Boolean.TRUE);
        diarrhoeaDTO.setIsBloodyDiarrhoea(Boolean.TRUE);
        diarrhoeaDTO.setZincDispensedStatus(com.mdtlabs.coreplatform.fhirmapper.common.Constants.DISPENSED);
        diarrhoeaDTO.setNumberOfDaysDiarrhoea(TestConstants.ONE);
        diarrhoeaDTO.setDiarrhoeaSigns(List.of(TestConstants.COUNTRY));
        diarrhoeaDTO.setOrsDispensedStatus(com.mdtlabs.coreplatform.fhirmapper.common.Constants.DISPENSED);
        return diarrhoeaDTO;
    }

    /**
     * Set Household Member for TestData
     *
     * @return HouseholdMember data
     */
    public static HouseholdMemberDTO getHouseHoldMember() {
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setGender(TestConstants.MALE);
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setHouseholdHeadRelationship(TestConstants.NAME);
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setPatientId(TestConstants.PATIENT_ID);
        householdMemberDTO.setPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        householdMemberDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        householdMemberDTO.setVillage(TestConstants.VILLAGE);
        householdMemberDTO.setVillageId(TestConstants.VILLAGE_ID);
        householdMemberDTO.setHouseholdId(TestConstants.PATIENT_ID);
        return householdMemberDTO;
    }

    public static FhirResponseDTO getFhirResponse(String id, String type, Object obj) {
        FhirResponseDTO dto = new FhirResponseDTO();
        dto.setId(id);
        dto.setResourceType(type);
        dto.setEntry(List.of(obj));
        return dto;
    }

    public static UserRequestDTO getUserRequestDTO() {
        UserRequestDTO user = new UserRequestDTO();
        user.setId(TestConstants.ONE);
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setTenantId(TestConstants.ONE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.setTimezone(getTimezone());
        user.setGender(TestConstants.MALE);
        user.setFhirId(TestConstants.ONE_TWO_THREE);
        return user;
    }

    public static HealthFacilityRequestDTO getHealthFacilityRequestDTO() {
        HealthFacilityRequestDTO healthFacilityRequestDTO = new HealthFacilityRequestDTO();
        healthFacilityRequestDTO.setId(TestConstants.ONE);
        healthFacilityRequestDTO.setTenantId(TestConstants.ONE);
        healthFacilityRequestDTO.setFhirId(TestConstants.ONE_TWO_THREE);
        healthFacilityRequestDTO.setName(TestConstants.NAME);
        healthFacilityRequestDTO.setPhuFocalPersonNumber(TestConstants.PHONE_NUMBER);
        healthFacilityRequestDTO.setDistrict(new District(TestConstants.DISTRICT));
        healthFacilityRequestDTO.setCountry(new Country(TestConstants.COUNTRY));
        healthFacilityRequestDTO.setPostalCode(TestConstants.POSTAL_CODE);
        healthFacilityRequestDTO.setAddress(TestConstants.ADDRESS);
        healthFacilityRequestDTO.setUsers(List.of(getUserRequestDTO(), getUserRequestDTO()));
        return healthFacilityRequestDTO;
    }

    public static Group getGroup() {
        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setName(getHouseholdData().getName());
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(getHouseholdData().getNoOfPeople());
        group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                .setValue(getHouseholdData().getHouseholdNo().toString());
        group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(getHouseholdData().getVillageId());
        group.setMember(List.of(new Group.GroupMemberComponent().setEntity(new Reference().setReference("Device"))));

        Coding coding = new Coding(TestConstants.URL, TestConstants.YES_CODE, TestConstants.YES);
        Group.GroupCharacteristicComponent groupCharacteristicComponent = new Group.GroupCharacteristicComponent();
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(TestConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);
        groupCharacteristicComponent.setCode(codeableConcept);
        groupCharacteristicComponent.getValueCodeableConcept().addCoding(coding);
        groupCharacteristicComponent.setValue(new Reference());
        groupCharacteristicComponent.getValueReference().setReference("Location");
        group.getCharacteristic().add(groupCharacteristicComponent);

        groupCharacteristicComponent = new Group.GroupCharacteristicComponent();
        codeableConcept = new CodeableConcept();
        codeableConcept.setText(TestConstants.OWNED_AN_IMPROVED_LATRINE);
        groupCharacteristicComponent.setCode(codeableConcept);
        groupCharacteristicComponent.getValueCodeableConcept().addCoding(coding);
        group.getCharacteristic().add(groupCharacteristicComponent);

        groupCharacteristicComponent = new Group.GroupCharacteristicComponent();
        codeableConcept = new CodeableConcept();
        codeableConcept.setText(TestConstants.OWNED_TREATED_BED_NET);
        groupCharacteristicComponent.setCode(codeableConcept);
        groupCharacteristicComponent.getValueCodeableConcept().addCoding(coding);
        group.getCharacteristic().add(groupCharacteristicComponent);

        groupCharacteristicComponent = new Group.GroupCharacteristicComponent();
        codeableConcept = new CodeableConcept();
        codeableConcept.setText(TestConstants.BED_NET_COUNT);
        groupCharacteristicComponent.setCode(codeableConcept);
        groupCharacteristicComponent.setValue(new Quantity(TestConstants.TEN));
        group.getCharacteristic().add(groupCharacteristicComponent);
        return group;
    }

    public static Patient getPatient() {
        Patient patient = new Patient();
        HumanName humanName = new HumanName();
        humanName.setText(TestConstants.NAME);
        humanName.getGiven().add(new StringType(TestConstants.FIRST_NAME));
        humanName.getGiven().add(new StringType(TestConstants.LAST_NAME));
        patient.getName().add(humanName);
        patient.setId(TestConstants.TWO_STR);
        patient.setGender(Enumerations.AdministrativeGender.MALE);
        ContactPoint contactPoint = new ContactPoint();
        contactPoint.setValue(TestConstants.PHONE_NUMBER);
        patient.getTelecom().add(contactPoint);
        patient.setBirthDate(new Date());
        Identifier patientId = new Identifier();
        patientId.setSystem(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL);
        patientId.setValue(TestConstants.PATIENT_ID);
        patient.addIdentifier(patientId);
        Identifier villageId = new Identifier();
        villageId.setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL);
        villageId.setValue(TestConstants.VILLAGE_ID);
        patient.addIdentifier(villageId);
        Identifier nationalId = new Identifier();
        nationalId.setSystem(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID);
        nationalId.setValue(TestConstants.NATIONAL_ID);
        patient.addIdentifier(nationalId);
        Identifier virtualId = new Identifier();
        virtualId.setSystem(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL);
        virtualId.setValue(TestConstants.VIRTUAL_ID);
        patient.addIdentifier(virtualId);
        Identifier referralStatus = new Identifier();
        referralStatus.setSystem(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL);
        referralStatus.setValue(TestConstants.YES);
        patient.addIdentifier(referralStatus);
        Identifier patientStatus = new Identifier();
        patientStatus.setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
        patientStatus.setValue(TestConstants.ENROLLED);
        patient.addIdentifier(patientStatus);
        patient.setActive(Boolean.TRUE);
        Address address = new Address();
        address.setCity(TestConstants.VILLAGE);
        address.setText(TestConstants.VILLAGE);
        patient.getAddress().add(address);
        patient.addLink().setOther(new Reference(TestConstants.PATIENT_LINK));
        return patient;
    }


    public static RelatedPerson getRelatedPerson() {
        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setId(TestConstants.UNIQUE_ID);
        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        relatedPerson.setName(List.of(name));
        relatedPerson.setPatient(new Reference(TestConstants.PATIENT_REFERENCE));
        relatedPerson.setGender(Enumerations.AdministrativeGender.MALE);
        relatedPerson.setActive(true);
        relatedPerson.setBirthDate(new Date());
        ContactPoint contactPoint = new ContactPoint();
        contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
        contactPoint.setValue(TestConstants.PHONE_NUMBER);
        contactPoint.setUse(ContactPoint.ContactPointUse.HOME);
        relatedPerson.addTelecom(contactPoint);
        relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)
                .setValue(TestConstants.PATIENT_ID);
        relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(TestConstants.VILLAGE_ID);
        CodeableConcept codeableConceptRelation = new CodeableConcept();
        Coding codingRelation = new Coding();
        codingRelation.setSystem(FhirIdentifierConstants.RELATIONSHIP_SYSTEM_URL);
        codingRelation.setCode(TestConstants.HOUSEHOLD_HEAD_RELATIONSHIP);
        codeableConceptRelation.addCoding(codingRelation);
        codeableConceptRelation.setText(TestConstants.HOUSEHOLD_HEAD_RELATIONSHIP);
        relatedPerson.setRelationship(List.of(codeableConceptRelation));
        relatedPerson.setAddress(List.of(new Address().setCity(TestConstants.VILLAGE)));
        return relatedPerson;
    }

    public static Device getDevice() {
        Device device = new Device();
        device.addIdentifier().setSystem(com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants.PHONE_TYPE_SYSTEM_URL)
                .setValue(TestConstants.PHONE_NUMBER);
        return device;
    }

    public static Location getLocation() {
        Location location = new Location();
        Address address = new Address();
        address.setCity(TestConstants.CITY);
        address.setText(TestConstants.LANDMARK);
        Location.LocationPositionComponent positionComponent = new Location.LocationPositionComponent();
        positionComponent.setLatitude(12.09);
        positionComponent.setLongitude(12.09);
        location.setAddress(address);
        location.setPosition(positionComponent);
        return location;
    }

    public static UnderFiveIccmDTO getUnderFiveIccmData() {
        UnderFiveIccmDTO underFiveIccmDTO = new UnderFiveIccmDTO();

        underFiveIccmDTO.setId(TestConstants.ONE_TWO_THREE);
        underFiveIccmDTO.setClinicalSummaryAndSigns(getClinicalSummaryAndSignsData());
        underFiveIccmDTO.setClinicalNotes(TestConstants.STRING_VALUE);
        underFiveIccmDTO.setPresentingComplaints(TestConstants.STRING_VALUE);
        underFiveIccmDTO.setSystemicExamination(Arrays.asList(TestConstants.STRING_VALUE,TestConstants.STRING_VALUE));
        underFiveIccmDTO.setSystemicExaminationNotes(TestConstants.STRING_VALUE);
        underFiveIccmDTO.setPatientId(TestConstants.STRING_VALUE);
        underFiveIccmDTO.setEncounter(TestDataProvider.getEncounterDetailsData());
        underFiveIccmDTO.setAssessmentName(TestConstants.STRING_VALUE);
        underFiveIccmDTO.setEncounter(getEncounterDetailsData());

        // Setting basic values for examination
        UnderFiveIccmDTO.Examination examination = new UnderFiveIccmDTO.Examination();
        examination.setVerySevereDisease(getVerySevereDiseaseData());
        examination.setJaundice(getJaundiceData());
        examination.setDiarrhoea(getDiarrhoeaData());
        examination.setHivInfection(getHIVInfectionData());
        examination.setBreastfeedingProblem(getBreastfeedingProblemData());
        examination.setNonBreastfeedingProblem(getNonBreastfeedingProblemData());
        examination.setGeneralDangerSigns(getGeneralDangerSignsData());
        examination.setCough(getCoughData());
        examination.setFever(getFeverData());
        examination.setEarProblem(getEarProblemData());
        examination.setAnaemia(getAnaemiaData());
        examination.setHivRDT(getHivRdtTestData());
        underFiveIccmDTO.setExamination(examination);
        return underFiveIccmDTO;
    }

    public static EncounterDetailsDTO getEncounterDetailsData() {
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();

        encounterDetailsDTO.setId(TestConstants.STRING_VALUE);
        encounterDetailsDTO.setPatientReference(TestConstants.STRING_VALUE);
        encounterDetailsDTO.setReferred(true);
        encounterDetailsDTO.setPatientId(TestConstants.STRING_VALUE);
        encounterDetailsDTO.setPatientStatus(TestConstants.STRING_VALUE);
        encounterDetailsDTO.setMemberId(TestConstants.STRING_VALUE);
        encounterDetailsDTO.setStartTime(new Date());
        encounterDetailsDTO.setEndTime(new Date());
        encounterDetailsDTO.setHouseholdId(TestConstants.ONE_TWO_THREE);
        encounterDetailsDTO.setLatitude(12.345678);
        encounterDetailsDTO.setLongitude(98.765432);
        encounterDetailsDTO.setVisitNumber(1);
        encounterDetailsDTO.setProvenance(new ProvenanceDTO());
        // Assuming ProvenanceDTO is initialized elsewhere, if needed

        return encounterDetailsDTO;
    }


    public static com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.DiarrhoeaDTO getDiarrhoeaData() {
        com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.DiarrhoeaDTO diarrhoeaDTO = new com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.DiarrhoeaDTO();
        diarrhoeaDTO.setTimePeriod(TestConstants.ONE_TWO_THREE);
        diarrhoeaDTO.setBloodInStool(true);
        diarrhoeaDTO.setMovementOnStimulation(true);
        diarrhoeaDTO.setNoMovementOnStimulation(false);
        diarrhoeaDTO.setRestlessOrIrritable(true);
        diarrhoeaDTO.setSunkenEyes(false);
        diarrhoeaDTO.setSkinPinch(TestConstants.STRING_VALUE);
        diarrhoeaDTO.setHasDiarrhoea(true);
        diarrhoeaDTO.setBloodyDiarrhoea(false);
        List<String> signs = new ArrayList<>();
        signs.add(TestConstants.STRING_VALUE);
        signs.add(TestConstants.STRING_VALUE);
        // Add more signs as needed
        diarrhoeaDTO.setSigns(signs);
        return diarrhoeaDTO;
    }


    public static com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.FeverDTO getFeverData() {
        com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.FeverDTO feverDTO = new com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.FeverDTO();
        feverDTO.setHasFever(true);
        feverDTO.setNoOfDays(3L);
        feverDTO.setIsMotherHasFever(TestConstants.YES);
        feverDTO.setMicroscopyResult("Positive");
        List<String> signs = new ArrayList<>();
        signs.add(TestConstants.STRING_VALUE);
        signs.add(TestConstants.STRING_VALUE);
        // Add more signs as needed
        feverDTO.setSigns(signs);
        return feverDTO;
    }


    public static com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.CoughDTO getCoughData() {
        com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.CoughDTO coughDTO = new com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.CoughDTO();
        coughDTO.setCoughOrDIfficultBreathing(true);
        coughDTO.setNoOfDays(5L);
        coughDTO.setChestIndrawing(false);
        coughDTO.setStridor(true);
        return coughDTO;
    }


    public static HivRdtTestDTO getHivRdtTestData() {
        HivRdtTestDTO hivRdtTestDTO = new HivRdtTestDTO();
        hivRdtTestDTO.setMother("Negative");
        hivRdtTestDTO.setChild("Positive");
        return hivRdtTestDTO;
    }


    public static AnaemiaDTO getAnaemiaData() {
        AnaemiaDTO anaemiaDTO = new AnaemiaDTO();
        anaemiaDTO.setAppetiteTest(true);
        List<String> signs = new ArrayList<>();
        signs.add(TestConstants.STRING_VALUE);
        signs.add(TestConstants.STRING_VALUE);
        signs.add(TestConstants.STRING_VALUE);
        // Add more signs as needed
        anaemiaDTO.setSigns(signs);
        return anaemiaDTO;
    }

    private static ClinicalSummaryAndSignsDTO getClinicalSummaryAndSignsData() {
        ClinicalSummaryAndSignsDTO clinicalSummaryAndSigns = new ClinicalSummaryAndSignsDTO();
        clinicalSummaryAndSigns.setWeight(15.5);
        clinicalSummaryAndSigns.setHeight(100.0);
        clinicalSummaryAndSigns.setHeightUnit(TestConstants.STRING_VALUE);
        clinicalSummaryAndSigns.setWeightUnit(TestConstants.STRING_VALUE);
        clinicalSummaryAndSigns.setTemperature(37.0);
        clinicalSummaryAndSigns.setTemperatureUnit(TestConstants.STRING_VALUE);
        clinicalSummaryAndSigns.setWaz(-1.0);
        clinicalSummaryAndSigns.setWhz(-1.0);
        clinicalSummaryAndSigns.setImmunisationStatus(TestConstants.STRING_VALUE);
        clinicalSummaryAndSigns.setRespirationRate(Arrays.asList(20.0, 22.0));
        clinicalSummaryAndSigns.setVitAForMother(true);
        clinicalSummaryAndSigns.setAlbendazole(true);
        clinicalSummaryAndSigns.setBreastFeeding(true);
        clinicalSummaryAndSigns.setExclusiveBreastFeeding(true);
        clinicalSummaryAndSigns.setMuacStatus(TestConstants.STRING_VALUE);

        return clinicalSummaryAndSigns;
    }

    public static VerySevereDiseaseDTO getVerySevereDiseaseData() {
        VerySevereDiseaseDTO verySevereDiseaseDTO = new VerySevereDiseaseDTO();

        verySevereDiseaseDTO.setStoppedFeeding(true);
        verySevereDiseaseDTO.setConvulsions(false);
        verySevereDiseaseDTO.setSevereChestIndrawing(true);
        verySevereDiseaseDTO.setMovementOnStimulation(false);
        verySevereDiseaseDTO.setLowBodyTemperature(true);
        verySevereDiseaseDTO.setUmbilicusRedOrDrainingPus(false);
        verySevereDiseaseDTO.setSkinPustules(true);

        return verySevereDiseaseDTO;
    }

    public static JaundiceDTO getJaundiceData() {
        JaundiceDTO jaundiceDTO = new JaundiceDTO();
        jaundiceDTO.setYellowSkinLessThan24hrs(true);
        jaundiceDTO.setYellowPalmsAndSoles(false);
        jaundiceDTO.setJaundiceAppearing(true);
        jaundiceDTO.setNoJaundice(false);
        jaundiceDTO.setSolesNotYellow(true);
        return jaundiceDTO;
    }

    public static HIVInfectionDTO getHIVInfectionData() {
        HIVInfectionDTO hivInfectionDTO = new HIVInfectionDTO();
        hivInfectionDTO.setHasPositiveVirologicalTestForInfant(true);
        hivInfectionDTO.setIsMotherPostiveAndChildNegative(false);
        hivInfectionDTO.setHasPositiveAntibodyTestForInfant(true);
        hivInfectionDTO.setIsMotherPostiveAndInfantNotTested(false);
        hivInfectionDTO.setHasNegativeForMotherAndChild(false);
        return hivInfectionDTO;
    }

    public static BreastfeedingProblemDTO getBreastfeedingProblemData() {
        BreastfeedingProblemDTO breastfeedingProblemDTO = new BreastfeedingProblemDTO();
        breastfeedingProblemDTO.setAnyBreastfeedingDifficulty(true);
        breastfeedingProblemDTO.setLessThan8BreastfeedIn24hrs(false);
        breastfeedingProblemDTO.setSwitchingBreastFrequently(true);
        breastfeedingProblemDTO.setNotIncreasingBFInIllness(false);
        breastfeedingProblemDTO.setReceivesOtherFoodsOrDrinks(true);
        breastfeedingProblemDTO.setMouthUlcersOrThrush(false);
        breastfeedingProblemDTO.setUnderweight(true);
        breastfeedingProblemDTO.setPositioning(TestConstants.STRING_VALUE);
        breastfeedingProblemDTO.setAttachment(TestConstants.STRING_VALUE);
        breastfeedingProblemDTO.setSuckling(TestConstants.STRING_VALUE);
        breastfeedingProblemDTO.setNoFeedingProblem(TestConstants.ONE_TWO_THREE);
        return breastfeedingProblemDTO;
    }

    public static NonBreastfeedingProblemDTO getNonBreastfeedingProblemData() {
        NonBreastfeedingProblemDTO nonBreastfeedingProblemDTO = new NonBreastfeedingProblemDTO();
        nonBreastfeedingProblemDTO.setInappropriateReplacementFeeds(true);
        nonBreastfeedingProblemDTO.setInsufficientReplacementFeeds(false);
        nonBreastfeedingProblemDTO.setIncorrectlyPreparedMilk(true);
        nonBreastfeedingProblemDTO.setUseOfFeedingBottle(false);
        nonBreastfeedingProblemDTO.setFeedFormHIVPositiveMother(true);
        nonBreastfeedingProblemDTO.setBottleFeeding(false);
        nonBreastfeedingProblemDTO.setLowWeightForAge(true);
        nonBreastfeedingProblemDTO.setThrush(TestConstants.STRING_VALUE);
        return nonBreastfeedingProblemDTO;
    }

    public static GeneralDangerSigns getGeneralDangerSignsData() {
        GeneralDangerSigns generalDangerSigns = new GeneralDangerSigns();
        generalDangerSigns.setUnableToDrinkOrBreastfeed(true);
        generalDangerSigns.setVomitingEverything(false);
        generalDangerSigns.setHistoryOfConvulsion(true);
        generalDangerSigns.setConvulsingNow(false);
        generalDangerSigns.setLethargicOrUnconscious(true);
        return generalDangerSigns;
    }

    public static EarProblemDTO getEarProblemData() {
        EarProblemDTO earProblemDTO = new EarProblemDTO();
        earProblemDTO.setHasEarPain(true);
        earProblemDTO.setNoOfDays(5L);
        earProblemDTO.setEarDischarge(TestConstants.STRING_VALUE);
        return earProblemDTO;
    }

    public static PncMotherMedicalReviewDTO getPncMotherMedicalReviewData() {
        PncMotherMedicalReviewDTO pncMotherMedicalReviewDTO = new PncMotherMedicalReviewDTO();

        pncMotherMedicalReviewDTO.setId(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setVisitNumber(1);
        pncMotherMedicalReviewDTO.setEncounter(getEncounterDTO());
        pncMotherMedicalReviewDTO.getEncounter().setPatientReference(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setPatientStatus(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setIsMotherAlive(true);
        pncMotherMedicalReviewDTO.setBreastCondition(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setBreastConditionNotes(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setInvolutionsOfTheUterus(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setInvolutionsOfTheUterusNotes(TestConstants.STRING_VALUE);

        List<String> presentingComplaints = new ArrayList<>();
        presentingComplaints.add(TestConstants.STRING_VALUE);
        presentingComplaints.add(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setPresentingComplaints(presentingComplaints);
        pncMotherMedicalReviewDTO.setPresentingComplaintsNotes(TestConstants.STRING_VALUE);

        List<String> systemicExaminations = new ArrayList<>();
        systemicExaminations.add(TestConstants.STRING_VALUE);
        systemicExaminations.add(TestConstants.STRING_VALUE);
        pncMotherMedicalReviewDTO.setSystemicExaminations(systemicExaminations);
        pncMotherMedicalReviewDTO.setSystemicExaminationsNotes(TestConstants.STRING_VALUE);

        pncMotherMedicalReviewDTO.setClinicalNotes(TestConstants.STRING_VALUE);

        // Assuming getEncounterDetailsData() method exists to initialize EncounterDetailsDTO
        pncMotherMedicalReviewDTO.setEncounter(getEncounterDetailsData());

        return pncMotherMedicalReviewDTO;
    }

    public static PncMedicalReviewDTO getPncMedicatReviewDto() {
        PncMedicalReviewDTO pncMedicalReviewDTO = new PncMedicalReviewDTO();
        pncMedicalReviewDTO.setPncChild(getPncChildMedicalReviewData());
        pncMedicalReviewDTO.setPncMother(getPncMotherMedicalReviewData());
        return pncMedicalReviewDTO;
    }


    public static PncChildMedicalReviewDTO getPncChildMedicalReviewData() {
        PncChildMedicalReviewDTO pncChildMedicalReviewDTO = new PncChildMedicalReviewDTO();
        pncChildMedicalReviewDTO.setId(TestConstants.STRING_VALUE);
        pncChildMedicalReviewDTO.setVisitNumber(1);
        pncChildMedicalReviewDTO.getEncounter().setPatientReference(TestConstants.STRING_VALUE);
        pncChildMedicalReviewDTO.setIsChildAlive(true);
        pncChildMedicalReviewDTO.setPatientStatus(TestConstants.STRING_VALUE);

        List<String> presentingComplaints = new ArrayList<>();
        presentingComplaints.add(TestConstants.STRING_VALUE);
        presentingComplaints.add(TestConstants.STRING_VALUE);
        pncChildMedicalReviewDTO.setPresentingComplaints(presentingComplaints);
        pncChildMedicalReviewDTO.setPresentingComplaintsNotes(TestConstants.STRING_VALUE);

        List<String> physicalExaminations = new ArrayList<>();
        physicalExaminations.add(TestConstants.STRING_VALUE);
        physicalExaminations.add(TestConstants.STRING_VALUE);
        pncChildMedicalReviewDTO.setPhysicalExaminations(physicalExaminations);
        pncChildMedicalReviewDTO.setPhysicalExaminationNotes(TestConstants.STRING_VALUE);

        pncChildMedicalReviewDTO.setCongenitalDetect(TestConstants.STRING_VALUE);
        pncChildMedicalReviewDTO.setCordExamination(TestConstants.STRING_VALUE);
        pncChildMedicalReviewDTO.setBreastFeeding(true);
        pncChildMedicalReviewDTO.setExclusiveBreastFeeding(true);
        pncChildMedicalReviewDTO.setClinicalNotes(TestConstants.STRING_VALUE);

        // Assuming getEncounterDetailsData() method exists to initialize EncounterDetailsDTO
        pncChildMedicalReviewDTO.setEncounter(getEncounterDetailsData());

        return pncChildMedicalReviewDTO;
    }

    public static PrescriptionDTO getPrescriptionDTO() {
        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();
        prescriptionDTO.setPrescribedDays(7L);
        prescriptionDTO.setMedicationName(TestConstants.STRING_VALUE);
        prescriptionDTO.setMedicationId(TestConstants.STRING_VALUE);
        prescriptionDTO.setFrequency(3);
        prescriptionDTO.setPrescribedSince(new Date()); // current date
        prescriptionDTO.setPrescriptionId(TestConstants.ONE_STR);

        return prescriptionDTO;
    }

    public static ObservationDTO getObservationDTO() {
        ObservationDTO observationDTO = new ObservationDTO();
        observationDTO.setPatientReference(TestConstants.STRING_VALUE);
        observationDTO.setWeight(70.5);
        observationDTO.setType(TestConstants.STRING_VALUE);
        observationDTO.setSystolic(120.0);
        observationDTO.setNumberValue(123456);
        observationDTO.setDateValue(new Date());
        observationDTO.setPulse(75.0);
        observationDTO.setDiastolic(80.0);
        observationDTO.setEncounter(getEncounterDetailsData());

        return observationDTO;
    }

    public static MedicalReviewPregnancyDTO getMedicalReviewPregnancyDTO() {
        MedicalReviewPregnancyDTO dto = new MedicalReviewPregnancyDTO();
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setId(TestConstants.TWO_STR);
        dto.setId(TestConstants.STRING_VALUE);
        dto.setPresentingComplaints(Arrays.asList(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE));
        dto.setPresentingComplaintsNotes(TestConstants.STRING_VALUE);
        dto.setObstetricExaminations(Arrays.asList(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE));
        dto.setObstetricExaminationNotes(TestConstants.STRING_VALUE);
        dto.setClinicalNotes(TestConstants.STRING_VALUE);
        dto.setEncounter(encounterDetailsDTO);
        dto.getEncounter().setPatientReference(TestConstants.STRING_VALUE);
        dto.setPatientId(TestConstants.STRING_VALUE);
        dto.setVisitNumber(5);
        dto.setFundalHeight(28.0);
        dto.setFetalHeartRate(140.0);
        PregnancyAncDetailsDTO pregnancyDetails = new PregnancyAncDetailsDTO();
        pregnancyDetails.setLastMenstrualPeriod(new Date());
        pregnancyDetails.setHeight(160.0);
        pregnancyDetails.setPulse(70.0);
        pregnancyDetails.setEstimatedDeliveryDate(new Date());
        pregnancyDetails.setGestationalAge(20);
        pregnancyDetails.setNoOfFetus(TestConstants.ONE.intValue());
        pregnancyDetails.setGravida(TestConstants.ONE.intValue());
        pregnancyDetails.setParity(TestConstants.ONE.intValue());
        pregnancyDetails.setPatientBloodGroup(TestConstants.STRING_VALUE);
        pregnancyDetails.setBmi(24.0);
        pregnancyDetails.setSystolic(120.0);
        pregnancyDetails.setDiastolic(80.0);
        pregnancyDetails.setWeight(65.0);
        dto.setPregnancyDetails(pregnancyDetails);
        dto.setPregnancyHistory(Arrays.asList(TestConstants.STRING_VALUE));
        dto.setPregnancyHistoryNotes(TestConstants.STRING_VALUE);
        dto.setDeliveryKit(true);
        dto.setEncounter(getEncounterDetailsData());
        return dto;
    }

    public static PatientRequestDTO getPatientRequestDTO() {
        PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
        patientRequestDTO.setVillageIds(List.of("1L"));
        patientRequestDTO.setSkip(0);
        patientRequestDTO.setLimit(10);
        return patientRequestDTO;
    }
    
    public static EnrollmentRequestDTO getEnrollmentRequestDTO() {
        EnrollmentRequestDTO request = new EnrollmentRequestDTO();
        request.setBioData(BioDataDTO());
        request.setBioMetrics(getBioMetricsDTO());
        request.setHealthFacilityFhirId("54");
        request.setHealthFacilityId(1l);
        request.setQrCode("TISSM92701");
        request.setProvenance(getProvenance());
        request.setTenantId(4l);

        return request;
    } 

    public static BioDataDTO BioDataDTO() {
        BioDataDTO bioData = new BioDataDTO();
        DataDTO dataDTO = new DataDTO(); 
        dataDTO.setId(1L);
        dataDTO.setName("Kenya");
        bioData.setCountry(dataDTO);
        dataDTO = new DataDTO(); 
        dataDTO.setId(1L);
        dataDTO.setName("Nakuru");
        bioData.setDistrict(dataDTO);
        dataDTO = new DataDTO(); 
        dataDTO.setId(1L);
        dataDTO.setName("Molo");
        bioData.setChiefdom(dataDTO);
        dataDTO = new DataDTO(); 
        dataDTO.setId(1L);
        dataDTO.setName("Molo");
        bioData.setVillage(dataDTO);
        dataDTO = new DataDTO(); 
        dataDTO.setId(1L);
        dataDTO.setName("Afya Dumu");
        bioData.setProgram(dataDTO);
        bioData.setFirstName("William");
        bioData.setLastName("S");
        bioData.setMiddleName(null);
        bioData.setIdentityType(null);
        bioData.setIdentityValue("101201301");
        bioData.setInitial("y");
        bioData.setLandmark("Near Church");
        bioData.setOccupation("Employed");
        bioData.setInsuranceId("65sdf67");
        bioData.setInsuranceStatus(true);
        bioData.setInsuranceType("NHIF");
        bioData.setPhoneNumber("87654567");
        bioData.setPhoneNumberCategory("Family Memeber");
        bioData.setLevelOfEducation("No formal Schooling");
        return bioData;
    }

    public static BioMetricsDTO getBioMetricsDTO() {
        BioMetricsDTO bioMetrics = new BioMetricsDTO();
        bioMetrics.setDateOfBirth(new Date());
        bioMetrics.setGender(com.mdtlabs.coreplatform.fhirmapper.common.Constants.FEMALE);
        bioMetrics.setAge(TestConstants.ONE.intValue());
        bioMetrics.setHeight(TestConstants.ONE_DOUBLE);
        bioMetrics.setWeight(TestConstants.ONE_DOUBLE);
        bioMetrics.setBmiCategory(TestConstants.TEXT);
        bioMetrics.setBmi(TestConstants.ONE_DOUBLE);
        bioMetrics.setIsRegularSmoker(Boolean.TRUE);
        return bioMetrics;
    }

    public static BioDataDTO getBioData() {
        BioDataDTO bioDataDTO = new BioDataDTO();
        bioDataDTO.setIdentityValue(FhirConstants.NATIONAL_ID);
        bioDataDTO.setIdentityType(FhirConstants.NATIONAL_ID);
        bioDataDTO.setFirstName(TestConstants.TEXT);
        bioDataDTO.setLandmark(TestConstants.TEXT);
        bioDataDTO.setLastName(TestConstants.TEXT);
        bioDataDTO.setMiddleName(TestConstants.TEXT);
        bioDataDTO.setPhoneNumber(TestConstants.TEXT);
        bioDataDTO.setPhoneNumberCategory(TestConstants.TEXT);
        return bioDataDTO;
    }

    public static ScreeningLogRequestDTO getScreeningLogRequest() {
       ScreeningLogRequestDTO screeningLogRequestDTO = new ScreeningLogRequestDTO();
       screeningLogRequestDTO.setBioData(getBioData());
       screeningLogRequestDTO.setSiteId(TestConstants.ONE);
       screeningLogRequestDTO.setBioMetrics(getBioMetricsDTO());
       screeningLogRequestDTO.setBpLog(getBpLogRequest());
       screeningLogRequestDTO.setGlucoseLog(getGlucoseLogRequest());
       screeningLogRequestDTO.setPhq4(getPhq4Request());
       screeningLogRequestDTO.setSuicidalIdeation(TestConstants.YES);
       screeningLogRequestDTO.setSuicideScreener(getSuicideScreenerDetails());
       screeningLogRequestDTO.setSubstanceAbuse(getSubstanceAbuseDetails());
       screeningLogRequestDTO.setLongitude(TestConstants.TEXT);
       screeningLogRequestDTO.setLatitude(TestConstants.ONE_TWO_THREE);
       screeningLogRequestDTO.setType(TestConstants.TEXT);
       screeningLogRequestDTO.setCageAid(TestConstants.ONE_DOUBLE);
       screeningLogRequestDTO.setCvdRiskLevel(TestConstants.TEXT);
       screeningLogRequestDTO.setLatitude(TestConstants.LANDMARK);
       screeningLogRequestDTO.setCvdRiskScore(TestConstants.ONE);
       screeningLogRequestDTO.setCvdRiskScoreDisplay(TestConstants.ONE_TWO_THREE);
       screeningLogRequestDTO.setIsReferAssessment(Boolean.TRUE);
       screeningLogRequestDTO.setPregnancyAnc(getPregnancyAncRequest());
       screeningLogRequestDTO.setCategory(TestConstants.TEXT);
       screeningLogRequestDTO.setUserId(TestConstants.ONE);
       screeningLogRequestDTO.setReferredReasons(List.of(TestConstants.TEXT));
        screeningLogRequestDTO.setGeneralHealth(getHiv());
        screeningLogRequestDTO.setHivHistory(getHiv());
        screeningLogRequestDTO.setHivRiskBehaviours(getHiv());
        screeningLogRequestDTO.setStdScreening(getHiv());
        screeningLogRequestDTO.setTbSymptoms(getHiv());
        return  screeningLogRequestDTO;
    }

    /**
     * Retrieves a map of HIV-related key-value pairs.
     * This method is intended to provide predefined HIV-related information or configurations,
     * which can be used throughout the application for consistency and reference.
     *
     * @return a Map<String, String> containing HIV-related keys and their corresponding values.
     *         This map can include various identifiers, codes, or descriptions relevant to HIV.
     */
    public static Map<String, String> getHiv() {
        Map<String, String> hiv = new HashMap<>();
        hiv.put(MetaCodeConstants.FEELING_NERVOUS_ANXIOUS_OR_ON_EDGE, TestConstants.YES);
        return hiv;
    }

    public static RiskDetailsRequestDTO getRiskDetailsRequest() {
        RiskDetailsRequestDTO riskDetailsRequestDTO = new RiskDetailsRequestDTO();
        Map<String, String> riskDetails = new HashMap<>();

        riskDetails.put(TestConstants.TEXT, TestConstants.TWO_THREE_FOUR_FIVE);
        riskDetailsRequestDTO.setRiskDetails(riskDetails);
        return riskDetailsRequestDTO;
    }

    public static GlucoseLogDTO getGlucoseLogRequest() {
        try {
            return new ObjectMapper().readValue(getGlucoseLogJsonString(), GlucoseLogDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static BpLogDTO getBpLogRequest() {
        try {
            return new ObjectMapper().readValue(getBpLogJsonString(), BpLogDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static Map<String, String> getCvdRiskDetails() {
        Map<String, String> cvdRiskDetails = new HashMap<>();
         cvdRiskDetails.put(FhirConstants.CVD_RISK_SCORE,
                    TestConstants.TEXT);
         cvdRiskDetails.put(FhirConstants.CVD_RISK_LEVEL,
                 TestConstants.TEXT);
         cvdRiskDetails.put(FhirConstants.CVD_RISK_SCORE_DISPLAY,
                 TestConstants.TEXT);
         return cvdRiskDetails;
    }

    public static List<ComplianceDTO> getComplianceList() {
        try {
            return new ObjectMapper().readValue(getComplianceListJsonString(),
                    new TypeReference<List<ComplianceDTO>>() {});
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static List<SymptomDTO> getSymptomList() {
        try {
            return new ObjectMapper().readValue(getSymptomsJsonString(),
                    new TypeReference<List<SymptomDTO>>() {});
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static PregnancyDetailsDTO getPregnancyAncRequest() {
        try {
            return new ObjectMapper().readValue(getPregnancyAncJsonString(),
                    PregnancyDetailsDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static MentalHealthDTO getPhq4Request() {
        try {
            return new ObjectMapper().readValue(getPhq4JsonString(),
                    MentalHealthDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static Map<String, String> getSuicideScreenerDetails() {
        try {
            return new ObjectMapper().readValue(getSuicideScreenerJsonString(),
                    new TypeReference<Map<String, String>>() {});
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static Map<String, String> getSubstanceAbuseDetails() {
        try {
            return new ObjectMapper().readValue(getSusbtanceAbuseJsonString(),
                    new TypeReference<Map<String, String>>() {});
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static LabTestRequestDTO getLabTestRequestDTO() {
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        labTestRequestDTO.setRequestFrom(TestConstants.AFRICA);
        return labTestRequestDTO;
    }

    public static MetaCodeDetails getMetaCodeDetails() {
        try {
            return new ObjectMapper().readValue(getMetaCodeJsonString(),
                    MetaCodeDetails.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static EncounterDetailsDTO getEncounterDTO() {
        EncounterDetailsDTO encounterDetails = new EncounterDetailsDTO();
        encounterDetails.setId("12");
        encounterDetails.setPatientVisitId("visit123");
        encounterDetails.setPatientReference("Patient/456");
        encounterDetails.setType("Routine Checkup");
        encounterDetails.setReferred(false);
        encounterDetails.setPatientId("patient789");
        encounterDetails.setPatientStatus("Active");
        encounterDetails.setMemberId("member001");
        return encounterDetails;
    }

    public static PregnancyDetailsDTO getPregnancyDetailsDTO() {
        try {
            return new ObjectMapper().readValue(getPregnancyDetailsJsonString(),
                    PregnancyDetailsDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static String getPregnancyDetailsJsonString() {
        return """
                {
                    "neonatalOutcomes": "Healthy",
                    "maternalOutcomes": "Stable",
                    "diagnosis": [
                        {
                            "diagnosisCode": "D123",
                            "diagnosisDescription": "Anemia"
                        },
                        {
                            "diagnosisCode": "D456",
                            "diagnosisDescription": "Hypertension"
                        }
                    ]
                }
                """;
    }

    public static String getMetaCodeJsonString() {
        return """
                {
                    "id": 79,
                    "name": "bloodPressure",
                    "codes": [
                        {
                            "code": "85354-9",
                            "system": "http://loinc.org",
                            "display": "Blood pressure"
                        }
                    ],
                    "text": "Blood Pressure"
                }
                """;
    }

    public static Bundle getEncounterBundle() {
        Bundle bundle = new Bundle();
        bundle.setType(Bundle.BundleType.TRANSACTION);
        Encounter encounter = new Encounter();
        encounter.setId(TestConstants.TWO_STR);
        Reference reference = new Reference();
        reference.setId(TestConstants.ONE_STR);
        reference.setReference("Reference");
        encounter.setPartOf(reference);
        encounter.setStatus(Encounter.EncounterStatus.FINISHED);
        Period period = new Period();
        period.setStart(new Date());
        encounter.setPeriod(period);
        encounter.setClass_(new Coding()
                .setSystem("http://terminology.hl7.org/CodeSystem/v3-ActCode")
                .setCode("AMB")
                .setDisplay("Ambulatory"));
        Narrative narrative = new Narrative();
        narrative.setStatus(Narrative.NarrativeStatus.GENERATED);
        narrative.setDivAsString("<div xmlns=\"http://www.w3.org/1999/xhtml\">Encounter for a general health checkup.</div>");
        encounter.setText(narrative);
        Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
        encounterEntry.setResource(encounter);
        encounterEntry.getRequest()
                .setMethod(Bundle.HTTPVerb.POST)
                .setUrl("Encounter");
        bundle.addEntry(encounterEntry);
        return bundle;
    }

    public static List<PrescriptionDTO> getPrescriptionDTOList() {
        List<PrescriptionDTO> prescriptions = new ArrayList<>();
        PrescriptionDTO prescriptionParacetamol = new PrescriptionDTO();
        prescriptionParacetamol.setPrescriptionId("prescriptionid1");
        prescriptionParacetamol.setMedicationName("Paracetamol");
        prescriptionParacetamol.setFrequency(TestConstants.INT_ONE);
        prescriptionParacetamol.setPrescribedDays(TestConstants.FIVE);
        prescriptions.add(prescriptionParacetamol);
        PrescriptionDTO prescriptionAmoxicillin = new PrescriptionDTO();
        prescriptionAmoxicillin.setMedicationName("Amoxicillin");
        prescriptionAmoxicillin.setFrequency(TestConstants.INT_ONE);
        prescriptionAmoxicillin.setPrescribedDays(TestConstants.FIVE);
        prescriptionParacetamol.setPrescriptionId("prescriptionid2");
        prescriptions.add(prescriptionAmoxicillin);
        return prescriptions;
    }

    public static Bundle getMedicationBundle() {
        MedicationRequest medicationRequest = new MedicationRequest();
        MedicationRequest.MedicationRequestDispenseRequestComponent dispenseRequest = new MedicationRequest.MedicationRequestDispenseRequestComponent();
        Period period = new Period();
        period.setStart(new Date());
        period.setEnd(new Date());
        dispenseRequest.setValidityPeriod(period);
        medicationRequest.setDispenseRequest(dispenseRequest);
        medicationRequest.setId("medreq1");
        medicationRequest.setSubject(new Reference("Patient/123"));
        medicationRequest.setRequester(new Reference("Practitioner/456"));
        medicationRequest.setStatus(MedicationRequest.MedicationRequestStatus.CANCELLED);
        medicationRequest.setIntent(MedicationRequest.MedicationRequestIntent.ORDER);
        Identifier identifier = new Identifier();
        identifier.setSystem("nullprescription-status");
        identifier.setValue("Prescribed");
        medicationRequest.addIdentifier(identifier);
        Bundle bundle = new Bundle();
        bundle.setType(Bundle.BundleType.TRANSACTION);
        List<Bundle.BundleEntryComponent> bundleEntryComponents = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(medicationRequest);
        bundleEntryComponents.add(bundleEntryComponent);
        bundle.setEntry(bundleEntryComponents);
        return bundle;
    }

    public static List<MedicationDTO> getMedicationDTOList() {
        List<MedicationDTO> medicationDTOList = new ArrayList<>();
        MedicationDTO medicationParacetamol = new MedicationDTO();
        medicationParacetamol.setId(1L);
        medicationParacetamol.setName("Paracetamol");
        medicationParacetamol.setClassificationId(101L);
        medicationParacetamol.setDosageFormId(201L);
        medicationParacetamol.setBrandId(301L);
        medicationParacetamol.setClassificationName("Analgesic");
        medicationParacetamol.setBrandName("Tylenol");
        medicationParacetamol.setDosageFormName("Tablet");
        medicationParacetamol.setCountryId(1L);
        medicationParacetamol.setTenantId(1001L);
        medicationParacetamol.setCodeDetails(new Code("PARA500", "Active"));
        MedicationDTO medicationIbuprofen = new MedicationDTO();
        medicationIbuprofen.setId(2L);
        medicationIbuprofen.setName("Ibuprofen");
        medicationIbuprofen.setClassificationId(102L);
        medicationIbuprofen.setDosageFormId(202L);
        medicationIbuprofen.setBrandId(302L);
        medicationIbuprofen.setClassificationName("Anti-inflammatory");
        medicationIbuprofen.setBrandName("Advil");
        medicationIbuprofen.setDosageFormName("Capsule");
        medicationIbuprofen.setCountryId(1L);
        medicationIbuprofen.setTenantId(1001L);
        medicationIbuprofen.setCodeDetails(new Code("IBU200", "Active"));
        medicationDTOList.add(medicationParacetamol);
        medicationDTOList.add(medicationIbuprofen);
        return medicationDTOList;
    }

    public static Bundle getObservationBundle() {
        Bundle bundle = new Bundle();
        bundle.setType(Bundle.BundleType.TRANSACTION);
        Observation observation = new Observation();
        observation.setId(TestConstants.TWO_STR);
        List<Identifier> theIdentifier = new ArrayList<>();
        Identifier patientIdentifier = new Identifier();
        patientIdentifier.setSystem("http://hospital.org/patient-id")
                .setValue("12345");
        Identifier insuranceIdentifier = new Identifier();
        insuranceIdentifier.setSystem("http://national.org/insurance-id")
                .setValue("98765");
        theIdentifier.add(patientIdentifier);
        theIdentifier.add(insuranceIdentifier);
        observation.setIdentifier(theIdentifier);
        Narrative narrative = new Narrative();
        narrative.setStatus(Narrative.NarrativeStatus.GENERATED);
        narrative.setDivAsString("<div xmlns=\"http://www.w3.org/1999/xhtml\">Patient's heart rate observed to be 72 beats per minute.</div>");
        observation.setText(narrative);
        Bundle.BundleEntryComponent observationEntry = new Bundle.BundleEntryComponent();
        observationEntry.setResource(observation);
        observationEntry.getRequest().setMethod(Bundle.HTTPVerb.POST).setUrl("Observation");
        bundle.addEntry(observationEntry);
        return bundle;
    }

    public static Bundle getDiagnosticReportBundle() {
        Bundle bundle = new Bundle();
        bundle.setType(Bundle.BundleType.TRANSACTION);
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setId("diag-001");
        diagnosticReport.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
        diagnosticReport.setIssued(new Date());
        diagnosticReport.setSubject(new Reference("Patient/pat-001"));
        Identifier identifier = new Identifier();
        identifier.setSystem("http://example-hospital.org/labtests");
        identifier.setValue("Complete Blood Count");
        diagnosticReport.addIdentifier(identifier);
        Reference reference = new Reference();
        reference.setId(TestConstants.ONE_STR);
        reference.setReference(TestConstants.ONE_STR);
        List<Reference> references = new ArrayList<>();
        references.add(reference);
        diagnosticReport.setResultsInterpreter(references);
        diagnosticReport.setResult(references);
        diagnosticReport.setPerformer(references);
        diagnosticReport.addPerformer(reference);
        diagnosticReport.addResultsInterpreter(reference);
        diagnosticReport.setConclusion("Normal results observed");
        diagnosticReport.setEffective(new DateTimeType(new Date()));
        bundle.addEntry().setResource(diagnosticReport);
        return bundle;
    }

    public static Bundle getPractitionerBundle() {
        Practitioner practitioner = new Practitioner();
        List<HumanName> theName = new ArrayList<>();
        HumanName humanName = new HumanName();
        humanName.setText("value");
        theName.add(humanName);
        practitioner.setName(theName);
        practitioner.setId(TestConstants.STRING_THREE);
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(practitioner);
        return bundle;
    }

    public static Condition getCondition() {
        Condition condition = new Condition();
        condition.setId("condition123");
        CodeableConcept clinicalStatus = new CodeableConcept();
        clinicalStatus.setText("Active");
        condition.setClinicalStatus(clinicalStatus);
        CodeableConcept verificationStatus = new CodeableConcept();
        verificationStatus.setText("Confirmed");
        condition.setVerificationStatus(verificationStatus);
        CodeableConcept category = new CodeableConcept();
        category.setText("Problem List Item");
        condition.addCategory(category);
        CodeableConcept code = new CodeableConcept();
        code.setText("Hypertension");
        condition.setCode(code);
        Reference subject = new Reference();
        subject.setReference("Patient/patient123");
        condition.setSubject(subject);
        Reference encounter = new Reference();
        encounter.setReference("Encounter/encounter123");
        condition.setEncounter(encounter);
        condition.setRecordedDate(new Date());
        Identifier identifier = new Identifier();
        identifier.setSystem("http://example.org/fhir/identifier");
        identifier.setValue("cond123");
        condition.addIdentifier(identifier);
        CodeableConcept evidenceCode = new CodeableConcept();
        evidenceCode.setText("Blood Pressure");
        Condition.ConditionEvidenceComponent evidence = new Condition.ConditionEvidenceComponent();
        evidence.addCode(evidenceCode);
        condition.addEvidence(evidence);
        condition.addNote().setText("Patient reports occasional headaches related to hypertension.");
        return condition;
    }

    public static FrequencyDTO getFrequencyDTO() {
        FrequencyDTO frequencyDTO = new FrequencyDTO();
        frequencyDTO.setName("Daily");
        frequencyDTO.setType("Fixed");
        frequencyDTO.setDuration(24);
        frequencyDTO.setPeriod("h");
        frequencyDTO.setRiskLevel("Low");
        frequencyDTO.setTitle("Daily Routine");
        frequencyDTO.setDisplayOrder(1);
        return frequencyDTO;
    }

    public static Bundle getCarePlanBundle() {
        CarePlan carePlan = new CarePlan();
                List<CarePlan.CarePlanActivityComponent> activities = new ArrayList<>();
        CarePlan.CarePlanActivityComponent assessmentActivity = new CarePlan.CarePlanActivityComponent();
        assessmentActivity.setDetail(new CarePlan.CarePlanActivityDetailComponent().setDescription("Take medication twice a day")
                .setStatus(CarePlan.CarePlanActivityStatus.INPROGRESS).setCode(getCodeableConceptTest().setText(TestConstants.ASSESSMENT))
                .setScheduled(getTiming()));
        CarePlan.CarePlanActivityComponent bpCheckActivity = new CarePlan.CarePlanActivityComponent();
        bpCheckActivity.setDetail(new CarePlan.CarePlanActivityDetailComponent().setDescription("Attend physiotherapy sessions")
                .setStatus(CarePlan.CarePlanActivityStatus.NOTSTARTED).setCode(getCodeableConceptTest().setText("BP Check"))
                .setScheduled(getTiming()));
        activities.add(assessmentActivity);
        activities.add(bpCheckActivity);
        carePlan.setActivity(activities);
        carePlan.setId("careplan-example");
        carePlan.setTitle("Example Care Plan");
        carePlan.setStatus(CarePlan.CarePlanStatus.ACTIVE);
        carePlan.setIntent(CarePlan.CarePlanIntent.PLAN);
        Bundle bundle = new Bundle();
        bundle.setType(Bundle.BundleType.TRANSACTION);
        Bundle.BundleEntryComponent entry = new Bundle.BundleEntryComponent();
        entry.setResource(carePlan);
        bundle.addEntry(entry);
        return bundle;
    }

    @NotNull
    private static Timing getTiming() {
        Timing timing = new Timing();
        Timing.TimingRepeatComponent repeatComponent = new Timing.TimingRepeatComponent();
        repeatComponent.setFrequency(2);
        repeatComponent.setDuration(1);
        repeatComponent.setDurationUnit(Timing.UnitsOfTime.D);
        timing.setRepeat(repeatComponent);
        return timing;
    }

    public static List<Observation.ObservationComponentComponent> getObservstionComponentList() {
        List<Observation.ObservationComponentComponent> theComponent = new ArrayList<>();
        Observation.ObservationComponentComponent maternalOutcomes = new Observation.ObservationComponentComponent();
        maternalOutcomes.getCode().setText("maternalOutcomes");
        theComponent.add(maternalOutcomes);
        Observation.ObservationComponentComponent neonatalOutcomes = new Observation.ObservationComponentComponent();
        neonatalOutcomes.getCode().setText("neonatalOutcomes");
        theComponent.add(neonatalOutcomes);
        Observation.ObservationComponentComponent actualDeliveryDate = new Observation.ObservationComponentComponent();
        actualDeliveryDate.getCode().setText("actualDeliveryDate");
        theComponent.add(actualDeliveryDate);
        Observation.ObservationComponentComponent gestationalAge = new Observation.ObservationComponentComponent();
        gestationalAge.getCode().setText("gestationalAge");
        theComponent.add(gestationalAge);
        Observation.ObservationComponentComponent lastMenstrualPeriodComponent = new Observation.ObservationComponentComponent();
        lastMenstrualPeriodComponent.getCode().setText("lastMenstrualPeriodDate");
        theComponent.add(lastMenstrualPeriodComponent);
        Observation.ObservationComponentComponent estimatedDeliveryComponent = new Observation.ObservationComponentComponent();
        estimatedDeliveryComponent.getCode().setText("estimatedDeliveryDate");
        theComponent.add(estimatedDeliveryComponent);
        Observation.ObservationComponentComponent gravidaComponent = new Observation.ObservationComponentComponent();
        gravidaComponent.getCode().setText("gravida");
        theComponent.add(gravidaComponent);
        Observation.ObservationComponentComponent parityComponent = new Observation.ObservationComponentComponent();
        parityComponent.getCode().setText("parity");
        theComponent.add(parityComponent);
        Observation.ObservationComponentComponent noOfFetusComponent = new Observation.ObservationComponentComponent();
        noOfFetusComponent.getCode().setText("noOfFetus");
        theComponent.add(noOfFetusComponent);
        Observation.ObservationComponentComponent isOnTreatmentComponent = new Observation.ObservationComponentComponent();
        isOnTreatmentComponent.getCode().setText("isOnTreatment");
        theComponent.add(isOnTreatmentComponent);
        Observation.ObservationComponentComponent isPregnantComponent = new Observation.ObservationComponentComponent();
        isPregnantComponent.getCode().setText("isPregnant");
        theComponent.add(isPregnantComponent);
        Observation.ObservationComponentComponent isPregnancyRiskComponent = new Observation.ObservationComponentComponent();
        isPregnancyRiskComponent.getCode().setText("isPregnancyRisk");
        theComponent.add(isPregnancyRiskComponent);
        Observation.ObservationComponentComponent cvdRiskLevel = new Observation.ObservationComponentComponent();
        cvdRiskLevel.getCode().setText("cvdRiskLevel");
        theComponent.add(cvdRiskLevel);
        Observation.ObservationComponentComponent riskLevel = new Observation.ObservationComponentComponent();
        riskLevel.getCode().setText("riskLevel");
        theComponent.add(riskLevel);
        Observation.ObservationComponentComponent averageSystolicBloodPressure = new Observation.ObservationComponentComponent();
        averageSystolicBloodPressure.getCode().setText("averageSystolicBloodPressure");
        theComponent.add(averageSystolicBloodPressure);
        Observation.ObservationComponentComponent averageDiastolicBloodPressure = new Observation.ObservationComponentComponent();
        averageDiastolicBloodPressure.getCode().setText("averageDiastolicBloodPressure");
        theComponent.add(averageDiastolicBloodPressure);
        Observation.ObservationComponentComponent hba1c = new Observation.ObservationComponentComponent();
        hba1c.getCode().setText("hba1c");
        theComponent.add(hba1c);
        return theComponent;
    }

    public static String getBpLogJsonString() {
        return """
                {
                "avgSystolic": 199.0,
                "isBeforeHtnDiagnosis": true,
                "avgDiastolic": 99.0,
                "avgBloodPressure": "199/99",
                "bpLogDetails": [
                    {
                        "systolic": "199",
                        "diastolic": "99",
                        "pulse": "55"
                    },
                    {
                        "systolic": "199",
                        "diastolic": "99",
                        "pulse": "55"
                    }
                ]
            }
        """;
    }

    public static String getComplianceListJsonString() {
      return """
              [
              {
                 "id": 1,
                 "name": "Ran out of medication"
              },
               {
                 "id": 2,
                 "name": "Medication caused side effects"
              },
               {
                 "id": 3,
                 "name": "Started taking medication but stopped"
              },
               {
                 "id": 4,
                 "name": "Took all medication"
              },
               {
                 "id": 5,
                 "name": "Could not afford/pay for medication"
              },
              {  "id": 6,
                 "name": "Not prescribed any medication"
              },
              {
                 "id": 7,
                 "name": "Missed a lot of medication"
              },
              {
                 "id": 8,
                 "name": "Didn’t take any medication"
              },
              {
                 "id": 9,
                 "name": "Other"
              }
              ]
              """;
    }

    public static String getGlucoseLogJsonString() {
        return """ 
        {
          "glucoseUnit": "mmol/L",
          "glucoseDateTime": "2024-08-14T11:21:01+05:30",
          "glucoseType": "fbs",
          "diabetes": [
           {
             "name": "Frequent urination",
             "id": 23.0
            },
            {
              "name": "Excessive thirst",
              "id": 24.0
            },
           {
              "name": "Significant hunger",
              "id": 36.0
           },
           {
             "name": "Slow healing or infected foot wound",
             "id": 32.0
           },
           {
             "name": "Foot numbness, tingling, and/ or pain",
              "id": 34.0
           },
           {
            "name": "Significant fatigue or weakness",
            "id": 26.0
           },
           {
            "name": "Unplanned weight loss",
            "id": 26.0
           },
           {
            "name": "No symptoms",
            "id": 26.0
           },
           {
            "name": "Other",
            "id": 26.0
           }
       ],
       "glucoseValue": 25.0,
       "hba1c": 23.0,
       "hba1cUnit": "%",
       "isBeforeDiabetesDiagnosis": true,
       "lastMealTime": "2024-08-07T08:30:00+05:30"
      }""";
    }

    public static String getPregnancyAncJsonString() {
        return """
                   {
                        "provenance" : {
                           "userId" : "30",
                           "spiceUserId" : "20",
                           "organizationId" : "106",
                           "modifiedDate" : "2024-08-07T08:30:00+05:30"
                        },
                        "pregnancySymptoms": [
                            {
                                "name": "Vaginal bleeding",
                                "id": 1.0
                            },
                            {
                                "name": "Convulsions",
                                "id": 2.0
                            },
                            {
                                "name": "Headache",
                                "id": 3.0
                            },
                            {
                                "name": "Blurred vision/difficulty seeing clearly",
                                "id": 4.0
                            },
                            {
                                "name": "Reduced/absent baby movements",
                                "id": 5.0
                            },
                            {
                                "name": "Fatigue/feeling tired/feeling ill/weak",
                                "id": 6.0
                            },
                            {
                                "name": "Difficulty breathing/fast breathing/cough/chest pain",
                                "id": 7.0
                            },
                            {
                                "name": "Breaking of water",
                                "id": 8.0
                            },
                            {
                                "name": "Pain in the abdomen",
                                "id": 9.0
                            },
                            {
                                "name": "Fever/hotness of body",
                                "id": 10.0
                            },
                            {
                                "name": "Swelling of the legs/hands/face",
                                "id": 11.0
                            },
                            {
                                "name": "Painful/burning feeling when passing urine",
                                "id": 12.0
                            },
                            {
                                "name": "Vaginal discharge/itchiness",
                                "id": 13.0
                            },
                            {
                                "name": "Other",
                                "id": 15.0
                            }
                        ],
                        "gestationalAge": "8",
                        "attendedAncClinic": true,
                        "pregnancyOtherSymptoms": "Vomiting",
                        "isIronFolateProvided": true,
                        "isInterestedToEnroll": true,
                        "isMosquitoNetProvided": true,
                        "lastMenstrualPeriod": "2024-06-13",
                        "isIptDrugProvided": true,
                        "isPregnant": true
                   }
                """;
    }

    public static String getPhq4JsonString() {
        return """
                {
                        "phq4Score": 2.0,
                        "phq4MentalHealth": [
                            {
                                "answerId": 325.0,
                                "score": 1.0,
                                "questionId": 1.0,
                                "answer": "Several days",
                                "question": "Feeling down, depressed or hopeless?",
                                "displayOrder": 4.0
                            },
                            {
                                "answerId": 1.0,
                                "score": 0.0,
                                "questionId": 114.0,
                                "answer": "Not at all",
                                "question": "Feeling nervous, anxious or on edge?",
                                "displayOrder": 1.0
                            },
                            {
                                "answerId": 4.0,
                                "score": 0.0,
                                "questionId": 35.0,
                                "answer": "Not at all",
                                "question": "Little interest or pleasure in doing things?",
                                "displayOrder": 3.0
                            },
                            {
                                "answerId": 327.0,
                                "score": 1.0,
                                "questionId": 111.0,
                                "answer": "Several days",
                                "question": "Not being able to stop or control worrying?",
                                "displayOrder": 2.0
                            }
                        ],
                        "phq4RiskLevel": "Normal"
                    }
                """;
    }

    public static String getSymptomsJsonString() {
        return """
                [
           {
             "name": "Frequent urination",
             "id": 23.0
            },
            {
             "name": "Fainting",
             "id": 23.0
            },
            {
              "name": "Excessive thirst",
              "id": 24.0
            },
            {
              "name": "Seizures",
              "id": 36.0
           },
           {
              "name": "Blurry vision",
              "id": 36.0
           },
           {
             "name": "Sudden vision loss",
             "id": 32.0
           },
           {
             "name": "Numbness on side of body/limb",
              "id": 34.0
           },
           {
            "name": "Swelling of limbs",
            "id": 26.0
           },
           {
            "name": "Dizziness upon standing up",
            "id": 26.0
           },
           {
            "name": "Chest pain or discomfort",
            "id": 26.0
           },
           {
            "name": "Swelling of tongue or lips",
            "id": 26.0
           }
           ,
           {
            "name": "Palpitations or irregular heart beats",
            "id": 26.0
           }
           ,
           {
            "name": "Shortness of breath with usual activities",
            "id": 26.0
           }
           ,
           {
            "name": "Excessive thirst",
            "id": 26.0
           },
            {
              "name": "Significant hunger",
              "id": 36.0
           },
           {
             "name": "Slow healing or infected foot wound",
             "id": 32.0
           },
           {
             "name": "Foot numbness, tingling, and/ or pain",
              "id": 34.0
           },
           {
            "name": "Significant fatigue or weakness",
            "id": 26.0
           },
           {
            "name": "Unplanned weight loss",
            "id": 26.0
           },
           {
            "name": "No symptoms",
            "id": 26.0
           },
           {
            "name": "Other",
            "id": 26.0
           }
       ]
        """;
    }

    public static String getSuicideScreenerJsonString() {
        return """
                {
                   "If Yes, have you ever attempted or had a plan to go ahead with it?": "Yes",
                   "Have you thought of/contemplated ending your life or not worth living?": "Yes"
                }
               """;
    }

    public static String getSusbtanceAbuseJsonString() {
        return """
                {
                     "Do you take alcohol or any other substance?": "Yes",
                     "Have you ever felt you ought to cut down on your drinking or drug use?": "Yes",
                     "Have you felt bad or guilty about your drinking or drug use?": "Yes",
                     "Have people annoyed you by criticizing your drinking or drug use?": "Yes",
                     "Have you ever had a drink or used drugs first thing in the morning to steady your nerves or to get rid of a hangover (eye-opener)?": "Yes"
                }
                """;
    }

    public static SearchPersonDetailsDTO getPersonDetailsDTO() {
        SearchPersonDetailsDTO personDetailsDTO = new SearchPersonDetailsDTO();
        personDetailsDTO.setPatient(getPatient());
        personDetailsDTO.setRelatedPerson(getFhirRelatedPerson());
        personDetailsDTO.setMentalHealthObservation(getMentalHealthObservation());
        personDetailsDTO.setSubstanceAbuseObservation(getSubstanceAbuseObservation());
        personDetailsDTO.setSuicideScreenerObservation(getSuicideScreenerObservation());
        personDetailsDTO.setRegularSmokerObservation(getRegularSmokerObservation());
        personDetailsDTO.setBmiObservation(getBmiObservation());
        personDetailsDTO.setHeightObservation(getHeightObservation());
        personDetailsDTO.setWeightObservation(getWeightObservation());
        personDetailsDTO.setBgObservation(getBloodGlucoseObservation());
        personDetailsDTO.setBpObservation(getBloodPressureObservation());
        personDetailsDTO.setPregnancyObservation(getPregnancyAncObservation());
        personDetailsDTO.setPhq4(getQuestionnaireResponse());
        return personDetailsDTO;
    }


    public static MentalHealthDTO getMentalHealthRequest() {
        MentalHealthDTO mentalHealthDTO = new MentalHealthDTO();
        mentalHealthDTO.setScore(TestConstants.INT_ONE);
        mentalHealthDTO.setRiskLevel(TestConstants.RISK_LEVEL);
        mentalHealthDTO.setMentalHealthDetails(List.of(getMentalHealthDetailsDTO()));
        return mentalHealthDTO;
    }

    public static MentalHealthDetailsDTO getMentalHealthDetailsDTO() {
        MentalHealthDetailsDTO mentalHealthDetailsDTO = new MentalHealthDetailsDTO();
        mentalHealthDetailsDTO.setScore(TestConstants.INT_ONE);
        mentalHealthDetailsDTO.setQuestionId(TestConstants.ONE);
        mentalHealthDetailsDTO.setQuestion("Little interest or pleasure in doing things?");
        mentalHealthDetailsDTO.setAnswer("Nearly every day");
        mentalHealthDetailsDTO.setAnswerId(TestConstants.ONE);
        return mentalHealthDetailsDTO;
    }


    public static Observation getRegularSmokerObservation() {
        return getObservation(getRegularSmokerObservationJsonString());
    }

    public static Observation getMentalHealthObservation() {
        return getObservation(getMentalHealthObservationJsonString());
    }

    public static Observation getSubstanceAbuseObservation() {
        return getObservation(getSubstanceAbuseObservationJsonString());
    }

    public static Observation getSuicideScreenerObservation() {
        return getObservation(getSuicideScreenerObservationJsonString());
    }

    public static Observation getHeightObservation() {
        return getObservation(getHeightObservationJsonString());
    }

    public static Observation getWeightObservation() {
        return getObservation(getWeightObservationJsonString());
    }

    public static Observation getBmiObservation() {
        return getObservation(getBmiObservationJsonString());
    }

    public static Observation getPregnancyAncObservation() {
        return getObservation(getPregnancyAncObservationJsonString());
    }

    public static Observation getBloodPressureObservation() {
        return getObservation(getBloodPressureObservationJsonString());
    }

    public static Observation getBloodGlucoseObservation() {
        return getObservation(getBloodGlucoseObservationJsonString());
    }

    public static Observation getObservation(String observationJson) {
        return FhirContext.forR4().newJsonParser().parseResource(Observation.class, observationJson);
    }

    public static QuestionnaireResponse getQuestionnaireResponse() {
        return FhirContext.forR4().newJsonParser().parseResource(QuestionnaireResponse.class, getQuestionnaireResponseJsonString());
    }

    public static Location getFhirLocation() {
        return FhirContext.forR4().newJsonParser().parseResource(Location.class, getLocationJsonString());
    }

    public static Encounter getEncounter() {
        return FhirContext.forR4().newJsonParser().parseResource(Encounter.class, getEncounterJsonString());
    }

    public static RelatedPerson getFhirRelatedPerson() {
        return FhirContext.forR4().newJsonParser().parseResource(RelatedPerson.class, getRelatedPersonJsonString());
    }

    public static Patient getFhirPatient() {
        return FhirContext.forR4().newJsonParser().parseResource(Patient.class, getPatientJsonString());
    }

    public static Bundle getRelatedPersonBundle() {
        Patient patient = TestDataProvider.getFhirPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(patient.getId())
                .setResource(patient)
                .getRequest()
                .setUrl(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        return bundle;
    }

    public static String getRegularSmokerObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Regular Smoker</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"83539-7\",\"display\":\"Cardiology Risk assessment and screening note\"}],\"text\":\"riskDetails\"}, \"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}]}";
    }

    public static String getMentalHealthObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Mental Health Details</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"83539-7\",\"display\":\"Cardiology Risk assessment and screening note\"}],\"text\":\"riskDetails\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"component\":[{\"code\":{\"text\":\"cvdRiskLevel\"},\"valueString\":\"Low risk\"},{\"code\":{\"text\":\"cvdRiskScore\"},\"valueString\":\"4.0\"},{\"code\":{\"text\":\"cvdRiskScoreDisplay\"},\"valueString\":\"4% - Low risk\"},{\"code\":{\"text\":\"phq4RiskLevel\"},\"valueString\":\"Normal\"}]}";
    }

    public static String getSubstanceAbuseObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Substance Abuse</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"74013-4\",\"display\":\"CAGE-AID substance abuse screening\"}],\"text\":\"substanceAbuse\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"valueString\":\"4.0\",\"component\":[{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"428211000000000\",\"display\":\"Assessment of substance use\"}],\"text\":\"doYouTakeAlcoholOrAnyOtherSubstance?\"}},{\"code\":{\"text\":\"haveYouEverFeltYouOughtToCutDownOnYourDrinkingOrDrugUse?\"}},{\"code\":{\"text\":\"haveYouFeltBadOrGuiltyAboutYourDrinkingOrDrugUse?\"}},{\"code\":{\"text\":\"havePeopleAnnoyedYouByCriticizingYourDrinkingOrDrugUse?\"}},{\"code\":{\"text\":\"haveYouEverHadaDrinkOrUsedDrugsFirstThingInTheMorningToSteadyYourNervesOrToGetRidOfaHangover(eye-opener)?\"}}]}";
    }

    public static String getSuicideScreenerObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Suicide Screener</div>\"},\"status\":\"final\",\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"component\":[{\"code\":{\"text\":\"If Yes, have you ever attempted or had a plan to go ahead with it?\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"6471006\",\"display\":\"Suicidal thoughts\"}],\"text\":\"haveYouThoughtOf/ContemplatingEndingYourLifeOrNotWorthLiving?\"}}]}";
    }

    public static String getBmiObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">BMI</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"39156-5\",\"display\":\"Body mass index (BMI) [Ratio]\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"60621009\",\"display\":\"Body mass index (observable entity)\"}],\"text\":\"bmi\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"valueQuantity\":{\"value\":24.49,\"unit\":\"kg/m^2\"},\"component\":[{\"code\":{\"text\":\"bmiCategory\"},\"valueString\":\"Normal Weight\"}]}";
    }

    public static String getWeightObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Weight</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"29463-7\",\"display\":\"Body weight\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"107647005\",\"display\":\"Weight finding\"}],\"text\":\"weight\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"valueQuantity\":{\"value\":75.0,\"unit\":\"kg\"}}";
    }

    public static String getHeightObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Height</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"8302-2\",\"display\":\"Body height\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"365714001\",\"display\":\"Height / growth finding\"}],\"text\":\"height\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"valueQuantity\":{\"value\":190.0,\"unit\":\"cm\"}}";
    }

    public static String getPregnancyAncObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Pregnancy Anc</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"66174-4\",\"display\":\"Are you currently pregnant\"},{\"system\":\"http://loinc.org\",\"code\":\"82810-3\",\"display\":\"Pregnancy status\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"249013004\",\"display\":\"Pregnant abdomen finding\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"233560009\",\"display\":\"Percutaneous intraperitoneal fetal blood transfusion\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"466471009\",\"display\":\"Bedding support\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"400572001\",\"display\":\"Prenatal vitamin\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"424525001\",\"display\":\"Antenatal care\"}],\"text\":\"pregnancyAnc\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"note\":[{\"text\":\"isInterestedToEnroll-true\"},{\"text\":\"pregnancyOtherSymptoms-Vomiting\"}],\"component\":[{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"289530006\",\"display\":\"Vaginal bleeding\"}],\"text\":\"vaginalBleeding\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"91175000\",\"display\":\"Seizure\"}],\"text\":\"convulsions\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"25064002\",\"display\":\"Headache\"}],\"text\":\"headache\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"111516008\",\"display\":\"Blurring of visual image (finding)\"}],\"text\":\"blurredVisionDifficultySeeingClearly\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"276369006\",\"display\":\"Reduced fetal movement (finding)\"}],\"text\":\"reducedAbsentBabyMovements\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue (finding)\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"88895004\",\"display\":\"Fatigue during pregnancy\"}],\"text\":\"fatigueFeelingTiredFeelingIllWeak\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"267036007\",\"display\":\"Dyspnea (finding)\"}],\"text\":\"difficultyBreathingFastBreathingCoughChestPain\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"44223004\",\"display\":\"Premature rupture of membranes (disorder)\"}],\"text\":\"breakingOfWater\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"21522001\",\"display\":\"Abdominal pain\"}],\"text\":\"painInTheAbdomen\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"386661006\",\"display\":\"Fever\"}],\"text\":\"feverHotnessOfBody\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"267038008\",\"display\":\"Oedema (finding)\"}],\"text\":\"swellingOfTheLegsHandsFace\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"58250006\",\"display\":\"Scalding pain on urination (finding)\"}],\"text\":\"painfulBurningFeelingWhenPassingUrine\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"289567003\",\"display\":\"Vaginal discharge problem (finding)\"}],\"text\":\"vaginalDischargeItchiness\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"74964007\",\"display\":\"Other\"}],\"text\":\"other\"}},{\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"11885-1\",\"display\":\"Gestational age Estimated from last menstrual period\"},{\"system\":\"http://loinc.org\",\"code\":\"18185-9\",\"display\":\"Gestational age\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"57036006\",\"display\":\"Fetal gestational age\"}],\"text\":\"gestationalPeriod\"},\"valueString\":\"8 weeks\"},{\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"8665-2\",\"display\":\"Last menstrual period start date\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"21840007\",\"display\":\"Date of last menstrual period\"}],\"text\":\"lastMenstrualPeriodDate\"},\"valueDateTime\":\"2024-06-13\"}]}";
    }

    public static String getBloodGlucoseObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Blood Glucose</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"1558-6\",\"display\":\"fbs\"},{\"system\":\"http://snomed.info/sct\",\"code\":\"405176005\",\"display\":\"Blood glucose status\"},{\"display\":\"haveYouBeenDiagnosedWithHighBloodSugarOrDiabetesBefore\"}]},\"subject\":{\"reference\":\"Patient/2667\"},\"encounter\":{\"reference\":\"Encounter/2670\"},\"effectiveDateTime\":\"2024-08-14T05:51:01+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2669\"}],\"valueQuantity\":{\"value\":25.0,\"unit\":\"mmol/L\"},\"component\":[{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"301897001\",\"display\":\"Time of last food intake\"}],\"text\":\"timeOfLastMeal\"},\"valueDateTime\":\"2024-08-07T03:00:00+00:00\"},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"17173007\",\"display\":\"Excessive thirst (finding)\"}],\"text\":\"excessiveThirst\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"55350005\",\"display\":\"Hungry\"}],\"text\":\"significantHunger\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"789507005\",\"display\":\"Delayed healing of wound\"}],\"text\":\"slowHealingOrInfectedFootWound\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"309538000\",\"display\":\"Numbness of Foot\"}],\"text\":\"footNumbnessTinglingAndOrPain\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue\"}],\"text\":\"significantFatigueOrWeakness\"}}, {\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue\"}],\"text\":\"unplannedWeightLoss\"}}, {\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue\"}],\"text\":\"noSymptoms\"}}, {\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue\"}],\"text\":\"unplannedWeightLoss\"}}, {\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue\"}],\"text\":\"other\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"84229001\",\"display\":\"Fatigue\"}],\"text\":\"frequentUrination\"}}]}";
    }

    public static String getBloodPressureObservationJsonString() {
        return "{\"resourceType\":\"Observation\",\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Blood Pressure</div>\"},\"status\":\"final\",\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"308502002\",\"display\":\"Hypertension monitoring status\"}],\"text\":\"haveYouBeenDiagnosedWithHighBloodPressureOrHypertensionBefore\"},\"subject\":{\"reference\":\"Patient/2682\"},\"encounter\":{\"reference\":\"Encounter/2698\"},\"effectiveDateTime\":\"2024-08-26T13:48:14+00:00\",\"performer\":[{\"reference\":\"RelatedPerson/2684\"}],\"component\":[{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"271649006\",\"display\":\"Systolic blood pressure\"},{\"system\":\"http://loinc.org\",\"code\":\"8480-6\",\"display\":\"Systolic blood pressure\"}],\"text\":\"Average Systolic Blood Pressure\"},\"valueQuantity\":{\"value\":199.0,\"unit\":\"mmHg\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"mm[Hg]\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"271650006\",\"display\":\"Diastolic blood pressure\"},{\"system\":\"http://loinc.org\",\"code\":\"8462-4\",\"display\":\"Diastolic blood pressure\"}],\"text\":\"Average Diastolic Blood Pressure\"},\"valueQuantity\":{\"value\":99.0,\"unit\":\"mmHg\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"mm[Hg]\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"271649006\",\"display\":\"Systolic blood pressure\"},{\"system\":\"http://loinc.org\",\"code\":\"8480-6\",\"display\":\"Systolic blood pressure\"}],\"text\":\"Systolic Blood Pressure 1\"},\"valueQuantity\":{\"value\":199,\"unit\":\"mmHg\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"mm[Hg]\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"271650006\",\"display\":\"Diastolic blood pressure\"},{\"system\":\"http://loinc.org\",\"code\":\"8462-4\",\"display\":\"Diastolic blood pressure\"}],\"text\":\"Diastolic Blood Pressure 1\"},\"valueQuantity\":{\"value\":99,\"unit\":\"mmHg\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"mm[Hg]\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"8499008\",\"display\":\"Pulse\"},{\"system\":\"http://loinc.org\",\"code\":\"8884-9\",\"display\":\"Heart rate rhythm\"}],\"text\":\"Pulse 1\"},\"valueQuantity\":{\"value\":55,\"unit\":\"BPM\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"BPM\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"271649006\",\"display\":\"Systolic blood pressure\"},{\"system\":\"http://loinc.org\",\"code\":\"8480-6\",\"display\":\"Systolic blood pressure\"}],\"text\":\"Systolic Blood Pressure 2\"},\"valueQuantity\":{\"value\":199,\"unit\":\"mmHg\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"mm[Hg]\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"271650006\",\"display\":\"Diastolic blood pressure\"},{\"system\":\"http://loinc.org\",\"code\":\"8462-4\",\"display\":\"Diastolic blood pressure\"}],\"text\":\"Diastolic Blood Pressure 2\"},\"valueQuantity\":{\"value\":99,\"unit\":\"mmHg\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"mm[Hg]\"}},{\"code\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"8499008\",\"display\":\"Pulse\"},{\"system\":\"http://loinc.org\",\"code\":\"8884-9\",\"display\":\"Heart rate rhythm\"}],\"text\":\"Pulse 2\"},\"valueQuantity\":{\"value\":55,\"unit\":\"BPM\",\"system\":\"http://unitsofmeasure.org\",\"code\":\"BPM\"}}]}";
    }

    public static String getQuestionnaireResponseJsonString() {
        return "{\"resourceType\":\"QuestionnaireResponse\",\"status\":\"completed\",\"encounter\":{\"reference\":\"Encounter/2698\"},\"authored\":\"2024-08-26T13:48:14+00:00\",\"source\":{\"reference\":\"Patient/2682\"},\"item\":[{\"linkId\":\"1.0\",\"text\":\"Feeling down, depressed or hopeless?\",\"answer\":[{\"valueCoding\":{\"system\":\"http://snomed.info/sct\",\"code\":\"35489007\",\"display\":\"Several days\"}}]},{\"linkId\":\"114.0\",\"text\":\"Feeling nervous, anxious or on edge?\",\"answer\":[{\"valueCoding\":{\"system\":\"http://snomed.info/sct\",\"code\":\"48694002\",\"display\":\"Not at all\"}}]},{\"linkId\":\"35.0\",\"text\":\"Little interest or pleasure in doing things?\",\"answer\":[{\"valueCoding\":{\"system\":\"http://snomed.info/sct\",\"code\":\"28669007\",\"display\":\"Not at all\"}}]},{\"linkId\":\"111.0\",\"text\":\"Not being able to stop or control worrying?\",\"answer\":[{\"valueCoding\":{\"system\":\"http://snomed.info/sct\",\"code\":\"79015004\",\"display\":\"Several days\"}}]}]}";
    }

    public static String getEncounterJsonString() {
        return "{\"resourceType\":\"Encounter\",\"identifier\":[{\"system\":\"type\",\"value\":\"assessment\"}],\"status\":\"finished\",\"classHistory\":[{\"class\":{\"system\":\"http://snomed.info/sct\",\"code\":\"440654001\",\"display\":\"Inpatient\"}}],\"serviceType\":{\"coding\":[{\"system\":\"http://snomed.info/sct\",\"code\":\"257622000\",\"display\":\"Healthcare facility (environment)\"}],\"text\":\"facility\"},\"subject\":{\"reference\":\"Patient/2682\"},\"participant\":[{\"individual\":{\"reference\":\"RelatedPerson/2684\"}}],\"period\":{\"start\":\"2024-08-26T13:48:14+00:00\"},\"location\":[{\"location\":{\"reference\":\"Location/2697\"}}]}";
    }

    public static String getLocationJsonString() {
        return "{\"resourceType\":\"Location\",\"status\":\"active\",\"name\":\"Chennai\",\"position\":{\"longitude\":80.2098614,\"latitude\":13.0145045}}";
    }

    public static String getRelatedPersonJsonString() {
        return "{\"resourceType\":\"RelatedPerson\",\"identifier\":[{\"system\":\"http://mdtlabs.org/national-id\",\"value\":\"ROSE123456\"},{\"system\":\"status\",\"value\":\"SCREENED\"}],\"patient\":{\"reference\":\"Patient/2074\"},\"name\":[{\"text\":\"ROSE LAST NAME\",\"given\":[\"ROSE\",\"MIDDLE NAME\",\"LAST NAME\"]}],\"telecom\":[{\"system\":\"phone\",\"value\":\"234848488\",\"use\":\"mobile\"}],\"gender\":\"female\",\"birthDate\":\"2002-08-29\"}";
    }

    public static String getPatientJsonString() {
        return "{\"resourceType\":\"Patient\",\"identifier\":[{\"system\":\"http://mdtlabs.org/national-id\",\"value\":\"ROSE123456\"}],\"name\":[{\"text\":\"ROSE LAST NAME\",\"given\":[\"ROSE\",\"MIDDLE NAME\",\"LAST NAME\"]}],\"telecom\":[{\"system\":\"phone\",\"value\":\"234848488\",\"use\":\"mobile\"}],\"gender\":\"female\",\"birthDate\":\"2002-08-29\",\"managingOrganization\":{\"reference\":\"Organization/1116\"},\"link\":[{\"other\":{\"reference\":\"RelatedPerson/2076\"}}]}";
    }

    public static String getRelatedPersonBundleJsonString() {
        return "{\"resourceType\":\"Bundle\",\"type\":\"transaction\",\"entry\":[{\"fullUrl\":\"urn:uuid:patient-1\",\"resource\":{\"resourceType\":\"Patient\",\"id\":\"patient-1\",\"identifier\":[{\"system\":\"http://hospital.smarthealthit.org\",\"value\":\"12345\"}],\"name\":[{\"use\":\"official\",\"family\":\"Doe\",\"given\":[\"John\"]}],\"gender\":\"male\",\"birthDate\":\"1980-01-01\",\"address\":[{\"use\":\"home\",\"line\":[\"123 Main St\"],\"city\":\"Anytown\",\"state\":\"Anystate\",\"postalCode\":\"12345\",\"country\":\"USA\"}]},\"request\":{\"method\":\"POST\",\"url\":\"Patient\"}},{\"fullUrl\":\"urn:uuid:relatedperson-1\",\"resource\":{\"resourceType\":\"RelatedPerson\",\"id\":\"relatedperson-1\",\"identifier\":[{\"system\":\"http://hospital.smarthealthit.org\",\"value\":\"54321\"}],\"patient\":{\"reference\":\"urn:uuid:patient-1\"},\"relationship\":[{\"coding\":[{\"system\":\"http://terminology.hl7.org/CodeSystem/v3-RoleCode\",\"code\":\"N\"}]}],\"name\":[{\"use\":\"official\",\"family\":\"Doe\",\"given\":[\"Jane\"]}],\"gender\":\"female\",\"birthDate\":\"1982-02-02\",\"address\":[{\"use\":\"home\",\"line\":[\"123 Main St\"],\"city\":\"Anytown\",\"state\":\"Anystate\",\"postalCode\":\"12345\",\"country\":\"USA\"}]},\"request\":{\"method\":\"POST\",\"url\":\"RelatedPerson\"}}]}";
    }

    public static RequestDTO getRequestDTO() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setId(TestConstants.TWO_STR);
        return requestDTO;
    }

    public static PatientFilterDTO getPatientFilterDTO() {
        return new PatientFilterDTO();
    }

    public static ReferralDetailsDTO getReferralDetailsDTO() {
        return new ReferralDetailsDTO();
    }

    public static ReferralTicketDTO getReferralTicketDTO() {
        return new ReferralTicketDTO();
    }

    public static DiagnosisDTO getDiagnosisDTO() {
        return new DiagnosisDTO();
    }

    public static PrescriptionRequestDTO getPrescriptionRequestDTO() {
        return new PrescriptionRequestDTO();
    }

    public static PrescriptionHistoryDTO getPrescriptionHistoryDTO() {
        return new PrescriptionHistoryDTO();
    }

    public static GeneralMedicalReviewDTO getGeneralMedicalReviewDTO() {
        return new GeneralMedicalReviewDTO();
    }

    public static GeneralMedicalReviewSummaryDetailsDTO getGeneralMedicalReviewSummaryDetailsDTO() {
        return new GeneralMedicalReviewSummaryDetailsDTO();
    }

    public static MedicalReviewRequestDTO getMedicalReviewRequestDTO() {
        return new MedicalReviewRequestDTO();
    }

    public static IccmResponseDTO getIccmResponseDTO() {
        return new IccmResponseDTO();
    }

    public static GeneralMedicalReviewSummaryDTO getGeneralMedicalReviewSummaryDTO() {
        return new GeneralMedicalReviewSummaryDTO();
    }

    public static MotherAndNeonateSummaryDTO getMotherAndNeonateSummaryDTO() {
        return new MotherAndNeonateSummaryDTO();
    }

    public static MedicalReviewHistoryDTO getMedicalReviewHistoryDTO() {
        return new MedicalReviewHistoryDTO();
    }

    public static MedicalReviewPregnancySummaryDetailsDTO getMedicalReviewPregnancySummaryDetailsDTO() {
        return new MedicalReviewPregnancySummaryDetailsDTO();
    }

    public static MotherNeonateDTO getMotherNeonateDTO() {
        return new MotherNeonateDTO();
    }

    public static BirthHistoryDTO getBirthHistoryDTO() {
        return new BirthHistoryDTO();
    }

    public static MedicationRequest getMedicationRequest() {
        return new MedicationRequest();
    }

    public static PregnancyInfo getPregnancyInfo() {
        return new PregnancyInfo();
    }

    public static UserResponseDTO getUserResponseDTO() {
        UserResponseDTO user = new UserResponseDTO();
        user.setId(TestConstants.ONE);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setTenantId(TestConstants.FIVE);
        user.setCountryCode(Constants.COUNTRY);
        user.setUsername(Constants.NAME);
        return user;
    }

    public static PatientDTO getPatientDTO() {
        PatientDTO patientDTO = new PatientDTO();
        patientDTO.setId(TestConstants.TWO_STR);
        patientDTO.setName(TestConstants.NAME);
        patientDTO.setBirthDate(new Date());
        patientDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        return patientDTO;
    }

    public static PatientNutritionLifestyle getPatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        Set<String> lifestyles = new HashSet<>();
        lifestyles.add("Healthy Eating");
        patientNutritionLifestyle.setId(TestConstants.ONE_TWO_THREE);
        patientNutritionLifestyle.setLifestyles(lifestyles);
        patientNutritionLifestyle.setLifestyleAssessment("Good");
        patientNutritionLifestyle.setPatientReference("Patient/1");
        patientNutritionLifestyle.setVisitId("Encounter/1");
        patientNutritionLifestyle.setReferredBy("Practitioner/1");
        patientNutritionLifestyle.setReferredByDisplay(TestConstants.NAME);
        patientNutritionLifestyle.setReferredDate(new Date());
        patientNutritionLifestyle.setAssessedBy(TestConstants.NAME);
        patientNutritionLifestyle.setAssessedByDisplay(TestConstants.NAME);
        patientNutritionLifestyle.setAssessedDate(new Date());
        patientNutritionLifestyle.setClinicianNote("Patient is doing well.");
        patientNutritionLifestyle.setOtherNote("No additional notes.");
        patientNutritionLifestyle.setViewed(Boolean.TRUE);
        patientNutritionLifestyle.setProvenance(getProvenance());
        patientNutritionLifestyle.setMemberReference("RelatedPerson/1");
        patientNutritionLifestyle.setIsNutritionist(true);
        return patientNutritionLifestyle;
    }

    public static LabTestDTO getLabTestDTO() {
        return new LabTestDTO();
    }

    public static ConfirmDiagnosisDTO getConfirmDiagnosisDTO() {
        ConfirmDiagnosisDTO confirmDiagnosisDTO = new ConfirmDiagnosisDTO();
        confirmDiagnosisDTO.setPatientReference(Constants.STRING_ONE);
        return confirmDiagnosisDTO;
    }

    public static VillageDTO getVillageDTO() {
        VillageDTO villageDTO = new VillageDTO();
        villageDTO.setId(1l);
        villageDTO.setName(TestConstants.NAME);
        villageDTO.setCode("1234");
        return villageDTO;
    }

    public static NCDMedicalReviewDTO getNCDMedicalReviewDTO() {
        try {
            return new ObjectMapper().readValue(getNCDMedicalReviewDTOjsonString(), NCDMedicalReviewDTO.class);
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static String getNCDMedicalReviewDTOjsonString() {
        return """ 
               {"continuousMedicalReview":{"clinicalNote":"clinical notes","complaintComments":"complaints","complaints":[{"id":47,"name":"Shortness of breath on activity","other":false,"value":"shortnessOfBreathOnActivity"},{"id":49,"name":"Loss of consciousness","other":false,"value":"lossOfConsciousness"},{"id":50,"name":"Foot complaints","other":false,"value":"footComplaints"},{"id":51,"name":"Recurrent dizziness","other":false,"value":"recurrentDizziness"},{"id":52,"name":"Fainting","other":false,"value":"fainting"},{"id":54,"name":"Leg swelling","other":false,"value":"legSwelling"}],"physicalExamComments":"obstetric","physicalExams":[{"id":89,"name":"Eye Exam","other":false,"value":"eyeExam"},{"id":94,"name":"Foot Exam","other":false,"value":"footExam"},{"id":96,"name":"Mental Health","other":false,"value":"mentalHealth"},{"id":92,"name":"Foetal Heartbeat","other":false,"value":"foetalHeartbeat"},{"id":90,"name":"Abdominal Exam","other":false,"value":"abdominalExam"}]},"encounterReference":"56261","initialMedicalReview":{"comorbidities":[{"id":13,"name":"COPD","other":false,"value":"copd"},{"id":16,"name":"Depression","other":false,"value":"depression"},{"id":14,"name":"Malaria","other":false,"value":"malaria"}],"complications":[{"id":26,"name":"Stroke","other":false,"value":"stroke"},{"id":28,"name":"Heart Failure","other":false,"value":"heartFailure"},{"id":23,"name":"Renal Disease","other":false,"value":"renalDisease"},{"id":27,"name":"Peripheral Neuropathy","other":false,"value":"peripheralNeuropathy"},{"id":22,"name":"Visual Impairment","other":false,"value":"visualImpairment"}],"lifestyle":[{"answer":{"name":"Yes, currently","value":"yesCurrently"},"comments":"23","id":2,"name":"Have you ever smoked cigarettes or used other tobacco products?","value":"haveYouEverSmokedCigarettesOrUsedOtherTobaccoProducts?"},{"answer":{"name":"Yes, currently","value":"yesCurrently"},"comments":"10","id":1,"name":"Have you ever consumed alcohol?","value":"haveYouEverConsumedAlcohol?"},{"answer":{"name":"Unhealthy","value":"unhealthy"},"comments":"no limit","id":4,"name":"How would you describe your diet and nutrition?","value":"howWouldYouDescribeYourDietAndNutrition?"},{"answer":{"name":"1-3","value":"1-3"},"id":3,"name":"About how many hours per week do you spend doing physical labor, walking, and/or exercising?","value":"aboutHowManyHoursPerWeekDoYouSpendDoingPhysicalLabor,Walking,And/OrExercising?"}]},"memberReference":"4313","patientReference":"4309","provenance":{"modifiedDate":"2024-11-25T07:48:32.031Z","organizationId":"30","spiceUserId":11,"userId":"146"}}
               """;
    }

    public static Bundle getBundle(Resource resource) {
        Bundle bundle = new Bundle();
        resource.setId(TestConstants.TWO_STR);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(resource);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        return bundle;
    }

    public static VitalSignsDTO getVitalSignsDTO() {
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(TestConstants.TWO_STR);
        vitalSignsDTO.setScreenedLandmark(TestConstants.LANDMARK);
        vitalSignsDTO.setBpObservation(new Observation());
        vitalSignsDTO.setBgObservation(new Observation());
        vitalSignsDTO.setHeightObservation(new Observation());
        vitalSignsDTO.setWeightObservation(new Observation());
        vitalSignsDTO.setBmiObservation(new Observation());
        vitalSignsDTO.setTemperatureObservation(new Observation());
        vitalSignsDTO.setSuicideObservation(new Observation());
        vitalSignsDTO.setRegularSmokerObservation(new Observation());
        vitalSignsDTO.setSubstanceAbuseObservation(new Observation());
        vitalSignsDTO.setPregnancyObservation(new Observation());
        vitalSignsDTO.setMentalHealthObservation(new Observation());
        vitalSignsDTO.setRedRiskObservation(new Observation());
        return vitalSignsDTO;
    }

    public static List<String> getSymptomName() {
        List<String> names = new ArrayList<>();
        names.add("Frequent urination");
        names.add("Fainting");
        names.add("Seizures");
        names.add("Blurry vision, Sudden vision loss");
        names.add("Numbness on side of body/ limb");
        names.add("Swelling of limbs");
        names.add("Dizziness upon standing up");
        names.add("Chest pain or discomfort");
        names.add("Swelling of tongue or lips");
        names.add("Palpitations or irregular heart beats");
        names.add("Shortness of breath with usual activities");
        names.add("Excessive thirst");
        names.add("Significant hunger");
        names.add("Slow healing or infected foot wound");
        names.add("Foot numbness, tingling, and/ or pain");
        names.add("Significant fatigue or weakness");
        names.add("Unplanned weight loss");
        names.add("No symptoms");
        names.add("Other");
        return names;
    }

    public static List<String> getObservationCode() {
        List<String> codes = new ArrayList<>();
        codes.add("riskLevel");
        codes.add("temperature");
        codes.add("mentalHealth");
        codes.add("pregnancy");
        codes.add("areYouaRegularSmokerOrHaveYouBeenaRegularSmokerInThePast12Months");
        codes.add("substanceAbuse");
        codes.add("suicideScreener");
        codes.add("weight");
        codes.add("height");
        codes.add("bmi");
        codes.add("bloodGlucose");
        codes.add("bloodPressure");
        return codes;
    }

}
