package com.mdtlabs.coreplatform.fhirmapper.assessment.service;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.CqlApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.assessment.service.impl.AssessmentServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AncDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.CoughDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiarrhoeaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FeverDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralDangerSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ICCMDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NutritionalStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.OtherSymptomsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncChildDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMotherDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncNeonatalDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TbDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.MentalHealthService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.impl.PatientServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;

/**
 * <p>
 * AssessmentServiceImplTest class used to test all possible positive
 * and negative cases for all methods and conditions used in AssessmentServiceImpl class.
 * </p>
 *
 * @author Nandhakumar
 * @since Feb 8, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AssessmentServiceImplTest {

    @InjectMocks
    AssessmentServiceImpl assessmentService;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private PatientServiceImpl patientService;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private HouseholdService householdService;

    @Mock
    private SpiceConverter spiceConverter;

    @Mock
    private PatientConverter patientConverter;

    @Mock
    private CommonConverter commonConverter;

    @Mock
    private EncounterConverter encounterConverter;

    @Mock
    private MentalHealthService mentalHealthService;

    @Mock
    private VitalSignsConverter vitalSignsConverter;

    @Mock
    private PatientTreatmentPlanService patientTreatmentPlanService;

    @Mock
    private CqlApiInterface cqlApiInterface;

    @Test
    void createNcdAssessmentException() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();

        //then
        Assertions.assertThrows(SpiceValidation.class, ()->assessmentService.createNcdAssessment(assessmentDTO));
    }

    @Test
    void createNcdAssessment() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentTakenOn(new Date());
        assessmentDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        assessmentDTO.setBioMetrics(TestDataProvider.getBioMetricsDTO());
        assessmentDTO.setMemberReference(TestConstants.PATIENT_REFERENCE);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle conditionBundle = new Bundle();
        conditionBundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(TestDataProvider.getCondition())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Patient patient = TestDataProvider.getPatient();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(restApiUtil.getPatientById(any())).thenReturn(patient);
        when(patientConverter.createPatient(any(), any(), any(), any())).thenReturn(TestDataProvider.getPatient());
        when(restApiUtil.getBatchRequest("Condition?asserter=RelatedPerson/abcd-efgh-ijkl-mnop&verification-status:text=provisional&_count=9999")).thenReturn(conditionBundle);
        when(encounterConverter.createEncounter(patient, (RelatedPerson) bundle.getEntry().getFirst().getResource(), null, null, assessmentDTO.getAssessmentTakenOn())).thenReturn(TestDataProvider.getEncounter());
        when(mentalHealthService.createQuestionnaireResponse(any(), any(), any(), any(), any(), any())).thenReturn(new HashMap<>());

        //then
        AssessmentDTO response = assessmentService.createNcdAssessment(assessmentDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void setObservationDetails() {
        //given
        Bundle bundle = new Bundle();
        Observation observation = TestDataProvider.getBloodGlucoseObservation();
        observation.setId(TestConstants.TWO_STR);
        String identifierUrl = TestConstants.URL;
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();

        //then
        assessmentService.setObservationDetails(bundle, observation, identifierUrl, assessmentDTO, patient, relatedPerson, provenanceDTO);
        verify(commonConverter, atLeastOnce()).setObservationReference(any(), any(), any());
    }

    @Test
    void createSubstanceAbuseObservationWithId() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        Map<String, String> substanceAbuse = new HashMap<>();
        substanceAbuse.put("ObId", TestConstants.TWO_STR);
        substanceAbuse.put("Observation", TestConstants.TWO_STR);
        assessmentDTO.setSubstanceAbuse(substanceAbuse);
        Encounter encounter = TestDataProvider.getEncounter();

        //when
        when(commonConverter.createSubstanceAbuseObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());

        //then
        Observation result = assessmentService.createSubstanceAbuseObservation(assessmentDTO, encounter);
        Assertions.assertNotNull(result);
    }

    @Test
    void createSubstanceAbuseObservation() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        Map<String, String> substanceAbuse = new HashMap<>();
        substanceAbuse.put("ObservationId", TestConstants.TWO_STR);
        substanceAbuse.put("Observation", TestConstants.TWO_STR);
        assessmentDTO.setSubstanceAbuse(substanceAbuse);
        Encounter encounter = TestDataProvider.getEncounter();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        Observation result = assessmentService.createSubstanceAbuseObservation(assessmentDTO, encounter);
        Assertions.assertNotNull(result);
    }

    @Test
    void createSuicideObservationWithId() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        Map<String, String> suicideScreener = new HashMap<>();
        suicideScreener.put("ObservationId", TestConstants.TWO_STR);
        suicideScreener.put("Observation", TestConstants.TWO_STR);
        assessmentDTO.setSuicideScreener(suicideScreener);
        Encounter encounter = TestDataProvider.getEncounter();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        Observation result = assessmentService.createSuicideObservation(assessmentDTO, encounter);
        Assertions.assertNotNull(result);
    }

    @Test
    void createSuicideObservation() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        Map<String, String> suicideScreener = new HashMap<>();
        suicideScreener.put("Ob", TestConstants.TWO_STR);
        suicideScreener.put("Obi", TestConstants.TWO_STR);
        assessmentDTO.setSuicideScreener(suicideScreener);
        Encounter encounter = TestDataProvider.getEncounter();

        //when
        when(commonConverter.createSuicideScreenerObservation(any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());

        //then
        Observation result = assessmentService.createSuicideObservation(assessmentDTO, encounter);
        Assertions.assertNotNull(result);
    }

    @Test
    void createTB() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        TbDTO tbDTO = new TbDTO();
        tbDTO.setHasCough(Boolean.TRUE);
        assessmentDetailsDTO.setTb(tbDTO);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        //then
        assessmentService.createTB(assessmentDTO);
        verify(fhirUtils, atLeastOnce()).getUniqueId();
    }

    @Test
    void getLatestBpLog() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        BpLogDTO response = assessmentService.getLatestBpLog(assessmentDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getLatestGlucoseLog() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        GlucoseLogDTO response = assessmentService.getLatestGlucoseLog(assessmentDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createAssessmentTypeNonCommunity() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("NON_COMMUNITY");
        assessmentDTO.setAssessmentTakenOn(new Date());
        assessmentDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        assessmentDTO.setBioMetrics(TestDataProvider.getBioMetricsDTO());
        assessmentDTO.setMemberReference(TestConstants.PATIENT_REFERENCE);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle conditionBundle = new Bundle();
        conditionBundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(TestDataProvider.getCondition())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Patient patient = TestDataProvider.getPatient();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(restApiUtil.getPatientById(any())).thenReturn(patient);
        when(patientConverter.createPatient(any(), any(), any(), any())).thenReturn(TestDataProvider.getPatient());
        when(restApiUtil.getBatchRequest("Condition?asserter=RelatedPerson/abcd-efgh-ijkl-mnop&verification-status:text=provisional&_count=9999")).thenReturn(conditionBundle);
        when(encounterConverter.createEncounter(patient, (RelatedPerson) bundle.getEntry().getFirst().getResource(), null, null, assessmentDTO.getAssessmentTakenOn())).thenReturn(TestDataProvider.getEncounter());
        when(mentalHealthService.createQuestionnaireResponse(any(), any(), any(), any(), any(), any())).thenReturn(new HashMap<>());

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createAssessmentException() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("ANC");
        FhirResponseDTO fhirResponseDTO = null;
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        //then
        Assertions.assertThrows(Validation.class, ()-> assessmentService.createAssessment(assessmentDTO));
    }

    @Test
    void createAssessmentWithANC() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("ANC");
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        AncDTO ancDTO = new AncDTO();
        ancDTO.setAncSigns(List.of(TestConstants.BLANK_STRING));
        ancDTO.setTakesFancidarTablets(Boolean.TRUE);
        ancDTO.setTakesIronFloatTablets(Boolean.TRUE);
        assessmentDetailsDTO.setAnc(ancDTO);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Encounter" ,List.of(TestConstants.BLANK_STRING));

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());
        when(fhirAssessmentMapper.createSignsObservation(any(), any(), any(), any())).thenReturn(TestDataProvider.getHeightObservation());

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createAssessmentWithICCM() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("ICCM");
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        ICCMDTO iccmdto = new ICCMDTO();
        iccmdto.setGeneralDangerSigns(new GeneralDangerSignsDTO());
        iccmdto.setNutritionalStatusDetails(new NutritionalStatusDTO());
        iccmdto.setDiarrhoea(new DiarrhoeaDTO());
        iccmdto.setFever(new FeverDTO());
        CoughDTO coughDTO = new CoughDTO();
        coughDTO.setNoOfDaysOfCough(TestConstants.ONE);
        coughDTO.setBreathPerMinute(TestConstants.ONE);
        coughDTO.setAmoxicillin(TestConstants.BLANK_STRING);
        coughDTO.setChestInDrawing(Boolean.TRUE);
        iccmdto.setCough(coughDTO);
        assessmentDetailsDTO.setIccm(iccmdto);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Encounter" ,List.of(TestConstants.BLANK_STRING));
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        LocalDate currentDate = LocalDate.now();
        LocalDate dateBefore500Days = currentDate.minusDays(500);
        Date dateOfBirth = Date.from(dateBefore500Days.atStartOfDay(ZoneId.systemDefault()).toInstant());
        householdMemberDTO.setDateOfBirth(dateOfBirth);

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());
        when(householdService.getHouseholdMemberById(any())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(any())).thenReturn(TestDataProvider.getPregnancyInfo());

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createAssessmentWithOtherSymptom() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("OTHER_SYMPTOMS");
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        OtherSymptomsDTO otherSymptomsDTO = new OtherSymptomsDTO();
        FeverDTO feverDTO = new FeverDTO();
        feverDTO.setRdtTest(TestConstants.BLANK_STRING);
        feverDTO.setNoOfDaysOfFever(TestConstants.ONE);
        feverDTO.setAct(TestConstants.BLANK_STRING);
        feverDTO.setTemperature(TestConstants.ONE_DOUBLE);
        otherSymptomsDTO.setFever(feverDTO);
        assessmentDetailsDTO.setOtherSymptoms(otherSymptomsDTO);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Encounter" ,List.of(TestConstants.BLANK_STRING));
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        LocalDate currentDate = LocalDate.now();
        LocalDate dateBefore500Days = currentDate.minusDays(500);
        Date dateOfBirth = Date.from(dateBefore500Days.atStartOfDay(ZoneId.systemDefault()).toInstant());
        householdMemberDTO.setDateOfBirth(dateOfBirth);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setDateOfDelivery(new Date());
        pregnancyInfo.setPncCreatedDate(new Date());

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());
        when(householdService.getHouseholdMemberById(any())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(any())).thenReturn(pregnancyInfo);

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createAssessmentWithPNCMother() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("PNC_MOTHER");
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        PncMotherDTO pncMotherDTO = new PncMotherDTO();
        pncMotherDTO.setPncMotherSigns(List.of(TestConstants.BLANK_STRING, TestConstants.BLANK_STRING));
        pncMotherDTO.setChlorhexidine(Boolean.TRUE);
        assessmentDetailsDTO.setPncMother(pncMotherDTO);
        EncounterDetailsDTO encounter = TestDataProvider.getEncounterDTO();
        encounter.setVisitNumber(TestConstants.INT_ONE);
        encounter.setProvenance(TestDataProvider.getProvenance());
        assessmentDTO.setEncounter(encounter);
        assessmentDetailsDTO.setPncMother(pncMotherDTO);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Encounter" ,List.of(TestConstants.BLANK_STRING));
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        LocalDate currentDate = LocalDate.now();
        LocalDate dateBefore500Days = currentDate.minusDays(500);
        Date dateOfBirth = Date.from(dateBefore500Days.atStartOfDay(ZoneId.systemDefault()).toInstant());
        householdMemberDTO.setDateOfBirth(dateOfBirth);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setDateOfDelivery(new Date());
        pregnancyInfo.setPncCreatedDate(new Date());

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());
        when(householdService.getHouseholdMemberById(any())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(any())).thenReturn(pregnancyInfo);
        when(fhirAssessmentMapper.createSignsObservation(any(), any(), any(), any())).thenReturn(TestDataProvider.getHeightObservation());

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createAssessmentWithPNCNeonate() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("PNC_NEONATE");
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();
        PncNeonatalDTO pncNeonatalDTO = new PncNeonatalDTO();
        pncNeonatalDTO.setDeathOfNewborn(Boolean.TRUE);
        pncNeonatalDTO.setPncNeonatalSigns(List.of(TestConstants.BLANK_STRING));
        EncounterDetailsDTO encounter = TestDataProvider.getEncounterDTO();
        encounter.setVisitNumber(TestConstants.INT_ONE);
        encounter.setProvenance(TestDataProvider.getProvenance());
        assessmentDTO.setEncounter(encounter);
        assessmentDetailsDTO.setPncNeonatal(pncNeonatalDTO);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Encounter" ,List.of(TestConstants.BLANK_STRING));
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        LocalDate currentDate = LocalDate.now();
        LocalDate dateBefore500Days = currentDate.minusDays(500);
        Date dateOfBirth = Date.from(dateBefore500Days.atStartOfDay(ZoneId.systemDefault()).toInstant());
        householdMemberDTO.setDateOfBirth(dateOfBirth);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setDateOfDelivery(new Date());
        pregnancyInfo.setPncCreatedDate(new Date());

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());
        when(householdService.getHouseholdMemberById(any())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(any())).thenReturn(pregnancyInfo);
        when(fhirAssessmentMapper.createSignsObservation(any(), any(), any(), any())).thenReturn(TestDataProvider.getHeightObservation());

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createAssessmentWithChildHoodVisit() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setAssessmentType("CHILDHOOD_VISIT");
        AssessmentDetailsDTO assessmentDetailsDTO = new AssessmentDetailsDTO();

        PncChildDTO pncChildDTO = new PncChildDTO();
        EncounterDetailsDTO encounter = TestDataProvider.getEncounterDTO();
        pncChildDTO.setDeathOfBaby(Boolean.TRUE);
        encounter.setVisitNumber(TestConstants.INT_ONE);
        encounter.setProvenance(TestDataProvider.getProvenance());
        assessmentDTO.setEncounter(encounter);

        assessmentDetailsDTO.setPncChild(pncChildDTO);
        assessmentDTO.setAssessmentDetails(assessmentDetailsDTO);
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Encounter" ,List.of(TestConstants.BLANK_STRING));
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        LocalDate currentDate = LocalDate.now();
        LocalDate dateBefore500Days = currentDate.minusDays(500);
        Date dateOfBirth = Date.from(dateBefore500Days.atStartOfDay(ZoneId.systemDefault()).toInstant());
        householdMemberDTO.setDateOfBirth(dateOfBirth);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setDateOfDelivery(new Date());
        pregnancyInfo.setPncCreatedDate(new Date());

        //when
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(TestDataProvider.getSubstanceAbuseObservation());
        when(householdService.getHouseholdMemberById(any())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(any())).thenReturn(pregnancyInfo);
        when(fhirAssessmentMapper.createSignsObservation(any(), any(), any(), any())).thenReturn(TestDataProvider.getHeightObservation());

        //then
        AssessmentDTO result = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(result);
    }

}
