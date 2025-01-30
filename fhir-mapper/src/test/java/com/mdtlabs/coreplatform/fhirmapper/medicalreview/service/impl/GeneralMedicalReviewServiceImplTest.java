package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.PaymentNotice;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdPatientStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientStatusConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.household.service.impl.HouseholdServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class GeneralMedicalReviewServiceImplTest {

    @InjectMocks
    private GeneralMedicalReviewServiceImpl generalMedicalReviewService;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private PatientService patientService;

    @Mock
    private HouseholdServiceImpl householdService;

    @Mock
    private PrescriptionRequestService prescriptionService;

    @Mock
    private InvestigationService investigationService;

    @Mock
    private PatientStatusConverter patientStatusConverter;

    @Mock
    private CommonConverter commonConverter;

    @Mock
    private PatientConverter patientConverter;

    @Mock
    private SpiceConverter spiceConverter;

    @Test
    void createGeneralMedicalReview() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.SYSTEMIC_EXAMINATIONS);
        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));

        List<Observation.ObservationComponentComponent> theComponent = new ArrayList<>();
        observation.setComponent(theComponent);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        GeneralMedicalReviewDTO generalMedicalReviewDTO = new GeneralMedicalReviewDTO();
        generalMedicalReviewDTO.setEncounter(TestDataProvider.getEncounterDetailsData());
        generalMedicalReviewDTO.setClinicalNotes(TestConstants.NAME);
        generalMedicalReviewDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        RequestDTO request = new RequestDTO();
        request.setMemberId(TestConstants.STRING_VALUE);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Observation),
                householdMemberDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(generalMedicalReviewService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        doNothing().when(patientService).setPatientReferenceInEncounterDetails(generalMedicalReviewDTO.getEncounter(),
                bundle);
        when(patientService.createOrUpdateMedicalReviewEncounter(generalMedicalReviewDTO.getId(),
                generalMedicalReviewDTO.getEncounter(), Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW,
                null, bundle)).thenReturn(TestConstants.STRING_VALUE);
        when(fhirAssessmentMapper.createNotes(Constants.CLINICAL_NOTES,
                generalMedicalReviewDTO.getClinicalNotes(), generalMedicalReviewDTO.getEncounter())).thenReturn(observation);
        when(fhirAssessmentMapper.createSignsObservation(generalMedicalReviewDTO.getPresentingComplaints(),
                generalMedicalReviewDTO.getEncounter(), Constants.PRESENTING_COMPLAINTS,
                generalMedicalReviewDTO.getPresentingComplaintsNotes())).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                generalMedicalReviewDTO.getEncounter().getProvenance())).thenReturn(TestConstants.STRING_VALUE);
        when(fhirAssessmentMapper.createSignsObservation(generalMedicalReviewDTO.getSystemicExaminations(),
                        generalMedicalReviewDTO.getEncounter(),
                        Constants.SYSTEMIC_EXAMINATIONS, generalMedicalReviewDTO.getSystemicExaminationsNotes()))
                .thenReturn(observation);
        when(householdService.getHouseholdMemberById(generalMedicalReviewDTO.getEncounter().getMemberId()))
                .thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(request)).thenReturn(pregnancyInfo);
        doNothing().when(fhirUtils).setBundle("",
                "",
                Bundle.HTTPVerb.PUT,
                observation,
                bundle,
                generalMedicalReviewDTO.getEncounter().getProvenance());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(new HashMap<>());
        when(fhirUtils.getIdFromReference(generalMedicalReviewDTO.getId())).thenReturn(generalMedicalReviewDTO.getEncounter().getId());
        when(fhirUtils.getIdFromReference(generalMedicalReviewDTO.getEncounter().getPatientReference())).thenReturn(generalMedicalReviewDTO.getEncounter().getPatientReference());

        //then
        Map<String, String> response = generalMedicalReviewService.createGeneralMedicalReview(
                generalMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getGeneralMedicalReviewDetails() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.REFERRED);

        Encounter encounter = new Encounter();
        encounter.setIdentifier(List.of(identifier));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(encounter);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(TestConstants.STRING_VALUE)).thenReturn(TestConstants.TWO_STR);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(labTestDTO));

        //then
        GeneralMedicalReviewSummaryDetailsDTO response = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getGeneralMedicalReviewDetailsWithPatient() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getPatient());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(TestConstants.STRING_VALUE)).thenReturn(TestConstants.TWO_STR);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(labTestDTO));

        //then
        GeneralMedicalReviewSummaryDetailsDTO response = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getGeneralMedicalReviewDetailsWithCondition() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getCondition());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(TestConstants.STRING_VALUE)).thenReturn(TestConstants.TWO_STR);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(labTestDTO));

        //then
        GeneralMedicalReviewSummaryDetailsDTO response = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getGeneralMedicalReviewDetailsForObservation() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL);
        identifier.setValue(Constants.CLINICAL_NOTES);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.SYSTEMIC_EXAMINATIONS);
        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));
        observation.setIdentifier(List.of(identifier));

        List<Observation.ObservationComponentComponent> theComponent = new ArrayList<>();
        observation.setComponent(theComponent);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(TestConstants.STRING_VALUE)).thenReturn(TestConstants.TWO_STR);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE))
                .thenReturn(List.of(labTestDTO));

        //then
        GeneralMedicalReviewSummaryDetailsDTO response = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);

        //given
        identifier.setValue(Constants.SYSTEMIC_EXAMINATIONS);
        //then
        response = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);

        //given
        identifier.setValue(Constants.PRESENTING_COMPLAINTS);
        //then
        response = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void saveSummaryDetails() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        GeneralMedicalReviewSummaryDTO generalMedicalReviewSummaryDTO = TestDataProvider.getGeneralMedicalReviewSummaryDTO();
        generalMedicalReviewSummaryDTO.setId(TestConstants.STRING_VALUE);
        generalMedicalReviewSummaryDTO.setPatientReference(TestConstants.STRING_VALUE);
        generalMedicalReviewSummaryDTO.getEncounter().setId(TestConstants.STRING_VALUE);
        generalMedicalReviewSummaryDTO.getEncounter().setPatientReference(TestConstants.STRING_VALUE);
        generalMedicalReviewSummaryDTO.setCost(TestConstants.STRING_VALUE);
        generalMedicalReviewSummaryDTO.setProvenance(TestDataProvider.getProvenance());
        generalMedicalReviewSummaryDTO.setPatientStatus(Constants.ON_TREATMENT);
        generalMedicalReviewSummaryDTO.setCategory(Constants.RMNCH);
        generalMedicalReviewSummaryDTO.setMedicalSupplies(List.of(TestConstants.BLANK_STRING));
        generalMedicalReviewSummaryDTO.setEncounterType("MOTHER_DELIVERY_REVIEW");
        generalMedicalReviewSummaryDTO.setNextVisitDate(new Date());

        PaymentNotice paymentNotice = new PaymentNotice();
        Encounter encounter = TestDataProvider.getEncounter();
        ReferralDetailsDTO referralDetailsDTO = new ReferralDetailsDTO();

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Observation),
                generalMedicalReviewSummaryDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(generalMedicalReviewService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.TWO_STR);
        doNothing().when(fhirUtils).setBundle("",
                StringUtil.concatString(Constants.FHIR_BASE_URL, TestConstants.TWO_STR),
                Bundle.HTTPVerb.POST,
                paymentNotice,
                bundle,
                generalMedicalReviewSummaryDTO.getProvenance());
        when(fhirAssessmentMapper.updateEncounterStatusDetails(any(), any(), any(), any())).thenReturn(encounter);
        doNothing().when(patientService).updateReferralTicketByMemberId(any(), any());
        when(patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE,
                Boolean.TRUE, null)).thenReturn(referralDetailsDTO);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        GeneralMedicalReviewSummaryDTO reviewSummaryDTO = generalMedicalReviewService.saveSummaryDetails(
                generalMedicalReviewSummaryDTO);
        Assertions.assertNotNull(reviewSummaryDTO);
    }

    @Test
    void updateConfirmDiagnosis() {
        //given
        ConfirmDiagnosisDTO confirmDiagnosisDTO = TestDataProvider.getConfirmDiagnosisDTO();
        DiagnosisDTO diagnosisDTO = TestDataProvider.getDiagnosisDTO();
        diagnosisDTO.setType(Constants.HYPERTENSION);
        diagnosisDTO.setValue(Constants.HYPERTENSION_DIAGNOSIS_VALUE);
        confirmDiagnosisDTO.setConfirmDiagnosis(List.of(diagnosisDTO));
        Condition condition = mock(Condition.class);
        String bundleDto = Constants.EMPTY_STRING;
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Condition),
                confirmDiagnosisDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        ReflectionTestUtils.setField(generalMedicalReviewService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirUtils.getUniqueId()).thenReturn(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_FIVE);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());
        when(patientStatusConverter.updateConfirmedDiagnosis(confirmDiagnosisDTO, null, Constants.HYPERTENSION, List.of(Constants.HYPERTENSION_DIAGNOSIS_VALUE))).thenReturn(condition);
        doNothing().when(patientStatusConverter).setReference(condition, confirmDiagnosisDTO.getPatientReference(),
                TestConstants.PATIENT_ID, confirmDiagnosisDTO.getMemberReference());
        doNothing().when(commonConverter).setConditionInBundle(new Bundle(),condition, FhirConstants.PATIENT_DIAGNOSIS_IDENTIFIER_URL.concat(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_FIVE), Boolean.FALSE, confirmDiagnosisDTO.getProvenanceDTO());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(responseEntity);

        //then
        generalMedicalReviewService.updateConfirmDiagnosis(confirmDiagnosisDTO);
        verify(fhirUtils, atLeastOnce()).initiateCodesMap();
    }

    @Test
    void testMedicalReviewCountTest() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference("");
        Bundle bundle = new Bundle();
        bundle.setTotal(TestConstants.INT_ONE);

        //when
        when(investigationService.getLabtestCount(request)).thenReturn(TestConstants.INT_ONE);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(prescriptionService.getPrescriptionCount(request)).thenReturn(TestConstants.ZERO.intValue());

        //then
        Map<String, Integer> actualResponse = generalMedicalReviewService.getMedicalReviewCount(request);
        assertNotNull(actualResponse);
        assertEquals(TestConstants.ZERO.intValue(),
                actualResponse.get(TestConstants.PRESCRIPTION_DAYS_COMPLETED_COUNT));
        assertEquals(TestConstants.INT_ONE, actualResponse.get(Constants.COUNT_NON_REVIEWED_TEST));

    }

    @Test
    void testUpdateViewStatus() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.TWO_STR);
        request.setMenuName(Constants.LIFESTYLE_REVIEW_STATUS);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Observation observation = new Observation();
        observation.setStatus(Observation.ObservationStatus.AMENDED);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Observation),
                request);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());
        doNothing().when(fhirUtils)
                .setBundle("", "", Bundle.HTTPVerb.PUT, observation, bundle, request.getProvenance());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        Boolean actualResponse = generalMedicalReviewService.updateViewCount(request);
        assertNotNull(actualResponse);
    }


    @Test
    void testCreatePatientStatus() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        Condition condition = new Condition();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        identifier.setValue(Constants.DIABETES);
        identifiers.add(identifier);
        condition.setIdentifier(identifiers);
        Bundle resultBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        resultBundle.setIdentifier(identifier);
        resultBundle.addEntry()
                .setResource(condition);
        MentalHealthStatus mentalHealthStatus = new MentalHealthStatus();
        mentalHealthStatus.setStatus(Constants.KNOWN_PATIENT);
        patientStatusDTO.setMentalHealthStatus(mentalHealthStatus);
        NcdPatientStatus ncdPatientStatus = new NcdPatientStatus();
        ncdPatientStatus.setDiabetesStatus(Constants.KNOWN_PATIENT);
        ncdPatientStatus.setHypertensionStatus(Constants.KNOWN_PATIENT);
        patientStatusDTO.setNcdPatientStatus(ncdPatientStatus);
        patientStatusDTO.setSubstanceUseStatus(mentalHealthStatus);
        ProvenanceDTO patientProvenance = new ProvenanceDTO();
        patientProvenance.setModifiedDate(new Date());
        patientStatusDTO.setProvenance(patientProvenance);
        patientStatusDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        RelatedPerson relatedPerson = new RelatedPerson();
        List<HumanName> theName = new ArrayList<>();
        HumanName name1 = new HumanName();
        name1.setUse(HumanName.NameUse.OFFICIAL);
        name1.setFamily("Doe");
        name1.addGiven("John");
        name1.addPrefix("Mr.");
        name1.addSuffix("Jr.");
        theName.add(name1);
        relatedPerson.setName(theName);
        relatedPerson.setBirthDate(TestDataProvider.getBioMetricsDTO().getDateOfBirth());

        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        when(restApiUtil.getBatchRequest(any())).thenReturn(resultBundle);
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=Patient/urn:uuid:null")).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);
        generalMedicalReviewService.createPatientStatus(patientStatusDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void testCreatePatientStatus_HYPERTENSION() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        Condition condition = new Condition();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        identifier.setValue(Constants.HYPERTENSION);
        identifiers.add(identifier);
        condition.setIdentifier(identifiers);
        Bundle resultBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        resultBundle.setIdentifier(identifier);
        resultBundle.addEntry()
                .setResource(condition);
        ProvenanceDTO patientProvenance = new ProvenanceDTO();
        patientProvenance.setModifiedDate(new Date());
        patientStatusDTO.setProvenance(patientProvenance);
        patientStatusDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);
        generalMedicalReviewService.createPatientStatus(patientStatusDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void testCreatePatientStatus_MENTAL_HEALTH_STATUS() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        Condition condition = new Condition();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        identifier.setValue(Constants.MENTAL_HEALTH_STATUS);
        identifiers.add(identifier);
        condition.setIdentifier(identifiers);
        Bundle resultBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        resultBundle.setIdentifier(identifier);
        resultBundle.addEntry()
                .setResource(condition);
        ProvenanceDTO patientProvenance = new ProvenanceDTO();
        patientProvenance.setModifiedDate(new Date());
        patientStatusDTO.setProvenance(patientProvenance);
        patientStatusDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);
        generalMedicalReviewService.createPatientStatus(patientStatusDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void testCreatePatientStatus_SUBSTANCE_DISORDER() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        Condition condition = new Condition();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        identifier.setValue(Constants.SUBSTANCE_DISORDER);
        identifiers.add(identifier);
        condition.setIdentifier(identifiers);
        Bundle resultBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        resultBundle.setIdentifier(identifier);
        resultBundle.addEntry()
                .setResource(condition);
        ProvenanceDTO patientProvenance = new ProvenanceDTO();
        patientProvenance.setModifiedDate(new Date());
        patientStatusDTO.setProvenance(patientProvenance);
        patientStatusDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);
        generalMedicalReviewService.createPatientStatus(patientStatusDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void testGetPatientStatusDetailsNull() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        Condition condition = new Condition();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        identifier.setValue(Constants.DIABETES);
        identifiers.add(identifier);
        condition.setIdentifier(identifiers);
        Bundle resultBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        resultBundle.setIdentifier(identifier);
        resultBundle.addEntry()
                .setResource(condition);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);
        PatientStatusDTO result = generalMedicalReviewService.getPatientStatusDetails(patientStatusDTO);
        Assertions.assertNull(result);
    }

    @Test
    void testGetPatientStatusDetails() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        Condition substanceCondition = new Condition();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        identifier.setValue(Constants.SUBSTANCE_DISORDER);
        identifiers.add(identifier);
        substanceCondition.setIdentifier(identifiers);
        MentalHealthStatus mentalHealthStatus = new MentalHealthStatus();
        Condition mentalHealthCondition = new Condition();
        List<Identifier> identifiersone = new ArrayList<>();
        Identifier identifierone = new Identifier();
        identifierone.setValue(Constants.MENTAL_HEALTH_STATUS);
        identifiersone.add(identifierone);
        mentalHealthCondition.setIdentifier(identifiersone);
        Condition hypertensionCondition = new Condition();
        List<Identifier> identifierstwo = new ArrayList<>();
        Identifier identifiertwo = new Identifier();
        identifiertwo.setValue(Constants.HYPERTENSION);
        identifierstwo.add(identifiertwo);
        hypertensionCondition.setIdentifier(identifierstwo);
        Condition diabetesCondition = new Condition();
        List<Identifier> identifiersthree = new ArrayList<>();
        Identifier identifierthree = new Identifier();
        identifierthree.setValue(Constants.DIABETES);
        identifiersthree.add(identifierthree);
        diabetesCondition.setIdentifier(identifiersthree);
        mentalHealthStatus.setStatus(TestConstants.STRING_VALUE);
        patientStatusDTO.setMentalHealthStatus(mentalHealthStatus);
        Bundle resultBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        resultBundle.setIdentifier(identifierone);
        resultBundle.addEntry().setResource(mentalHealthCondition);
        resultBundle.addEntry().setResource(substanceCondition);
        resultBundle.addEntry().setResource(hypertensionCondition);
        resultBundle.addEntry().setResource(diabetesCondition);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);
        PatientStatusDTO result = generalMedicalReviewService.getPatientStatusDetails(patientStatusDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void checkAndCloseRmnchDetails() {
        //given
        GeneralMedicalReviewDTO generalMedicalReviewDTO = TestDataProvider.getGeneralMedicalReviewDTO();
        generalMedicalReviewDTO.setEncounter(TestDataProvider.getEncounterDTO());
        Bundle bundle = new Bundle();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        LocalDate currentDate = LocalDate.now();
        LocalDate dateBefore500Days = currentDate.minusDays(500);
        Date dateOfBirth = Date.from(dateBefore500Days.atStartOfDay(ZoneId.systemDefault()).toInstant());
        householdMemberDTO.setDateOfBirth(dateOfBirth);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setDateOfDelivery(dateOfBirth);

        //when
        when(householdService.getHouseholdMemberById(any())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(any())).thenReturn(pregnancyInfo);

        //then
        generalMedicalReviewService.checkAndCloseRmnchDetails(generalMedicalReviewDTO, bundle);
    }

}
