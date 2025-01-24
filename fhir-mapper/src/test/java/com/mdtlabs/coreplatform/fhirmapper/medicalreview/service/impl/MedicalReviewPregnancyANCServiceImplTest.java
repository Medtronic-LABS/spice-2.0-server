package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.*;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.*;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

import org.hl7.fhir.r4.model.*;
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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MedicalReviewPregnancyANCServiceImplTest {

    @InjectMocks
    private MedicalReviewPregnancyANCServiceImpl medicalReviewPregnancyANCService;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private PatientService patientService;

    @Mock
    private PrescriptionRequestService prescriptionService;

    @Mock
    private InvestigationService investigationService;

    @Test
    void createValueObservation() {
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        when(restApiUtil.postBatchRequest(null, null)).thenReturn(responseEntity);
        Map<String, List<String>> mapValue = new HashMap<>();
        mapValue.put(String.valueOf(ResourceType.Encounter), List.of(TestConstants.PATIENT_ID));
        mapValue.put(String.valueOf(ResourceType.Patient), List.of(TestConstants.PATIENT_ID));
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(mapValue);
        ObservationDTO response = medicalReviewPregnancyANCService.createValueObservation(
                TestDataProvider.getObservationDTO());
        assertNotNull(response);
    }

    @Test
    void getPatientWeight() {
        RequestDTO requestDTO = new RequestDTO();
        Observation observation = new Observation();
        observation.getCode().setText(Constants.WEIGHT);
        List<String> types = new ArrayList<>();
        types.add(requestDTO.getType());
        observation.getValueQuantity().setValue(Constants.ONE);
        Bundle bundle = new Bundle().addEntry(new Bundle.BundleEntryComponent());
        when(fhirAssessmentMapper.getPatientBasicDetails(requestDTO.getMemberId(), types)).thenReturn(
                bundle);
        Map<String, Double> response = medicalReviewPregnancyANCService.getPatientWeight(requestDTO);
        assertNotNull(response);

        when(fhirAssessmentMapper.getPatientBasicDetails(requestDTO.getMemberId(), types)).thenReturn(
                new Bundle());
        response = medicalReviewPregnancyANCService.getPatientWeight(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getPatientBp() {
        RequestDTO requestDTO = new RequestDTO();
        Observation observation = new Observation();
        List<String> types = new ArrayList<>();
        types.add(requestDTO.getType());
        Observation.ObservationComponentComponent observationComponentComponent = new Observation.ObservationComponentComponent();
        observationComponentComponent.getCode().setText(Constants.BP);
        observationComponentComponent.getValueQuantity().setValue(Constants.ONE);
        observation.addComponent(observationComponentComponent);
        Bundle bundle = new Bundle().addEntry(new Bundle.BundleEntryComponent());
        when(fhirAssessmentMapper.getPatientBasicDetails(requestDTO.getMemberId(), types)).thenReturn(
                bundle);
        Map<String, Double> response = medicalReviewPregnancyANCService.getPatientBp(requestDTO);
        assertNotNull(response);

        when(fhirAssessmentMapper.getPatientBasicDetails(requestDTO.getMemberId(), types)).thenReturn(
                new Bundle());
        response = medicalReviewPregnancyANCService.getPatientBp(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createMedicalReview() {
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

        MedicalReviewPregnancyDTO medicalReviewPregnancyDTO = TestDataProvider.getMedicalReviewPregnancyDTO();
        medicalReviewPregnancyDTO.getEncounter().setMemberId(TestConstants.TWO_STR);
        RequestDTO request = new RequestDTO();
        request.setMemberId(TestConstants.TWO_STR);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setPncVisitNo(TestConstants.INT_THREE);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                medicalReviewPregnancyDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(medicalReviewPregnancyANCService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        doNothing().when(patientService).setPatientReferenceInEncounterDetails(medicalReviewPregnancyDTO.getEncounter(), bundle);
        when(patientService.createOrUpdateMedicalReviewEncounter(medicalReviewPregnancyDTO.getId(),
                medicalReviewPregnancyDTO.getEncounter(), Constants.PREGNANCY_ANC_MEDICAL_REVIEW,
                null, bundle)).thenReturn(TestConstants.TWO_STR);
        when(fhirAssessmentMapper.setObservation(medicalReviewPregnancyDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MEDICAL_REVIEW, Constants.PREGNANCY_ANC_MEDICAL_REVIEW)).thenReturn(
                observation);
        when(fhirAssessmentMapper.createNotes(Constants.CLINICAL_NOTES,
                medicalReviewPregnancyDTO.getClinicalNotes(), medicalReviewPregnancyDTO.getEncounter())).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle, medicalReviewPregnancyDTO.getEncounter()
                .getProvenance())).thenReturn(TestConstants.STRING_VALUE);
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, medicalReviewPregnancyDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MEDICAL_REVIEW, medicalReviewPregnancyDTO.getEncounter().getVisitNumber(),
                medicalReviewPregnancyDTO.getEncounter().getPatientReference());
        when(patientService.getPatientVitals(request)).thenReturn(pregnancyInfo);
        doNothing().when(patientService).closePncDetails(bundle, new RequestDTO());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, medicalReviewPregnancyDTO.getEncounter(),
                Constants.HEIGHT, medicalReviewPregnancyDTO.getPregnancyDetails().getHeight(),
                medicalReviewPregnancyDTO.getEncounter().getPatientReference());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, medicalReviewPregnancyDTO.getEncounter(),
                medicalReviewPregnancyDTO.getPregnancyDetails().getSystolic(),
                medicalReviewPregnancyDTO.getPregnancyDetails().getDiastolic(),
                medicalReviewPregnancyDTO.getPregnancyDetails().getPulse());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, medicalReviewPregnancyDTO.getEncounter(),
                Constants.LAST_MENSTRUAL_PERIOD,
                medicalReviewPregnancyDTO.getPregnancyDetails().getLastMenstrualPeriod(),
                medicalReviewPregnancyDTO.getEncounter().getPatientReference());
        when(fhirAssessmentMapper.createSignsObservation(medicalReviewPregnancyDTO.getPresentingComplaints(),
                medicalReviewPregnancyDTO.getEncounter(), Constants.PRESENTING_COMPLAINTS,
                medicalReviewPregnancyDTO.getPresentingComplaintsNotes())).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                medicalReviewPregnancyDTO.getEncounter().getProvenance())).thenReturn(TestConstants.URL);
        when(fhirAssessmentMapper.createSignsObservation(medicalReviewPregnancyDTO.getObstetricExaminations(),
                medicalReviewPregnancyDTO.getEncounter(), Constants.OBSTETRIC_EXAMINATION,
                medicalReviewPregnancyDTO.getObstetricExaminationNotes())).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                medicalReviewPregnancyDTO.getEncounter().getProvenance())).thenReturn(TestConstants.URL);
        when(fhirAssessmentMapper.createSignsObservation(medicalReviewPregnancyDTO.getPregnancyHistory(),
                medicalReviewPregnancyDTO.getEncounter(),
                Constants.PREGNANCY_HISTORY, medicalReviewPregnancyDTO.getPregnancyHistoryNotes())).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle, medicalReviewPregnancyDTO.getEncounter()
                .getProvenance())).thenReturn(TestConstants.URL);
        doNothing().when(fhirAssessmentMapper).createObservationComponent(medicalReviewPregnancyDTO.isDeliveryKit(),
                Constants.DOES_MOTHER_HAVE_A_DELIVERY_KIT,
                observation.getComponent());
        doNothing().when(patientService).updatePatientStatus(bundle,
                Boolean.TRUE,
                medicalReviewPregnancyDTO.getEncounter().getProvenance(),
                medicalReviewPregnancyDTO.getEncounter().getPatientId(), Boolean.TRUE);
        doNothing().when(fhirUtils).setBundle(TestConstants.URL, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, medicalReviewPregnancyDTO.getEncounter().getProvenance());
        when(fhirUtils.getIdFromResourceUrl(medicalReviewPregnancyDTO.getEncounter().getPatientReference())).thenReturn(TestConstants.STRING_VALUE);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(new HashMap<>());
        when(fhirUtils.getIdFromReference(medicalReviewPregnancyDTO.getId())).thenReturn(TestConstants.STRING_VALUE);
        when(fhirUtils.getIdFromReference(medicalReviewPregnancyDTO.getEncounter().getPatientReference())).thenReturn(TestConstants.STRING_VALUE);

        //then
        Map<String, String> response = medicalReviewPregnancyANCService.createMedicalReview(
                medicalReviewPregnancyDTO);
        assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetails() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.REFERRED);
        Identifier visitIdentifier = new Identifier();
        visitIdentifier.setSystem(FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL);
        visitIdentifier.setValue(TestConstants.TWO_STR);
        Encounter encounter = new Encounter();
        encounter.setIdentifier(List.of(visitIdentifier, identifier));
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(encounter);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        bundle.setTotal(Constants.ZERO);
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        List<String> types = new ArrayList<>();
        types.add(Constants.HEIGHT);

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getPatientBasicDetails(types, TestConstants.STRING_VALUE)).thenReturn(bundle);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(labTestDTO));

        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewPregnancyANCService.getPregnancyMedicalReviewDetails(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetailsForCondition() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getCondition());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        bundle.setTotal(Constants.ZERO);
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        List<String> types = new ArrayList<>();
        types.add(Constants.HEIGHT);

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getPatientBasicDetails(types, TestConstants.STRING_VALUE)).thenReturn(bundle);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(labTestDTO));

        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewPregnancyANCService.getPregnancyMedicalReviewDetails(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetailsForPatient() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getPatient());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        bundle.setTotal(Constants.ZERO);
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        List<String> types = new ArrayList<>();
        types.add(Constants.HEIGHT);

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getPatientBasicDetails(types, TestConstants.STRING_VALUE)).thenReturn(bundle);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(labTestDTO));

        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewPregnancyANCService.getPregnancyMedicalReviewDetails(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetailsForObservation() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        Observation observation = new Observation();
        observation.setId(TestConstants.TWO_STR);
        List<Identifier> theIdentifier = new ArrayList<>();
        Identifier identifier1 = new Identifier();
        identifier1.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue("presentingComplaints");
        Identifier identifier2 = new Identifier();
        identifier2.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue("obstetricExaminations");
        Identifier identifier3 = new Identifier();
        identifier3.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue("ANC_REVIEW");
        theIdentifier.add(identifier1);
        theIdentifier.add(identifier2);
        theIdentifier.add(identifier3);
        observation.setIdentifier(theIdentifier);
        observation.setCode(new CodeableConcept().setText("clinicalNotes"));
        List<Observation.ObservationComponentComponent> observationComponentComponents = new ArrayList<>();
        for (String name : List.of("bmi","fundalHeight","fetalHeartRate","weight","systolic","diastolic","pulse")) {
            Observation.ObservationComponentComponent observationComponentComponent = new Observation.ObservationComponentComponent();
            observationComponentComponent.setValue(new Quantity().setValue(TestConstants.ONE));
            observationComponentComponent.setCode(new CodeableConcept().setText(name));
            observationComponentComponents.add(observationComponentComponent);
        }
        observation.setComponent(observationComponentComponents);
        bundle.getEntry().getFirst().setResource(observation);
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        List<String> types = new ArrayList<>();
        types.add(Constants.HEIGHT);

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.STRING_VALUE, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getPatientBasicDetails(types, TestConstants.STRING_VALUE)).thenReturn(bundle);
        when(prescriptionService.getPrescriptionsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE)).thenReturn(List.of(labTestDTO));

        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewPregnancyANCService.getPregnancyMedicalReviewDetails(TestConstants.STRING_VALUE, TestConstants.STRING_VALUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getLatestEncounter() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<String> types = new ArrayList<>();
        types.add(Constants.HEIGHT);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getEncounterBundle());
        when(fhirAssessmentMapper.getEncounterDetails(TestConstants.TWO_STR, Boolean.TRUE)).thenReturn(TestDataProvider.getEncounterBundle());
        when(fhirAssessmentMapper.getPatientBasicDetails(types, requestDTO.getPatientReference())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewPregnancyANCService.getLatestEncounter(requestDTO);
        Assertions.assertNotNull(response);
    }
}