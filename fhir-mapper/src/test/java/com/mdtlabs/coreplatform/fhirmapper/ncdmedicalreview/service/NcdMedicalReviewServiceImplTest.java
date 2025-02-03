package com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.MedicationRequest;
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
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.CurrentMedicationDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewMetaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientStatusConverter;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.impl.NcdMedicalReviewServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.impl.PatientVisitServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class NcdMedicalReviewServiceImplTest {

    @InjectMocks
    private NcdMedicalReviewServiceImpl ncdMedicalReviewService;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    PatientVisitServiceImpl patientVisitService;

    @Mock
    private PatientService patientService;

    @Mock
    private PatientStatusConverter patientStatusConverter;

    @Mock
    private EncounterConverter encounterConverter;

    @Mock
    private PatientConverter patientConverter;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private PatientTreatmentPlanService patientTreatmentPlanService;

    @Mock
    private FhirMapper fhirMapper;

    @Mock
    private PrescriptionRequestService prescriptionRequestService;

    @Test
    void getPatientLifestyleAndResultBundleEmpty() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference("12345");

        Bundle resultBundle = new Bundle();
        resultBundle.setEntry(new ArrayList<Bundle.BundleEntryComponent>());  // Empty entries

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);

        //then
        List<LifestyleResponseDTO> lifestyleResponses = ncdMedicalReviewService.getPatientLifestyle(requestDTO);
        Assertions.assertTrue(lifestyleResponses.isEmpty(), "The lifestyle responses should be empty when the result bundle is empty.");
    }

    @Test
    void getPatientLifestyleAndObservationIsNull() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference("12345");

        Bundle resultBundle = new Bundle();
        Bundle.BundleEntryComponent entry = new Bundle.BundleEntryComponent();
        Observation observation = new Observation();
        observation.setComponent(new ArrayList<>());
        entry.setResource(observation);

        resultBundle.setEntry(List.of(entry));

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);

        //then
        List<LifestyleResponseDTO> lifestyleResponses = ncdMedicalReviewService.getPatientLifestyle(requestDTO);
        Assertions.assertNotNull(lifestyleResponses, "The lifestyle responses should not be null.");
        Assertions.assertTrue(lifestyleResponses.isEmpty(), "The lifestyle responses should be empty when the observation has no components.");
    }

    @Test
    void getPatientLifestyleAndValidData() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference("12345");

        Bundle resultBundle = new Bundle();
        List<Bundle.BundleEntryComponent> entries = new ArrayList<>();
        Bundle.BundleEntryComponent entry = new Bundle.BundleEntryComponent();
        Observation observation = new Observation();

        // Create a mock component for the observation
        Observation.ObservationComponentComponent component = new Observation.ObservationComponentComponent();
        CodeableConcept code = new CodeableConcept();
        code.setText("Smoking");
        component.setCode(code);
        observation.setComponent(List.of(component));

        // Set the entry with the observation
        entry.setResource(observation);
        entries.add(entry);

        resultBundle.setEntry(entries);

        // Mock the fhirUtils call for "Smoking"
        when(fhirUtils.getText("Smoking")).thenReturn("Smoker");

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(resultBundle);

        //then
        List<LifestyleResponseDTO> lifestyleResponses = ncdMedicalReviewService.getPatientLifestyle(requestDTO);
        Assertions.assertNotNull(lifestyleResponses);
        Assertions.assertEquals(1, lifestyleResponses.size(), "There should be one lifestyle response.");
        Assertions.assertEquals("Smoking", lifestyleResponses.get(0).getValue());
        Assertions.assertEquals("Smoker", lifestyleResponses.get(0).getLifestyle());
    }

    @Test
    void createNcdMedicalReviewThrowAnException() {
        Bundle bundle = new Bundle();
        NCDMedicalReviewDTO request = new NCDMedicalReviewDTO();
        request.setMemberReference("dsds");
        when(restApiUtil.getBatchRequest(String.format("RelatedPerson?_id=%s&_include=RelatedPerson:patient",
                request.getMemberReference()))).thenReturn(bundle);
        assertThrows(NullPointerException.class,()->{
            ncdMedicalReviewService.createNcdMedicalReview(request);
        });
    }

    @Test
    void createNcdMedicalReviewPatientIsNull() {
        //given
        NCDMedicalReviewDTO ncdMedicalReviewDTO = TestDataProvider.getNCDMedicalReviewDTO();
        ncdMedicalReviewDTO.setPatientReference(null);
        ncdMedicalReviewDTO.setEncounterReference(TestConstants.ENCOUNTER_REFERENCE);
        String patientUrl = "RelatedPerson?_id=4313&_include=RelatedPerson:patient";
        String resultBundleUrl = "Condition?identifier=nullpatient-diagnosis-status|&subject=Patient/urn:uuid:null";
        Bundle resultBundle = new Bundle();
        resultBundle.addEntry().setResource(TestDataProvider.getCondition());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> fhirResponse = new HashMap<>();
        fhirResponse.put("Encounter", Arrays.asList("Encounter", "Encounter"));
        fhirResponse.put("Patient", Arrays.asList("Patient", "Patient"));
        Bundle bundle = TestDataProvider.getRelatedPersonBundle();
        bundle.getEntry().getFirst().setResource(null);

        //when
        when(restApiUtil.getBatchRequest(patientUrl)).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(resultBundleUrl)).thenReturn(resultBundle);
        when(patientConverter.createPatientFromRelatedPerson(any())).thenReturn((Patient) TestDataProvider.getRelatedPersonBundle().getEntry().getFirst().getResource());
        when(encounterConverter.createEncounter("urn:uuid:null", ncdMedicalReviewDTO.getMemberReference(), FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                Constants.NCD_MEDICAL_REVIEW_ENCOUNTER_TYPE, ncdMedicalReviewDTO.getProvenance())).thenReturn(new Encounter());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(fhirResponse);

        //then
        Map<String, String> response = ncdMedicalReviewService.createNcdMedicalReview(ncdMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createNcdMedicalReviewAndValidPatientAndRelatedPerson() {
        //given
        NCDMedicalReviewDTO ncdMedicalReviewDTO = TestDataProvider.getNCDMedicalReviewDTO();
        ncdMedicalReviewDTO.setPatientReference(null);
        ncdMedicalReviewDTO.setEncounterReference(TestConstants.ENCOUNTER_REFERENCE);
        String patientUrl = "RelatedPerson?_id=4313&_include=RelatedPerson:patient";
        String resultBundleUrl = "Condition?identifier=nullpatient-diagnosis-status|&subject=Patient/null";
        Bundle resultBundle = new Bundle();
        resultBundle.addEntry().setResource(TestDataProvider.getCondition());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        Map<String, List<String>> fhirResponse = new HashMap<>();
        fhirResponse.put("Encounter", Arrays.asList("Encounter", "Encounter"));
        fhirResponse.put("Patient", Arrays.asList("Patient", "Patient"));

        //when
        when(restApiUtil.getBatchRequest(patientUrl)).thenReturn(TestDataProvider.getRelatedPersonBundle());
        when(restApiUtil.getBatchRequest(resultBundleUrl)).thenReturn(resultBundle);
        when(encounterConverter.createEncounter(ncdMedicalReviewDTO.getPatientReference(), ncdMedicalReviewDTO.getMemberReference(), FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                Constants.NCD_MEDICAL_REVIEW_ENCOUNTER_TYPE, ncdMedicalReviewDTO.getProvenance())).thenReturn(new Encounter());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(fhirResponse);

        //then
        Map<String, String> response = ncdMedicalReviewService.createNcdMedicalReview(ncdMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void addOrUpdateNextVisitDateThrowAnExceptionWhenMedicalReviewDateIsNull() {
        NCDMedicalReviewDTO ncdMedicalReviewDTO = new NCDMedicalReviewDTO();
        ncdMedicalReviewDTO.setNextMedicalReviewDate(null);
        ncdMedicalReviewDTO.setMemberReference("member123");

        assertThrows(DataNotFoundException.class, () -> {
            ncdMedicalReviewService.addOrUpdateNextVisitDate(ncdMedicalReviewDTO);
        });

        ncdMedicalReviewDTO.setNextMedicalReviewDate(new Date());
        ncdMedicalReviewDTO.setMemberReference(null);

        assertThrows(DataNotFoundException.class, () -> {
            ncdMedicalReviewService.addOrUpdateNextVisitDate(ncdMedicalReviewDTO);
        });
    }

    @Test
    void addOrUpdateNextVisitDateWhenValidDataProvided() {
        NCDMedicalReviewDTO ncdMedicalReviewDTO = new NCDMedicalReviewDTO();
        Date nextVisitDate = new Date();
        ProvenanceDTO provenance = TestDataProvider.getProvenance();
        ncdMedicalReviewDTO.setNextMedicalReviewDate(nextVisitDate);
        ncdMedicalReviewDTO.setMemberReference("member123");
        ncdMedicalReviewDTO.setPatientReference("patient123");
        ncdMedicalReviewDTO.setProvenance(provenance);

        ncdMedicalReviewService.addOrUpdateNextVisitDate(ncdMedicalReviewDTO);
        verify(patientTreatmentPlanService).createOrUpdateAppointment(
                Constants.FREQUENCY_MEDICAL_REVIEW,
                ncdMedicalReviewDTO.getNextMedicalReviewDate(),
                ncdMedicalReviewDTO.getMemberReference(),
                ncdMedicalReviewDTO.getPatientReference(),
                null,
                ncdMedicalReviewDTO.getProvenance(), Boolean.FALSE
        );
        assertEquals(nextVisitDate, ncdMedicalReviewDTO.getNextMedicalReviewDate());
    }

    @Test
    void getEncounterIdsByVisit() {
        String encounterId = "encounter-123";
        Encounter encounter = TestDataProvider.getEncounter();
        encounter.setId("encounter-123");
        Bundle bundle = TestDataProvider.getBundle(encounter);
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_PART_OF, encounterId))).thenReturn(bundle);
        List<String> encounterIdsByVisit = ncdMedicalReviewService.getEncounterIdsByVisit(encounterId);
        assertFalse("Should not return empty list",encounterIdsByVisit.isEmpty());
    }

    @Test
    void ncdMedicalReviewSummary() {
        //given
        String batchRequest = "Encounter?_id=3&_revinclude=Observation:encounter&_revinclude=MedicationRequest:encounter&_revinclude=DiagnosticReport:encounter&_count=9999999";
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        medicalReviewRequestDTO.setPatientVisitId(TestConstants.STRING_THREE);
        Patient patient = TestDataProvider.getPatient();
        Condition condition = TestDataProvider.getCondition();
        condition.getIdentifier().getFirst().setValue("comorbidities");
        Observation observation = TestDataProvider.getHeightObservation();
        observation.getCode().setText("physicalExaminations");
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setResource(patient)
                .getRequest()
                .setMethod(Bundle.HTTPVerb.PUT);
        bundle.addEntry()
                .setResource(condition)
                .getRequest()
                .setMethod(Bundle.HTTPVerb.PUT);
        bundle.addEntry()
                .setResource(observation)
                .getRequest()
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle medicationRequestBundle = TestDataProvider.getMedicationBundle();
        MedicationRequest medicationRequest = TestDataProvider.getMedicationRequest();
        medicationRequest.setMedication(new CodeableConcept());
        medicationRequest.setStatus(MedicationRequest.MedicationRequestStatus.COMPLETED);
        medicationRequest.hasMedicationCodeableConcept();
        medicationRequestBundle.getEntry().getFirst().setResource(medicationRequest);

        //when
        when(restApiUtil.getBatchRequest("Encounter?part-of=null&_sort=part-of&status:not=cancelled")).thenReturn(TestDataProvider.getEncounterBundle());
        when(restApiUtil.getBatchRequest(batchRequest)).thenReturn(medicationRequestBundle);
        when(restApiUtil.getBatchRequest("Encounter/null/$everything?_count=9999999")).thenReturn(bundle);
        when(fhirMapper.mapPrescriptionDTO(any())).thenReturn(TestDataProvider.getPrescriptionDTO());

        //then
        NcdMedicalReviewResponse response = ncdMedicalReviewService.ncdMedicalReviewSummary(medicalReviewRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createInitialEncounter() {
        //given
        NCDMedicalReviewDTO ncdMedicalReviewDTO = TestDataProvider.getNCDMedicalReviewDTO();
        ncdMedicalReviewDTO.getInitialMedicalReview().setDiagnosis(new PatientStatusDTO());
        CurrentMedicationDetailsDTO currentMedicationDetailsDTO = new CurrentMedicationDetailsDTO();
        Set<MedicalReviewMetaDTO> medicationSet = new HashSet<>();
        MedicalReviewMetaDTO medicalReviewMetaDTO = new MedicalReviewMetaDTO();
        medicalReviewMetaDTO.setId(TestConstants.ONE);
        medicationSet.add(medicalReviewMetaDTO);
        currentMedicationDetailsDTO.setMedications(medicationSet);
        ncdMedicalReviewDTO.getInitialMedicalReview().setCurrentMedications(currentMedicationDetailsDTO);
        Bundle resultBundle = new Bundle();
        Condition condition = new Condition();
        condition.setId(TestConstants.TWO_STR);
        condition.setIdentifier(Arrays.asList(new Identifier().setValue("Hypertension")));
        resultBundle.addEntry().setResource(condition);
        Condition firstcondition = new Condition();
        firstcondition.setIdentifier(Arrays.asList(new Identifier().setValue("Diabetes")));
        resultBundle.addEntry().setResource(firstcondition);
        String resultBundleUrl = "Condition?identifier=nullpatient-diagnosis-status|&subject=Patient/4309";
        Patient patient = new Patient();
        RelatedPerson relatedPerson = new RelatedPerson();

        //when
        when(restApiUtil.getBatchRequest(resultBundleUrl)).thenReturn(resultBundle);
        when(patientStatusConverter.createHypertensionStatus(any(), any())).thenReturn(condition);
        when(patientStatusConverter.createDiabetesStatus(any(), any())).thenReturn(firstcondition);

        //then
        ncdMedicalReviewService.createInitialEncounter(ncdMedicalReviewDTO, resultBundle, patient, relatedPerson);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createInitialEncounterAndConditionIsNull() {
        //given
        NCDMedicalReviewDTO ncdMedicalReviewDTO = TestDataProvider.getNCDMedicalReviewDTO();
        ncdMedicalReviewDTO.getInitialMedicalReview().setDiagnosis(new PatientStatusDTO());
        CurrentMedicationDetailsDTO currentMedicationDetailsDTO = new CurrentMedicationDetailsDTO();
        Set<MedicalReviewMetaDTO> medicationSet = new HashSet<>();
        MedicalReviewMetaDTO medicalReviewMetaDTO = new MedicalReviewMetaDTO();
        medicalReviewMetaDTO.setId(TestConstants.ONE);
        medicationSet.add(medicalReviewMetaDTO);
        currentMedicationDetailsDTO.setMedications(medicationSet);
        ncdMedicalReviewDTO.getInitialMedicalReview().setCurrentMedications(currentMedicationDetailsDTO);
        Bundle resultBundle = new Bundle();
        resultBundle.addEntry().setResource(null);
        String resultBundleUrl = "Condition?identifier=nullpatient-diagnosis-status|&subject=Patient/4309";
        Patient patient = new Patient();
        patient.setId(TestConstants.TWO_STR);
        patient.setIdentifier(List.of(new Identifier().setSystem("nullpatient-status").setValue("SCREENED")));
        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setIdentifier(List.of(new Identifier().setSystem("nullpatient-status").setValue("SCREENED")));

        //when
        when(restApiUtil.getBatchRequest(resultBundleUrl)).thenReturn(resultBundle);
        when(patientStatusConverter.createHypertensionStatus(any(), any())).thenReturn(new Condition());
        when(patientStatusConverter.createDiabetesStatus(any(), any())).thenReturn(new Condition());

        //then
        ncdMedicalReviewService.createInitialEncounter(ncdMedicalReviewDTO, resultBundle, patient, relatedPerson);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

}

