package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.Date;
import java.util.HashMap;
import java.util.List;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Reference;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.NcdMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HistoryServiceImplTest {

    @InjectMocks
    HistoryServiceImpl historyService;

    @Mock
    FhirUtils fhirUtils;

    @Mock
    RestApiUtil restApiUtil;

    @Mock
    NcdMedicalReviewService medicalReviewService;

    @Mock
    private PatientService patientService;

    @ParameterizedTest
    @ValueSource(strings = {"physicalExaminations", "complaints", "clinicalNotes"})
    void getNCDMedicalReviewHistory(String meta) {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        requestDTO.setPatientReference(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE);
        Bundle bundle = mock(Bundle.class);
        Bundle.BundleEntryComponent component = mock(Bundle.BundleEntryComponent.class);
        List<Bundle.BundleEntryComponent> components = List.of(component);
        Encounter encounter = mock(Encounter.class);
        Period period = mock(Period.class);
        Bundle observationBundle = mock(Bundle.class);
        Observation observation = mock(Observation.class);
        Bundle.BundleEntryComponent observationComponent = mock(Bundle.BundleEntryComponent.class);
        List<Bundle.BundleEntryComponent> observationComponents = List.of(observationComponent);
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(meta);

        //when
        when(bundle.getEntry()).thenReturn(components);
        when(component.getResource()).thenReturn(encounter);
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_LATEST_ENCOUNTER_BY_PERIOD, requestDTO.getPatientReference(), Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE, Constants.NCD_MEDICAL_REVIEWED))).thenReturn(bundle);
        when(encounter.getIdPart()).thenReturn(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE);
        when(encounter.getPeriod()).thenReturn(period);
        when(period.getStart()).thenReturn(new Date());
        when(medicalReviewService.getEncounterIdsByVisit(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE)).thenReturn(List.of(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE));
        when(restApiUtil.getBatchRequest(String.format(Constants.OBSERVATION_REV_INCLUDE_ENCOUNTER_IDS, String.join(Constants.COMMA, List.of(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE))))).thenReturn(observationBundle);
        when(observationBundle.getEntry()).thenReturn(observationComponents);
        when(observationComponent.getResource()).thenReturn(observation);
        when(observation.getCode()).thenReturn(codeableConcept);
        when(observation.getValueCodeableConcept()).thenReturn(codeableConcept);

        //then
        NCDMedicalReviewHistoryDTO response = historyService.getNCDMedicalReviewHistory(requestDTO);
        assertNotNull(response);
        assertEquals(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE, response.getPatientVisitId());
    }

    @Test
    void getNCDMedicalReviewSummaryHistory() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        requestDTO.setPatientReference(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE);
        Bundle bundle = mock(Bundle.class);
        Bundle.BundleEntryComponent component = mock(Bundle.BundleEntryComponent.class);
        List<Bundle.BundleEntryComponent> components = List.of(component);
        Encounter encounter = mock(Encounter.class);
        Period period = mock(Period.class);
        Bundle observationBundle = mock(Bundle.class);
        Observation observation = mock(Observation.class);
        MedicationRequest medication = mock(MedicationRequest.class);
        DiagnosticReport diagnostic = mock(DiagnosticReport.class);
        Bundle.BundleEntryComponent observationComponent = mock(Bundle.BundleEntryComponent.class);
        Bundle.BundleEntryComponent mediationComponent = mock(Bundle.BundleEntryComponent.class);
        Bundle.BundleEntryComponent diagnosticReportComponent = mock(Bundle.BundleEntryComponent.class);

        List<Bundle.BundleEntryComponent> observationComponents = List.of(observationComponent, mediationComponent, diagnosticReportComponent);
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(MetaCodeConstants.PHYSICAL_EXAMINATION);
        Reference reference = new Reference();
        reference.setId(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE);

        //when
        when(bundle.getEntry()).thenReturn(components);
        when(component.getResource()).thenReturn(encounter);
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_LATEST_ENCOUNTER_BY_IDENTIFIER, requestDTO.getPatientReference(), Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_PART_OF, String.join(Constants.COMMA, List.of(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE))))).thenReturn(bundle);
        when(encounter.getIdPart()).thenReturn(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE);
        when(encounter.getPeriod()).thenReturn(period);
        when(period.getStart()).thenReturn(new Date());
        when(encounter.getPartOf()).thenReturn(reference);
        when(medicalReviewService.getEncounterIdsByVisit(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE)).thenReturn(List.of(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE));
        when(restApiUtil.getBatchRequest(String.format(Constants.REV_INCLUDE_ENCOUNTER_IDS,  String.join(Constants.COMMA,List.of(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE)),
                Constants.OBSERVATION, Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT))).thenReturn(observationBundle);
        when(observationBundle.getEntry()).thenReturn(observationComponents);
        when(observationComponent.getResource()).thenReturn(observation);
        when(mediationComponent.getResource()).thenReturn(medication);
        when(diagnosticReportComponent.getResource()).thenReturn(diagnostic);
        when(observation.getCode()).thenReturn(codeableConcept);
        when(medication.getMedicationCodeableConcept()).thenReturn(codeableConcept);
        when(diagnostic.getCode()).thenReturn(codeableConcept);
        when(observation.getValueCodeableConcept()).thenReturn(codeableConcept);

        //then
        NCDMedicalReviewHistoryDTO response = historyService.getNCDMedicalReviewSummaryHistory(requestDTO);
        assertNotNull(response);
        assertEquals(com.mdtlabs.coreplatform.commonservice.common.Constants.STRING_ONE, response.getPatientVisitId());

    }

//    @Test
    void getHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        medicalReviewRequestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        medicalReviewRequestDTO.setEncounterId(TestConstants.STRING_THREE);
        TestDataProvider.init();
        String fhirServerUrl = "http://fhirserver.com";
        FhirContext fhirContext = FhirContext.forR4();
        IGenericClient client = fhirContext.newRestfulGenericClient(fhirServerUrl);

        //when
        when(fhirUtils.getClient(null, null, null)).thenReturn(client);

        //then
        historyService.getHistory(medicalReviewRequestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPncHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        medicalReviewRequestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        medicalReviewRequestDTO.setEncounterId(TestConstants.STRING_THREE);
        TestDataProvider.init();

        //when
        when(patientService.getPatientDetailsByPatientReference(TestConstants.PATIENT_REFERENCE)).thenReturn(TestDataProvider.getRelatedPersonBundle());

        //then
        MedicalReviewHistoryDTO result = historyService.getPncHistory(medicalReviewRequestDTO);
        Assertions.assertNull(result);
        TestDataProvider.cleanUp();
    }
}