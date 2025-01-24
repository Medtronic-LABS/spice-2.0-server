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
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
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
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.household.service.impl.HouseholdServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.impl.PatientServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class IccmMedicalReviewServiceImplTest {

    @Mock
    PatientServiceImpl patientService;

    @InjectMocks
    IccmMedicalReviewServiceImpl iccmMedicalReviewService;

    @Mock
    FhirUtils fhirUtils;

    @Mock
    FhirMapper fhirMapper;

    @Mock
    FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    RestApiUtil restApiUtil;

    @Mock
    HouseholdServiceImpl householdService;

    @Mock
    PrescriptionRequestService prescriptionService;

    @Mock
    InvestigationService investigationService;

    @Test
    void createMedicalReviewForUnder5years() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        UnderFiveIccmDTO iccmRequest = TestDataProvider.getUnderFiveIccmData();
        iccmRequest.getEncounter().setPatientReference(null);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        RequestDTO request = new RequestDTO();
        request.setMemberId(TestConstants.STRING_VALUE);
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setDateOfDelivery(new Date());

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        doNothing().when(patientService).setPatientReferenceInEncounterDetails(iccmRequest.getEncounter(), bundle);
        when(patientService.createOrUpdateMedicalReviewEncounter(iccmRequest.getId(), iccmRequest.getEncounter(),
                iccmRequest.getAssessmentName(), null, bundle)).thenReturn(TestConstants.STRING_VALUE);
        when(householdService.getHouseholdMemberById(iccmRequest.getEncounter().getMemberId())).thenReturn(householdMemberDTO);
        when(patientService.getPatientVitals(request)).thenReturn(pregnancyInfo);
        when(patientService.createPatient(iccmRequest.getPatientId(), new Bundle(),
                iccmRequest.getEncounter().getProvenance())).thenReturn(Map.of(String.valueOf(ResourceType.Patient), ""));
        when(fhirAssessmentMapper.createEncounter(iccmRequest.getEncounter(), new Bundle(), iccmRequest.getAssessmentName(),
                null)).thenReturn(TestConstants.UNIQUE_ID);
        when(fhirAssessmentMapper.setObservation(any(), any(), any())).thenReturn(new Observation());
        when(fhirAssessmentMapper.createObservationComponent(any())).thenReturn(
                new Observation.ObservationComponentComponent());
        when(restApiUtil.postBatchRequest(null, null)).thenReturn(responseEntity);
        Map<String, List<String>> mapValue = new HashMap<>();
        mapValue.put(String.valueOf(ResourceType.Encounter), List.of(TestConstants.PATIENT_ID));
        mapValue.put(String.valueOf(ResourceType.Patient), List.of(TestConstants.PATIENT_ID));
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(mapValue);

        //then
        Map<String, String> response = iccmMedicalReviewService.createMedicalReviewForUnder5years(iccmRequest);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMedicalReviewSummaryByEncounter() {
        //given
        MedicalReviewRequestDTO medicalReviewRequest = TestDataProvider.getMedicalReviewRequestDTO();
        List<String> examinationName = new ArrayList<>();

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());

        //then
        IccmResponseDTO response = iccmMedicalReviewService.getMedicalReviewSummaryByEncounter(medicalReviewRequest, examinationName, TestConstants.DIAGNOSIS_TYPE);
        Assertions.assertNotNull(response);

        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        medicalReviewRequest.setEncounterId(TestConstants.TWO_STR);
        medicalReviewRequest.setPatientReference(TestConstants.TWO_STR);
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.SYSTEMIC_EXAMINATIONS);
        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(prescriptionService.getPrescriptionsByEncounter(medicalReviewRequest.getEncounterId(),
                medicalReviewRequest.getPatientReference())).thenReturn(List.of(prescriptionDTO));
        when(investigationService.getInvestigationsByEncounter(medicalReviewRequest.getEncounterId(),
                medicalReviewRequest.getPatientReference())).thenReturn(List.of(labTestDTO));
        when(fhirAssessmentMapper.getEncounterDetails(medicalReviewRequest.getEncounterId(),
                Boolean.TRUE)).thenReturn(bundle);

        //then
        response = iccmMedicalReviewService.getMedicalReviewSummaryByEncounter(medicalReviewRequest, examinationName, TestConstants.DIAGNOSIS_TYPE);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.CLINICAL_NOTES);
        observation.setCode(codeableConcept);
        //then
        response = iccmMedicalReviewService.getMedicalReviewSummaryByEncounter(medicalReviewRequest, examinationName, TestConstants.DIAGNOSIS_TYPE);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.PRESENTING_COMPLAINTS);
        observation.setCode(codeableConcept);
        //then
        response = iccmMedicalReviewService.getMedicalReviewSummaryByEncounter(medicalReviewRequest, examinationName, TestConstants.DIAGNOSIS_TYPE);
        Assertions.assertNotNull(response);
    }

    @Test
    void checkAndCloseRmnchDetails() {
        //given
        UnderFiveIccmDTO underFiveIccmDTO = TestDataProvider.getUnderFiveIccmData();
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
        iccmMedicalReviewService.checkAndCloseRmnchDetails(underFiveIccmDTO, bundle);
        verify(patientService, atLeastOnce()).getPatientVitals(any());
    }

    @Test
    void getMedicalReviewDetailsForUnderFive() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        medicalReviewRequestDTO.setAssessmentName("UNDER_FIVE_YEARS");

        //then
        IccmResponseDTO result = iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(medicalReviewRequestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void getMedicalReviewDetailsForUnderTwo() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        medicalReviewRequestDTO.setAssessmentName("UNDER_TWO_MONTHS");

        //then
        IccmResponseDTO result = iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(medicalReviewRequestDTO);
        Assertions.assertNotNull(result);
    }

}