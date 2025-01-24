package com.mdtlabs.coreplatform.fhirmapper.patient.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Patient;
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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientFilterDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

/**
 * <p>
 * PatientControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in PatientController class.
 * </p>
 *
 * @author Nandhakumar
 * @since Feb 8, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientControllerTest {

    @InjectMocks
    PatientController patientController;

    @Mock
    PatientService patientService;

    @Mock
    private FhirMapper fhirMapper;

    @Test
    void createPatientByPatientId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.createPatientByPatientId(requestDTO)).thenReturn(TestConstants.STRING_VALUE);

        //then
        String response = patientController.createPatientByPatientId(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatient() {
        when(patientService.getPatientList(new PatientRequestDTO())).thenReturn(Map.of());
        Map<String, Object> map =  patientController.getPatient(new PatientRequestDTO());
        assertNotNull(map);
    }

    @Test
    void getPatientDetails() {
        RequestDTO requestDTO = new RequestDTO();
        when(patientService.getPatientDetails(requestDTO)).thenReturn(new PatientDetailsDTO());
        PatientDetailsDTO patientDetailsDTO = patientController.getPatientDetails(requestDTO);
        assertNotNull(patientDetailsDTO);
    }

    @Test
    void searchPatient() {
        //given
        PatientRequestDTO requestDTO = new PatientRequestDTO();

        //when
        when(patientService.searchPatient(requestDTO)).thenReturn(Map.of());

        //then
        Map<String, Object>  patientDetailsDTO = patientController.searchPatient(requestDTO);
        Assertions.assertNotNull(patientDetailsDTO);

        //given
        PatientFilterDTO filterDTO = TestDataProvider.getPatientFilterDTO();
        filterDTO.setPatientStatus(List.of(TestConstants.RECOVERED));
        requestDTO.setFilter(filterDTO);

        //when
        when(patientService.getPatientList(requestDTO)).thenReturn(Map.of());

        //then
        patientDetailsDTO = patientController.searchPatient(requestDTO);
        Assertions.assertNotNull(patientDetailsDTO);
    }

    @Test
    void getPatientStatus() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientId(TestConstants.ONE_TWO_THREE_FOUR);
        requestDTO.setAssessmentType(List.of(TestConstants.ACTIVE_UPPER));
        Map<String, Object> patientDetailsDTO = patientController.getPatientStatus(requestDTO);
        assertNotNull(patientDetailsDTO);
    }

    @Test
    void updatePatientStatus() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientId(TestConstants.ONE_TWO_THREE_FOUR);
        requestDTO.setAssessmentType(List.of(TestConstants.ACTIVE_UPPER));
        when(patientService.updateStatusOfServiceRequest(requestDTO)).thenReturn(new ResponseEntity<>(HttpStatus.OK));
        ResponseEntity patientDetailsDTO = patientController.updatePatientStatus(requestDTO);
        assertNotNull(patientDetailsDTO);
    }

    @Test
    void createReferralTicket() {
        //given
        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();

        //when
        when(patientService.createReferralTicketForMedicalReview(referralDetailsDTO,
                new Bundle().setType(Bundle.BundleType.TRANSACTION), Boolean.TRUE, Boolean.FALSE)).thenReturn(referralDetailsDTO);

        //then
        ReferralDetailsDTO response = patientController.createReferralTicket(referralDetailsDTO);
        Assertions.assertNull(response);
    }

    @Test
    void getReferralTicket() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(TestConstants.PATIENT_ID);
        requestDTO.setTicketId(TestConstants.TWO_STR);
        requestDTO.setType(TestConstants.ICCM);
        requestDTO.setTicketType(TestConstants.ASSESSMENT);
        List<ReferralTicketDTO> referralTicketDTOS = List.of(TestDataProvider.getReferralTicketDTO());

        //when
        when(patientService.getReferralTicketes(requestDTO.getPatientId(), requestDTO.getTicketId(),
                requestDTO.getType(), requestDTO.getTicketType())).thenReturn(referralTicketDTOS);

        //then
        List<ReferralTicketDTO> response = patientController.getReferralTicket(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientDiagnosis() {
        //given
        DiagnosisDTO diagnosisDTO = TestDataProvider.getDiagnosisDTO();

        //when
        doNothing().when(patientService).createDiagnosis(diagnosisDTO);

        //then
        patientController.updatePatientDiagnosis(diagnosisDTO);
        verify(patientService).createDiagnosis(diagnosisDTO);
    }

    @Test
    void getPatientDiagnosis() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.getPatientDiagnosis(requestDTO, Boolean.FALSE, Boolean.TRUE)).thenReturn(List.of());

        //then
        List<DiagnosisDTO.DiseaseDTO> response = patientController.getPatientDiagnosis(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyInfoByVillages() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.getPregnancyInfoByVillages(requestDTO)).thenReturn(List.of());

        //then
        List<PregnancyInfo> response = patientController.getPregnancyInfoByVillages(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateReferralTicketByMemberId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        doNothing().when(patientService).updateReferralTicketByMemberId(requestDTO, null);

        //then
        patientController.updateReferralTicketByMemberId(requestDTO);
        verify(patientService).updateReferralTicketByMemberId(requestDTO, null);
    }

    @Test
    void getPatientVitals() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();

        //when
        when(patientService.getPatientVitals(requestDTO)).thenReturn(pregnancyInfo);

        //then
        PregnancyInfo response = patientController.getPatientVitals(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void listPatient() {
        //given
        PatientRequestDTO requestDTO = new PatientRequestDTO();

        //when
        when(patientService.getPatientList(requestDTO)).thenReturn(Map.of());

        //then
        Map<String, Object>  patientDetailsDTO = patientController.listPatient(requestDTO);
        Assertions.assertNotNull(patientDetailsDTO);
    }

    @Test
    void updatePatient(){
        //given
        EnrollmentRequestDTO enrollmentRequestDto = TestDataProvider.getEnrollmentRequestDTO();
        EnrollmentResponseDTO enrollmentResponseDto = new EnrollmentResponseDTO();
        //when
        when(patientService.updatePatient(enrollmentRequestDto)).thenReturn(enrollmentResponseDto);
        //then
        enrollmentResponseDto = patientController.updatePatient(enrollmentRequestDto);
        Assertions.assertNotNull(enrollmentResponseDto);
    }

    @Test
    void deletePatientByPatientId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        doNothing().when(patientService).deletePatientByPatientId(requestDTO);

        //then
        patientController.deletePatientByPatientId(requestDTO);
        verify(patientService).deletePatientByPatientId(requestDTO);
    }

    @Test
    void searchPatientDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.searchPatientDetails(requestDTO)).thenReturn(new PatientDetailsDTO());

        //then
        PatientDetailsDTO response = patientController.searchPatientDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void searchPatients() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();

        //when
        when(patientService.searchPatient(patientRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, Object> response = patientController.searchPatients(patientRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPregnancyDetails() {
        //given
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();

        //when
        when(patientService.createPregnancyDetails(pregnancyDetailsDTO)).thenReturn(new PregnancyDetailsDTO());

        //then
        PregnancyDetailsDTO response = patientController.createPregnancyDetails(pregnancyDetailsDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.getPregnancyDetails(requestDTO)).thenReturn(new PregnancyDetailsDTO());

        //then
        PregnancyDetailsDTO response = patientController.getPregnancyDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientDiagnosisDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.getPatientDiagnosisDetails(requestDTO, Boolean.FALSE)).thenReturn(new ConfirmDiagnosisDTO());

        //then
        ConfirmDiagnosisDTO response = patientController.getPatientDiagnosisDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientById() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Patient patient = TestDataProvider.getPatient();
        Bundle bundle = new Bundle();
        bundle.setEntry(TestDataProvider.getRelatedPersonBundle().getEntry());

        //when
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientId())).thenReturn(bundle);
        when(fhirMapper.toPatient(patient, new PatientDTO())).thenReturn(new PatientDTO());

        //then
        patientController.getPatientById(requestDTO);
        verify(fhirMapper, atLeastOnce()).toPatient(any(), any());
    }

    @Test
    void listPatients() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();

        //when
        when(patientService.listNcdPatients(patientRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, PatientDetailsDTO> result = patientController.listPatients(patientRequestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void listPatientDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientService.getPatientDetailsByVillageIds(requestDTO)).thenReturn(new ArrayList<>());

        //then
        List<PatientDetailsDTO> result = patientController.listPatientDetails(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void updateReferredSite() {
        //given
        ScreeningLogRequestDTO screeningLogRequestDTO = TestDataProvider.getScreeningLogRequest();

        //then
        patientController.updateReferredSite(screeningLogRequestDTO);
        verify(patientService, atLeastOnce()).updateReferredSite(screeningLogRequestDTO);
    }

    @Test
    void updatePregnancyANCRisk() {
        //given
        PregnancyDetailsDTO pregnancyDetailsDTO = TestDataProvider.getPregnancyDetailsDTO();

        //when
        when(patientService.updatePregnancyANCRisk(pregnancyDetailsDTO)).thenReturn(Boolean.TRUE);

        //then
        Boolean result = patientController.updatePregnancyANCRisk(pregnancyDetailsDTO);
        Assertions.assertNotNull(result);
    }

}