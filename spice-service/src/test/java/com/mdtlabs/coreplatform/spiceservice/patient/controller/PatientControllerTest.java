package com.mdtlabs.coreplatform.spiceservice.patient.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patient.service.PatientService;

/**
 * <p>
 * PatientControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in PatientController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientControllerTest {

    @InjectMocks
    PatientController patientController;

    @Mock
    PatientService patientService;

    @Test
    void getPatient() {
        //given
        PatientRequestDTO requestDTO = new PatientRequestDTO();

        //when
        when(patientService.getPatientList(requestDTO)).thenReturn(new HashMap<String, Object>());

        //then
        SuccessResponse<Map<String, Object>> response = patientController.getPatient(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void searchPatient() {
        //given
        PatientRequestDTO requestDTO = new PatientRequestDTO();
        SelectedAppTypeContextHolder.set(Constants.NON_COMMUNITY);

        //when
        when(patientService.searchPatient(requestDTO)).thenReturn(new HashMap<String, Object>());

        //then
        SuccessResponse<Map<String, Object>> response = patientController.searchPatient(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientDetails() {
        //given
        PatientDTO requestDTO = new PatientDTO();
        SelectedAppTypeContextHolder.set(Constants.NON_COMMUNITY);

        //when
        when(patientService.getPatientDetails(requestDTO)).thenReturn(new PatientDetailsDTO());

        //then
        SuccessResponse<PatientDetailsDTO> response = patientController.getPatientDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientStatus() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(patientService.getPatientStatus(requestDTO)).thenReturn(new HashMap<String, Object>());

        //then
        SuccessResponse<Map<String, Object>> response = patientController.getPatientStatus(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientStatus() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(patientService.updateStatusOfServiceRequest(requestDTO)).thenReturn(Boolean.TRUE);

        //then
        SuccessResponse<Boolean> response = patientController.updatePatientStatus(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getReferralTicket() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(patientService.getReferralTickets(requestDTO)).thenReturn(List.of(new ReferralTicketDTO()));

        //then
        SuccessResponse<ReferralTicketDTO> response = patientController.getReferralTicket(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createReferralTicket() {
        //given
        ReferralDetailsDTO requestDTO = new ReferralDetailsDTO();

        //when
        when(patientService.createReferralTicket(requestDTO)).thenReturn(new ReferralDetailsDTO());

        //then
        SuccessResponse<ReferralDetailsDTO> response = patientController.createReferralTicket(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientDiagnosis() {
        //given
        DiagnosisDTO requestDTO = new DiagnosisDTO();

        //then
        SuccessResponse<Boolean> response = patientController.updatePatientDiagnosis(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientDiagnosis() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(patientService.getPatientDiagnosis(requestDTO)).thenReturn(List.of(new DiagnosisDTO.DiseaseDTO()));

        //then
        SuccessResponse<DiagnosisDTO.DiseaseDTO> response = patientController.getPatientDiagnosis(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientDiagnosisDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(patientService.getPatientDiagnosisDetails(requestDTO)).thenReturn(new ConfirmDiagnosisDTO());

        //then
        SuccessResponse<ConfirmDiagnosisDTO> response = patientController.getPatientDiagnosisDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyInfoByVillages() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //then
        List<PregnancyInfo> response = patientController.getPregnancyInfoByVillages(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPatientByPatientId() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //then
        ResponseEntity<String> response = patientController.createPatientByPatientId(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientWgsData() {
        //given
        WgsDataDTO wgsDataDTO = new WgsDataDTO();

        //when
        when(patientService.getPatientWgsData(wgsDataDTO)).thenReturn(new HashMap<>());

        //then
        SuccessResponse<Map<String, Object>> response = patientController.getPatientWgsData(wgsDataDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatient() {
        //given
        EnrollmentRequestDTO requestDTO = new EnrollmentRequestDTO();
        //then
        SuccessResponse<EnrollmentResponseDTO> response = patientController.updatePatient(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPregnancyDetails() {
        //given
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        //when
        when(patientService.createPregnancyDetails(pregnancyDetailsDTO)).thenReturn(pregnancyDetailsDTO);
        //then
        SuccessResponse<PregnancyDetailsDTO> response = patientController.createPregnancyDetails(pregnancyDetailsDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyDetails() {
        //given
        RequestDTO requestData = new RequestDTO();
        //when
        when(patientService.getPregnancyDetails(requestData)).thenReturn(new PregnancyDetailsDTO());
        //then
        SuccessResponse<PregnancyDetailsDTO> response = patientController.getPregnancyDetails(requestData);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePregnancyANCRisk() {
        //given
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setIsPregnant(Boolean.TRUE);
        //when
        when(patientService.updatePregnancyANCRisk(pregnancyDetailsDTO)).thenReturn(Boolean.TRUE);
        //then
        SuccessResponse<Boolean> response = patientController.updatePregnancyANCRisk(pregnancyDetailsDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void deletePatientByPatientId() {
        //given
        RequestDTO request = new RequestDTO();
        //when
        doNothing().when(patientService).deletePatientByPatientId(request);
        //then
        SuccessResponse<String> response = patientController.deletePatientByPatientId(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateReferredSite() {
        //given
        ScreeningLogRequestDTO requestDTO = new ScreeningLogRequestDTO();
        //when
        doNothing().when(patientService).updateReferredSite(requestDTO);
        //then
        SuccessResponse<String> response = patientController.updateReferredSite(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void listPatientDetails() {
        //given
        RequestDTO request = new RequestDTO();
        //when
        when(patientService.getPatientDetailsByVillageIds(request)).thenReturn(List.of(new PatientDetailsDTO()));
        //then
        List<PatientDetailsDTO> response = patientController.listPatientDetails(request);
        Assertions.assertNotNull(response);
    }
}
