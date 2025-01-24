package com.mdtlabs.coreplatform.spiceservice.medicalreview.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.spiceservice.medicalreview.service.MedicalReviewService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import org.springframework.http.HttpStatus;

/**
 * <p>
 * MedicalReviewControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in MedicalReviewController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MedicalReviewControllerTest {

    @InjectMocks
    MedicalReviewController medicalReviewController;

    @Mock
    MedicalReviewService medicalReviewService;

    @Test
    void createGeneralMedicalReview() {
        //given
        GeneralMedicalReviewDTO generalMedicalReviewDTO = TestDataProvider.getGeneralMedicalReviewDTO();

        //when
        when(medicalReviewService.createGeneralMedicalReview(generalMedicalReviewDTO)).thenReturn(new HashMap<>());
        //then
        SuccessResponse<Map<String, String>> response = medicalReviewController.createGeneralMedicalReview(generalMedicalReviewDTO);
        assertNotNull(response);
    }

    @Test
    void getMedicalReviewDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getGeneralMedicalReviewDetails(requestDTO)).thenReturn(new GeneralMedicalReviewSummaryDetailsDTO());
        //then
        SuccessResponse<GeneralMedicalReviewSummaryDetailsDTO> response = medicalReviewController.getMedicalReviewDetails(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createSummaryDetails() {
        //given
        GeneralMedicalReviewSummaryDTO requestDTO = new GeneralMedicalReviewSummaryDTO();

        //when
        when(medicalReviewService.saveSummaryDetails(requestDTO)).thenReturn(new GeneralMedicalReviewSummaryDTO());
        //then
        SuccessResponse<GeneralMedicalReviewSummaryDTO> response = medicalReviewController.createSummaryDetails(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createICCMUnder2months() {
        //given
        UnderFiveIccmDTO requestDTO = new UnderFiveIccmDTO();

        //when
        when(medicalReviewService.createICCMUnder2months(requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        SuccessResponse<Map<String, String>> response = medicalReviewController.createICCMUnder2months(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getIccmUnder2MSummary() {
        //given
        MedicalReviewRequestDTO requestDTO = new MedicalReviewRequestDTO();

        //when
        when(medicalReviewService.getIccmUnder2MSummary(requestDTO)).thenReturn(new IccmResponseDTO());
        //then
        SuccessResponse<IccmResponseDTO> response = medicalReviewController.getIccmUnder2MSummary(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createICCMUnder5Years() {
        //given
        UnderFiveIccmDTO requestDTO = new UnderFiveIccmDTO();

        //when
        when(medicalReviewService.createICCMUnder5Years(requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        SuccessResponse<Map<String, String>> response = medicalReviewController.createICCMUnder5Years(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getMedicalReviewHistory() {
        //given
        MedicalReviewRequestDTO requestDTO = new MedicalReviewRequestDTO();

        //when
        when(medicalReviewService.getHistory(requestDTO)).thenReturn(new MedicalReviewHistoryDTO());
        //then
        SuccessResponse<MedicalReviewHistoryDTO> response = medicalReviewController.getMedicalReviewHistory(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createPnc() {
        //given
        PncMedicalReviewDTO requestDTO = new PncMedicalReviewDTO();

        //when
        when(medicalReviewService.savePncMedicalReview(requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        SuccessResponse<Map<String, String>> response = medicalReviewController.createPnc(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getIccmUnder2YSummary() {
        //given
        MedicalReviewRequestDTO requestDTO = new MedicalReviewRequestDTO();

        //when
        when(medicalReviewService.getIccmUnder5YSummary(requestDTO)).thenReturn(new IccmResponseDTO());
        //then
        SuccessResponse<IccmResponseDTO> response = medicalReviewController.getIccmUnder2YSummary(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getPregnancyMedicalReviewDetails(requestDTO)).thenReturn(new MedicalReviewPregnancySummaryDetailsDTO());
        //then
        SuccessResponse<MedicalReviewPregnancySummaryDetailsDTO> response = medicalReviewController.getPregnancyMedicalReviewDetails(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createGeneralMedicalReviewTest() {
        //given
        MedicalReviewPregnancyDTO requestDTO = new MedicalReviewPregnancyDTO();

        //when
        when(medicalReviewService.createMedicalReview(requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        SuccessResponse<Map<String, String>> response = medicalReviewController.createGeneralMedicalReview(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createLabourMotherAndNeonateMedicalReview() {
        //given
        MotherNeonateDTO requestDTO = new MotherNeonateDTO();

        //when
        when(medicalReviewService.createLabourMotherAndNeonate(requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        SuccessResponse<Map<String, String>> response = medicalReviewController.createLabourMotherAndNeonateMedicalReview(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getLabourMotherAndNeonateDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getLabourMotherAndNeonateDetails(requestDTO)).thenReturn(new MotherNeonateDTO());
        //then
        SuccessResponse<MotherDTO> response = medicalReviewController.getLabourMotherAndNeonateDetails(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getPncDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getPNCMedicalReviewDetails(requestDTO)).thenReturn(new PncMedicalReviewDTO());
        //then
        SuccessResponse<PncMedicalReviewDTO> response = medicalReviewController.getPncDetails(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createBp() {
        //given
        ObservationDTO requestDTO = new ObservationDTO();

        //when
        when(medicalReviewService.createBp(requestDTO)).thenReturn(new ObservationDTO());
        //then
        SuccessResponse<ObservationDTO> response = medicalReviewController.createBp(requestDTO);
        assertNotNull(response);
    }

    @Test
    void createWeight() {
        //given
        ObservationDTO requestDTO = new ObservationDTO();

        //when
        when(medicalReviewService.createWeight(requestDTO)).thenReturn(new ObservationDTO());
        //then
        SuccessResponse<ObservationDTO> response = medicalReviewController.createWeight(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getWeight() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getPatientWeight(requestDTO)).thenReturn(new HashMap<String, Double>());
        //then
        SuccessResponse<Map<String, Double>> response = medicalReviewController.getWeight(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getBp() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getPatientBp(requestDTO)).thenReturn(new HashMap<String, Double>());
        //then
        SuccessResponse<Map<String, Double>> response = medicalReviewController.getBp(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getBirthHistory() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(medicalReviewService.getBirthHistory(requestDTO)).thenReturn(new BirthHistoryDTO());

        //then
        SuccessResponse<BirthHistoryDTO> response = medicalReviewController.getBirthHistory(requestDTO);
        assertNotNull(response);
    }

    @Test
    void getPncMedicalReviewHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = new MedicalReviewRequestDTO();

        //then
        SuccessResponse<MedicalReviewHistoryDTO> response = medicalReviewController.getPncMedicalReviewHistory(medicalReviewRequestDTO);
        assertNotNull(response);
    }

    @Test
    void updateConfirmDiagnosis() {
        //given
        ConfirmDiagnosisDTO confirmDiagnosisDTO = TestDataProvider.getConfirmDiagnosisDTO();

        //when
        doNothing().when(medicalReviewService).updateConfirmDiagnosis(confirmDiagnosisDTO);

        //then
        SuccessResponse<ConfirmDiagnosisDTO> response = medicalReviewController.updateConfirmDiagnosis(confirmDiagnosisDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createPncChild() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        doNothing().when(medicalReviewService).createPncChild(householdMemberDTO);

        //then
        SuccessResponse<String> response = medicalReviewController.createPncChild(householdMemberDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMedicalReviewCountTest() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        Map<String, Integer> response = Map.of(TestConstants.PRESCRIPTION_DAYS_COMPLETED_COUNT, TestConstants.INT_ONE);

        //when
        when(medicalReviewService.getMedicalReviewCount(request)).thenReturn(response);

        //then
        SuccessResponse<Map<String, Integer>> actualResponse = medicalReviewController.getMedicalReviewCount(request);
        assertNotNull(actualResponse);
        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void getNCDMedicalReviewHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(medicalReviewService.getNCDMedicalReviewHistory(medicalReviewRequestDTO)).thenReturn(new NCDMedicalReviewHistoryDTO());

        //then
        SuccessResponse<NCDMedicalReviewHistoryDTO> response = medicalReviewController.getNCDMedicalReviewHistory(medicalReviewRequestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getNCDMedicalReviewSummaryHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(medicalReviewService.getNCDMedicalReviewSummaryHistory(medicalReviewRequestDTO)).thenReturn(new NCDMedicalReviewHistoryDTO());

        //then
        SuccessResponse<NCDMedicalReviewHistoryDTO> response = medicalReviewController.getNCDMedicalReviewSummaryHistory(medicalReviewRequestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    void testCreatePatientStatus() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        SuccessResponse<PatientStatusDTO> patientStatusResponse = medicalReviewController.createPatientStatus(patientStatusDTO);
        Assertions.assertEquals(HttpStatus.OK, patientStatusResponse.getStatusCode());
    }

    @Test
    void testGetPatientStatusDetails() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        SuccessResponse<PatientStatusDTO> patientStatusResponse = medicalReviewController.getPatientStatusDetails(patientStatusDTO);
        Assertions.assertEquals(HttpStatus.OK, patientStatusResponse.getStatusCode());
    }

    @Test
    void testCreateNcdMedicalReview() {
        NCDMedicalReviewDTO ncdMedicalReviewDTO = new NCDMedicalReviewDTO();
        SuccessResponse<Map<String, String>> patientStatusResponse = medicalReviewController.createNcdMedicalReview(ncdMedicalReviewDTO);
        Assertions.assertEquals(HttpStatus.CREATED, patientStatusResponse.getStatusCode());
    }

    @Test
    void testNcdMedicalReviewSummary() {
        MedicalReviewRequestDTO medicalReviewRequestDTO = new MedicalReviewRequestDTO();
        SuccessResponse<NcdMedicalReviewResponse> patientStatusResponse = medicalReviewController.getNcdMedicalReviewDetails(medicalReviewRequestDTO);
        Assertions.assertEquals(HttpStatus.OK, patientStatusResponse.getStatusCode());
    }

    @Test
    void testUpdateViewStatus() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        //when
        when(medicalReviewService.updateViewCount(request)).thenReturn(Boolean.TRUE);

        //then
        SuccessResponse<Boolean> actualResponse = medicalReviewController.updateViewCount(request);
        assertNotNull(actualResponse);
        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void getPatientLifestyleDetails() {
        RequestDTO request = new RequestDTO();
        when(medicalReviewService.getPatientLifestyleDetails(request)).thenReturn(List.of());
        SuccessResponse<LifestyleResponseDTO> response = medicalReviewController.getPatientLifestyleDetails(request);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getInstruction() {
        when(medicalReviewService.getInstruction()).thenReturn(List.of());
        SuccessResponse<Map<String, List<String>>> response = medicalReviewController.getInstruction();
        assertNotNull(response);
    }

    @Test
    void updateAppointmentDate() {
        NCDMedicalReviewDTO request = new NCDMedicalReviewDTO();
        doNothing().when(medicalReviewService).updateNCDAppointment(request);
        SuccessResponse response = medicalReviewController.updateAppointmentDate(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPatientStatus() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        when(medicalReviewService.createPatientStatus(patientStatusDTO)).thenReturn(patientStatusDTO);
        SuccessResponse<PatientStatusDTO> response = medicalReviewController.createPatientStatus(patientStatusDTO);
        assertNotNull(response);
    }

    @Test
    void getPatientStatusDetails() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        when(medicalReviewService.createPatientStatus(patientStatusDTO)).thenReturn(patientStatusDTO);
        SuccessResponse<PatientStatusDTO> response = medicalReviewController.getPatientStatusDetails(patientStatusDTO);
        assertNotNull(response);
    }

    @Test
    void createSummary() {
        NCDMedicalReviewDTO request = new NCDMedicalReviewDTO();
        doNothing().when(medicalReviewService).createSummary(request);
        SuccessResponse response = medicalReviewController.createSummary(request);
        assertNotNull(response);
    }

}
