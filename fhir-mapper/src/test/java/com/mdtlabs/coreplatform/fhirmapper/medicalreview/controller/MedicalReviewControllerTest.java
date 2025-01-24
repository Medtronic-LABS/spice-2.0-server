package com.mdtlabs.coreplatform.fhirmapper.medicalreview.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.HistoryService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.IccmMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.LabourService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyANCService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyPNCService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl.GeneralMedicalReviewServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.NcdMedicalReviewService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MedicalReviewControllerTest {

    @InjectMocks
    MedicalReviewController medicalReviewController;

    @Mock
    GeneralMedicalReviewServiceImpl generalMedicalReviewService;

    @Mock
    MedicalReviewPregnancyPNCService medicalPncReviewService;

    @Mock
    MedicalReviewPregnancyANCService medicalReviewANCService;

    @Mock
    IccmMedicalReviewService iccmMedicalReviewService;

    @Mock
    LabourService labourService;

    @Mock
    HistoryService historyService;

    @Mock
    private NcdMedicalReviewService ncdMedicalReviewService;

    @Test
    void createGeneralMedicalReview() {
        //given
        GeneralMedicalReviewDTO generalMedicalReviewDTO = TestDataProvider.getGeneralMedicalReviewDTO();

        //when
        when(generalMedicalReviewService.createGeneralMedicalReview(generalMedicalReviewDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = medicalReviewController.createGeneralMedicalReview(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMedicalReviewDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        GeneralMedicalReviewSummaryDetailsDTO generalMedicalReviewSummaryDetailsDTO = TestDataProvider.getGeneralMedicalReviewSummaryDetailsDTO();

        //when
        when(generalMedicalReviewService.getGeneralMedicalReviewDetails(requestDTO.getId(), requestDTO.getPatientReference())).thenReturn(generalMedicalReviewSummaryDetailsDTO);

        //then
        GeneralMedicalReviewSummaryDetailsDTO response = medicalReviewController.getMedicalReviewDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createICCMUnder2months() {
        //given
        UnderFiveIccmDTO underFiveIccmDTO = new UnderFiveIccmDTO();

        //when
        when(iccmMedicalReviewService.createMedicalReviewForUnder5years(underFiveIccmDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = medicalReviewController.createICCMUnder2months(underFiveIccmDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getIccmUnder2MSummary() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        IccmResponseDTO iccmResponseDTO = TestDataProvider.getIccmResponseDTO();

        //when
        when(iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(medicalReviewRequestDTO)).thenReturn(iccmResponseDTO);

        //then
        IccmResponseDTO response = medicalReviewController.getIccmUnder2MSummary(medicalReviewRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createICCMUnder5Years() {
        //given
        UnderFiveIccmDTO underFiveIccmDTO = new UnderFiveIccmDTO();

        //when
        when(iccmMedicalReviewService.createMedicalReviewForUnder5years(underFiveIccmDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = medicalReviewController.createICCMUnder5Years(underFiveIccmDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getIccmUnder2YSummary() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        IccmResponseDTO iccmResponseDTO = TestDataProvider.getIccmResponseDTO();

        //when
        when(iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(medicalReviewRequestDTO)).thenReturn(iccmResponseDTO);

        //then
        IccmResponseDTO response = medicalReviewController.getIccmUnder2YSummary(medicalReviewRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createSummaryDetails() {
        //given
        GeneralMedicalReviewSummaryDTO generalMedicalReviewSummaryDTO = TestDataProvider.getGeneralMedicalReviewSummaryDTO();

        //when
        when(generalMedicalReviewService.saveSummaryDetails(generalMedicalReviewSummaryDTO)).thenReturn(generalMedicalReviewSummaryDTO);

        //then
        GeneralMedicalReviewSummaryDTO response = medicalReviewController.createSummaryDetails(generalMedicalReviewSummaryDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMedicalReviewHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        MedicalReviewHistoryDTO medicalReviewHistoryDTO = TestDataProvider.getMedicalReviewHistoryDTO();

        //when
        when(historyService.getHistory(medicalReviewRequestDTO)).thenReturn(medicalReviewHistoryDTO);

        //then
        MedicalReviewHistoryDTO response = medicalReviewController.getMedicalReviewHistory(medicalReviewRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMedicalReviewPncHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        MedicalReviewHistoryDTO medicalReviewHistoryDTO = TestDataProvider.getMedicalReviewHistoryDTO();

        //when
        when(historyService.getPncHistory(medicalReviewRequestDTO)).thenReturn(medicalReviewHistoryDTO);

        //then
        MedicalReviewHistoryDTO response = medicalReviewController.getMedicalReviewPncHistory(medicalReviewRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createGeneralMedicalReviewANC() {
        //given
        MedicalReviewPregnancyDTO medicalReviewPregnancyDTO = TestDataProvider.getMedicalReviewPregnancyDTO();

        //when
        when(medicalReviewANCService.createMedicalReview(medicalReviewPregnancyDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = medicalReviewController.createGeneralMedicalReview(medicalReviewPregnancyDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPreviousHistory(Boolean.TRUE);
        requestDTO.setPatientReference(TestConstants.TWO_STR);
        MedicalReviewPregnancySummaryDetailsDTO medicalReviewPregnancySummaryDetailsDTO = TestDataProvider.getMedicalReviewPregnancySummaryDetailsDTO();

        //when
        when(medicalReviewANCService.getLatestEncounter(requestDTO)).thenReturn(medicalReviewPregnancySummaryDetailsDTO);

        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewController.getPregnancyMedicalReviewDetails(requestDTO);
        Assertions.assertNotNull(response);

        //given
        requestDTO.setPreviousHistory(Boolean.FALSE);

        //when
        when(medicalReviewANCService.getPregnancyMedicalReviewDetails(requestDTO.getId(), requestDTO.getPatientReference())).thenReturn(medicalReviewPregnancySummaryDetailsDTO);

        //then
        response = medicalReviewController.getPregnancyMedicalReviewDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPnc() {
        //given
        PncMedicalReviewDTO pncMedicalReviewDTO = new PncMedicalReviewDTO();

        //when
        when(medicalPncReviewService.savePncMedicalReview(pncMedicalReviewDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = medicalReviewController.createPnc(pncMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPncDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientReference(TestConstants.TWO_STR);
        requestDTO.setChildId(TestConstants.UNIQUE_ID);
        PncMedicalReviewDTO pncMedicalReviewDTO = new PncMedicalReviewDTO();

        //when
        when(medicalPncReviewService.getPNCMedicalReviewDetails(requestDTO.getId(), requestDTO.getChildId(), requestDTO.getPatientReference())).thenReturn(pncMedicalReviewDTO);

        //then
        PncMedicalReviewDTO response = medicalReviewController.getPncDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createLabourMotherAndNeonateMedicalReview() {
        //given
        MotherNeonateDTO motherNeonateDTO = TestDataProvider.getMotherNeonateDTO();

        //when
        when(labourService.createLabourMotherAndNeonateMedicalReview(motherNeonateDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = medicalReviewController.createLabourMotherAndNeonateMedicalReview(motherNeonateDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getLabourMotherAndNeonateDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        MotherNeonateDTO motherNeonateDTO = TestDataProvider.getMotherNeonateDTO();

        //when
        when(labourService.getLabourMotherAndNeonateDetails(requestDTO)).thenReturn(motherNeonateDTO);

        //then
        MotherNeonateDTO response = medicalReviewController.getLabourMotherAndNeonateDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createBp() {
        //given
        ObservationDTO observationDTO = TestDataProvider.getObservationDTO();

        //when
        when(medicalReviewANCService.createValueObservation(observationDTO)).thenReturn(observationDTO);

        //then
        ObservationDTO response = medicalReviewController.createBp(observationDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createWeight() {
        //given
        ObservationDTO observationDTO = TestDataProvider.getObservationDTO();

        //when
        when(medicalReviewANCService.createValueObservation(observationDTO)).thenReturn(observationDTO);

        //then
        ObservationDTO response = medicalReviewController.createWeight(observationDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getWeight() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(medicalReviewANCService.getPatientWeight(requestDTO)).thenReturn(Map.of());

        //then
        Map<String, Double> response = medicalReviewController.getWeight(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getBp() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(medicalReviewANCService.getPatientBp(requestDTO)).thenReturn(Map.of());

        //then
        Map<String, Double> response = medicalReviewController.getBp(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getBirthHistory() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setMemberId(TestConstants.TWO_STR);
        BirthHistoryDTO birthHistoryDTO = TestDataProvider.getBirthHistoryDTO();

        //when
        when(labourService.getBirthHistory(requestDTO.getMemberId())).thenReturn(birthHistoryDTO);

        //then
        BirthHistoryDTO response = medicalReviewController.getBirthHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateConfirmDiagnosis() {
        //given
        ConfirmDiagnosisDTO confirmDiagnosisDTO = TestDataProvider.getConfirmDiagnosisDTO();

        //when
        doNothing().when(generalMedicalReviewService).updateConfirmDiagnosis(confirmDiagnosisDTO);

        //then
        medicalReviewController.updateConfirmDiagnosis(confirmDiagnosisDTO);
        verify(generalMedicalReviewService, atLeastOnce()).updateConfirmDiagnosis(confirmDiagnosisDTO);
    }

    @Test
    void createPncChild() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        doNothing().when(medicalPncReviewService).createPncChild(householdMemberDTO);

        //then
        medicalReviewController.createPncChild(householdMemberDTO);
        verify(medicalPncReviewService).createPncChild(householdMemberDTO);
    }

    @Test
    void testMedicalReviewCount() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        Map<String, Integer> response = Map.of(TestConstants.PRESCRIPTION_DAYS_COMPLETED_COUNT, TestConstants.INT_ONE);

        //when
        when(generalMedicalReviewService.getMedicalReviewCount(request)).thenReturn(response);

        //then
        Map<String, Integer> actualResponse = medicalReviewController.getMedicalReviewCount(request);
        assertNotNull(actualResponse);
        assertEquals(TestConstants.INT_ONE, actualResponse.get(TestConstants.PRESCRIPTION_DAYS_COMPLETED_COUNT));
    }

    @Test
    void getNCDMedicalReviewHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(historyService.getNCDMedicalReviewHistory(medicalReviewRequestDTO)).thenReturn(new NCDMedicalReviewHistoryDTO());
        //then
        NCDMedicalReviewHistoryDTO response = medicalReviewController.getNCDMedicalReviewHistory(medicalReviewRequestDTO);
        assertNotNull(response);
    }

    @Test
    void getNCDMedicalReviewSummaryHistory() {
        //given
        MedicalReviewRequestDTO medicalReviewRequestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(historyService.getNCDMedicalReviewSummaryHistory(medicalReviewRequestDTO)).thenReturn(new NCDMedicalReviewHistoryDTO());

        //then
        NCDMedicalReviewHistoryDTO response = medicalReviewController.getNCDMedicalReviewSummaryHistory(medicalReviewRequestDTO);
        assertNotNull(response);
    }

    @Test
    void testUpdateViewStatus() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        //when
        when(generalMedicalReviewService.updateViewCount(request)).thenReturn(Boolean.TRUE);

        //then
        Boolean actualResponse = medicalReviewController.updateViewCount(request);
        assertNotNull(actualResponse);
        assertEquals(Boolean.TRUE, actualResponse);
    }

    @Test
    void createChild() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        when(medicalPncReviewService.createChild(householdMemberDTO)).thenReturn(new HouseholdMemberDTO());

        //then
        HouseholdMemberDTO result = medicalReviewController.createChild(householdMemberDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void updateAppointmentDate() {
        //given
        NCDMedicalReviewDTO request = TestDataProvider.getNCDMedicalReviewDTO();

        medicalReviewController.updateAppointmentDate(request);
        //then
        verify(ncdMedicalReviewService, times(1)).addOrUpdateNextVisitDate(request);
    }

    @Test
    void getPatientLifestyleDetails() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        List<LifestyleResponseDTO> lifestyleResponseDTOS = new ArrayList<>();

        //when
        when(ncdMedicalReviewService.getPatientLifestyle(request)).thenReturn(lifestyleResponseDTOS);

        //then
        List<LifestyleResponseDTO> result = medicalReviewController.getPatientLifestyleDetails(request);
        Assertions.assertNotNull(result);
    }

    @Test
    void createSummary() {
        //test
        NCDMedicalReviewDTO request = TestDataProvider.getNCDMedicalReviewDTO();

        //then
        medicalReviewController.createSummary(request);
        verify(ncdMedicalReviewService, atLeastOnce()).addOrUpdateNextVisitDate(request);
    }

    @Test
    void getNcdMedicalReviewDetails() {
        //given
        MedicalReviewRequestDTO request = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(ncdMedicalReviewService.ncdMedicalReviewSummary(request)).thenReturn(new NcdMedicalReviewResponse());

        //then
        NcdMedicalReviewResponse result = medicalReviewController.getNcdMedicalReviewDetails(request);
        Assertions.assertNotNull(result);
    }

    @Test
    void createNcdMedicalReview() {
        //given
        NCDMedicalReviewDTO request = TestDataProvider.getNCDMedicalReviewDTO();

        //when
        when(ncdMedicalReviewService.createNcdMedicalReview(request)).thenReturn(new HashMap<>());

        //then
        Map<String, String> result = medicalReviewController.createNcdMedicalReview(request);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPatientStatusDetails() {
        //given
        PatientStatusDTO patientStatus = new PatientStatusDTO();

        //when
        when(generalMedicalReviewService.getPatientStatusDetails(patientStatus)).thenReturn(new PatientStatusDTO());

        //then
        PatientStatusDTO result = medicalReviewController.getPatientStatusDetails(patientStatus);
        Assertions.assertNotNull(result);
    }

    @Test
    void createPatientStatus() {
        //given
        PatientStatusDTO patientStatus = new PatientStatusDTO();

        //when
        when(generalMedicalReviewService.createPatientStatus(patientStatus)).thenReturn(new PatientStatusDTO());

        //then
        PatientStatusDTO result = medicalReviewController.createPatientStatus(patientStatus);
        Assertions.assertNotNull(result);
    }
}