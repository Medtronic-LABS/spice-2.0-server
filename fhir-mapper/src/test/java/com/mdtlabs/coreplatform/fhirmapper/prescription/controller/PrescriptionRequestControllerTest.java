package com.mdtlabs.coreplatform.fhirmapper.prescription.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.MedicationRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.IntensificationAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PrescriptionRequestControllerTest {

    @InjectMocks
    private PrescriptionRequestController prescriptionRequestController;

    @Mock
    private PrescriptionRequestService prescriptionRequestService;

    @Mock
    private IntensificationAlgorithm intensificationAlgorithm;

    @Test
    void getPrescriptions() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();

        //when
        when(prescriptionRequestService.getPrescriptions(requestDTO)).thenReturn(List.of(prescriptionDTO));

        //then
        List<PrescriptionDTO> response = prescriptionRequestController.getPrescriptions(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdatePrescriptionRequest() {
        //given
        PrescriptionRequestDTO prescriptionDTO = TestDataProvider.getPrescriptionRequestDTO();

        //when
        when(prescriptionRequestService.createOrUpdateMedicationRequest(prescriptionDTO)).thenReturn(Map.of());

        //then
        Map<String, String> response = prescriptionRequestController.createOrUpdatePrescriptionRequest(prescriptionDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void removePrescription() {
        //given
        PrescriptionRequestDTO prescriptionDTO = TestDataProvider.getPrescriptionRequestDTO();

        //when
        doNothing().when(prescriptionRequestService).removePrescription(prescriptionDTO, MedicationRequest.MedicationRequestStatus.COMPLETED);

        //then
        prescriptionRequestController.removePrescription(prescriptionDTO);
        verify(prescriptionRequestService).removePrescription(prescriptionDTO, MedicationRequest.MedicationRequestStatus.COMPLETED);
    }

    @Test
    void getPrescribedDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        PrescriptionHistoryDTO prescriptionHistoryDTO = TestDataProvider.getPrescriptionHistoryDTO();

        //when
        when(prescriptionRequestService.getNcdPrescribedDetails(requestDTO)).thenReturn(prescriptionHistoryDTO);

        //then
        PrescriptionHistoryDTO response = prescriptionRequestController.getPrescribedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionHistory() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();

        //when
        when(prescriptionRequestService.getPrescriptionHistory(requestDTO)).thenReturn(List.of(prescriptionDTO));

        //then
        List<PrescriptionDTO> response = prescriptionRequestController.getPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdPrescriptions() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(prescriptionRequestService.getNcdPrescriptions(requestDTO)).thenReturn(List.of(new PrescriptionDTO()));

        //then
        List<PrescriptionDTO> response = prescriptionRequestController.getNcdPrescriptions(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdatePrescription() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = new PrescriptionRequestDTO();

        //when
        when(prescriptionRequestService.createOrUpdateNcdPrescription(prescriptionRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, String> response = prescriptionRequestController.createOrUpdatePrescription(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void removePrescriptionRequestFormNotNull() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = new PrescriptionRequestDTO();

        //when
        doNothing().when(prescriptionRequestService).removePrescription(prescriptionRequestDTO,
                MedicationRequest.MedicationRequestStatus.CANCELLED);

        //then
        prescriptionRequestController.removePrescription(prescriptionRequestDTO);
        verify(prescriptionRequestService, atLeastOnce()).removePrescription(prescriptionRequestDTO, MedicationRequest.MedicationRequestStatus.COMPLETED);
    }


    @Test
    void getPrescriptionsRequestFormNotNull() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(prescriptionRequestService.getPrescribedDetails(requestDTO)).thenReturn(new PrescriptionHistoryDTO());

        //then
        PrescriptionHistoryDTO response = prescriptionRequestController.getPrescribedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionHistoryRequestFormNotNull() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(prescriptionRequestService.getNcdPrescriptionHistory(requestDTO)).thenReturn(List.of(new PrescriptionDTO()));

        //then
        List<PrescriptionDTO> response = prescriptionRequestController.getPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void listDispensePrescription() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(prescriptionRequestService.listDispensePrescription(requestDTO)).thenReturn(List.of(new PrescriptionDTO()));

        //then
        List<PrescriptionDTO> response = prescriptionRequestController.listDispensePrescription(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateDispensePrescription() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = new PrescriptionRequestDTO();

        //when
        when(prescriptionRequestService.updateDispensePrescription(prescriptionRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, String> response = prescriptionRequestController.updateDispensePrescription(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getDispenseHistory() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(prescriptionRequestService.getDispenseHistory(requestDTO)).thenReturn(List.of(new PrescriptionDTO()));

        //then
        List<PrescriptionDTO> response = prescriptionRequestController.getDispenseHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionPrediction() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(intensificationAlgorithm.getMedicationSuggestion(requestDTO.getMemberId())).thenReturn(new PrescriptionPredictionDTO());

        //then
        PrescriptionPredictionDTO response = prescriptionRequestController.getPrescriptionPrediction(requestDTO);
        Assertions.assertNotNull(response);
    }
    @Test
    void removePrescriptionRequestForNCD() {
        SelectedAppTypeContextHolder.set(Constants.NON_COMMUNITY);
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = new PrescriptionRequestDTO();

        //when
        doNothing().when(prescriptionRequestService).removePrescription(prescriptionRequestDTO,
                MedicationRequest.MedicationRequestStatus.CANCELLED);

        //then
        prescriptionRequestController.removePrescription(prescriptionRequestDTO);

        verify(prescriptionRequestService, times(1)).removePrescription(
                prescriptionRequestDTO,
                MedicationRequest.MedicationRequestStatus.CANCELLED
        );
    }

    @Test
    void getPrescriptionsRequestForNCD() {
        SelectedAppTypeContextHolder.set(Constants.NON_COMMUNITY);
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        PrescriptionHistoryDTO prescriptionHistoryDTO = new PrescriptionHistoryDTO();

        when(prescriptionRequestService.getNcdPrescribedDetails(requestDTO)).thenReturn(prescriptionHistoryDTO);

        PrescriptionHistoryDTO response = prescriptionRequestController.getPrescribedDetails(requestDTO);
        verify(prescriptionRequestService, times(1)).getNcdPrescribedDetails(requestDTO);
        Assertions.assertEquals(prescriptionHistoryDTO, response);
    }
}