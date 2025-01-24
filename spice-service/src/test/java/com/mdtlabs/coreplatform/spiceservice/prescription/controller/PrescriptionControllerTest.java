package com.mdtlabs.coreplatform.spiceservice.prescription.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionPredictionDTO;
import org.hibernate.collection.spi.PersistentList;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockMultipartFile;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.prescription.service.PrescriptionService;

/**
 * <p>
 * PrescriptionControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in PrescriptionController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PrescriptionControllerTest {

    @InjectMocks
    PrescriptionController prescriptionController;

    @Mock
    PrescriptionService prescriptionRequestService;

    @Mock
    ObjectMapper mapper;

    @Mock
    private ObjectMapper objectMapper;

    @Test
    void getPrescriptions() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(prescriptionRequestService.getPrescriptions(requestDTO)).thenReturn(List.of(new PrescriptionDTO()));

        //then
        SuccessResponse<List<PrescriptionDTO>> response = prescriptionController.getPrescriptions(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void removePrescription() {
        //given
        PrescriptionRequestDTO requestDTO = new PrescriptionRequestDTO();

        //then
        SuccessResponse<Object> response = prescriptionController.removePrescription(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescribedDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(prescriptionRequestService.getPrescribedDetails(requestDTO)).thenReturn(new PrescriptionHistoryDTO());

        //then
        SuccessResponse<PrescriptionHistoryDTO> response = prescriptionController.getPrescribedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionHistoryList() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(prescriptionRequestService.getPrescriptionHistory(requestDTO)).thenReturn(List.of(new PrescriptionDTO()));

        //then
        SuccessResponse<PrescriptionDTO> response = prescriptionController.getPrescriptionHistoryList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void testCreatePrescriptionRequest_Success() throws Exception {
        String prescriptionRequestJson = "{ \"signature\": \"someValue\" }";
        MockMultipartFile signatureFile = new MockMultipartFile("signature", "signature.pdf", "application/pdf", "signature content".getBytes());

        Map<String, String> responseMap = new HashMap<>();
        responseMap.put("status", "success");

        when(mapper.readValue(prescriptionRequestJson, PrescriptionRequestDTO.class)).thenReturn(new PrescriptionRequestDTO());
        prescriptionController.createPrescriptionRequest(prescriptionRequestJson, signatureFile);

        when(prescriptionRequestService.createMedicationRequest(any(PrescriptionRequestDTO.class))).thenReturn(responseMap);

        verify(prescriptionRequestService, times(1)).createMedicationRequest(any(PrescriptionRequestDTO.class));
    }

    @Test
    void listFillPrescription_success() {
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<PrescriptionDTO> prescriptionList = Arrays.asList(TestDataProvider.getPrescriptionDTO());
        when(prescriptionRequestService.getFillPrescriptions(any())).thenReturn(prescriptionList);
        SuccessResponse<PrescriptionDTO> response = prescriptionController.listFillPrescription(requestDTO);
        Assertions.assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void listFillPrescription() {
        RequestDTO requestDTO = new RequestDTO();
        when(prescriptionRequestService.getFillPrescriptions(any())).thenReturn(new ArrayList<>());
        SuccessResponse<PrescriptionDTO> response = prescriptionController.listFillPrescription(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void testUpdateFillPrescription_Success() {
        PrescriptionRequestDTO requestDTO = TestDataProvider.getPrescriptionRequestDTO();
        requestDTO.setPrescriptionId("12345");
        Map<String, String> updateResponse = new HashMap<>();
        updateResponse.put("status", "success");
        updateResponse.put("message", "Prescription updated successfully");
        when(prescriptionRequestService.updateFillPrescription(requestDTO)).thenReturn(updateResponse);
        SuccessResponse<Map<String, String>> response = prescriptionController.updateFillPrescription(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void getReFillPrescriptionHistory_success() {
        RequestDTO requestDTO = new RequestDTO();
        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();
        prescriptionDTO.setPrescriptionId("12345");

        List<PrescriptionDTO> prescriptionList = List.of(prescriptionDTO);

        when(prescriptionRequestService.getRefillPrescriptionHistory(requestDTO)).thenReturn(prescriptionList);

        SuccessResponse<PrescriptionDTO> response = prescriptionController.getReFillPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetReFillPrescriptionHistory_EmptyList() {
        RequestDTO requestDTO = new RequestDTO();

        when(prescriptionRequestService.getRefillPrescriptionHistory(requestDTO)).thenReturn(Collections.emptyList());

        SuccessResponse<PrescriptionDTO> response = prescriptionController.getReFillPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testGetPrescriptionPrediction_Success() {
        RequestDTO requestDTO = new RequestDTO();
        PrescriptionPredictionDTO predictionDTO = new PrescriptionPredictionDTO();

        when(prescriptionRequestService.getPrescriptionPrediction(requestDTO)).thenReturn(predictionDTO);
        SuccessResponse<PrescriptionPredictionDTO> response = prescriptionController.getPrescriptionPrediction(requestDTO);

        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

}
