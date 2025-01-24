package com.mdtlabs.coreplatform.spiceservice.labtest.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;

import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InvestigationControllerTest {

    @InjectMocks
    private InvestigationController investigationController;

    @Mock
    private InvestigationService investigationService;

    @Test
    @DisplayName("createInvestigationTest")
    void createInvestigationTest() {
        //given
        LabTestRequestDTO requestDTO = TestDataProvider.getLabTestRequestDTO();
        Map<String, String> result = Map.of("id", "123");
        SuccessResponse<Map<String, String>> response = new SuccessResponse<>(SuccessCode.INVESTIGATION_CREATE_STATUS,
                result, HttpStatus.CREATED);
        //when
        when(investigationService.createOrUpdateInvestigation(requestDTO)).thenReturn(result);
        //then
        SuccessResponse<Map<String, String>> actualResponse = investigationController.createInvestigation(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.CREATED, actualResponse.getStatusCode());

    }

    @Test
    @DisplayName("getInvestigationDetails")
    void getInvestigationDetailsTest() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        SuccessResponse<LabTestDTO> response = new SuccessResponse<>(SuccessCode.INVESTIGATION_HISTORY_DETAILS,
                labTestDTO, HttpStatus.OK);
        //when
        when(investigationService.getInvestigatedDetails(requestDTO)).thenReturn(labTestDTO);
        //then
        SuccessResponse<LabTestDTO> actualResponse = investigationController.getInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    @DisplayName("getInvestigationByEncounter")
    void getInvestigationByEncounterTest() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<LabTestDTO> labTestDTOList = List.of(TestDataProvider.getLabTestDTO());
        SuccessResponse<LabTestDTO> response = new SuccessResponse<>(SuccessCode.INVESTIGATION_LIST,
                labTestDTOList, Long.valueOf(labTestDTOList.size()), HttpStatus.OK);
        //when
        when(investigationService.getInvestigationsByEncounter(requestDTO)).thenReturn(labTestDTOList);
        //then
        SuccessResponse<LabTestDTO> actualResponse = investigationController.getInvestigationByEncounter(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    @DisplayName("removeInvestigationDetails")
    void removeInvestigationDetailsTest() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Map<String, String> result = Map.of("status", "success");
        SuccessResponse<Map<String, String>> response = new SuccessResponse<>(SuccessCode.INVESTIGATION_REMOVE_STATUS,
                result, HttpStatus.OK);
        //when
        when(investigationService.removeInvestigation(requestDTO)).thenReturn(result);
        //then
        SuccessResponse<Map<String, String>> actualResponse = investigationController.removeInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    @DisplayName("getHistoryInvestigatedDetails")
    void getHistoryInvestigatedDetailsTest() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        LabTestHistoryDTO labTestHistoryDTO = TestDataProvider.getLabTestHistoryDTO();
        SuccessResponse<LabTestHistoryDTO> response = new SuccessResponse<>(SuccessCode.INVESTIGATION_HISTORY_DETAILS,
                labTestHistoryDTO, HttpStatus.OK);
        //when
        when(investigationService.getHistoryInvestigatedDetails(requestDTO)).thenReturn(labTestHistoryDTO);
        //then
        SuccessResponse<LabTestHistoryDTO> actualResponse = investigationController.getHistoryInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void testCreateOrUpdateInvestigationResult() {
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        SuccessResponse<String> response = investigationController.createOrUpdateInvestigationResult(labTestRequestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        verify(investigationService, times(1)).updateInvestigationResult(labTestRequestDTO);
    }

    @Test
    void testGetIntensificationDetails() {
        RequestDTO requestData = new RequestDTO();

        List<LabTestDTO> labTestDTOList = new ArrayList<>();
        LabTestDTO labTestDTO = new LabTestDTO();
        labTestDTOList.add(labTestDTO);

        Map<String, List<LabTestDTO>> mockResponse = new HashMap<>();
        mockResponse.put("testKey", labTestDTOList);

        when(investigationService.getIntensificationDetails(any())).thenReturn(mockResponse);

        SuccessResponse<Map<String, List<LabTestDTO>>> response = investigationController.getIntensificationDetails(requestData);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(mockResponse, investigationService.getIntensificationDetails(requestData));
    }

    @Test
    void testReviewInvestigation() {
        RequestDTO requestDTO = new RequestDTO();
        SuccessResponse<String> response = investigationController.reviewInvestigation(requestDTO);

        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        verify(investigationService, times(1)).reviewInvestigation(requestDTO);
    }
}
