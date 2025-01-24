package com.mdtlabs.coreplatform.offlineservice.offlinesync.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import com.mdtlabs.coreplatform.offlineservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.offlineservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.OfflineSyncService;
import jakarta.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * <p>
 * OfflineSyncControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in OfflineSyncControllerTest class.
 * </p>
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class OfflineSyncControllerTest {

    @InjectMocks
    OfflineSyncController offlineSyncController;

    @Mock
    OfflineSyncService offlineSyncService;

    @Mock
    private ObjectMapper objectMapper;

    @Test
    void createOfflineSync() {
        //given
        Map<String, Object> request = new HashMap<>();
        request.put(Constants.HOUSEHOLDS, List.of(TestDataProvider.getHouseholdData()));

        //when
        doNothing().when(offlineSyncService).createOfflineSync(request);

        //then
        SuccessResponse<Boolean> actualResponse = offlineSyncController.createOfflineSync(request);
        Assertions.assertEquals(HttpStatus.SC_CREATED, actualResponse.getStatusCode().value());
    }

    @Test
    void generateResponse() {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);

        //when
        doNothing().when(offlineSyncService).generateResponse(request);

        //then
        SuccessResponse<Boolean> actualResponse = offlineSyncController.createResponse(request);
        Assertions.assertEquals(HttpStatus.SC_CREATED, actualResponse.getStatusCode().value());
    }

    @Test
    void getOfflineSyncStatusList() {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);
        List<OfflineSyncDTO> offlineSyncList = List.of(TestDataProvider.getOfflineSyncDTO());
        ResponseListDTO<OfflineSyncDTO> responseList = new ResponseListDTO();
        responseList.setData(offlineSyncList);
        responseList.setTotalCount(Long.valueOf(offlineSyncList.size()));

        //when
        when(offlineSyncService.getOfflineSyncStatusList(request)).thenReturn(responseList);

        //then
        SuccessResponse<OfflineSyncDTO> actualResponse = offlineSyncController.getOfflineSyncStatusList(request);
        SuccessMessage actualResponseMessage = (SuccessMessage) actualResponse.getBody();
        Assertions.assertEquals(HttpStatus.SC_OK, actualResponse.getStatusCode().value());
        Assertions.assertEquals(responseList.getData(), actualResponseMessage.getEntityList());
        Assertions.assertEquals(responseList.getTotalCount(), actualResponseMessage.getTotalCount());
    }

    @Test
    void fetchSyncedData() throws IOException, ExecutionException, InterruptedException {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();

        //then
        offlineSyncController.fetchSyncedData(httpServletResponse, request);
        verify(offlineSyncService, atLeastOnce()).fetchSyncedData(request);
    }

    @Test
    void uploadSignatures() throws JsonProcessingException {
        //given
        List<MultipartFile> signatureFiles = List.of(TestDataProvider.getMockedSignatureFile());
        String provenanceRequest = TestDataProvider.getProvenanceString();

        //then
        SuccessResponse<Boolean> actualResponse = offlineSyncController.uploadSignatures(provenanceRequest, signatureFiles);
        Assertions.assertEquals(HttpStatus.SC_OK, actualResponse.getStatusCode().value());
    }
}
