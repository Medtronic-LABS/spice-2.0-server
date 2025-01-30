package com.mdtlabs.coreplatform.spiceservice.followup.controller;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * <p>
 * FollowUpControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in FollowUpController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FollowUpControllerTest {

    @InjectMocks
    FollowUpController followUpController;

    @Mock
    FollowUpService followUpService;

    @Test
    void createFollowUp() {
        //given
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();

        //when
        when(followUpService.createFollowUp(followUpDTO)).thenReturn(followUpDTO);

        //then
        FollowUpDTO response = followUpController.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateFollowUp() {
        //given
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();

        //when
        when(followUpService.updateFollowUp(followUpDTO)).thenReturn(followUpDTO);

        //then
        FollowUpDTO response = followUpController.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getFollowUpList() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<FollowUpDTO> followUpDTO = List.of(TestDataProvider.getFollowUp());

        //when
        when(followUpService.getFollowUpList(requestDTO)).thenReturn(followUpDTO);

        //then
        List<FollowUpDTO> response = followUpController.getFollowUpList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getFollowUpCriteria() {
        //given
        FollowUpCriteria followUpCriteria = TestDataProvider.getFollowUpCriteria();
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        //when
        when(followUpService.getFollowUpCriteria(requestDTO)).thenReturn(followUpCriteria);

        //then
        FollowUpCriteria response = followUpController.getFollowUpCriteria(requestDTO);
        Assertions.assertNotNull(response);
    }


    @Test
    void testGetFollowUpPatients() {
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        List<FollowUpDTO> followUpList = new ArrayList<>();
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUpDTO();
        followUpList.add(followUpDTO);

        ResponseListDTO<FollowUpDTO> mockResponse = new ResponseListDTO<>();
        mockResponse.setData(followUpList);
        mockResponse.setTotalCount((long) followUpList.size());

        when(followUpService.getFollowUpPatients(any())).thenReturn(mockResponse);

        SuccessResponse<FollowUpDTO> response = followUpController.getFollowUpPatients(patientRequestDTO);

        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetPendingCallRegister() {
        CallRegisterDto mockCallRegisterDto = new CallRegisterDto();
        when(followUpService.getPendingCallRegister()).thenReturn(mockCallRegisterDto);

        SuccessResponse<CallRegisterDto> response = followUpController.getPendingCallRegister();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testGetOfflineScreeningFollowUps() {
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setLastSyncTime(Date.from(Instant.parse("2023-01-01T00:00:00Z")));
        patientRequestDTO.setCurrentSyncTime(Date.from(Instant.parse("2023-01-02T00:00:00Z")));

        List<FollowUpDTO> firstBatch = new ArrayList<>();
        firstBatch.add(new FollowUpDTO());

        List<FollowUpDTO> secondBatch = new ArrayList<>();
        when(followUpService.getAllCallRegistersByVillages(any(), any())).thenReturn(firstBatch).thenReturn(secondBatch);

        List<FollowUpDTO> result = followUpController.getOfflineScreeningFollowUps(patientRequestDTO);

        Assertions.assertEquals(1, result.size());
    }

    @Test
    void testGetOfflineAssessmentFollowUps() {
        List<FollowUpDTO> firstBatch = new ArrayList<>();
        firstBatch.add(TestDataProvider.getFollowUpDTO());

        List<FollowUpDTO> secondBatch = new ArrayList<>();
        secondBatch.add(TestDataProvider.getFollowUpDTO());

        when(followUpService.getAllCallRegistersByVillages(any(), any()))
                .thenReturn(firstBatch) // First call returns first batch
                .thenReturn(secondBatch) // Second call returns second batch
                .thenReturn(new ArrayList<>()); // Third call returns empty list

        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        List<FollowUpDTO> result = followUpController.getOfflineAssessmentFollowUps(patientRequestDTO);
        Assertions.assertNotNull(result);
        Assertions.assertEquals(2, result.size());
        Assertions.assertEquals(firstBatch.getFirst().getName(), result.getFirst().getName());
    }

    @Test
    void testGetOfflineMedicalReviewFollowUps() {
        List<FollowUpDTO> firstBatch = new ArrayList<>();
        firstBatch.add(TestDataProvider.getFollowUpDTO());

        List<FollowUpDTO> secondBatch = new ArrayList<>();
        secondBatch.add(TestDataProvider.getFollowUpDTO());

        when(followUpService.getAllCallRegistersByVillages(any(), any()))
                .thenReturn(firstBatch) // First call returns first batch
                .thenReturn(secondBatch) // Second call returns second batch
                .thenReturn(new ArrayList<>()); // Third call returns empty list

        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        List<FollowUpDTO> result = followUpController.getOfflineMedicalReviewFollowUps(patientRequestDTO);
        Assertions.assertNotNull(result);
        Assertions.assertEquals(2, result.size());
        Assertions.assertEquals(firstBatch.getFirst().getName(), result.getFirst().getName());
    }

    @Test
    void updateNcdFollowUp() {
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUpDTO();
        when(followUpService.updateNcdFollowUp(any())).thenReturn(followUpDTO);
        SuccessResponse<FollowUpDTO> response = followUpController.updateNcdFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getOfflineLostToFollowUps() {
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        List<FollowUpDTO> result = followUpController.getOfflineLostToFollowUps(patientRequestDTO);
        Assertions.assertNotNull(result);
    }
}
