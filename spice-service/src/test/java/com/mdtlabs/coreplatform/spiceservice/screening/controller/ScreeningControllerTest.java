package com.mdtlabs.coreplatform.spiceservice.screening.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.screening.service.ScreeningService;

/**
 * <p>
 * ScreeningControllerTEST class used to test all possible positive
 * and negative cases for all methods and conditions used in ScreeningController class.
 * </p>
 *
 * @author Gokul created on Aug 27 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ScreeningControllerTest {
    @InjectMocks
    ScreeningController screeningController;

    @Mock
    ScreeningService screeningService;

    @Test
    void createScreeningLogTest() throws Exception {
        //given
        ScreeningLogRequestDTO screeningLogRequestDTO = new ScreeningLogRequestDTO();
        boolean response;
        ObjectMapper objectMapper = new ObjectMapper();
        String screeningRequestJson = objectMapper.writeValueAsString(screeningLogRequestDTO);
        MockMultipartFile signatureFile = new MockMultipartFile("signatureFile", "signature.jpg", MediaType.IMAGE_JPEG_VALUE, "signature content".getBytes());

        //when
        when(screeningService.processScreeningLog(any(), any())).thenReturn(new BioDataDTO());

        //then
        response = screeningController.processScreeningLog(screeningRequestJson, signatureFile).hasBody();
        Assertions.assertTrue(response);
    }

    @Test
    void getCountOfUserTest() {
        DashboardDetailsRequestDTO request = new DashboardDetailsRequestDTO();
        //when
        when(screeningService.getPatientCountOfUsers(request)).thenReturn(new DashboardDetails());
        //then
        SuccessResponse<DashboardDetails> response = screeningController.getCountOfUser(request);
        Assertions.assertNotNull(response);
    }
}
