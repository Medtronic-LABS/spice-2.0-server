package com.mdtlabs.coreplatform.spiceservice.inappanalytics.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.spiceservice.inappanalytics.service.InAppAnalyticsService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * <p>
 * InAppAnalyticsControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in InAppAnalyticsController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InAppAnalyticsControllerTest {

    @InjectMocks
    InAppAnalyticsController inAppAnalyticsController;

    @Mock
    InAppAnalyticsService inAppAnalyticsService;

    @Test
    void uploadFileInS3() {
        //given
        MultipartFile mockMultipartFile = Mockito.mock(MultipartFile.class);

        //then
        SuccessResponse response = inAppAnalyticsController.uploadFileInS3(mockMultipartFile);
        Assertions.assertNotNull(response);
    }
}
