package com.mdtlabs.coreplatform.spiceservice.inappanalytics.service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.util.Objects;

import com.amazonaws.services.s3.AmazonS3;
import io.minio.errors.MinioException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.inappanalytics.service.impl.InAppAnalyticsServiceImpl;

/**
 * <p>
 * InAppAnalyticsServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in InAppAnalyticsService class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InAppAnalyticsServiceTest {

    @InjectMocks
    InAppAnalyticsServiceImpl inAppAnalyticsService;

    @Mock
    private AmazonS3 s3Client;

    @Mock
    private MultipartFile multipartFile;

    @Test
    void uploadFileInS3() throws IOException, GeneralSecurityException, MinioException {
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        ReflectionTestUtils.setField(inAppAnalyticsService, "inAppAnalyticsBucket", TestConstants.STRING_ONE);
        ReflectionTestUtils.setField(inAppAnalyticsService, "bucketName", TestConstants.STRING_ONE);

        String originalFilename = "test.txt";
        byte[] content = "Test Content".getBytes();
        prescriptionRequestDTO.setSignatureFile(multipartFile);
        URL expectedUrl = new URL("https://example-bucket.s3.amazonaws.com/" + originalFilename);

        // Mock the behavior of MultipartFile
        when(multipartFile.getOriginalFilename()).thenReturn(originalFilename);
        when(multipartFile.getBytes()).thenReturn(content);
        when(multipartFile.getName()).thenReturn(originalFilename);

        File convertedFile = new File(Objects.requireNonNull(multipartFile.getOriginalFilename()));
        try (FileOutputStream fos = new FileOutputStream(convertedFile)) {
            fos.write(multipartFile.getBytes());
        }
        when(s3Client.getUrl(anyString(), anyString())).thenReturn(expectedUrl);

        String response = inAppAnalyticsService.uploadFileInS3(multipartFile);
        Assertions.assertNotNull(response);
    }
}
