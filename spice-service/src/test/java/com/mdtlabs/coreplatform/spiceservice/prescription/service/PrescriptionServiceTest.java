package com.mdtlabs.coreplatform.spiceservice.prescription.service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.security.GeneralSecurityException;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.amazonaws.services.s3.AmazonS3;
import io.minio.errors.MinioException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.prescription.service.impl.PrescriptionServiceImpl;

/**
 * <p>
 * OfflineSyncServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in OfflineSyncServiceTest class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class PrescriptionServiceTest {

    @Mock
    private AmazonS3 s3Client;

    @Mock
    private MultipartFile multipartFile;

    @InjectMocks
    private PrescriptionServiceImpl prescriptionService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @BeforeEach
    public void setUp() {
    }

    @Test
    void createMedicationRequest() throws IOException, GeneralSecurityException, MinioException {
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        ReflectionTestUtils.setField(prescriptionService, "signatureBucket", TestConstants.STRING_ONE);
        ReflectionTestUtils.setField(prescriptionService, "bucketName", TestConstants.STRING_ONE);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

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

        Map<String, String> response = prescriptionService.createMedicationRequest(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPrescriptionHistory() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.getPrescriptionHistoryList(CommonUtil.getAuthToken(), CommonUtil.getClient(), CommonUtil.getTenantId(), requestDTO)).thenReturn(List.of(TestDataProvider.getPrescriptionDTO()));

        //then
        List<PrescriptionDTO> response = prescriptionService.getPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPrescribedDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.getPrescribedDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), CommonUtil.getTenantId(), requestDTO)).thenReturn(new PrescriptionHistoryDTO());

        //then
        PrescriptionHistoryDTO response = prescriptionService.getPrescribedDetails(requestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPrescriptions() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPrescriptionId("123");
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.getPrescriptions(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(List.of(TestDataProvider.getPrescriptionDTO()));

        //then
        List<PrescriptionDTO> response = prescriptionService.getPrescriptions(requestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void removePrescription() {
        //given
        PrescriptionRequestDTO requestDTO = new PrescriptionRequestDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //then
        prescriptionService.removePrescription(requestDTO);
        verify(fhirServiceApiInterface, atLeastOnce()).removePrescription(CommonUtil.getAuthToken(), CommonUtil.getClient(), CommonUtil.getTenantId(), requestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateFillPrescription() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        PrescriptionRequestDTO prescriptionRequestDTO = new PrescriptionRequestDTO();
        Map<String, String> expectedResponse = new HashMap<>();
        expectedResponse.put("status", "success");
        when(fhirServiceApiInterface.updatePrescriptionDispense(CommonUtil.getAuthToken(), CommonUtil.getClient(), prescriptionRequestDTO)).thenReturn(expectedResponse);
        Map<String, String> response = prescriptionService.updateFillPrescription(prescriptionRequestDTO);
        assertNotNull(response);
        assertEquals("success", response.get("status"));
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetFillPrescriptions() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<PrescriptionDTO> mockPrescriptionList = new ArrayList<>();
        mockPrescriptionList.add(TestDataProvider.getPrescriptionDTO());
        mockPrescriptionList.add(TestDataProvider.getPrescriptionDTO());

        when(fhirServiceApiInterface.getPrecriptionDispenseList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO))
                .thenReturn(mockPrescriptionList);
        List<PrescriptionDTO> result = prescriptionService.getFillPrescriptions(requestDTO);
        assertNotNull(result);
        assertEquals(2, result.size()); // Verify the list contains 2 items
        verify(fhirServiceApiInterface, times(1)).getPrecriptionDispenseList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetPrescriptionPrediction() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        // Create the PrescriptionPredictionDTO and set its values
        PrescriptionPredictionDTO prescriptionPrediction = TestDataProvider.getPrescriptionPrediction();

        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        when(fhirServiceApiInterface.getPrescriptionPrediction(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO))
                .thenReturn(prescriptionPrediction);
        PrescriptionPredictionDTO response = prescriptionService.getPrescriptionPrediction(requestDTO);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }


    @Test
    void testGetRefillPrescriptionHistory() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<PrescriptionDTO> mockPrescriptionHistory = new ArrayList<>();
        mockPrescriptionHistory.add(TestDataProvider.getPrescriptionDTO());
        mockPrescriptionHistory.add(TestDataProvider.getPrescriptionDTO());
        when(fhirServiceApiInterface.getPrescriptionDispenseHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO))
                .thenReturn(mockPrescriptionHistory);
        List<PrescriptionDTO> result = prescriptionService.getRefillPrescriptionHistory(requestDTO);
        assertNotNull(result);
        assertEquals(2, result.size());
        TestDataProvider.cleanUp();
    }
}
