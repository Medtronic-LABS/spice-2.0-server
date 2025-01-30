package com.mdtlabs.coreplatform.spiceservice.registration.service.impl;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;

import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientValidationResponseDTO;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RegistrationServiceImplTest {

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    private FollowUpService followUpService;

    @Mock
    private AmazonS3 s3Client;

    @Mock
    private CustomizedModulesService customizedModulesService;

    @InjectMocks
    private RegistrationServiceImpl registrationService;


    @BeforeEach
    public void setUp() {
        TestDataProvider.init();
    }

    @AfterEach
    public void endUp() {
        TestDataProvider.cleanUp();
    }


    @Test
    void testIsPatientRegisteredInRelatedPerson() {
        // Setup
        TestDataProvider.getStaticMock();
        BioDataDTO request = new BioDataDTO();
        PatientValidationResponseDTO responseDTO = new PatientValidationResponseDTO();
        when(fhirServiceApiInterface.validatePatient(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(responseDTO);

        // Call the method
        PatientValidationResponseDTO response = registrationService.isPatientRegisteredInRelatedPerson(request);

        // Verify results
        assertNotNull(response);
        verify(fhirServiceApiInterface, times(1)).validatePatient(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    @Test
    void testRegisterPatientAndMember_noFile() {
        // Given
        EnrollmentRequestDTO request = new EnrollmentRequestDTO();
        request.setTenantId(1L);
        request.setCustomizedWorkflows(null);
        request.setBioData(new BioDataDTO());

        EnrollmentResponseDTO response = new EnrollmentResponseDTO();
        response.setMemberId("member123");
        response.setPatientId("patient123");

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();

        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.registerPatientAndMember(anyString(), anyString(), eq(request)))
                .thenReturn(response);
        when(adminServiceApiInterface.getHealthFacilitiy(anyString(), any(), any()))
                .thenReturn(healthFacility);

        AmazonS3 mockS3Client = mock(AmazonS3.class);
        when(mockS3Client.putObject(any(PutObjectRequest.class))).thenReturn(null);  // Simulate successful upload

        doNothing().when(followUpService).deleteNcdCallRegister(any());

        // When
        EnrollmentResponseDTO result = registrationService.registerPatientAndMember(request, null);  // Pass null for no file

        // Then
        assertNotNull(result);
        verify(fhirServiceApiInterface).registerPatientAndMember(anyString(), anyString(), eq(request));
        verify(adminServiceApiInterface).getHealthFacilitiy(anyString(), any(), eq(1L));
        verify(followUpService).deleteNcdCallRegister(any());
        verify(mockS3Client, never()).putObject(any(PutObjectRequest.class));  // Ensure that S3 upload is not called when no file is passed
    }

    @Test
    void testRegisterPatientAndMember_withFile() throws Exception {

        TestDataProvider.getStaticMock();
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDto();

        EnrollmentResponseDTO response = new EnrollmentResponseDTO();
        response.setMemberId("member123");
        response.setPatientId("patient123");

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();


        when(fhirServiceApiInterface.registerPatientAndMember(CommonUtil.getAuthToken(), CommonUtil.getClient(), request))
                .thenReturn(response);
        when(adminServiceApiInterface.getHealthFacilitiy(anyString(), any(), any()))
                .thenReturn(healthFacility);

        MultipartFile mockFile = mock(MultipartFile.class);
        when(mockFile.getOriginalFilename()).thenReturn("signature.png");

        File tempFile = Files.createTempFile("signature", ".png").toFile();
        tempFile.deleteOnExit();

        when(CommonUtil.convertMultipartFileToFile(mockFile)).thenReturn(tempFile);


        URL mockUrl = new URL("http://example.com/signature.png");


        when(s3Client.putObject(any(PutObjectRequest.class))).thenReturn(null);
        when(s3Client.getUrl(any(), any())).thenReturn(mockUrl);
        when(fhirServiceApiInterface.updateMemberSignature(anyString(), anyString(), any(RequestDTO.class)))
                .thenReturn(true);

        doNothing().when(followUpService).deleteNcdCallRegister(any());

        EnrollmentResponseDTO result = registrationService.registerPatientAndMember(request, mockFile);

        assertNotNull(result);
        verify(fhirServiceApiInterface).registerPatientAndMember(anyString(), anyString(), eq(request));
        verify(adminServiceApiInterface).getHealthFacilitiy(any(), any(), any());

        verify(s3Client).getUrl(any(), any());
        verify(fhirServiceApiInterface).updateMemberSignature(anyString(), anyString(), any(RequestDTO.class));

        verify(s3Client).putObject(any(PutObjectRequest.class));
    }

    @Test
    void testRegisterPatientAndMemberThrowsDataNotFoundExceptionException() throws Exception {
        TestDataProvider.getStaticMock();
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDto();

        EnrollmentResponseDTO response = new EnrollmentResponseDTO();

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();

        when(fhirServiceApiInterface.registerPatientAndMember(CommonUtil.getAuthToken(), CommonUtil.getClient(), request))
                .thenReturn(response);
        when(adminServiceApiInterface.getHealthFacilitiy(anyString(), any(), any()))
                .thenReturn(healthFacility);

        MultipartFile mockFile = mock(MultipartFile.class);
        when(mockFile.getOriginalFilename()).thenReturn("signature.png");

        File tempFile = Files.createTempFile("signature", ".png").toFile();
        tempFile.deleteOnExit();

        when(CommonUtil.convertMultipartFileToFile(mockFile)).thenReturn(tempFile);

        URL mockUrl = new URL("http://example.com/signature.png");

        when(s3Client.putObject(any(PutObjectRequest.class))).thenReturn(null);
        when(s3Client.getUrl(any(), any())).thenReturn(mockUrl);
        when(fhirServiceApiInterface.updateMemberSignature(anyString(), anyString(), any(RequestDTO.class)))
                .thenReturn(true);

        doNothing().when(followUpService).deleteNcdCallRegister(any());

        assertThrows(DataNotFoundException.class, () -> {
            registrationService.registerPatientAndMember(request, mockFile);
        });

    }

    @Test
    void testRegisterPatientAndMemberThrowException() throws Exception {
        TestDataProvider.getStaticMock();
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDto();

        EnrollmentResponseDTO response = new EnrollmentResponseDTO();
        response.setMemberId("member123");
        response.setPatientId("patient123");

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();

        when(fhirServiceApiInterface.registerPatientAndMember(any(), any(), any()))
                .thenReturn(response);
        when(adminServiceApiInterface.getHealthFacilitiy(anyString(), any(), any()))
                .thenReturn(healthFacility);

        MultipartFile mockFile = mock(MultipartFile.class);
        when(mockFile.getOriginalFilename()).thenReturn("signature.png");

        File tempFile = Files.createTempFile("signature", ".png").toFile();
        tempFile.deleteOnExit();

        doThrow(new IOException("Error converting file")).when(CommonUtil.class);
        CommonUtil.convertMultipartFileToFile(mockFile);
        assertThrows(SpiceValidation.class, () -> {
            registrationService.registerPatientAndMember(request, mockFile);
        });
    }
    @Test
    void testRegisterPatientAndMemberSignatureNotUpdate() throws Exception {
        TestDataProvider.getStaticMock();
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDto();

        EnrollmentResponseDTO response = new EnrollmentResponseDTO();
        response.setMemberId("member123");
        response.setPatientId("patient123");

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        when(fhirServiceApiInterface.registerPatientAndMember(any(), any(), any()))
                .thenReturn(response);
        when(adminServiceApiInterface.getHealthFacilitiy(anyString(), any(), any()))
                .thenReturn(healthFacility);
        MultipartFile mockFile = mock(MultipartFile.class);
        when(mockFile.getOriginalFilename()).thenReturn("signature.png");

        File tempFile = Files.createTempFile("signature", ".png").toFile();
        tempFile.deleteOnExit();
        when(CommonUtil.convertMultipartFileToFile(mockFile)).thenReturn(tempFile);
        URL mockUrl = new URL("http://example.com/signature.png");
        when(s3Client.putObject(any(PutObjectRequest.class))).thenReturn(null);
        when(s3Client.getUrl(any(), any())).thenReturn(mockUrl);
        when(fhirServiceApiInterface.updateMemberSignature(anyString(), anyString(), any(RequestDTO.class)))
                .thenReturn(false);
        doNothing().when(followUpService).deleteNcdCallRegister(any());
        EnrollmentResponseDTO result = registrationService.registerPatientAndMember(request, mockFile);
        assertNotNull(result);
        verify(fhirServiceApiInterface).updateMemberSignature(anyString(), anyString(), any(RequestDTO.class));
    }

}