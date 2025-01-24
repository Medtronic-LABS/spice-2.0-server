package com.mdtlabs.coreplatform.spiceservice.screening.service;

import java.io.File;
import java.net.URL;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.amazonaws.services.s3.AmazonS3;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.screening.service.impl.ScreeningServiceImpl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ScreeningServiceTest {
    @InjectMocks
    ScreeningServiceImpl screeningService;

    @Mock
    FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    CustomizedModulesService customizedModulesService;

    @Mock
    FollowUpService followUpService;

    private static MockedStatic<Files> files;

    @Mock
    Files file;

    @Mock
    Path path;




    @Mock
    private AmazonS3 s3Client;

    @Mock
    private MultipartFile multipartFile;

    private String bucketName;

    @Test
    void createScreeningLogTest() throws Exception {
        //init
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //given
        ScreeningLogRequestDTO requestDTO = new ScreeningLogRequestDTO();
        requestDTO.setCustomizedWorkflows(List.of(Map.of(Constants.ID, Constants.STRING_FIVE)));

        //when
        doNothing().when(customizedModulesService).createCustomizedModules(requestDTO.getCustomizedWorkflows(),
                Constants.WORKFLOW_SCREENING, Constants.STRING_ONE, Constants.STRING_ONE);
        when(fhirServiceApiInterface.screeningCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO))
                .thenReturn(new BioDataDTO());
        TestDataProvider.cleanUp();
    }

    @Test
    void createScreeningLogTestWithException() {
        //init
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //given
        ScreeningLogRequestDTO requestDTO = new ScreeningLogRequestDTO();

        //when
        when(fhirServiceApiInterface.screeningCreate(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO)).thenReturn(null);

        Assertions.assertThrows(Exception.class, () ->
                screeningService.processScreeningLog(requestDTO, null));
        TestDataProvider.cleanUp();
    }

   @Test
    void processScreeningLog() throws Exception {
        TestDataProvider.init();
        BioDataDTO response = new BioDataDTO();
        ScreeningLogRequestDTO request = new ScreeningLogRequestDTO();
        request.setIsReferAssessment(Boolean.TRUE);
        response.setRelatedPersonStatus(Constants.SCREENED);
        response.setMemberReference(TestConstants.MEMBER_ID);
        response.setPatientReference(TestConstants.PATIENT_ID);
        request.setUserId(TestConstants.ONE);
        request.setSiteId(TestConstants.ONE);
        request.setScreeningDateTime(new Date());
        request.setMemberReference(TestConstants.MEMBER_ID);
        request.setCustomizedWorkflows(List.of(Map.of(Constants.ID, Constants.STRING_FIVE)));

        CallRegister callRegister = new CallRegister();
        callRegister.setMemberId(response.getMemberReference());
        callRegister.setPatientId(response.getPatientReference());
        callRegister.setIsInitiated(Boolean.FALSE);
        callRegister.setType(AppointmentType.SCREENED);
        callRegister.setIsWrongNumber(Boolean.FALSE);
        callRegister.setReferredSiteId(String.valueOf(request.getSiteId()));
        callRegister.setVillageId(TestConstants.STRING_ONE);

        request.setBioData(new BioDataDTO());
        request.getBioData().setFirstName(TestConstants.FIRST_NAME);
        request.setMemberReference(TestConstants.MEMBER_ID);
        ReflectionTestUtils.setField(screeningService, "bucketName", "name");
        URL expectedUrl = new URL("https://test-bucket.s3.amazonaws.com/screening/122_testfirstname_20241219123228730_screeningsign.jpeg");
        URL mockUrl = mock(URL.class);
        when(mockUrl.toString()).thenReturn("http://example.com/signature.png");

        MultipartFile mockFile = mock(MultipartFile.class);
        when(mockFile.getOriginalFilename()).thenReturn("signature.png");

        File tempFile = Files.createTempFile("signature", ".png").toFile();
        tempFile.deleteOnExit();

        //when
        TestDataProvider.getStaticMock();
        when(CommonUtil.convertMultipartFileToFile(mockFile)).thenReturn(tempFile);
        when(s3Client.getUrl(anyString(), anyString())).thenReturn(expectedUrl);
        when(fhirServiceApiInterface.screeningCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(response);
        screeningService.processScreeningLog(request, mockFile);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientCountOfUsers() {
        //given
        TestDataProvider.init();
        DashboardDetailsRequestDTO request = new DashboardDetailsRequestDTO();
        DashboardDetails response = new DashboardDetails();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getPatientCount(CommonUtil.getAuthToken()
                , CommonUtil.getClient(), request)).thenReturn(response);
        //then
        DashboardDetails result = screeningService.getPatientCountOfUsers(request);
        Assertions.assertNotNull(result);
        TestDataProvider.cleanUp();
    }

}
