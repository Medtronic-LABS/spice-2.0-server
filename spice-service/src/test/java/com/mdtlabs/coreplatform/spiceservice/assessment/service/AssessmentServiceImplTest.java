package com.mdtlabs.coreplatform.spiceservice.assessment.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.CommonLists;
import com.mdtlabs.coreplatform.spiceservice.common.RiskAlgorithm;
import com.mdtlabs.coreplatform.spiceservice.common.RiskLevelAlgorithm;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RiskAlgorithmDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.pregnancy.PregnancySymptomDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Message;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.RedRiskNotification;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.NotificationApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.assessment.service.impl.AssessmentServiceImpl;
import com.mdtlabs.coreplatform.spiceservice.common.Repository.RedRiskNotificationRepository;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;


/**
 * <p>
 * AssessmentControllerTest class is used to test each and every method
 * for both positive and negative with respective scenarios.
 * <p/>
 *
 *
 *
 * @author Jaganathan.R created on Jan 30 2023.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AssessmentServiceImplTest {

    @InjectMocks
    private AssessmentServiceImpl assessmentService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private StaticDataService staticDataService;

    @Mock
    private RiskAlgorithm riskAlgorithm;
    @Mock
    private RedRiskNotificationRepository notificationRepository;

    @Mock
    private NotificationApiInterface notificationApiInterface;

    @Mock
    private UserServiceApiInterface userApiInterface;

    @Mock
    private CustomizedModulesService customizedModulesService;

    @Mock
    private RiskLevelAlgorithm riskLevelAlgorithm;

    @Mock
    private FollowUpService followUpService;

    @Test
    @DisplayName("AddRedRiskNotification Test")
    void addRedRiskNotification() {
        //given
        ReflectionTestUtils.setField(assessmentService, TestConstants.ENABLE_NOTIFICATION, Boolean.TRUE);
        List<SmsDTO> smsDTOS = List.of(TestDataProvider.getSmsDTO());
        List<UserResponseDTO> users = List.of(TestDataProvider.getUserResponseDTO());
        SMSTemplate smsTemplate = TestDataProvider.getSmsTemplate();
        ResponseEntity<SMSTemplate> smsResponseEntity = new ResponseEntity<>(smsTemplate, HttpStatus.OK);
        ResponseEntity<List<UserResponseDTO>> responseEntity = new ResponseEntity<>(users, HttpStatus.OK);
        TestDataProvider.init();
        RedRiskNotification redRiskNotification = TestDataProvider.getRedRiskNotification();
        //when
        when(notificationRepository.save(redRiskNotification)).thenReturn(redRiskNotification);
        TestDataProvider.getStaticMock();
        when(userApiInterface.getUsersByRoleName(TestConstants.BEARER_TEST,
                UserSelectedTenantContextHolder.get(), Constants.ROLE_RED_RISK_USER)).thenReturn(responseEntity);
        when(notificationApiInterface.getSmsTemplateValues(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), Constants.TEMPLATE_TYPE_RED_RISK)).thenReturn(smsResponseEntity);
        when(notificationApiInterface.saveOutBoundSms(TestConstants.BEARER_TEST,
                UserSelectedTenantContextHolder.get(), smsDTOS)).thenReturn("");
        //then
        assessmentService.addRedRiskNotification(Constants.STRING_ONE, Constants.STRING_ONE, Constants.STRING_ONE);
        Assertions.assertNotNull(redRiskNotification);
        TestDataProvider.cleanUp();
    }

    @Test
    void testCreateAssessmentWithAfrica() {
        //given
        TestDataProvider.init();
        MockedStatic<com.mdtlabs.coreplatform.spiceservice.common.CommonUtil> commonUtilMockedStatic = mockStatic(com.mdtlabs.coreplatform.spiceservice.common.CommonUtil.class);
        CommonLists commonListInstance = mock(CommonLists.class);
        commonUtilMockedStatic.when(com.mdtlabs.coreplatform.spiceservice.common.CommonUtil::getCommonListsInstance).thenReturn(commonListInstance);


        EncounterDetailsDTO encounter = new EncounterDetailsDTO();
        encounter.setId(String.valueOf(1L));

        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setEncounter(encounter);
        Culture culture = new Culture();
        culture.setId(1L);
        assessmentDTO.setCustomizedWorkflows(List.of(Map.of(com.mdtlabs.coreplatform.spiceservice.common.Constants.ID, com.mdtlabs.coreplatform.spiceservice.common.Constants.STRING_FIVE)));
        PregnancyDetailsDTO pregnancyAncDTO = new PregnancyDetailsDTO();

        PregnancySymptomDTO pregnancySymptomDTO = new PregnancySymptomDTO();
        pregnancySymptomDTO.setName("");
        pregnancyAncDTO.setPregnancySymptoms(List.of(pregnancySymptomDTO));
        assessmentDTO.setPregnancyAnc(pregnancyAncDTO);

        ScreeningLog screeningLog = new ScreeningLog();
        screeningLog.setPatientStatus(com.mdtlabs.coreplatform.spiceservice.common.Constants.ENROLLED);
        screeningLog.setRiskLevel(com.mdtlabs.coreplatform.spiceservice.common.Constants.HIGH);
        screeningLog.setInitialReview(Boolean.TRUE);
        screeningLog.setRedRisk(Boolean.FALSE);
        Message message = new Message();
        message.setCategory("Moderate");
        message.setId(1L);

        List<MetaDataDTO> messageMetaData = new ArrayList<>();
        MetaDataDTO metaData = new MetaDataDTO();
        metaData.setId(1l);
        metaData.setCategory("Moderate");

        MetaDataDTO metaData2 = new MetaDataDTO();
        metaData2.setId(2l);
        metaData2.setCategory("Red");

        messageMetaData.add(metaData);
        messageMetaData.add(metaData2);

        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class))).thenReturn(screeningLog);
        when(staticDataService.findCulture(com.mdtlabs.coreplatform.spiceservice.common.Constants.DEFAULT_CULTURE_VALUE)).thenReturn(culture);
        when(riskAlgorithm.getRiskLevelInAssessmentDbm(any(RiskAlgorithmDTO.class))).thenReturn("Moderate");

        when(staticDataService.getMessageMetaData()).thenReturn(messageMetaData);
        when(fhirServiceApiInterface.assessmentCreate(any(), any(), any())).thenReturn(assessmentDTO);
        doNothing().when(customizedModulesService).createCustomizedModules(assessmentDTO.getCustomizedWorkflows(),
                com.mdtlabs.coreplatform.spiceservice.common.Constants.WORKFLOW_ENROLLMENT, com.mdtlabs.coreplatform.spiceservice.common.Constants.STRING_ONE, com.mdtlabs.coreplatform.spiceservice.common.Constants.STRING_ONE);

        AssessmentDTO ncdAssessment = assessmentService.createNcdAssessment(assessmentDTO);
        Assertions.assertNotNull(ncdAssessment);
        assertEquals(ncdAssessment, assessmentDTO);
        verify(fhirServiceApiInterface).getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class));
        verify(fhirServiceApiInterface).assessmentCreate(any(), any(), any());

        when(riskAlgorithm.getRiskLevelInAssessmentDbm(any(RiskAlgorithmDTO.class))).thenReturn("High");
        when(staticDataService.getMessageMetaData()).thenReturn(messageMetaData);
        when(fhirServiceApiInterface.assessmentCreate(any(), any(), any())).thenReturn(assessmentDTO);
        doNothing().when(customizedModulesService).createCustomizedModules(assessmentDTO.getCustomizedWorkflows(),
                com.mdtlabs.coreplatform.spiceservice.common.Constants.WORKFLOW_ENROLLMENT, com.mdtlabs.coreplatform.spiceservice.common.Constants.STRING_ONE, com.mdtlabs.coreplatform.spiceservice.common.Constants.STRING_ONE);
        AssessmentDTO ncdAssessment2 = assessmentService.createNcdAssessment(assessmentDTO);

        assertNotNull(ncdAssessment2);
        assertEquals(ncdAssessment2, assessmentDTO);
        verify(fhirServiceApiInterface, times(2)).getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class));
        verify(fhirServiceApiInterface, times(2)).assessmentCreate(any(), any(), any());
        verify(riskAlgorithm, times(2)).getRiskLevelInAssessmentDbm(any(RiskAlgorithmDTO.class));

        commonUtilMockedStatic.close();
        TestDataProvider.cleanUp();
    }


    @Test
    void testCreateBpAssessment() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        MockedStatic<com.mdtlabs.coreplatform.spiceservice.common.CommonUtil> commonUtilMockedStatic = mockStatic(com.mdtlabs.coreplatform.spiceservice.common.CommonUtil.class);
        CommonLists commonListInstance = mock(CommonLists.class);
        commonUtilMockedStatic.when(com.mdtlabs.coreplatform.spiceservice.common.CommonUtil::getCommonListsInstance).thenReturn(commonListInstance);

        EncounterDetailsDTO encounter = new EncounterDetailsDTO();
        encounter.setId(String.valueOf(1L));

        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();

        assessmentDTO.setEncounter(encounter);
        assessmentDTO.setAssessmentTakenOn(new Date());

        ScreeningLog screeningLog = new ScreeningLog();
        screeningLog.setPatientStatus(com.mdtlabs.coreplatform.spiceservice.common.Constants.ENROLLED);
        screeningLog.setRiskLevel(com.mdtlabs.coreplatform.spiceservice.common.Constants.HIGH);
        screeningLog.setInitialReview(Boolean.TRUE);
        screeningLog.setRedRisk(Boolean.TRUE);
        screeningLog.setIsPregnant(Boolean.TRUE);

        Culture culture = new Culture();
        culture.setId(1L);

        List<MetaDataDTO> messageMetaData = new ArrayList<>();
        MetaDataDTO metaData = new MetaDataDTO();
        metaData.setId(1l);
        metaData.setCategory("Moderate");

        MetaDataDTO metaData2 = new MetaDataDTO();
        metaData2.setId(2l);
        metaData2.setCategory("Red");

        messageMetaData.add(metaData);
        messageMetaData.add(metaData2);

        BpLogDTO existingBpLog = new BpLogDTO();
        existingBpLog.setBpTakenOn(new Date());

        //when
        when(fhirServiceApiInterface.getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class))).thenReturn(screeningLog);
        when(fhirServiceApiInterface.getExistingBpLog(any(), any(), any())).thenReturn(existingBpLog);
        when(staticDataService.findCulture(com.mdtlabs.coreplatform.spiceservice.common.Constants.DEFAULT_CULTURE_VALUE)).thenReturn(culture);
        when(staticDataService.getMessageMetaData()).thenReturn(messageMetaData);
        when(riskAlgorithm.getRiskLevelInAssessmentDbm(any(RiskAlgorithmDTO.class))).thenReturn("Moderate");
        when(fhirServiceApiInterface.assessmentCreate(any(), any(), any())).thenReturn(assessmentDTO);

        assessmentService.createBpAssessment(assessmentDTO);
        verify(fhirServiceApiInterface).getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class));
        verify(fhirServiceApiInterface).getExistingBpLog(any(), any(), any());
        verify(fhirServiceApiInterface).assessmentCreate(any(), any(), any());

        when(fhirServiceApiInterface.getExistingBpLog(any(), any(), any())).thenReturn(null);
        when(riskAlgorithm.getRiskLevelInAssessmentDbm(any(RiskAlgorithmDTO.class))).thenReturn("High");
        when(fhirServiceApiInterface.assessmentCreate(any(), any(), any())).thenReturn(assessmentDTO);
        assessmentService.createBpAssessment(assessmentDTO);
        verify(fhirServiceApiInterface, times(2)).assessmentCreate(any(), any(), any());

        TestDataProvider.cleanUp();
        commonUtilMockedStatic.close();
    }

    @Test
    void testCreateGlucoseLog() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        MockedStatic<com.mdtlabs.coreplatform.spiceservice.common.CommonUtil> commonUtilMockedStatic = mockStatic(com.mdtlabs.coreplatform.spiceservice.common.CommonUtil.class);
        CommonLists commonListInstance = mock(CommonLists.class);
        commonUtilMockedStatic.when(com.mdtlabs.coreplatform.spiceservice.common.CommonUtil::getCommonListsInstance).thenReturn(commonListInstance);

        EncounterDetailsDTO encounter = new EncounterDetailsDTO();
        encounter.setId(String.valueOf(1L));

        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.setEncounter(encounter);
        assessmentDTO.setAssessmentTakenOn(new Date());

        ScreeningLog screeningLog = new ScreeningLog();
        screeningLog.setPatientStatus(com.mdtlabs.coreplatform.spiceservice.common.Constants.ENROLLED);
        screeningLog.setRiskLevel(com.mdtlabs.coreplatform.spiceservice.common.Constants.HIGH);
        screeningLog.setInitialReview(Boolean.TRUE);
        screeningLog.setRedRisk(Boolean.FALSE);
        screeningLog.setIsPregnant(Boolean.FALSE);

        Culture culture = new Culture();
        culture.setId(1L);

        List<MetaDataDTO> messageMetaData = new ArrayList<>();
        MetaDataDTO metaData = new MetaDataDTO();
        metaData.setId(1l);
        metaData.setCategory("Moderate");

        MetaDataDTO metaData2 = new MetaDataDTO();
        metaData2.setId(2l);
        metaData2.setCategory("Moderate");

        messageMetaData.add(metaData);
        messageMetaData.add(metaData2);

        GlucoseLogDTO existingGlucoseLog = new GlucoseLogDTO();
        existingGlucoseLog.setBgTakenOn(new Date());

        when(staticDataService.findCulture(com.mdtlabs.coreplatform.spiceservice.common.Constants.DEFAULT_CULTURE_VALUE)).thenReturn(culture);
        when(fhirServiceApiInterface.getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class))).thenReturn(screeningLog);
        when(staticDataService.getMessageMetaData()).thenReturn(messageMetaData);
        when(fhirServiceApiInterface.getExistingGlucoseLog(any(), any(), any())).thenReturn(existingGlucoseLog);
        when(fhirServiceApiInterface.assessmentCreate(any(), any(), any())).thenReturn(assessmentDTO);

        assessmentService.createGlucoseLog(assessmentDTO);
        verify(fhirServiceApiInterface).getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class));
        verify(fhirServiceApiInterface).getExistingGlucoseLog(any(), any(), any());
        verify(fhirServiceApiInterface).assessmentCreate(any(), any(), any());

        when(fhirServiceApiInterface.getExistingGlucoseLog(any(), any(), any())).thenReturn(null);
        when(fhirServiceApiInterface.assessmentCreate(any(), any(), any())).thenReturn(assessmentDTO);
        when(riskAlgorithm.getRiskLevelInAssessmentDbm(any(RiskAlgorithmDTO.class))).thenReturn("High");

        assessmentService.createGlucoseLog(assessmentDTO);
        verify(fhirServiceApiInterface, times(2)).getScreeningLog(any(), any(), any(ScreeningLogRequestDTO.class));
        verify(fhirServiceApiInterface, times(2)).getExistingGlucoseLog(any(), any(), any());

        TestDataProvider.cleanUp();
        commonUtilMockedStatic.close();
    }
}
