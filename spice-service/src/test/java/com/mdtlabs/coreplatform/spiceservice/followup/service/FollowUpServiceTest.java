package com.mdtlabs.coreplatform.spiceservice.followup.service;

import java.util.*;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegisterDetail;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterDetailRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.impl.FollowUpServiceImpl;

/**
 * <p>
 * FollowUpServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in FollowUpService class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FollowUpServiceTest {

    @InjectMocks
    FollowUpServiceImpl followUpService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private CallRegisterRepository callRegisterRepository;

    @Mock
    private CallRegisterDetailRepository callRegisterDetailRepository;

    @Mock
    ModelMapper mapper;

    @BeforeEach
    void init() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
    }

    @AfterEach
    void cleanUp() {
        TestDataProvider.cleanUp();
    }

    @Test
    void createFollowUp() {
        //given
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        CallRegister callRegister = TestDataProvider.getCallRegister();
        callRegister.setCallRegisterDetail(null);
        CallRegisterDetail callRegisterDetail = TestDataProvider.getCallRegisterDetail();
        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();
        //when
        when(callRegisterRepository.save(any())).thenReturn(callRegister);
        when(callRegisterDetailRepository.save(callRegisterDetail)).thenReturn(callRegisterDetail);
        when(fhirServiceApiInterface.createReferralTicket(CommonUtil.getAuthToken(), CommonUtil.getClient(), referralDetailsDTO)).thenReturn(referralDetailsDTO);
        //then
        FollowUpDTO response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        callRegister.setType(AppointmentType.REFERRED);
        followUpDTO.setType(AppointmentType.REFERRED);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(TestConstants.RECOVERED);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(Constants.ON_TREATMENT);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setType(AppointmentType.MEDICAL_REVIEW);
        callRegister.setType(AppointmentType.MEDICAL_REVIEW);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(TestConstants.RECOVERED);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(Constants.REFERRED);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setFollowUpDetails(null);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(null);
        response = followUpService.createFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateFollowUp() {
        //given
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        CallRegister callRegister = TestDataProvider.getCallRegister();
        CallRegisterDetail callRegisterDetail = TestDataProvider.getCallRegisterDetail();
        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();

        //when
        when(callRegisterRepository.findByIdAndIsDeletedFalse(followUpDTO.getId())).thenReturn(callRegister);
        when(callRegisterRepository.save(any())).thenReturn(callRegister);
        when(callRegisterDetailRepository.save(callRegisterDetail)).thenReturn(callRegisterDetail);
        when(fhirServiceApiInterface.createReferralTicket(CommonUtil.getAuthToken(), CommonUtil.getClient(), referralDetailsDTO)).thenReturn(referralDetailsDTO);

        //then
        FollowUpDTO response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setFollowUpDetails(null);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(TestConstants.RECOVERED);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        callRegister.setType(AppointmentType.REFERRED);
        followUpDTO.setType(AppointmentType.REFERRED);
        followUpDTO.setCurrentPatientStatus(Constants.RECOVERED);
        List<CallRegister> callRegisters = List.of(callRegister);
        when(callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                any(), any(), any(), any())).thenReturn(callRegisters);
        when(callRegisterRepository.findByIdAndIsDeletedFalse(followUpDTO.getId())).thenReturn(callRegister);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(Constants.REFERRED);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCalledAt(TestConstants.ONE);
        followUpDTO.setIsWrongNumber(Boolean.TRUE);
        when(callRegisterRepository
                .findByMemberIdAndIsCompletedAndIsDeletedFalse(any(), any())).thenReturn(callRegisters);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        callRegister.setType(AppointmentType.MEDICAL_REVIEW);
        followUpDTO.setType(AppointmentType.MEDICAL_REVIEW);
        followUpDTO.setCurrentPatientStatus(Constants.REFERRED);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setCurrentPatientStatus(Constants.RECOVERED);
        response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
        followUpDTO.setId(TestConstants.TWO);
        Assertions.assertThrows(DataNotFoundException.class, () -> followUpService.updateFollowUp(followUpDTO));
        followUpDTO.setId(null);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> followUpService.updateFollowUp(followUpDTO));
    }

    @Test
    void updateFollowUp_NON_COMMUNITY() {
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUpDTO();
        followUpDTO.setId(TestConstants.TWO);
        followUpDTO.setAppType(Constants.NON_COMMUNITY);
        CallRegister oldCallRegister = TestDataProvider.getCallRegister();
        followUpDTO.setFollowUpDetails(TestDataProvider.getFollowUpDetailDTO());
        //when
        when(callRegisterRepository.findById(followUpDTO.getId())).thenReturn(Optional.of(oldCallRegister));
        when(callRegisterRepository.save(oldCallRegister)).thenReturn(oldCallRegister);
        //then
        FollowUpDTO response = followUpService.updateFollowUp(followUpDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getFollowUpList() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        Calendar calendar = Calendar.getInstance();

        requestDTO.setVillageIds(List.of(new String[]{"123", "234"}));
        requestDTO.setCurrentSyncTime(calendar.getTime());
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        //when
        when(callRegisterRepository.findByVillageIds(any(), any(), any(), any(), any(), any(), any())).thenReturn(callRegisters);
        when(mapper.map(any(), any())).thenReturn(new FollowUpDTO());

        //then
        List<FollowUpDTO> response = followUpService.getFollowUpList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getFollowUpCriteria() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        ReflectionTestUtils.setField(followUpService, TestConstants.MALARIA_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.DIARRHEA__CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.PNEUMONIA_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.MUAC_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.ANC_VISIT_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.PNC_VISIT_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.SUCCESSFUL_CALL_ATTEMPTS, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.UNSUCCESSFUL_CALL_ATTEMPTS, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.ESCALATION_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.REFERRAL_CRITERIA, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(followUpService, TestConstants.CHILD_VISIT_CRITERIA, TestConstants.INT_ONE);

        //then
        FollowUpCriteria response = followUpService.getFollowUpCriteria(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getFollowUpCriteria_ICCM() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setAppType(Constants.NON_COMMUNITY);
        FollowUpCriteria response = followUpService.getFollowUpCriteria(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void testGetScreeningFollowUpPatients_WithSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText("testSearchText");
        Map<String, Object> callRegisterData = new HashMap<>();
        callRegisterData.put(FieldConstants.PATIENT_ID, "patientId1");
        List<Map<String, Object>> callRegisterPatientIds = Collections.singletonList(callRegisterData);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegisterRepository.getScreeningCallRegisterPatientIds(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString()))
                .thenReturn(callRegisterPatientIds);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(),
                any(PatientRequestDTO.class))).thenReturn(patientDetailsMap);
        when(callRegisterRepository.getCallRegisterByPatientIds(anyString(), anyString(), anyBoolean(), anySet()))
                .thenReturn(callRegisters);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getScreeningFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetScreeningFollowUpPatients_WithoutSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText(null);
        Page<Map<String, Object>> callRegistersPage = mock(Page.class);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegistersPage.getContent()).thenReturn(callRegisters);
        when(callRegisterRepository.getScreeningCallRegister(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString(), any(Pageable.class)))
                .thenReturn(callRegistersPage);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(), any(PatientRequestDTO.class)))
                .thenReturn(patientDetailsMap);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getScreeningFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetAssessmentFollowUpPatients_WithSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_DAILY);
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText("testSearchText");
        Map<String, Object> callRegisterData = new HashMap<>();
        callRegisterData.put(FieldConstants.PATIENT_ID, "patientId1");
        List<Map<String, Object>> callRegisterPatientIds = Collections.singletonList(callRegisterData);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegisterRepository.getAssessmentCallRegisterPatientIds(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString()))
                .thenReturn(callRegisterPatientIds);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(),
                any(PatientRequestDTO.class))).thenReturn(patientDetailsMap);
        when(callRegisterRepository.getCallRegisterByPatientIds(anyString(), anyString(), anyBoolean(), anySet()))
                .thenReturn(callRegisters);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getAssessmentFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetAssessmentFollowUpPatients_WithoutSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setDateRange(Constants.DASHBOARD_YESTERDAY);
        patientRequestDTO.setSearchText(null); // Set search text to null
        Page<Map<String, Object>> callRegistersPage = mock(Page.class);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegistersPage.getContent()).thenReturn(callRegisters);
        when(callRegisterRepository.getAssessmentCallRegister(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString(), any(Pageable.class)))
                .thenReturn(callRegistersPage);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(), any(PatientRequestDTO.class)))
                .thenReturn(patientDetailsMap);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getAssessmentFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetLostToFollowUpPatients_WithSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText("testSearchText");
        Map<String, Object> callRegisterData = new HashMap<>();
        callRegisterData.put(FieldConstants.PATIENT_ID, "patientId1");
        List<Map<String, Object>> callRegisterPatientIds = Collections.singletonList(callRegisterData);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegisterRepository.getLostToFollowUpCallRegisterPatientIds(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString()))
                .thenReturn(callRegisterPatientIds);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(),
                any(PatientRequestDTO.class))).thenReturn(patientDetailsMap);
        when(callRegisterRepository.getCallRegisterByPatientIds(anyString(), anyString(), anyBoolean(), anySet()))
                .thenReturn(callRegisters);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getLostToFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetLostToFollowUpPatients_WithoutSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_MONTHLY);
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText(null); // Set search text to null
        Page<Map<String, Object>> callRegistersPage = mock(Page.class);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegistersPage.getContent()).thenReturn(callRegisters);
        when(callRegisterRepository.getLostToFollowUpCallRegister(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString(), any(Pageable.class)))
                .thenReturn(callRegistersPage);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(), any(PatientRequestDTO.class)))
                .thenReturn(patientDetailsMap);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getLostToFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetMedicalReviewFollowUpPatients_WithSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText("testSearchText");
        Map<String, Object> callRegisterData = new HashMap<>();
        callRegisterData.put(FieldConstants.PATIENT_ID, "patientId1");
        List<Map<String, Object>> callRegisterPatientIds = Collections.singletonList(callRegisterData);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegisterRepository.getMedicalReviewFollowUpCallRegisterPatientIds(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString()))
                .thenReturn(callRegisterPatientIds);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(),
                any(PatientRequestDTO.class))).thenReturn(patientDetailsMap);
        when(callRegisterRepository.getCallRegisterByPatientIds(anyString(), anyString(), anyBoolean(), anySet()))
                .thenReturn(callRegisters);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getMedicalReviewFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetMedicalReviewFollowUpPatients_WithoutSearchText() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setDateRange(null);
        patientRequestDTO.setSiteId("testSiteId");
        patientRequestDTO.setSearchText(null);
        Page<Map<String, Object>> callRegistersPage = mock(Page.class);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegistersPage.getContent()).thenReturn(callRegisters);
        when(callRegisterRepository.getMedicalReviewFollowUpCallRegister(
                anyString(), anyString(), anyBoolean(), anyList(),
                anyInt(), anyString(), anyString(), any(Pageable.class)))
                .thenReturn(callRegistersPage);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(), any(PatientRequestDTO.class)))
                .thenReturn(patientDetailsMap);

        // then
        ResponseListDTO<FollowUpDTO> response = followUpService.getMedicalReviewFollowUpPatients(patientRequestDTO);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertFalse(response.getData().isEmpty());
        Assertions.assertEquals(1, response.getData().size());
    }

    @Test
    void testGetAllCallRegistersByVillages_WithData() {
        // given
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        Page<Map<String, Object>> callRegistersPage = mock(Page.class);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        TestDataProvider.getDateUtilStaticMock();
        when(callRegistersPage.getContent()).thenReturn(callRegisters);
        when(callRegisterRepository.getAllCallRegisters(
                anyString(), any(), any(), anyList(), any(Pageable.class)))
                .thenReturn(callRegistersPage);
        when(fhirServiceApiInterface.getPatientNcdListBySearchText(anyString(), anyString(),
                any(PatientRequestDTO.class))).thenReturn(patientDetailsMap);

        // then
        List<FollowUpDTO> followUpList = followUpService.getAllCallRegistersByVillages(patientRequestDTO,
                Constants.SCREENED);
        TestDataProvider.dateUtilCleanUp();
        Assertions.assertNotNull(followUpList);
        Assertions.assertFalse(followUpList.isEmpty());
        Assertions.assertEquals(1, followUpList.size());
    }

    @Test
    void testGetAllCallRegistersByVillages_WithoutData() {
        // given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSiteId("testSiteId");
        Page<Map<String, Object>> callRegistersPage = mock(Page.class);
        Map<String, PatientDetailsDTO> patientDetailsMap = new HashMap<>();
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsMap.put("patientId1", patientDetailsDTO);
        List<Map<String, Object>> callRegisters = List.of(TestDataProvider.getCallRegisterDetails());

        // when
        when(callRegistersPage.getContent()).thenReturn(callRegisters);
        when(callRegisterRepository.getAllCallRegisters(
                anyString(), any(), any(), anyList(), any(Pageable.class)))
                .thenReturn(callRegistersPage);

        // then
        List<FollowUpDTO> followUpList = followUpService.getAllCallRegistersByVillages(patientRequestDTO,
                Constants.SCREENED);
        Assertions.assertNotNull(followUpList);
        Assertions.assertTrue(followUpList.isEmpty());
    }

    @Test
    void addCallRegister() {
        CallRegister callRegister = TestDataProvider.getCallRegister();
        boolean updateOldCallRegister = Boolean.TRUE;
        CallRegister oldCallRegister = TestDataProvider.getCallRegister();
        oldCallRegister.setDeleted(Boolean.TRUE);
        oldCallRegister.setIsCompleted(Boolean.TRUE);
        oldCallRegister.setNextBPAssessmentDate(new Date());
        oldCallRegister.setNextBGAssessmentTime(new Date());
        callRegister.setType(AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW);
        callRegister.setType(AppointmentType.ASSESSMENT);
        callRegister.setMemberId(TestConstants.MEMBER_ID);
        callRegister.setNextBPAssessmentDate(new Date());
        callRegister.setNextBGAssessmentTime(new Date());
        List<CallRegister> oldCallRegisters = List.of(oldCallRegister);

        //then
        when(callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(
                callRegister.getMemberId(), callRegister.getType())).thenReturn(oldCallRegisters);
        FollowUpDTO response = followUpService.addCallRegister(callRegister, updateOldCallRegister);
        System.out.println("response"+response);
        //Assertions.assertNotNull(response);
    }

    @Test
    void createNcdFollowUp() {

        FollowUpDTO followUpDTO = TestDataProvider.getFollowUpDTO();
        CallRegister oldCallRegister = TestDataProvider.getCallRegister();
        oldCallRegister.setDeleted(Boolean.TRUE);
        oldCallRegister.setIsCompleted(Boolean.TRUE);
        //when
        TestDataProvider.getStaticMock();
        when(callRegisterRepository.save(any())).thenReturn(oldCallRegister);
        //then
        FollowUpDTO response = followUpService.createNcdFollowUp(followUpDTO, true);
        Assertions.assertNotNull(response);
    }

    @Test
    void transferCallRegisters() {
        String patientReference = TestConstants.REFERRED;
        String siteReference = TestConstants.STRING_ONE;
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        //when
        when(callRegisterRepository.findByMemberIdAndIsCompletedAndIsDeletedFalse(
                patientReference, Boolean.FALSE)).thenReturn(callRegisters);
        //then
        followUpService.transferCallRegisters(patientReference, siteReference);
    }

    @Test
    void deleteNcdCallRegister() {
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUpDTO();
        followUpDTO.setMemberId(TestConstants.MEMBER_ID);
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        //when
        when(callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(
                followUpDTO.getMemberId(), followUpDTO.getType())).thenReturn(callRegisters);
        //then
        followUpService.deleteNcdCallRegister(followUpDTO);
    }

    @Test
    void getFollowUpPatients() {
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setType(AppointmentType.SCREENED.name());
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_DAILY);

        //when
        TestDataProvider.getDateUtilStaticMock();
        ResponseListDTO<FollowUpDTO> response = followUpService.getFollowUpPatients(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.dateUtilCleanUp();
    }

    @Test
    void getFollowUpPatients_ASSESSMENT() {
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setType(AppointmentType.ASSESSMENT.name());
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_DAILY);

        //when
        TestDataProvider.getDateUtilStaticMock();
        ResponseListDTO<FollowUpDTO> response = followUpService.getFollowUpPatients(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.dateUtilCleanUp();
    }

    @Test
    void getFollowUpPatients_NON_COMMUNITY_MEDICAL_REVIEW() {
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setType(AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.name());
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_DAILY);


        //when
        TestDataProvider.getDateUtilStaticMock();
        ResponseListDTO<FollowUpDTO> response = followUpService.getFollowUpPatients(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.dateUtilCleanUp();
    }

    @Test
    void getFollowUpPatients_LOST_TO_FOLLOW_UP() {
        TestDataProvider.initDateUtil();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setType(AppointmentType.LOST_TO_FOLLOW_UP.name());
        patientRequestDTO.setDateRange(Constants.FOLLOWUP_DAILY);


        //when
        TestDataProvider.getDateUtilStaticMock();
        ResponseListDTO<FollowUpDTO> response = followUpService.getFollowUpPatients(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.dateUtilCleanUp();
    }

    @Test
    void updateNcdFollowUp() {
        //given
        FollowUpDTO followUp = TestDataProvider.getFollowUpDTO();
        CallRegister oldCallRegister = TestDataProvider.getCallRegister();

        //when
        when(callRegisterRepository.findByIdAndIsDeletedFalseAndIsCompletedFalse(
                followUp.getId())).thenReturn(oldCallRegister);

        //then
        TestDataProvider.getStaticMock();
        when(callRegisterRepository.save(any())).thenReturn(oldCallRegister);
        FollowUpDTO response = followUpService.updateNcdFollowUp(followUp);
        Assertions.assertNotNull(response);

    }
}
