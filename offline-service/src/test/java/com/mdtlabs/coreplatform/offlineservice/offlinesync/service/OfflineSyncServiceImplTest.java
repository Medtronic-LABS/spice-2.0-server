package com.mdtlabs.coreplatform.offlineservice.offlinesync.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.apiinterface.AuthServiceApiInterface;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.offlineservice.apiinterface.CqlApiInterface;
import com.mdtlabs.coreplatform.offlineservice.apiinterface.SpiceServiceApiInterface;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import com.mdtlabs.coreplatform.offlineservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AncResultDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpCriteria;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberMapDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.MemberAssessmentFollowupMapDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncResponseDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.offlineservice.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.SqsResponseDTO;
import com.mdtlabs.coreplatform.offlineservice.common.model.OfflineSync;
import com.mdtlabs.coreplatform.offlineservice.common.model.OfflineSyncLog;
import com.mdtlabs.coreplatform.offlineservice.helper.HelperService;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.repository.OfflineSyncLogRepository;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.repository.OfflineSyncRepository;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.impl.OfflineSyncServiceImpl;
import com.mdtlabs.coreplatform.offlineservice.producer.ProducerService;
import feign.FeignException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.MessageHeaders;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
class OfflineSyncServiceImplTest {

    private static MockedStatic<CommonUtil> commonUtil;

    @InjectMocks
    OfflineSyncServiceImpl offlineSyncService;

    @Mock
    private ScheduledExecutorService scheduler;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private OfflineSyncRepository offlineSyncRepository;

    @Mock
    private OfflineSyncLogRepository offlineSyncLogRepository;

    @Mock
    private HelperService helperService;

    @Mock
    private ProducerService producerService;

    @Mock
    private AuthServiceApiInterface authServiceApiInterface;

    @Mock
    private SpiceServiceApiInterface spiceServiceApiInterface;

    @Mock
    private CqlApiInterface cqlApiInterface;

    @Mock
    private RedisTemplate<String, String> redisTemplate;

    @Mock
    private ValueOperations valueOperations;

    @Test
    void createOfflineSyncWithoutRequestId() {
        //given
        Map<String, Object> request = new HashMap<>();
        request.put(Constants.REQUEST_ID, null);
        OfflineSyncLog offlineSyncLog = TestDataProvider.getOfflineSyncLog(TestDataProvider.convertNonNullFieldsToHashMap(request));

        //when
        when(offlineSyncLogRepository.save(any())).thenReturn(offlineSyncLog);

        //then
        assertThrows(ServicesException.class, () -> offlineSyncService.createOfflineSync(request));
    }

    @Test
    void createOfflineSyncWithoutData() {
        //given
        Map<String, Object> request = new HashMap<>();
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);

        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithHouseholdData() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdDTO.getHouseholdMembers().get(Constants.ZERO));
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.HOUSEHOLDS, List.of(householdObj));
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), householdObj,
                TestDataProvider.getFixedDate(), Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        OfflineSync memberOfflineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID),
                householdMemberObj, TestDataProvider.getFixedDate(), Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);

        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(HouseholdDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(objectMapper.convertValue(any(HouseholdMemberDTO.class), any(TypeReference.class))).thenReturn(memberOfflineSync.getRequestData());
        when(offlineSyncRepository.saveAll(offlineSyncList)).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithFollowUpData() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        Map<String, Object> offlineRequestData = new HashMap<>();
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        Map<String, Object> followUpDTOObj = TestDataProvider.convertToHashMap(followUpDTO);
        List<Object> followupObjList = new ArrayList<>();
        followupObjList.add(followUpDTOObj);
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.FOLLOW_UPS, followupObjList);
        request.put(Constants.ASSESSMENTS, new ArrayList<>());
        offlineRequestData.put(Constants.FOLLOW_UPS, followupObjList);
        offlineRequestData.put(Constants.ASSESSMENTS, new ArrayList<>());
        offlineRequestData.put(Constants.HOUSEHOLD_MEMBER_ID, TestConstants.ONE);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID),
                offlineRequestData, null, Constants.MEMBER_ASSESSMENT_FOLLOW_UP_MAP, Constants.IN_PROGRESS);
        offlineSync.setReferenceId(null);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);
        String serializedHouseholdData = TestDataProvider.serializeData(followUpDTO);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        when(objectMapper.convertValue(any(FollowUpDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(offlineSyncRepository.saveAll(any())).thenReturn(offlineSyncList);

        //then
        offlineSyncService.createOfflineSync(request);
        verify(producerService, atLeastOnce()).sendBulkMessage(any(), any());
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithHouseholdDataOnly() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        householdDTO.setHouseholdMembers(null);
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.HOUSEHOLDS, List.of(householdObj));
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), householdObj,
                TestDataProvider.getFixedDate(), Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(HouseholdDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(offlineSyncRepository.saveAll(offlineSyncList)).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithHouseholdMemberLinks() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdDTO.getHouseholdMembers().get(Constants.ZERO));
        Map<String, Object> householdMemberLinks = TestDataProvider.convertToHashMap(TestDataProvider.getHouseholdMemberLinkDTO());
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        request.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);
        request.put(Constants.REFERENCE_ID, TestConstants.REQUEST_ID);
        request.put(Constants.HOUSEHOLD_MEMBERS, List.of(householdMemberObj));
        request.put(Constants.HOUSEHOLD_MEMBER_LINKS, List.of(householdMemberLinks));
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), householdObj,
                TestDataProvider.getFixedDate(), Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        OfflineSync memberOfflineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID),
                householdMemberObj, TestDataProvider.getFixedDate(), Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);

        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(HouseholdDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(objectMapper.convertValue(any(HouseholdMemberDTO.class), any(TypeReference.class))).thenReturn(memberOfflineSync.getRequestData());
        when(offlineSyncRepository.saveAll(offlineSyncList)).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithAppTypeNonCommunity() throws JsonProcessingException {
        //given
        Map<String, Object> request = TestDataProvider.getOfflineSyncObject();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUpDTO();
        followUpDTO.getProvenance().setModifiedDate(new Date());
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), householdObj,
                TestDataProvider.getFixedDate(), Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);

        OfflineSyncLog offlineSyncLog = new OfflineSyncLog();
        offlineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(offlineSyncLog);
        when(objectMapper.convertValue(any(HouseholdDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(objectMapper.convertValue(any(HouseholdMemberDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(offlineSyncRepository.saveAll(offlineSyncList)).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void processBatchRequest() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdDTO.getHouseholdMembers().get(Constants.ZERO));
        Map<String, Object> householdMemberLinks = TestDataProvider.convertToHashMap(TestDataProvider.getHouseholdMemberLinkDTO());

        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        request.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);
        request.put(Constants.REFERENCE_ID, TestConstants.REQUEST_ID);
        request.put(Constants.HOUSEHOLD_MEMBERS, List.of(householdMemberObj));
        request.put(Constants.HOUSEHOLD_MEMBER_LINKS, List.of(householdMemberLinks));
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), householdObj,
                TestDataProvider.getFixedDate(), Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        OfflineSync memberOfflineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID),
                householdMemberObj, TestDataProvider.getFixedDate(), Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        List<OfflineSync> offlineSyncList = List.of(offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync,
                offlineSync);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(HouseholdDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(objectMapper.convertValue(any(HouseholdMemberDTO.class), any(TypeReference.class))).thenReturn(memberOfflineSync.getRequestData());
        when(offlineSyncRepository.saveAll(any())).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithMemberOnly() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        Map<String, Object> offlineRequestData = new HashMap<>();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        String serializedHouseholdMemberData = TestDataProvider.serializeData(householdMemberDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdMemberDTO);
        List<Object> householdMemberObjList = new ArrayList<>();
        householdMemberObjList.add(householdMemberObj);
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.HOUSEHOLD_MEMBERS, List.of(householdMemberObj));
        offlineRequestData.put(Constants.HOUSEHOLD_ID, TestConstants.ONE);
        offlineRequestData.put(Constants.HOUSEHOLD_MEMBERS, householdMemberObjList);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), offlineRequestData,
                null, Constants.HOUSEHOLD_MEMBER_MAP, Constants.IN_PROGRESS);
        offlineSync.setReferenceId(null);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(HouseholdMemberDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(offlineSyncRepository.saveAll(any())).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdMemberData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(producerService, atLeastOnce()).sendBulkMessage(any(), any());
    }

    @Test
    void createOfflineSyncWithAssessmentData() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        Map<String, Object> offlineRequestData = new HashMap<>();
        AssessmentDTO assessment = TestDataProvider.getAssessmentData();
        String serializedAssessmentData = TestDataProvider.serializeData(assessment);
        Map<String, Object> assessmentObj = TestDataProvider.convertToHashMap(assessment);
        List<Object> assessmentObjList = new ArrayList<>();
        assessmentObjList.add(assessmentObj);
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.ASSESSMENTS, assessmentObjList);
        request.put(Constants.FOLLOW_UPS, new ArrayList<>());
        offlineRequestData.put(Constants.ASSESSMENTS, assessmentObjList);
        offlineRequestData.put(Constants.FOLLOW_UPS, new ArrayList<>());
        offlineRequestData.put(Constants.HOUSEHOLD_MEMBER_ID, TestConstants.ONE);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID),
                offlineRequestData, null, Constants.MEMBER_ASSESSMENT_FOLLOW_UP_MAP, Constants.IN_PROGRESS);
        offlineSync.setReferenceId(null);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(AssessmentDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(offlineSyncRepository.saveAll(any())).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedAssessmentData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(producerService, atLeastOnce()).sendBulkMessage(any(), any());
    }

    @Test
    void createOfflineSyncWithMultipleHouseholdData() throws JsonProcessingException {
        //given
        Map<String, Object> request = new HashMap<>();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        householdDTO.setHouseholdMembers(null);
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        request.put(Constants.REQUEST_ID, TestConstants.REQUEST_ID);
        request.put(Constants.HOUSEHOLDS, Collections.nCopies(Constants.TWELVE, householdObj));
        OfflineSync offlineSync = TestDataProvider.getOfflineSync((String) request.get(Constants.REQUEST_ID), householdObj,
                TestDataProvider.getFixedDate(), Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        List<OfflineSync> offlineSyncList = Collections.nCopies(Constants.TWELVE, offlineSync);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(any(HouseholdDTO.class), any(TypeReference.class))).thenReturn(offlineSync.getRequestData());
        when(offlineSyncRepository.saveAll(offlineSyncList)).thenReturn(offlineSyncList);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdData);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.createOfflineSync(request);
        verify(offlineSyncLogRepository, times(2)).save(any(OfflineSyncLog.class));
    }

    @Test
    void createOfflineSyncWithException() {
        //given
        Map<String, Object> request = new HashMap<>();
        request.put(Constants.REQUEST_ID, null);
        OfflineSyncLog offlineSyncLog = TestDataProvider.getOfflineSyncLog(TestDataProvider.convertNonNullFieldsToHashMap(request));

        //when
        when(offlineSyncLogRepository.save(any())).thenReturn(offlineSyncLog);

        //then
        ServicesException exception = assertThrows(ServicesException.class, () -> offlineSyncService.createOfflineSync(request));
        assertEquals("Error while fetching the concurrent data.", exception.getMessage());

    }

    @Test
    void processHouseholdMemberLink() throws JsonProcessingException {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        HouseholdMemberLinkDTO householdMemberLinkDTO = TestDataProvider.getHouseholdMemberLinkDTO();
        String serializedHouseholdMemberLink = TestDataProvider.serializeData(householdMemberLinkDTO);
        Map<String, Object> householdMemberLinkMap = TestDataProvider.convertToHashMap(householdMemberLinkDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdMemberLinkMap, null, Constants.HOUSEHOLD_MEMBER_LINK, Constants.IN_PROGRESS);
        offlineSync.setType(Constants.HOUSEHOLD_MEMBER_LINK);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(objectMapper.readValue(serializedHouseholdMemberLink, HouseholdMemberLinkDTO.class)).thenReturn(householdMemberLinkDTO);
        getAccessToken();

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdMemberLink, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
        assertEquals(Constants.SUCCESS, offlineSync.getStatus());
    }

    @Test
    void getAccessToken() {
        //given
        String accessToken = TestConstants.ACCESS_TOKEN;
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.ENCRYPTED_PASSWORD_KEY, TestConstants.ENCRYPTED_PASSWORD);
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        LinkedMultiValueMap<String, String> requestData = new LinkedMultiValueMap<>();
        requestData.add(com.mdtlabs.coreplatform.commonservice.common.Constants.USERNAME, TestConstants.USERNAME);
        requestData.add(com.mdtlabs.coreplatform.commonservice.common.Constants.PASSWORD, TestConstants.ENCRYPTED_PASSWORD);
        MultiValueMap<String, String> responseHeaders = new LinkedMultiValueMap<>();
        responseHeaders.add(HttpHeaders.AUTHORIZATION, accessToken);
        ResponseEntity<String> response = new ResponseEntity<>(TestConstants.HASH_STRING, responseHeaders, HttpStatus.OK);

        //when
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(null);
        when(authServiceApiInterface.login(requestData, Constants.CLIENT)).thenReturn(response);
        String bearerTokenResponse = offlineSyncService.getAccessToken();

        //then
        assertEquals(accessToken, bearerTokenResponse);
    }

    @Test
    void getAccessTokenFromRedis() {
        //given
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);

        //when
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(redisTemplate.getExpire(accessToken, TimeUnit.SECONDS)).thenReturn(TestConstants.ZERO);

        //then
        String bearerTokenResponse = offlineSyncService.getAccessToken();
        assertEquals(bearerToken, bearerTokenResponse);
    }

    @Test
    void getAccessTokenFromRedisExpiredData() {
        //given
        String accessToken = TestConstants.ACCESS_TOKEN;
        String newAccessToken = TestConstants.ACCESS_TOKEN;
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.ENCRYPTED_PASSWORD_KEY, TestConstants.ENCRYPTED_PASSWORD);
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        LinkedMultiValueMap<String, String> requestData = new LinkedMultiValueMap<>();
        requestData.add(com.mdtlabs.coreplatform.commonservice.common.Constants.USERNAME, TestConstants.USERNAME);
        requestData.add(com.mdtlabs.coreplatform.commonservice.common.Constants.PASSWORD, TestConstants.ENCRYPTED_PASSWORD);
        MultiValueMap<String, String> responseHeaders = new LinkedMultiValueMap<>();
        responseHeaders.add(HttpHeaders.AUTHORIZATION, newAccessToken);
        ResponseEntity<String> response = new ResponseEntity<>(TestConstants.HASH_STRING, responseHeaders, HttpStatus.OK);

        //when
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(redisTemplate.getExpire(accessToken, TimeUnit.SECONDS)).thenReturn(TestConstants.ONE);
        when(authServiceApiInterface.login(requestData, Constants.CLIENT)).thenReturn(response);

        //then
        String bearerTokenResponse = offlineSyncService.getAccessToken();
        assertEquals(newAccessToken, bearerTokenResponse);

        //when
        when(redisTemplate.getExpire(accessToken, TimeUnit.SECONDS)).thenReturn(null);

        //then
        bearerTokenResponse = offlineSyncService.getAccessToken();
        assertEquals(newAccessToken, bearerTokenResponse);
    }

    @Test
    void processDataWithNoMatchingInput() throws JsonProcessingException {
        //given
        String serializedData = TestDataProvider.serializeData(new HashMap<>());
        Map<String, Object> obj = TestDataProvider.convertToHashMap(new HashMap<>());
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, obj, null, Constants.TYPE, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);

        //then
        offlineSyncService.processRequestQueue(serializedData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processHouseholdCreateData() throws JsonProcessingException {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdDTO.getHouseholdMembers().get(Constants.ZERO));
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdObj, null, Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        OfflineSync memberOfflineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdMemberObj, null, Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        HouseholdDTO householdResponse = TestDataProvider.getHouseholdData();
        householdResponse.setId(TestConstants.FHIR_ID);
        householdResponse.getHouseholdMembers().get(Constants.ZERO).setId(TestConstants.FHIR_ID);
        ResponseEntity<HouseholdDTO> response = new ResponseEntity<>(householdResponse, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedHouseholdData, HouseholdDTO.class)).thenReturn(householdDTO);
        when(spiceServiceApiInterface.createHouseHold(bearerToken, Constants.CLIENT, householdDTO)).thenReturn(response);
        when(offlineSyncRepository.findByRequestIdAndReferenceIdAndType(offlineSync.getRequestId(),
                householdDTO.getHouseholdMembers().get(Constants.ZERO).getReferenceId(),
                Constants.HOUSEHOLD_MEMBER)).thenReturn(memberOfflineSync);

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());

        householdResponse.getHouseholdMembers().get(0).setAssessments(null);
        offlineSyncService.processRequestQueue(serializedHouseholdData, messageHeaders);

        householdResponse.setHouseholdMembers(null);
        offlineSyncService.processRequestQueue(serializedHouseholdData, messageHeaders);

    }


    @Test
    void processHouseholdCreateDataWithFirstRetry() throws JsonProcessingException {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdObj, null, Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        HouseholdDTO householdResponse = TestDataProvider.getHouseholdData();
        householdResponse.setId(TestConstants.FHIR_ID);
        householdResponse.getHouseholdMembers().get(Constants.ZERO).setId(TestConstants.FHIR_ID);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedHouseholdData, HouseholdDTO.class)).thenReturn(householdDTO);
        when(spiceServiceApiInterface.createHouseHold(bearerToken, Constants.CLIENT, householdDTO)).thenThrow(FeignException.class);

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processHouseholdCreateWithAssessmentData() throws JsonProcessingException {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();
        householdDTO.getHouseholdMembers().get(Constants.ZERO).setAssessments(List.of(assessmentDTO));
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        String serializedAssessmentData = TestDataProvider.serializeData(assessmentDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdDTO.getHouseholdMembers().get(Constants.ZERO));
        Map<String, Object> assessmentObj = TestDataProvider.convertToHashMap(assessmentDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdObj, null, Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        OfflineSync assessmentOfflineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, assessmentObj, null, Constants.ASSESSMENT, Constants.IN_PROGRESS);
        List<OfflineSync> assessmentOfflineSyncList = List.of(assessmentOfflineSync);
        OfflineSync memberOfflineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdMemberObj, null, Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        HouseholdDTO householdResponse = TestDataProvider.getHouseholdData();
        householdResponse.setId(TestConstants.FHIR_ID);
        householdResponse.getHouseholdMembers().get(Constants.ZERO).setId(TestConstants.FHIR_ID);
        ResponseEntity<HouseholdDTO> response = new ResponseEntity<>(householdResponse, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedHouseholdData, HouseholdDTO.class)).thenReturn(householdDTO);
        when(spiceServiceApiInterface.createHouseHold(bearerToken, Constants.CLIENT, householdDTO)).thenReturn(response);
        when(offlineSyncRepository.findByRequestIdAndReferenceIdAndType(offlineSync.getRequestId(),
                householdDTO.getHouseholdMembers().get(Constants.ZERO).getReferenceId(),
                Constants.HOUSEHOLD_MEMBER)).thenReturn(memberOfflineSync);
        when(objectMapper.convertValue(any(AssessmentDTO.class), any(TypeReference.class))).thenReturn(assessmentOfflineSync.getRequestData());
        when(offlineSyncRepository.saveAll(assessmentOfflineSyncList)).thenReturn(assessmentOfflineSyncList);
        when(objectMapper.writeValueAsString(assessmentOfflineSync.getRequestData())).thenReturn(serializedAssessmentData);
        when(helperService.getHashString(assessmentOfflineSync.getRequestData().toString())).thenReturn(TestConstants.HASH_STRING);
        doNothing().when(producerService).sendBulkMessage(any(), any());

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processHouseholdUpdateData() throws JsonProcessingException {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        householdDTO.setHouseholdMembers(null);
        householdDTO.setId(TestConstants.FHIR_ID);
        String serializedHouseholdData = TestDataProvider.serializeData(householdDTO);
        Map<String, Object> householdObj = TestDataProvider.convertToHashMap(householdDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdObj, null, Constants.HOUSEHOLD, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        ResponseEntity<HouseholdDTO> response = new ResponseEntity<>(householdDTO, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedHouseholdData, HouseholdDTO.class)).thenReturn(householdDTO);
        when(spiceServiceApiInterface.updateHouseHold(bearerToken, Constants.CLIENT, householdDTO)).thenReturn(response);

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processHouseholdMemberCreateData() throws JsonProcessingException {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        String serializedHouseholdMemberData = TestDataProvider.serializeData(householdMemberDTO);
        householdMemberDTO.setAssessments(List.of(TestDataProvider.getAssessmentData()));
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdMemberDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdMemberObj, null, Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        HouseholdMemberDTO householdMemberResponse = TestDataProvider.getHouseHoldMember();
        householdMemberResponse.setId(TestConstants.FHIR_ID);
        ResponseEntity<HouseholdMemberDTO> response = new ResponseEntity<>(householdMemberResponse, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedHouseholdMemberData, HouseholdMemberDTO.class)).thenReturn(householdMemberDTO);
        when(spiceServiceApiInterface.createHouseHoldMember(bearerToken, Constants.CLIENT, householdMemberDTO)).thenReturn(response);
        when(spiceServiceApiInterface.updateHouseHoldMember(bearerToken, Constants.CLIENT, householdMemberDTO)).thenReturn(response);

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdMemberData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
        householdMemberDTO.setId(TestConstants.STRING_ONE);
        serializedHouseholdMemberData = TestDataProvider.serializeData(householdMemberDTO);
        when(objectMapper.readValue(serializedHouseholdMemberData, HouseholdMemberDTO.class)).thenReturn(householdMemberDTO);
        offlineSyncService.processRequestQueue(serializedHouseholdMemberData, messageHeaders);

    }

    @Test
    void processHouseholdMemberUpdateData() throws JsonProcessingException {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setId(TestConstants.FHIR_ID);
        String serializedHouseholdMemberData = TestDataProvider.serializeData(householdMemberDTO);
        Map<String, Object> householdMemberObj = TestDataProvider.convertToHashMap(householdMemberDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, householdMemberObj, null, Constants.HOUSEHOLD_MEMBER, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        ResponseEntity<HouseholdMemberDTO> response = new ResponseEntity<>(householdMemberDTO, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedHouseholdMemberData, HouseholdMemberDTO.class)).thenReturn(householdMemberDTO);
        when(spiceServiceApiInterface.updateHouseHoldMember(bearerToken, Constants.CLIENT, householdMemberDTO)).thenReturn(response);

        //then
        offlineSyncService.processRequestQueue(serializedHouseholdMemberData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processAssessmentData() throws JsonProcessingException {
        //given
        AssessmentDTO assessment = TestDataProvider.getAssessmentData();
        String serializedAssessmentData = TestDataProvider.serializeData(assessment);
        Map<String, Object> assessmentObj = TestDataProvider.convertToHashMap(assessment);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, assessmentObj, null, Constants.ASSESSMENT, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        AssessmentDTO assessmentResponse = TestDataProvider.getAssessmentData();
        assessmentResponse.setId(TestConstants.FHIR_ID);
        ResponseEntity<AssessmentDTO> response = new ResponseEntity<>(assessmentResponse, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedAssessmentData, AssessmentDTO.class)).thenReturn(assessment);
        when(spiceServiceApiInterface.createAssessment(bearerToken, Constants.CLIENT, assessment)).thenReturn(response);

        //then
        offlineSyncService.processRequestQueue(serializedAssessmentData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processHouseholdMemberData() throws JsonProcessingException {
        //given
        String data = TestDataProvider.serializeData(TestDataProvider.getHouseholdMemberMapDTO());
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, null, null, Constants.HOUSEHOLD_MEMBER_MAP, Constants.IN_PROGRESS);
        offlineSync.setCreatedBy(TestConstants.ONE);
        offlineSync.setRetryAttempts(TestConstants.INT_ONE);
        HouseholdMemberMapDTO householdMemberMapDTO = TestDataProvider.getHouseholdMemberMapDTO();

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(TestConstants.ONE)).thenReturn(offlineSync);
        when(objectMapper.readValue(data, HouseholdMemberMapDTO.class)).thenReturn(householdMemberMapDTO);
        when(offlineSyncRepository.save(any())).thenReturn(offlineSync);

        //then
        offlineSyncService.processRequestQueue(data, TestDataProvider.getMessageHeader(offlineSync.getId()));

        verify(offlineSyncRepository).save(argThat(saved -> saved.getStatus().equals(Constants.SUCCESS)));
    }

    @Test
    void processMultipleFollowUps() throws JsonProcessingException {
        //given
        String data = TestDataProvider.serializeData(TestDataProvider.getHouseholdMemberMapDTO());
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, null, null, Constants.HOUSEHOLD_MEMBER_MAP, Constants.IN_PROGRESS);
        offlineSync.setCreatedBy(TestConstants.ONE);
        Map<String, Object> offlineRequestData = new HashMap<>();
        offlineRequestData.put(Constants.FOLLOW_UPS, new ArrayList<>());
        offlineRequestData.put(Constants.HOUSEHOLD_MEMBER_ID, TestConstants.ONE);
        offlineSync.setRequestData(offlineRequestData);
        offlineSync.setRetryAttempts(TestConstants.INT_ONE);
        List<OfflineSync> offlineSyncs = Arrays.asList(offlineSync);
        HouseholdMemberMapDTO householdMemberMapDTO = TestDataProvider.getHouseholdMemberMapDTO();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMemberWithChildren();
        householdMemberDTO.setAssessments(Arrays.asList(TestDataProvider.getAssessmentData()));
        householdMemberDTO.getChildren().getFirst().setAssessments(Arrays.asList(TestDataProvider.getAssessmentData()));
        householdMemberDTO.getChildren().getFirst().getAssessments().getFirst().getEncounter().setProvenance(TestDataProvider.getProvenance());
        householdMemberDTO.getChildren().getFirst().getAssessments().getFirst().getEncounter().getProvenance().setModifiedDate(new Date());
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        ResponseEntity<HouseholdMemberDTO> response = new ResponseEntity<>(householdMemberDTO, HttpStatus.OK);
        response.getBody().setId(TestConstants.FHIR_ID);
        ResponseEntity<AssessmentDTO> assessmentDTOResponseEntity = new ResponseEntity<>(TestDataProvider.getAssessmentData(), HttpStatus.OK);
        assessmentDTOResponseEntity.getBody().setId(TestConstants.FHIR_ID);
        String serializedHouseholdMemberData = TestDataProvider.serializeData(householdMemberDTO);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(TestConstants.ONE)).thenReturn(offlineSync);
        when(objectMapper.readValue(data, HouseholdMemberMapDTO.class)).thenReturn(householdMemberMapDTO);
        when(offlineSyncRepository.save(any())).thenReturn(offlineSync);
        when(offlineSyncRepository.saveAll(any())).thenReturn(offlineSyncs);
        when(objectMapper.writeValueAsString(offlineSync.getRequestData())).thenReturn(serializedHouseholdMemberData);
        when(objectMapper.readValue(serializedHouseholdMemberData, HouseholdMemberDTO.class)).thenReturn(householdMemberDTO);
        getAccessToken();
        when(spiceServiceApiInterface.updateHouseHoldMember(bearerToken, Constants.CLIENT, householdMemberDTO)).thenReturn(response);
        getAccessTokenFromRedis();
        when(spiceServiceApiInterface.createHouseHoldMember(anyString(), anyString(), any())).thenReturn(response);
        when(spiceServiceApiInterface.createAssessment(anyString(), anyString(), any())).thenReturn(assessmentDTOResponseEntity);

        //then
        offlineSyncService.processRequestQueue(data, TestDataProvider.getMessageHeader(offlineSync.getId()));

        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processMemberAssessmentFollowUpData() throws JsonProcessingException {
        //given
        String data = TestDataProvider.serializeData(TestDataProvider.getMemberAssessmentFollowUpData());
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();
        assessmentDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, null, null, Constants.MEMBER_ASSESSMENT_FOLLOW_UP_MAP, Constants.IN_PROGRESS);
        offlineSync.setCreatedBy(TestConstants.ONE);
        offlineSync.setUpdatedAt(new Date());
        offlineSync.setRetryAttempts(TestConstants.INT_ONE);
        FollowUpDTO followUpDTOTwo = TestDataProvider.getFollowUpDTO();
        followUpDTOTwo.setId(23L);
        followUpDTOTwo.setUpdatedAt(2L);
        List<FollowUpDTO> followUps = Arrays.asList(TestDataProvider.getFollowUp(), followUpDTOTwo);
        MemberAssessmentFollowupMapDTO memberAssessmentFollowupMap = new MemberAssessmentFollowupMapDTO();
        memberAssessmentFollowupMap.setAssessments(Arrays.asList(assessmentDTO, assessmentDTO));
        memberAssessmentFollowupMap.setFollowUps(followUps);
        memberAssessmentFollowupMap.setHouseholdMemberId(TestConstants.PATIENT_ID);
        HouseholdMemberMapDTO householdMemberMapDTO = TestDataProvider.getHouseholdMemberMapDTO();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDetailsDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        memberAssessmentFollowupMap.getAssessments().getFirst().setEncounter(encounterDetailsDTO);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        followUpDTO.setId(Long.valueOf(TestConstants.FHIR_ID));
        ResponseEntity<AssessmentDTO> assessmentResponse = new ResponseEntity<>(assessmentDTO, HttpStatus.OK);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(1L)).thenReturn(offlineSync);
        when(objectMapper.readValue(data, HouseholdMemberMapDTO.class)).thenReturn(householdMemberMapDTO);
        when(objectMapper.convertValue(followUps.getFirst(), FollowUpDTO.class)).thenReturn(followUps.getFirst());
        when(offlineSyncRepository.saveAll(any())).thenReturn(List.of(offlineSync));
        when(offlineSyncRepository.saveAll(any())).thenReturn(List.of(offlineSync));
        when(objectMapper.writeValueAsString(any())).thenReturn(data);
        when(offlineSyncRepository.save(offlineSync)).thenReturn(offlineSync);
        when(objectMapper.readValue(anyString(), ArgumentMatchers.eq(MemberAssessmentFollowupMapDTO.class))).thenReturn(memberAssessmentFollowupMap);
        when(objectMapper.readValue(anyString(), ArgumentMatchers.eq(FollowUpDTO.class))).thenReturn(followUpDTO);
        when(offlineSyncRepository.save(offlineSync)).thenReturn(offlineSync);
        getAccessToken();
        getAccessToken();
        getAccessToken();
        when(objectMapper.readValue(anyString(), ArgumentMatchers.eq(AssessmentDTO.class))).thenReturn(assessmentDTO);
        when(spiceServiceApiInterface.createAssessment(bearerToken, Constants.CLIENT, assessmentDTO)).thenReturn(assessmentResponse);
        when(spiceServiceApiInterface.createFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenReturn(followUpDTO);
        when(spiceServiceApiInterface.updateFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenReturn(followUpDTO);

        //then
        offlineSyncService.processRequestQueue(data, TestDataProvider.getMessageHeader(offlineSync.getId()));

        verify(offlineSyncRepository, times(3)).save(offlineSync);
    }


    @Test
    void processRequestDataWhenIdIsNull() throws JsonProcessingException {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        followUpDTO.setId(null);
        String serializedFollowUpData = TestDataProvider.serializeData(followUpDTO);
        Map<String, Object> followupObj = TestDataProvider.convertToHashMap(followUpDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, followupObj, null, Constants.FOLLOW_UP, Constants.IN_PROGRESS);
        offlineSync.setType(Constants.FOLLOW_UPS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        followUpDTO.setId(Long.valueOf(TestConstants.FHIR_ID));

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedFollowUpData, FollowUpDTO.class)).thenReturn(followUpDTO);
        when(spiceServiceApiInterface.createFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenReturn(followUpDTO);
        when(spiceServiceApiInterface.updateFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenReturn(followUpDTO);

        //then
        offlineSyncService.processRequestQueue(serializedFollowUpData, messageHeaders);
        followUpDTO.setId(null);
        offlineSyncService.processRequestQueue(serializedFollowUpData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
        assertEquals(Constants.SUCCESS, offlineSync.getStatus());
    }


    @Test
    void processFollowUpData() throws JsonProcessingException {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        String serializedFollowUpData = TestDataProvider.serializeData(followUpDTO);
        Map<String, Object> followupObj = TestDataProvider.convertToHashMap(followUpDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, followupObj, null, Constants.FOLLOW_UP, Constants.IN_PROGRESS);
        offlineSync.setType(Constants.FOLLOW_UPS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        followUpDTO.setId(Long.valueOf(TestConstants.FHIR_ID));

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedFollowUpData, FollowUpDTO.class)).thenReturn(followUpDTO);
        when(spiceServiceApiInterface.createFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenReturn(followUpDTO);
        when(spiceServiceApiInterface.updateFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenReturn(followUpDTO);


        //then
        offlineSyncService.processRequestQueue(serializedFollowUpData, messageHeaders);
        followUpDTO.setId(null);
        offlineSyncService.processRequestQueue(serializedFollowUpData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
        assertEquals(Constants.SUCCESS, offlineSync.getStatus());
    }

    @Test
    void processFollowUpDataWithFirstRetry() throws JsonProcessingException {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        FollowUpDTO followUpDTO = TestDataProvider.getFollowUp();
        String serializedFollowUpData = TestDataProvider.serializeData(followUpDTO);
        Map<String, Object> followupObj = TestDataProvider.convertToHashMap(followUpDTO);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, followupObj, null, Constants.FOLLOW_UP, Constants.IN_PROGRESS);
        offlineSync.setType(Constants.FOLLOW_UPS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedFollowUpData, FollowUpDTO.class)).thenReturn(followUpDTO);
        when(spiceServiceApiInterface.createFollowUp(bearerToken, Constants.CLIENT, followUpDTO)).thenThrow(FeignException.FeignClientException.class);

        //then
        offlineSyncService.processRequestQueue(serializedFollowUpData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
        assertEquals(Constants.SUCCESS, offlineSync.getStatus());
    }

    @Test
    void processAssessmentDataWithFirstRetry() throws JsonProcessingException {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        AssessmentDTO assessment = TestDataProvider.getAssessmentData();
        String serializedAssessmentData = TestDataProvider.serializeData(assessment);
        Map<String, Object> assessmentObj = TestDataProvider.convertToHashMap(assessment);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, assessmentObj, null, Constants.ASSESSMENT, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        AssessmentDTO assessmentResponse = TestDataProvider.getAssessmentData();
        assessmentResponse.setId(TestConstants.FHIR_ID);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedAssessmentData, AssessmentDTO.class)).thenReturn(assessment);
        when(spiceServiceApiInterface.createAssessment(bearerToken, Constants.CLIENT, assessment)).thenThrow(FeignException.FeignClientException.class);

        //then
        offlineSyncService.processRequestQueue(serializedAssessmentData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
    }

    @Test
    void processAssessmentDataWithRetryAttemptExceeded() throws JsonProcessingException {
        //given
        AssessmentDTO assessment = TestDataProvider.getAssessmentData();
        String serializedAssessmentData = TestDataProvider.serializeData(assessment);
        Map<String, Object> assessmentObj = TestDataProvider.convertToHashMap(assessment);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, assessmentObj, null, Constants.ASSESSMENT, Constants.IN_PROGRESS);
        offlineSync.setRetryAttempts(TestConstants.THREE);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.USERNAME_KEY, TestConstants.USERNAME);
        String accessToken = TestConstants.ACCESS_TOKEN;
        String bearerToken = StringUtil.concatString(accessToken);
        AssessmentDTO assessmentResponse = TestDataProvider.getAssessmentData();
        assessmentResponse.setId(TestConstants.FHIR_ID);

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(TestConstants.USERNAME)).thenReturn(accessToken);
        when(objectMapper.readValue(serializedAssessmentData, AssessmentDTO.class)).thenReturn(assessment);
        when(spiceServiceApiInterface.createAssessment(bearerToken, Constants.CLIENT, assessment)).thenThrow(FeignException.FeignClientException.class);

        //then
        offlineSyncService.processRequestQueue(serializedAssessmentData, messageHeaders);
        verify(offlineSyncRepository, atLeastOnce()).save(any());
        verify(producerService, never()).sendMessage(any(), any(), any(), any());
    }

    @Test
    void generateResponseWithoutRequestId() {
        //given
        RequestDTO request = new RequestDTO();

        //then
        assertThrows(DataNotAcceptableException.class, () -> offlineSyncService.generateResponse(request));
    }

    @Test
    void generateResponse() throws JsonProcessingException {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);
        Map<String, Object> requestObj = TestDataProvider.convertToHashMap(request);
        String serializedRequestData = TestDataProvider.serializeData(request);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, requestObj, null, null, Constants.IN_PROGRESS);
        offlineSync.setId(TestConstants.ONE);
        offlineSync.setReferenceId(null);

        //when
        when(objectMapper.convertValue(any(RequestDTO.class), any(TypeReference.class))).thenReturn(requestObj);
        when(offlineSyncRepository.save(any())).thenReturn(offlineSync);
        when(objectMapper.writeValueAsString(request)).thenReturn(serializedRequestData);
        doNothing().when(producerService).sendMessage(any(), any(), any(), any());

        //then
        offlineSyncService.generateResponse(request);
        verify(producerService, atLeastOnce()).sendMessage(any(), any(), any(), any());
    }

    @Test
    void generateResponseParseError() throws JsonProcessingException {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);
        Map<String, Object> requestObj = TestDataProvider.convertToHashMap(request);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, requestObj, null, null, Constants.IN_PROGRESS);
        offlineSync.setReferenceId(null);

        //when
        when(objectMapper.convertValue(any(RequestDTO.class), any(TypeReference.class))).thenReturn(requestObj);
        when(offlineSyncRepository.save(offlineSync)).thenReturn(offlineSync);
        when(objectMapper.writeValueAsString(request)).thenThrow(JsonProcessingException.class);

        //then
        assertThrows(ServicesException.class, () -> offlineSyncService.generateResponse(request));
    }

    @Test
    void processResponseQueue() throws JsonProcessingException {
        //given
        String serializedData = TestDataProvider.serializeData(new HashMap<>());
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, new HashMap<>(), null, Constants.TYPE, Constants.IN_PROGRESS);
        String fileName = StringUtil.concatString(offlineSync.getRequestId(), Constants.FILE_EXTENSION_JSON);
        SqsResponseDTO sqsResponseDTO = new SqsResponseDTO();
        File file = new File(fileName);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(objectMapper.writeValueAsString(sqsResponseDTO)).thenReturn(serializedData);
        doNothing().when(helperService).uploadFileToS3(fileName, file);

        //then
        offlineSyncService.processResponseQueue(serializedData, messageHeaders);
        verify(helperService, atLeastOnce()).uploadFileToS3(any(), any());
        file.deleteOnExit();
    }

    @Test
    void processResponseQueueWithParseError() throws JsonProcessingException {
        //given
        String serializedData = TestDataProvider.serializeData(new HashMap<>());
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(TestConstants.REQUEST_ID, new HashMap<>(), null, Constants.TYPE, Constants.IN_PROGRESS);
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(offlineSync.getId());

        //when
        when(offlineSyncRepository.findByIdAndIsDeletedFalse(offlineSync.getId())).thenReturn(offlineSync);
        when(objectMapper.writeValueAsString(new SqsResponseDTO())).thenThrow(JsonProcessingException.class);

        //then
        offlineSyncService.processResponseQueue(serializedData, messageHeaders);
        verify(helperService, never()).uploadFileToS3(any(), any());
    }

    @Test
    void testGetOfflineSyncStatusList_withDataRequired() {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);
        request.setUserId(TestConstants.ONE);
        request.setDataRequired(Boolean.TRUE);
        Map<String, Object> mockRequestData = new HashMap<>();
        mockRequestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        mockRequestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);
        OfflineSync offlineSync = TestDataProvider.getOfflineSync(
                request.getRequestId(), new HashMap<>(), null, Constants.HOUSEHOLD, Constants.SUCCESS);
        List<OfflineSync> offlineSyncList = List.of(offlineSync);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(request, Map.class)).thenReturn(mockRequestData);
        when(offlineSyncRepository.getOfflineSync(request.getRequestId(),
                request.getUserId(), request.getTypes(), request.getStatuses())).thenReturn(offlineSyncList);

        //then
        ResponseListDTO<OfflineSyncDTO> response = offlineSyncService.getOfflineSyncStatusList(request);
        assertEquals(request.getRequestId(), response.getData().get(Constants.ZERO).getRequestId());
        assertNotNull(response.getData().get(Constants.ZERO).getData());
    }

    @Test
    void testGetOfflineSyncStatusList_withException() {
        //given
        RequestDTO request = new RequestDTO();
        request.setRequestId(TestConstants.REQUEST_ID);
        request.setUserId(TestConstants.ONE);
        request.setTypes(List.of("String1", "String2"));
        request.setStatuses(List.of("String1", "String2"));
        Map<String, Object> mockRequestData = new HashMap<>();
        mockRequestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        mockRequestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);
        OfflineSyncLog mockOfflineSyncLog = new OfflineSyncLog();
        mockOfflineSyncLog.setStatus(Constants.IN_PROGRESS);

        //when
        when(offlineSyncLogRepository.save(any(OfflineSyncLog.class))).thenReturn(mockOfflineSyncLog);
        when(objectMapper.convertValue(request, Map.class)).thenReturn(mockRequestData);
        when(offlineSyncRepository.getOfflineSync(request.getRequestId(),
                request.getUserId(), request.getTypes(), request.getStatuses()))
                .thenThrow(new RuntimeException("Database error"));

        //then
        ServicesException exception = assertThrows(ServicesException.class, () -> offlineSyncService.getOfflineSyncStatusList(request));
        assertEquals(1006, exception.getCode());
    }

    @Test
    void testFetchSyncedDataWhenIsSmartAncIsTrue() {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.IS_SMART_ANC, Boolean.TRUE);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setVillageIds(List.of(TestConstants.STRING_ONE));
        requestDTO.setUserId(TestConstants.ONE);
        requestDTO.setDataRequired(true);
        Map<String, Object> mockRequestData = new HashMap<>();
        mockRequestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        mockRequestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);
        OfflineSyncLog offlineSyncLog = TestDataProvider.getOfflineSyncLog(TestDataProvider.convertNonNullFieldsToHashMap(requestDTO));
        List<HouseholdDTO> householdDTOList = List.of(TestDataProvider.getHouseholdData());
        List<HouseholdMemberDTO> memberDTOList = List.of(TestDataProvider.getHouseHoldMember());
        List<PregnancyInfo> pregnancyInfoList = List.of(TestDataProvider.getPregnancyInfo());
        List<FollowUpDTO> followUpDTOList = List.of(TestDataProvider.getFollowUp());
        FollowUpCriteria followUpCriteria = TestDataProvider.getFollowUpCriteria();
        List<AncResultDTO> ancResultList = List.of(TestDataProvider.getAncResultDTO());
        commonUtil = mockStatic(CommonUtil.class);

        //when
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(objectMapper.convertValue(requestDTO, Map.class)).thenReturn(mockRequestData);
        when(offlineSyncLogRepository.save(any())).thenReturn(offlineSyncLog);
        when(spiceServiceApiInterface.getHouseholdList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(householdDTOList);
        when(spiceServiceApiInterface.getHouseholdMemberList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(memberDTOList);
        when(spiceServiceApiInterface.getPregnancyInfoByVillages(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(pregnancyInfoList);
        when(spiceServiceApiInterface.getFollowUpList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(followUpDTOList);
        when(spiceServiceApiInterface.getFollowUpCriteria(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(followUpCriteria);
        when(cqlApiInterface.getAncResultByVillages(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(ancResultList);

        //then
        OfflineSyncResponseDTO response = offlineSyncService.fetchSyncedData(requestDTO);
        assertNotNull(response);
        assertEquals(householdDTOList, response.getHouseholds());
        assertEquals(followUpDTOList, response.getFollowUps());
        assertEquals(followUpCriteria, response.getFollowUpCriteria());
        assertEquals(ancResultList, response.getAncResults());
        commonUtil.close();
    }

    @Test
    void testFetchSyncedDataWhenIsSmartAncIsFalse() {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.IS_SMART_ANC, Boolean.FALSE);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setVillageIds(List.of(TestConstants.STRING_ONE));
        requestDTO.setUserId(TestConstants.ONE);
        requestDTO.setDataRequired(true);
        Map<String, Object> mockRequestData = new HashMap<>();
        mockRequestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        mockRequestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);
        OfflineSyncLog offlineSyncLog = TestDataProvider.getOfflineSyncLog(TestDataProvider.convertNonNullFieldsToHashMap(requestDTO));
        List<HouseholdDTO> householdDTOList = List.of(TestDataProvider.getHouseholdData());
        List<HouseholdMemberDTO> memberDTOList = List.of(TestDataProvider.getHouseHoldMember());
        List<PregnancyInfo> pregnancyInfoList = List.of(TestDataProvider.getPregnancyInfo());
        List<FollowUpDTO> followUpDTOList = List.of(TestDataProvider.getFollowUp());
        FollowUpCriteria followUpCriteria = TestDataProvider.getFollowUpCriteria();
        List<AncResultDTO> ancResultList = List.of(TestDataProvider.getAncResultDTO());
        commonUtil = mockStatic(CommonUtil.class);

        //when
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(objectMapper.convertValue(requestDTO, Map.class)).thenReturn(mockRequestData);
        when(offlineSyncLogRepository.save(any())).thenReturn(offlineSyncLog);
        when(spiceServiceApiInterface.getHouseholdList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(householdDTOList);
        when(spiceServiceApiInterface.getHouseholdMemberList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(memberDTOList);
        when(spiceServiceApiInterface.getPregnancyInfoByVillages(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(pregnancyInfoList);
        when(spiceServiceApiInterface.getFollowUpList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(followUpDTOList);
        when(spiceServiceApiInterface.getFollowUpCriteria(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(followUpCriteria);
        when(cqlApiInterface.getAncResultByVillages(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO)).thenReturn(ancResultList);

        //then
        OfflineSyncResponseDTO response = offlineSyncService.fetchSyncedData(requestDTO);
        assertNotNull(response);
        assertEquals(householdDTOList, response.getHouseholds());
        assertEquals(memberDTOList, response.getMembers());
        assertEquals(pregnancyInfoList, response.getPregnancyInfos());
        assertNull(response.getAncResults());
        commonUtil.close();
    }

    @Test
    void testFetchSyncedDataWithException() {
        //given
        ReflectionTestUtils.setField(offlineSyncService, TestConstants.IS_SMART_ANC, Boolean.TRUE);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setVillageIds(List.of(TestConstants.STRING_ONE));
        requestDTO.setUserId(TestConstants.ONE);
        Map<String, Object> mockRequestData = new HashMap<>();
        mockRequestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        mockRequestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);

        //when
        commonUtil = mockStatic(CommonUtil.class);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(offlineSyncLogRepository.save(any())).thenReturn(new OfflineSyncLog());
        when(objectMapper.convertValue(requestDTO, Map.class)).thenReturn(mockRequestData);
        when(spiceServiceApiInterface.getHouseholdList(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO))
                .thenThrow(new RuntimeException());

        //then
        ServicesException exception = assertThrows(
                ServicesException.class,
                () -> offlineSyncService.fetchSyncedData(requestDTO)
        );
        assertEquals(1006, exception.getCode());
        commonUtil.close();
    }


    @Test
    void uploadSignatures() throws Exception {
        //given
        List<MultipartFile> signatureFiles = List.of(TestDataProvider.getMockedSignatureFile());
        ProvenanceDTO provenance = new ProvenanceDTO();
        provenance.setUserId(TestConstants.USER_ID);
        provenance.setSpiceUserId(TestConstants.ONE);
        commonUtil = mockStatic(CommonUtil.class);

        //when
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(helperService.uploadConsentFormFileToS3(any())).thenReturn(CompletableFuture.completedFuture(TestConstants.URL));

        //then
        offlineSyncService.uploadSignatures(signatureFiles, provenance);
        verify(spiceServiceApiInterface, atLeast(signatureFiles.size())).updateSignature(any(), any(), any());
        commonUtil.close();
    }

    @Test
    void testFetchSyncedDataWhenAppTypeIsNonCommunity() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setAppType(Constants.NON_COMMUNITY);
        requestDTO.setVillageIds(List.of(TestConstants.STRING_ONE));
        requestDTO.setPatientId(TestConstants.PATIENT_ID);
        requestDTO.setAppVersionName(TestConstants.APP_VERSION);
        requestDTO.setDeviceId(TestConstants.DEVICE_ID);
        requestDTO.setDataRequired(true);
        requestDTO.setLastSyncTime(new Date());
        requestDTO.setCurrentSyncTime(new Date());
        requestDTO.setDataRequired(true);
        Map<String, Object> requestData = new HashMap<>();
        requestData.put(Constants.SYNC_MODE, TestConstants.NAME);
        requestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        requestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);

        OfflineSyncLog offlineSyncLog = TestDataProvider.getOfflineSyncLog(TestDataProvider.convertNonNullFieldsToHashMap(requestDTO));
        List<FollowUpDTO> screeningFollowUps = List.of(TestDataProvider.getScreeningFollowUp());
        List<FollowUpDTO> assessmentFollowUps = List.of(TestDataProvider.getAssessmentFollowUp());
        List<FollowUpDTO> lostToFollowUps = List.of(TestDataProvider.getLostToFollowUp());
        List<FollowUpDTO> medicalReviewFollowUps = List.of(TestDataProvider.getMedicalReviewFollowUp());
        FollowUpCriteria followUpCriteria = TestDataProvider.getFollowUpCriteria();
        commonUtil = mockStatic(CommonUtil.class);

        //when
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(offlineSyncLogRepository.save(any())).thenReturn(offlineSyncLog);
        when(objectMapper.convertValue(any(RequestDTO.class), eq(Map.class))).thenReturn(requestData);
        when(spiceServiceApiInterface.getScreeningFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(screeningFollowUps);
        when(spiceServiceApiInterface.getAssessmentFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(assessmentFollowUps);
        when(spiceServiceApiInterface.getLostToFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(lostToFollowUps);
        when(spiceServiceApiInterface.getMedicalReviewFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(medicalReviewFollowUps);
        when(spiceServiceApiInterface.getFollowUpCriteria(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO))
                .thenReturn(followUpCriteria);

        //then
        OfflineSyncResponseDTO response = offlineSyncService.fetchSyncedData(requestDTO);

        assertNotNull(response);
        assertNotNull(response.getFollowUps());
        assertTrue(response.getFollowUps().containsAll(screeningFollowUps));
        assertTrue(response.getFollowUps().containsAll(assessmentFollowUps));
        assertTrue(response.getFollowUps().containsAll(lostToFollowUps));
        assertTrue(response.getFollowUps().containsAll(medicalReviewFollowUps));
        assertEquals(followUpCriteria, response.getFollowUpCriteria());

        commonUtil.close();
    }

    @Test
    void fetchSyncedDataThrowException() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setAppType(Constants.NON_COMMUNITY);
        requestDTO.setVillageIds(List.of(TestConstants.STRING_ONE));
        requestDTO.setCurrentSyncTime(new Date());
        requestDTO.setLastSyncTime(new Date());
        Map<String, Object> requestData = new HashMap<>();
        requestData.put(Constants.SYNC_MODE, TestConstants.NAME);
        requestData.put(Constants.APP_VERSION_NAME, TestConstants.APP_VERSION);
        requestData.put(Constants.DEVICE_ID, TestConstants.DEVICE_ID);

        PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
        patientRequestDTO.setAppType(Constants.NON_COMMUNITY);
        patientRequestDTO.setVillageIds(List.of(1L));
        List<FollowUpDTO> assessmentFollowUps = List.of(TestDataProvider.getAssessmentFollowUp());
        List<FollowUpDTO> lostToFollowUps = List.of(TestDataProvider.getLostToFollowUp());
        List<FollowUpDTO> medicalReviewFollowUps = List.of(TestDataProvider.getMedicalReviewFollowUp());
        FollowUpCriteria followUpCriteria = TestDataProvider.getFollowUpCriteria();
        commonUtil = mockStatic(CommonUtil.class);

        OfflineSyncLog offlineSyncLog = new OfflineSyncLog();

        //when
        when(offlineSyncLogRepository.save(any())).thenReturn(offlineSyncLog);
        when(objectMapper.convertValue(any(RequestDTO.class), eq(Map.class))).thenReturn(requestData);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(spiceServiceApiInterface.getScreeningFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(null);
        when(spiceServiceApiInterface.getAssessmentFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(assessmentFollowUps);
        when(spiceServiceApiInterface.getLostToFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(lostToFollowUps);
        when(spiceServiceApiInterface.getMedicalReviewFollowUps(eq(TestConstants.TOKEN), eq(TestConstants.CLIENT), any(PatientRequestDTO.class)))
                .thenReturn(medicalReviewFollowUps);
        when(spiceServiceApiInterface.getFollowUpCriteria(TestConstants.TOKEN, TestConstants.CLIENT, requestDTO))
                .thenReturn(followUpCriteria);

        //then
        ServicesException thrownException = assertThrows(ServicesException.class,
                () -> offlineSyncService.fetchSyncedData(requestDTO));

        assertEquals(1006, thrownException.getCode());

        commonUtil.close();
    }


    @Test
    void uploadSignaturesThrowException() {
        // given
        List<MultipartFile> signatureFiles = List.of(TestDataProvider.getMockedSignatureFile());
        ProvenanceDTO provenance = new ProvenanceDTO();
        provenance.setUserId(TestConstants.USER_ID);
        provenance.setSpiceUserId(TestConstants.ONE);
        commonUtil = mockStatic(CommonUtil.class);

        //when
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT);
        when(helperService.uploadConsentFormFileToS3(any())).thenReturn(CompletableFuture.failedFuture(new RuntimeException()));

        //then
        assertThrows(ServicesException.class, () ->
                offlineSyncService.uploadSignatures(signatureFiles, provenance)
        );
        commonUtil.close();
    }

}