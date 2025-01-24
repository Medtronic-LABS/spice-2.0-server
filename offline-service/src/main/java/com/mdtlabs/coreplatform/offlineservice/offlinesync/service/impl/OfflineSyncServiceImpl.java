package com.mdtlabs.coreplatform.offlineservice.offlinesync.service.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.offlineservice.common.dto.PatientDetailsDTO;
import feign.FeignException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.commonservice.apiinterface.AuthServiceApiInterface;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.offlineservice.apiinterface.CqlApiInterface;
import com.mdtlabs.coreplatform.offlineservice.apiinterface.SpiceServiceApiInterface;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AncResultDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AssessmentDTO;
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
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.OfflineSyncService;
import com.mdtlabs.coreplatform.offlineservice.producer.ProducerService;

@Service
public class OfflineSyncServiceImpl implements OfflineSyncService {

    ProducerService producerService;

    ObjectMapper objectMapper;

    HelperService helperService;

    OfflineSyncRepository offlineSyncRepository;

    OfflineSyncLogRepository offlineSyncLogRepository;

    private SpiceServiceApiInterface spiceServiceApiInterface;

    private CqlApiInterface cqlApiInterface;

    private AuthServiceApiInterface authServiceApiInterface;

    private RedisTemplate<String, String> redisTemplate;

    @Value("${cloud.aws.sqs.url.requestQueue}")
    private String requestQueueURL;

    @Value("${cloud.aws.sqs.url.responseQueue}")
    private String responseQueueURL;

    @Value("${app.jobUserPassword}")
    private String encryptedPassword;

    @Value("${app.jobUserName}")
    private String username;

    @Value("${app.smart-anc}")
    private Boolean isSmartAnc;

    public OfflineSyncServiceImpl(ProducerService producerService, ObjectMapper objectMapper, HelperService helperService, OfflineSyncRepository offlineSyncRepository,
                                  OfflineSyncLogRepository offlineSyncLogRepository, SpiceServiceApiInterface spiceServiceApiInterface, CqlApiInterface cqlApiInterface,
                                  AuthServiceApiInterface authServiceApiInterface, RedisTemplate<String, String> redisTemplate) {
        this.producerService = producerService;
        this.objectMapper = objectMapper;
        this.helperService = helperService;
        this.offlineSyncRepository = offlineSyncRepository;
        this.offlineSyncLogRepository = offlineSyncLogRepository;
        this.spiceServiceApiInterface = spiceServiceApiInterface;
        this.cqlApiInterface = cqlApiInterface;
        this.authServiceApiInterface = authServiceApiInterface;
        this.redisTemplate = redisTemplate;
    }

    /**
     * {@inheritDoc}
     */
    public void createOfflineSync(Map<String, Object> request) {
        OfflineSyncLog offlineSyncLog = logOfflineSync(request, Constants.CREATE_METHOD);
        try {
            if (Objects.isNull(request) || Objects.isNull(request.get(Constants.REQUEST_ID))) {
                throw new DataNotAcceptableException(1003);
            }
            Logger.logInfo("Constructing message queue");
            List<OfflineSync> offlineSyncs = constructOfflineSync(request);
            if (!offlineSyncs.isEmpty()) {
                processMessageEntries(constructMessageEntries(offlineSyncs));
            }
            Map<String, Object> responseMap = new LinkedHashMap<>();
            responseMap.put(Constants.MESSAGE, Constants.OFFLINE_CREATE_MESSAGE);
            offlineSyncLog.setStatus(Constants.SUCCESS);
            offlineSyncLog.setResponseData(responseMap);
        } catch (Exception e) {
            offlineSyncLog.setStatus(Constants.FAILED);
            offlineSyncLog.setErrorMessage(e.getMessage());
            Logger.logError(ErrorConstants.EXECUTION_EXCEPTION_MESSAGE + e);
            throw new ServicesException(1006, e.getMessage());
        } finally {
            offlineSyncLogRepository.save(offlineSyncLog);
        }
    }

    /**
     * Construct request and save offline sync request.
     *
     * @param request - Offline sync DTO
     * @return - List of Offline Sync
     */
    private List<OfflineSync> constructOfflineSync(Map<String, Object> request) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        String requestId = (String) request.get(Constants.REQUEST_ID);
        String appVersion = (String) request.get(Constants.APP_VERSION_NAME);
        String deviceId = (String) request.get(Constants.DEVICE_ID);
        if (Constants.NON_COMMUNITY.equals(request.get(Constants.APP_TYPE))) {
            List<Map<String, Object>> followUps = (List<Map<String, Object>>) request.get(Constants.FOLLOW_UPS);
            if (Objects.nonNull(followUps)) {
                offlineSyncs.addAll(constructOfflineFollowupMapData(followUps, requestId, appVersion, deviceId));
            }
        } else {
            List<Map<String, Object>> households = (List<Map<String, Object>>) request.get(Constants.HOUSEHOLDS);
            List<Map<String, Object>> householdMembers = (List<Map<String, Object>>) request.get(Constants.HOUSEHOLD_MEMBERS);
            List<Map<String, Object>> assessments = (List<Map<String, Object>>) request.get(Constants.ASSESSMENTS);
            List<Map<String, Object>> followUps = (List<Map<String, Object>>) request.get(Constants.FOLLOW_UPS);
            List<Map<String, Object>> householdMemberLinks = (List<Map<String, Object>>) request.get(Constants.HOUSEHOLD_MEMBER_LINKS);
            if (Objects.nonNull(households)) {
                offlineSyncs.addAll(constructOfflineHouseholdData(households, requestId, appVersion, deviceId));
            }
            if (Objects.nonNull(householdMembers)) {
                offlineSyncs.addAll(constructOfflineHouseholdMemberMapData(householdMembers, requestId, appVersion, deviceId));
            }
            if (Objects.nonNull(assessments) || Objects.nonNull(followUps)) {
                offlineSyncs.addAll(constructOfflineMemberAssessmentFollowupMapData(Objects.requireNonNullElse(assessments, Collections.emptyList()),
                        Objects.requireNonNullElse(followUps, Collections.emptyList()), requestId, appVersion, deviceId));
            }
            if (Objects.nonNull(householdMemberLinks)) {
                offlineSyncs.addAll(constructOfflineHouseholdMemberLinksData(householdMemberLinks, requestId, appVersion, deviceId));
            }
        }
        return offlineSyncRepository.saveAll(offlineSyncs);
    }

    private List<OfflineSync> constructOfflineFollowupMapData(List<Map<String, Object>> followUps, String requestId,
                                                              String appVersion, String deviceId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        followUps.forEach(followUp -> {
            Instant modifiedDateInstant = Instant.parse((String) ((Map<String, Object>) followUp.get(Constants.PROVENANCE)).get(Constants.MODIFIED_DATE));
            followUp.put(Constants.APP_TYPE, Constants.NON_COMMUNITY);
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.FOLLOW_UPS);
            offlineSync.setRequestData(followUp);
            offlineSync.setRequestTime(Date.from(modifiedDateInstant));
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct household and household member request.
     *
     * @param households - List of Household DTO
     * @param requestId  - Request ID
     * @param appVersion - App version
     * @param deviceId   - Device Id
     * @return - offline Sync
     */
    private List<OfflineSync> constructOfflineHouseholdData(List<Map<String, Object>> households,
                                                            String requestId, String appVersion, String deviceId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        households.forEach(household -> {
            Instant modifiedDateInstant = Instant.parse((String) ((Map<String, Object>) household.get(Constants.PROVENANCE)).get(Constants.MODIFIED_DATE));
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.HOUSEHOLD);
            offlineSync.setRequestData(household);
            offlineSync.setRequestTime(Date.from(modifiedDateInstant));
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setReferenceId((String) household.get(Constants.REFERENCE_ID));
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct household member map data.
     *
     * @param householdMembers - List of Household member DTO
     * @param requestId        - Request ID
     * @param appVersion       - App version
     * @param deviceId         - Device Id
     * @return - offline Sync
     */
    private List<OfflineSync> constructOfflineHouseholdMemberMapData(List<Map<String, Object>> householdMembers,
                                                                     String requestId, String appVersion, String deviceId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        Map<String, Map<String, Object>> householdMemberMap = new HashMap<>();

        householdMembers.forEach(householdMember -> {
            String householdId = (String) householdMember.get(Constants.HOUSEHOLD_ID);
            if (Objects.isNull(householdMemberMap.get(householdId))) {
                householdMemberMap.put(householdId, Map.of(Constants.HOUSEHOLD_MEMBERS, new ArrayList<>()));
            }
            ((List<Object>) householdMemberMap.get(householdId).get(Constants.HOUSEHOLD_MEMBERS)).add(householdMember);
        });

        householdMemberMap.forEach((key, value) -> {
            Map<String, Object> householdMemberMapDTO = new HashMap<>();
            householdMemberMapDTO.put(Constants.HOUSEHOLD_ID, key);
            householdMemberMapDTO.put(Constants.HOUSEHOLD_MEMBERS, value.get(Constants.HOUSEHOLD_MEMBERS));
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.HOUSEHOLD_MEMBER_MAP);
            offlineSync.setRequestData(householdMemberMapDTO);
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct offline member assessment followup map data.
     *
     * @param assessments - List of AssessmentDTO
     * @param followUps   - List of FollowUpDTO
     * @param requestId   - Request ID
     * @param appVersion  - App version
     * @param deviceId    - Device Id
     * @return - offline Sync
     */
    private List<OfflineSync> constructOfflineMemberAssessmentFollowupMapData(List<Map<String, Object>> assessments, List<Map<String, Object>> followUps,
                                                                              String requestId, String appVersion, String deviceId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        assessments.sort(Comparator.comparingLong(assessment -> (assessment.get(Constants.UPDATED_AT) instanceof Long longValue) ?
                longValue : Long.parseLong(String.valueOf(assessment.get(Constants.UPDATED_AT)))));
        followUps.sort(Comparator.comparingLong(followUp -> (followUp.get(Constants.UPDATED_AT) instanceof Long longValue) ?
                longValue : Long.parseLong(String.valueOf(followUp.get(Constants.UPDATED_AT)))));
        Map<String, Map<String, Object>> memberAssessmentFollowUpMap = new HashMap<>();

        assessments.forEach(assessment -> {
            String memberId = (String) ((Map<String, Object>) assessment.get(Constants.ENCOUNTER)).get(Constants.MEMBER_ID);
            if (Objects.isNull(memberAssessmentFollowUpMap.get(memberId))) {
                memberAssessmentFollowUpMap.put(memberId, Map.of(Constants.ASSESSMENTS, new ArrayList<>(), Constants.FOLLOW_UPS, new ArrayList<>()));
            }
            ((List<Object>) memberAssessmentFollowUpMap.get(memberId).get(Constants.ASSESSMENTS)).add(assessment);
        });

        followUps.forEach(followUp -> {
            String memberId = (String) followUp.get(Constants.MEMBER_ID);
            if (Objects.isNull(memberAssessmentFollowUpMap.get(memberId))) {
                memberAssessmentFollowUpMap.put(memberId, Map.of(Constants.ASSESSMENTS, new ArrayList<>(), Constants.FOLLOW_UPS, new ArrayList<>()));
            }
            ((List<Object>) memberAssessmentFollowUpMap.get(memberId).get(Constants.FOLLOW_UPS)).add(followUp);
        });

        memberAssessmentFollowUpMap.forEach((key, value) -> {
            Map<String, Object> memberAssessmentFollowupDTO = new HashMap<>();
            memberAssessmentFollowupDTO.put(Constants.HOUSEHOLD_MEMBER_ID, key);
            memberAssessmentFollowupDTO.put(Constants.ASSESSMENTS, value.get(Constants.ASSESSMENTS));
            memberAssessmentFollowupDTO.put(Constants.FOLLOW_UPS, value.get(Constants.FOLLOW_UPS));
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.MEMBER_ASSESSMENT_FOLLOW_UP_MAP);
            offlineSync.setRequestData(memberAssessmentFollowupDTO);
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct householdMemberLinks request.
     *
     * @param householdMemberLinks - List of Household DTO
     * @param requestId            - Request ID
     * @param appVersion           - App version
     * @param deviceId             - Device Id
     * @return - offline Sync
     */
    private List<OfflineSync> constructOfflineHouseholdMemberLinksData(List<Map<String, Object>> householdMemberLinks,
                                                                       String requestId, String appVersion, String deviceId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        householdMemberLinks.forEach(householdMemberLink -> {
            Instant modifiedDateInstant = Instant.parse((String) ((Map<String, Object>) householdMemberLink.get(Constants.PROVENANCE)).get(Constants.MODIFIED_DATE));
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.HOUSEHOLD_MEMBER_LINK);
            offlineSync.setRequestData(householdMemberLink);
            offlineSync.setRequestTime(Date.from(modifiedDateInstant));
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct member data.
     *
     * @param householdMembers - List of Household members
     * @param requestId        - Request ID
     * @param appVersion       - App version
     * @param deviceId         - Device Id
     * @param userId           - User ID
     * @return - offline Sync
     */
    private List<OfflineSync> constructMemberData(List<HouseholdMemberDTO> householdMembers, String requestId,
                                                  String appVersion, String deviceId, Long userId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        householdMembers.forEach(householdMember -> {
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.HOUSEHOLD_MEMBER);
            offlineSync.setRequestData(objectMapper.convertValue(householdMember, new TypeReference<>() {
            }));
            offlineSync.setRequestTime(householdMember.getProvenance().getModifiedDate());
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setReferenceId(householdMember.getReferenceId());
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSync.setCreatedBy(userId);
            offlineSync.setUpdatedBy(userId);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct assessment data.
     *
     * @param assessments - List of AssessmentDTO
     * @param requestId   - Request ID
     * @param appVersion  - App version
     * @param deviceId    - Device Id
     * @param userId      - User ID
     * @return - offline Sync
     */
    private List<OfflineSync> constructAssessmentData(List<AssessmentDTO> assessments, String requestId,
                                                      String appVersion, String deviceId, Long userId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        assessments.forEach(assessment -> {
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.ASSESSMENT);
            offlineSync.setRequestData(objectMapper.convertValue(assessment, new TypeReference<>() {
            }));
            offlineSync.setRequestTime(assessment.getEncounter().getProvenance().getModifiedDate());
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSync.setCreatedBy(userId);
            offlineSync.setUpdatedBy(userId);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct followup data.
     *
     * @param followUps  - List of FollowUpDTO
     * @param requestId  - Request ID
     * @param appVersion - App version
     * @param deviceId   - Device Id
     * @param userId     - User ID
     * @return - offline Sync
     */
    private List<OfflineSync> constructFollowUpData(List<FollowUpDTO> followUps, String requestId,
                                                    String appVersion, String deviceId, Long userId) {
        List<OfflineSync> offlineSyncs = new ArrayList<>();
        followUps.forEach(followUp -> {
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setType(Constants.FOLLOW_UP);
            offlineSync.setRequestData(objectMapper.convertValue(followUp, new TypeReference<>() {
            }));
            offlineSync.setRequestTime(followUp.getProvenance().getModifiedDate());
            offlineSync.setRequestId(requestId);
            offlineSync.setAppVersion(appVersion);
            offlineSync.setDeviceId(deviceId);
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSync.setCreatedBy(userId);
            offlineSync.setUpdatedBy(userId);
            offlineSyncs.add(offlineSync);
        });
        return offlineSyncs;
    }

    /**
     * Construct message entries.
     *
     * @param offlineSyncs - List of offline sync
     * @return - List of message entries
     */
    private List<Message<String>> constructMessageEntries(List<OfflineSync> offlineSyncs) {
        List<Message<String>> messageEntries = new ArrayList<>();
        String groupId = Instant.now().toString();
        offlineSyncs.forEach(offlineSync -> {
            try {
                String messageBody = objectMapper.writeValueAsString(offlineSync.getRequestData());
                Map<String, Object> headerParams = new HashMap<>();
                headerParams.put(Constants.FIELD_TRACE_ID, offlineSync.getId());
                headerParams.put(Constants.FIELD_MESSAGE_GROUP_ID, groupId);
                MessageHeaders messageHeaders = new MessageHeaders(headerParams);
                Message<String> message = MessageBuilder
                        .withPayload(messageBody)
                        .copyHeaders(messageHeaders)
                        .build();
                messageEntries.add(message);
            } catch (JsonProcessingException e) {
                Logger.logError(ErrorConstants.INVALID_OFFLINE_SYNC_MESSAGE + e);
                throw new ServicesException(1004);
            }
        });
        return messageEntries;
    }

    /**
     * Process message entries to push the data to queue.
     *
     * @param messageEntries - List of message entries
     */
    private void processMessageEntries(List<Message<String>> messageEntries) {
        if (Constants.TEN < messageEntries.size()) {
            processBatchRequest(messageEntries);
        } else {
            Logger.logInfo("Sending Bulk message");
            producerService.sendBulkMessage(messageEntries, requestQueueURL);
        }
    }

    /**
     * Process batch request for push the data to queue.
     *
     * @param messageEntries - List of message entries
     */
    private void processBatchRequest(List<Message<String>> messageEntries) {
        int batchSize = Constants.TEN;
        for (int i = Constants.ZERO; i < messageEntries.size(); i += batchSize) {
            int end = Math.min(i + batchSize, messageEntries.size());
            producerService.sendBulkMessage(messageEntries.subList(i, end), requestQueueURL);
        }
    }

    /**
     * Construct and send the message to retry queue.
     *
     * @param data          - Content of the message
     * @param headers       - Queue headers
     */
    private void constructAndSendRetryQueue(String data, MessageHeaders headers) {
        Logger.logInfo("Push message to retry queue");
        Map<String, Object> messageAttributes = new HashMap<>();
        messageAttributes.put(Constants.FIELD_TRACE_ID, String.valueOf(headers.get(Constants.FIELD_TRACE_ID)));
        producerService.sendMessage(data, Instant.now().toString(),
                messageAttributes, requestQueueURL
        );
    }

    /**
     * Get Access token from Spice
     *
     * @return - Bearer Token
     */
    public String getAccessToken() {
        String bearerToken;
        String authToken = redisTemplate.opsForValue().get(username);
        if (null != authToken) {
            bearerToken = authToken;
        } else {
            List<String> authorizationHeaders = getJobUserHeaders(username, encryptedPassword);
            bearerToken = authorizationHeaders.getFirst();
            redisTemplate.opsForValue().set(username, bearerToken);
            redisTemplate.expire(username,
                    com.mdtlabs.coreplatform.commonservice.common.Constants.AUTH_TOKEN_EXPIRY_MINUTES - Constants.TEN, TimeUnit.MINUTES);
        }
        return bearerToken;
    }

    /**
     * {@inheritDoc}
     */
    public void processRequestQueue(String data, MessageHeaders headers) {
        Long traceId = Long.valueOf(headers.get(Constants.FIELD_TRACE_ID).toString());
        OfflineSync offlineSync = offlineSyncRepository.findByIdAndIsDeletedFalse(traceId);
        Logger.logInfo(StringUtil.concatString("Processing request queue - ", String.valueOf(traceId),
                " (", String.valueOf(offlineSync.getRetryAttempts()), ")"));
        try {
            processRequestData(data, offlineSync);
        } catch (Exception e) {
            String errorMessage = e instanceof FeignException.FeignClientException feignException ?
                    feignException.contentUTF8() : e.getMessage();
            Logger.logError("Error while processing request queue", errorMessage);
            offlineSync.setErrorMessage(errorMessage);
            if (Constants.THREE <= offlineSync.getRetryAttempts()) {
                offlineSync.setStatus(Constants.FAILED);
                offlineSyncRepository.save(offlineSync);
            } else {
                int retryAttempts = offlineSync.getRetryAttempts() + Constants.ONE;
                offlineSync.setRetryAttempts(retryAttempts);
                offlineSyncRepository.save(offlineSync);
                constructAndSendRetryQueue(data, headers);
            }
        }
    }

    /**
     * Constructing session api call from feignClient.
     *
     * @param username - Name of the user
     * @param password - password of the user
     * @return - List of headers from header-Authorization
     */
    public List<String> getJobUserHeaders(String username, String password) {
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add(com.mdtlabs.coreplatform.commonservice.common.Constants.USERNAME, username);
        formData.add(com.mdtlabs.coreplatform.commonservice.common.Constants.PASSWORD, password);
        return authServiceApiInterface.login(formData, Constants.CLIENT)
                .getHeaders()
                .get(HttpHeaders.AUTHORIZATION);
    }

    /**
     * Constructing householdData and householdMembers
     *
     * @param data        - Data for Household and for its members
     * @param offlineSync - Offline sync
     */
    private void processRequestData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        switch (offlineSync.getType()) {
            case Constants.HOUSEHOLD:
                processHouseholdData(data, offlineSync);
                break;
            case Constants.HOUSEHOLD_MEMBER_MAP:
                processHouseholdMemberData(data, offlineSync);
                break;
            case Constants.MEMBER_ASSESSMENT_FOLLOW_UP_MAP:
                processMemberAssessmentFollowUpData(data, offlineSync);
                break;
            case Constants.HOUSEHOLD_MEMBER_LINK:
                processHouseholdMemberLink(data, offlineSync);
                break;
            case Constants.FOLLOW_UPS:
                processFollowUpData(data, offlineSync);
                break;
            default:
                break;
        }
        offlineSyncRepository.save(offlineSync);
    }

    /**
     * Process household and household member data.
     *
     * @param data        - household and household member data
     * @param offlineSync - offline sync
     */
    private void processHouseholdData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo("Push household to fhir server");
        String bearerToken = getAccessToken();
        HouseholdDTO household = objectMapper.readValue(data, HouseholdDTO.class);
        HouseholdDTO householdResponse = Objects.isNull(household.getId()) ?
                spiceServiceApiInterface.createHouseHold(bearerToken, Constants.CLIENT, household).getBody() :
                spiceServiceApiInterface.updateHouseHold(bearerToken, Constants.CLIENT, household).getBody();
        offlineSync.setFhirId(householdResponse.getId());
        offlineSync.setStatus(Constants.SUCCESS);
        if (Objects.nonNull(household.getHouseholdMembers())) {
            updateHouseholdMemberData(household.getHouseholdMembers(), householdResponse, offlineSync);
            updateMemberLink(householdResponse.getHouseholdMembers(), bearerToken);
        }
        household.setHouseholdMembers(null);
        offlineSync.setRequestData(objectMapper.convertValue(household, new TypeReference<>() {
        }));
    }

    /**
     * update HouseholdMemberLink data.
     *
     * @param householdMembersList - household member data
     * @param bearerToken          - token
     */
    private void updateMemberLink(List<HouseholdMemberDTO> householdMembersList, String bearerToken) {
        List<HouseholdMemberDTO> householdMembers = householdMembersList.stream().filter(householdMember -> Boolean.TRUE.equals(householdMember.getAssignHousehold())).toList();
        if (!householdMembers.isEmpty()) {
            spiceServiceApiInterface.updateMemberLink(bearerToken, Constants.CLIENT, householdMembers);
        }
    }

    /**
     * Process HouseholdMemberLink data.
     *
     * @param data        - household and household member data
     * @param offlineSync - offline sync
     */
    private void processHouseholdMemberLink(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo("Push HouseholdMemberLink Notification");
        String bearerToken = getAccessToken();
        HouseholdMemberLinkDTO householdMemberLinks = objectMapper.readValue(data, HouseholdMemberLinkDTO.class);
        spiceServiceApiInterface.updateHouseholdMemberLink(bearerToken, Constants.CLIENT, householdMemberLinks);
        offlineSync.setStatus(Constants.SUCCESS);
    }

    /**
     * Update household member data from household response.
     *
     * @param householdMembers     - List of HouseholdMemberDTO
     * @param householdResponse    - HouseholdDTO
     * @param householdOfflineSync - Household Offline Sync
     */
    private void updateHouseholdMemberData(List<HouseholdMemberDTO> householdMembers,
                                           HouseholdDTO householdResponse, OfflineSync householdOfflineSync) {
        householdMembers.forEach(householdMember -> {
            OfflineSync offlineSync = new OfflineSync();
            mapHouseholdMemberDataToOfflineSync(householdMember, householdOfflineSync, offlineSync);
            HouseholdMemberDTO householdMemberResponse = householdResponse.getHouseholdMembers().stream()
                    .filter(member -> member.getReferenceId().equals(householdMember.getReferenceId())).toList().get(Constants.ZERO);
            offlineSync.setFhirId(householdMemberResponse.getId());
            offlineSync.setStatus(Constants.SUCCESS);
            Map<String, String> neonateReference = new HashMap<>();
            if (Objects.nonNull(householdMember.getChildren()) && !householdMember.getChildren().isEmpty()) {
                householdMember.getChildren().forEach(child -> {
                    child.setMotherPatientId(householdMemberResponse.getPatientId());
                    child.setHouseholdId(householdMemberResponse.getHouseholdId());
                    child.setPatientId(processChild(child, householdOfflineSync, neonateReference).getPatientId());
                });
            }
            if (Objects.nonNull(householdMember.getAssessments()) && !householdMember.getAssessments().isEmpty()) {
                householdMember.getAssessments().forEach(assessment -> {
                    if (Objects.nonNull(householdMember.getChildren()) && !householdMember.getChildren().isEmpty()
                            && Constants.PNC_MOTHER.equals(assessment.getAssessmentType())
                            && Objects.isNull(assessment.getAssessmentDetails().getPncMother().getNeonatePatientId())) {
                        assessment.getAssessmentDetails().getPncMother().setNeonatePatientId(neonateReference.get(assessment.getAssessmentDetails().getPncMother().getNeonatePatientReferenceId()));
                    }
                    assessment.getEncounter().setHouseholdId(householdOfflineSync.getFhirId());
                    assessment.getEncounter().setPatientId(householdMemberResponse.getPatientId());
                    assessment.getEncounter().setMemberId(offlineSync.getFhirId());
                });
                processMultipleAssessments(householdMember.getAssessments(), offlineSync.getRequestId(),
                        offlineSync.getAppVersion(), offlineSync.getDeviceId(), offlineSync.getCreatedBy());
                householdMember.setAssessments(null);
                offlineSync.setRequestData(objectMapper.convertValue(householdMember, new TypeReference<>() {
                }));
            }
            offlineSyncRepository.save(offlineSync);
        });
    }

    /**
     * <p>
     * Map household member details to offlineSync Database.
     * </p>
     *
     * @param householdMember      - HouseholdMemberDTO
     * @param householdOfflineSync - Household Offline Sync
     * @param offlineSync          - offline sync
     */
    private void mapHouseholdMemberDataToOfflineSync(HouseholdMemberDTO householdMember, OfflineSync householdOfflineSync, OfflineSync offlineSync) {
        offlineSync.setType(Constants.HOUSEHOLD_MEMBER);
        offlineSync.setRequestData(objectMapper.convertValue(householdMember, new TypeReference<>() {
        }));
        offlineSync.setRequestTime(householdMember.getProvenance().getModifiedDate());
        offlineSync.setRequestId(householdOfflineSync.getRequestId());
        offlineSync.setAppVersion(householdOfflineSync.getAppVersion());
        offlineSync.setDeviceId(householdOfflineSync.getDeviceId());
        offlineSync.setReferenceId(householdMember.getReferenceId());
        offlineSync.setCreatedBy(householdOfflineSync.getCreatedBy());
        offlineSync.setUpdatedBy(householdOfflineSync.getCreatedBy());
    }

    /**
     * <p>
     * process the pnc-child details from household member.
     * </p>
     *
     * @param householdMember      - HouseholdMemberDTO
     * @param householdOfflineSync - OfflineSync
     * @return HouseholdMemberDTO
     */
    private HouseholdMemberDTO processChild(HouseholdMemberDTO householdMember, OfflineSync householdOfflineSync, Map<String, String> neonateReference) {
        OfflineSync offlineSync = new OfflineSync();
        mapHouseholdMemberDataToOfflineSync(householdMember, householdOfflineSync, offlineSync);
        HouseholdMemberDTO householdMemberResponse = spiceServiceApiInterface.createHouseHoldMember(getAccessToken(), Constants.CLIENT, householdMember).getBody();
        offlineSync.setFhirId(householdMemberResponse.getId());
        offlineSync.setStatus(Constants.SUCCESS);
        neonateReference.put(householdMember.getReferenceId(), householdMemberResponse.getPatientId());
        if (Objects.nonNull(householdMember.getAssessments()) && !householdMember.getAssessments().isEmpty()) {
            householdMember.getAssessments().forEach(assessment -> {
                assessment.getEncounter().setHouseholdId(householdMemberResponse.getHouseholdId());
                assessment.getEncounter().setPatientId(householdMemberResponse.getPatientId());
                assessment.getEncounter().setMemberId(offlineSync.getFhirId());
            });
            processMultipleAssessments(householdMember.getAssessments(), offlineSync.getRequestId(),
                    offlineSync.getAppVersion(), offlineSync.getDeviceId(), offlineSync.getCreatedBy());
            householdMember.setAssessments(null);
            offlineSync.setRequestData(objectMapper.convertValue(householdMember, new TypeReference<>() {
            }));
        }
        offlineSyncRepository.save(offlineSync);
        return householdMemberResponse;
    }

    /**
     * Process household member data.
     *
     * @param data        - household member data
     * @param offlineSync - offline sync
     */
    private void processHouseholdMemberData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo("Push household member to fhir server");
        HouseholdMemberMapDTO householdMemberMap = objectMapper.readValue(data, HouseholdMemberMapDTO.class);
        if (Objects.nonNull(householdMemberMap.getHouseholdMembers()) && !householdMemberMap.getHouseholdMembers().isEmpty()) {
            processMultipleMembers(householdMemberMap.getHouseholdMembers(), offlineSync.getRequestId(),
                    offlineSync.getAppVersion(), offlineSync.getDeviceId(), offlineSync.getCreatedBy());
        }
        householdMemberMap.setHouseholdMembers(null);
        offlineSync.setRequestData(objectMapper.convertValue(householdMemberMap, new TypeReference<>() {
        }));
        offlineSync.setStatus(Constants.SUCCESS);
        offlineSync.setReferenceId(householdMemberMap.getHouseholdId());
    }

    /**
     * Process member assessment followup data.
     *
     * @param data        - household member data
     * @param offlineSync - offline sync
     */
    private void processMemberAssessmentFollowUpData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo("Push member assessment followup to fhir server");
        MemberAssessmentFollowupMapDTO memberAssessmentFollowup = objectMapper.readValue(data, MemberAssessmentFollowupMapDTO.class);
        if (Objects.nonNull(memberAssessmentFollowup.getFollowUps()) && !memberAssessmentFollowup.getFollowUps().isEmpty()) {
            processMultipleFollowUps(memberAssessmentFollowup.getFollowUps(), offlineSync.getRequestId(),
                    offlineSync.getAppVersion(), offlineSync.getDeviceId(), offlineSync.getCreatedBy());
        }
        if (Objects.nonNull(memberAssessmentFollowup.getAssessments()) && !memberAssessmentFollowup.getAssessments().isEmpty()) {
            processMultipleAssessments(memberAssessmentFollowup.getAssessments(), offlineSync.getRequestId(),
                    offlineSync.getAppVersion(), offlineSync.getDeviceId(), offlineSync.getCreatedBy());
        }
        memberAssessmentFollowup.setFollowUps(null);
        memberAssessmentFollowup.setAssessments(null);
        offlineSync.setRequestData(objectMapper.convertValue(memberAssessmentFollowup, new TypeReference<>() {
        }));
        offlineSync.setStatus(Constants.SUCCESS);
        offlineSync.setReferenceId(memberAssessmentFollowup.getHouseholdMemberId());
    }

    /**
     * Process multiple members.
     *
     * @param householdMembers - List of Household members
     * @param requestId        - Request ID
     * @param appVersion       - App version
     * @param deviceId         - Device Id
     * @param userId           - user ID
     */
    private void processMultipleMembers(List<HouseholdMemberDTO> householdMembers, String requestId,
                                        String appVersion, String deviceId, Long userId) {
        List<OfflineSync> offlineSyncs = offlineSyncRepository.saveAll(constructMemberData(householdMembers, requestId, appVersion, deviceId, userId));
        offlineSyncs.forEach(offlineSync -> {
            try {
                processMemberAssessmentFollowup(objectMapper.writeValueAsString(offlineSync.getRequestData()), Constants.HOUSEHOLD_MEMBER, offlineSync);
                offlineSyncRepository.save(offlineSync);
            } catch (JsonProcessingException e) {
                Logger.logError("Error while processing member request", e.getMessage());
            }
        });
    }

    /**
     * Process multiple assessments.
     *
     * @param assessments - List of AssessmentDTO
     * @param requestId   - Request ID
     * @param appVersion  - App version
     * @param deviceId    - Device Id
     * @param userId      - user ID
     */
    private void processMultipleAssessments(List<AssessmentDTO> assessments, String requestId,
                                            String appVersion, String deviceId, Long userId) {
        assessments.sort(Comparator.comparingLong(AssessmentDTO::getUpdatedAt));
        List<OfflineSync> offlineSyncs = offlineSyncRepository.saveAll(constructAssessmentData(assessments, requestId, appVersion, deviceId, userId));
        offlineSyncs.forEach(offlineSync -> {
            try {
                processMemberAssessmentFollowup(objectMapper.writeValueAsString(offlineSync.getRequestData()), Constants.ASSESSMENT, offlineSync);
                offlineSyncRepository.save(offlineSync);
            } catch (JsonProcessingException e) {
                Logger.logError("Error while processing assessment request", e.getMessage());
            }
        });
    }

    /**
     * Process multiple followups.
     *
     * @param followUps  - List of FollowUpDTO
     * @param requestId  - Request ID
     * @param appVersion - App version
     * @param deviceId   - Device Id
     * @param userId     - user ID
     */
    private void processMultipleFollowUps(List<FollowUpDTO> followUps, String requestId,
                                          String appVersion, String deviceId, Long userId) {
        followUps.sort(Comparator.comparingLong(FollowUpDTO::getUpdatedAt));
        List<OfflineSync> offlineSyncs = offlineSyncRepository.saveAll(constructFollowUpData(followUps, requestId, appVersion, deviceId, userId));
        offlineSyncs.forEach(offlineSync -> {
            try {
                processMemberAssessmentFollowup(objectMapper.writeValueAsString(offlineSync.getRequestData()), Constants.FOLLOW_UP, offlineSync);
                offlineSyncRepository.save(offlineSync);
            } catch (JsonProcessingException e) {
                Logger.logError("Error while processing followup request", e.getMessage());
            }
        });
    }

    /**
     * Process member, assessment, followup.
     *
     * @param data        - household member / assessment / followup data
     * @param type        - type of the data
     * @param offlineSync - offline sync
     */
    private void processMemberAssessmentFollowup(String data, String type, OfflineSync offlineSync) {
        try {
            if (Constants.HOUSEHOLD_MEMBER.equals(type)) {
                processMemberData(data, offlineSync);
            } else if (Constants.FOLLOW_UP.equals(type)) {
                processFollowUpData(data, offlineSync);
            } else if (Constants.ASSESSMENT.equals(type)) {
                processAssessmentData(data, offlineSync);
            }
        } catch (Exception e) {
            String errorMessage = e instanceof FeignException.FeignClientException feignException ?
                    feignException.contentUTF8() : e.getMessage();
            Logger.logError(StringUtil.concatString("Error while processing request ", type), errorMessage);
            offlineSync.setErrorMessage(errorMessage);
            if (Constants.THREE <= offlineSync.getRetryAttempts()) {
                offlineSync.setStatus(Constants.FAILED);
                offlineSyncRepository.save(offlineSync);
            } else {
                int retryAttempts = offlineSync.getRetryAttempts() + Constants.ONE;
                offlineSync.setRetryAttempts(retryAttempts);
                offlineSyncRepository.save(offlineSync);
                processMemberAssessmentFollowup(data, type, offlineSync);
            }
        }
    }

    /**
     * Process member data.
     *
     * @param data        - household member data
     * @param offlineSync - offline sync
     */
    private void processMemberData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo(StringUtil.concatString("Processing Member - ", String.valueOf(offlineSync.getId()),
                " (", String.valueOf(offlineSync.getRetryAttempts()), ")"));
        HouseholdMemberDTO householdMember = objectMapper.readValue(data, HouseholdMemberDTO.class);
        String bearerToken = getAccessToken();
        HouseholdMemberDTO householdMemberResponse = Objects.isNull(householdMember.getId()) ?
                spiceServiceApiInterface.createHouseHoldMember(bearerToken, Constants.CLIENT, householdMember).getBody() :
                spiceServiceApiInterface.updateHouseHoldMember(bearerToken, Constants.CLIENT, householdMember).getBody();
        offlineSync.setFhirId(householdMemberResponse.getId());
        offlineSync.setStatus(Constants.SUCCESS);
        Map<String, String> neonateReference = new HashMap<>();
        if (Objects.nonNull(householdMember.getChildren()) && !householdMember.getChildren().isEmpty()) {
            householdMember.getChildren().forEach(child -> {
                child.setMotherPatientId(householdMemberResponse.getPatientId());
                child.setHouseholdId(householdMemberResponse.getHouseholdId());
                child.setPatientId(processChild(child, offlineSync, neonateReference).getPatientId());
            });
        }
        if (Objects.nonNull(householdMember.getAssessments()) && !householdMember.getAssessments().isEmpty()) {
            householdMember.getAssessments().forEach(assessment -> {
                if (Objects.nonNull(householdMember.getChildren()) && !householdMember.getChildren().isEmpty()
                        && Constants.PNC_MOTHER.equals(assessment.getAssessmentType())
                        && Objects.isNull(assessment.getAssessmentDetails().getPncMother().getNeonatePatientId())) {
                    assessment.getAssessmentDetails().getPncMother().setNeonatePatientId(
                            neonateReference.get(assessment.getAssessmentDetails().getPncMother().getNeonatePatientReferenceId()));
                }
                assessment.getEncounter().setMemberId(offlineSync.getFhirId());
                assessment.getEncounter().setPatientId(householdMemberResponse.getPatientId());
            });
            processMultipleAssessments(householdMember.getAssessments(), offlineSync.getRequestId(),
                    offlineSync.getAppVersion(), offlineSync.getDeviceId(), offlineSync.getCreatedBy());
            householdMember.setAssessments(null);
            offlineSync.setRequestData(objectMapper.convertValue(householdMember, new TypeReference<>() {
            }));
        }
        updateMemberLink(List.of(householdMemberResponse), bearerToken);
    }

    /**
     * Process assessment data.
     *
     * @param data        - assessment data
     * @param offlineSync - offline sync
     */
    private void processAssessmentData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo(StringUtil.concatString("Processing Assessment - ", String.valueOf(offlineSync.getId()),
                " (", String.valueOf(offlineSync.getRetryAttempts()), ")"));
        AssessmentDTO assessment = objectMapper.readValue(data, AssessmentDTO.class);
        String bearerToken = getAccessToken();
        AssessmentDTO assessmentResponse = spiceServiceApiInterface.createAssessment(bearerToken, Constants.CLIENT, assessment).getBody();
        offlineSync.setFhirId(assessmentResponse.getId());
        offlineSync.setStatus(Constants.SUCCESS);
        offlineSync.setReferenceId(assessment.getReferenceId());
    }

    /**
     * Process followup data.
     *
     * @param data        - followup data
     * @param offlineSync - offline sync
     */
    private void processFollowUpData(String data, OfflineSync offlineSync) throws JsonProcessingException {
        Logger.logInfo(StringUtil.concatString("Processing followup - ", String.valueOf(offlineSync.getId()),
                " (", String.valueOf(offlineSync.getRetryAttempts()), ")"));
        FollowUpDTO followUp = objectMapper.readValue(data, FollowUpDTO.class);
        String bearerToken = getAccessToken();
        FollowUpDTO followUpResponse = Objects.isNull(followUp.getId()) ?
                spiceServiceApiInterface.createFollowUp(bearerToken, Constants.CLIENT, followUp) :
                spiceServiceApiInterface.updateFollowUp(bearerToken, Constants.CLIENT, followUp);
        offlineSync.setStatus(Constants.SUCCESS);
        offlineSync.setReferenceId(String.valueOf(followUpResponse.getId()));
    }

    /**
     * {@inheritDoc}
     */
    public void generateResponse(RequestDTO request) {
        try {
            if (Objects.isNull(request.getRequestId())) {
                throw new DataNotAcceptableException(1003);
            }
            Logger.logInfo("Push message to response queue");
            OfflineSync offlineSync = new OfflineSync();
            offlineSync.setRequestData(objectMapper.convertValue(request, new TypeReference<>() {
            }));
            offlineSync.setRequestId(request.getRequestId());
            offlineSync.setStatus(Constants.IN_PROGRESS);
            offlineSync = offlineSyncRepository.save(offlineSync);
            String messageBody = objectMapper.writeValueAsString(request);
            Map<String, Object> messageAttributes = new HashMap<>();
            messageAttributes.put(Constants.FIELD_TRACE_ID, offlineSync.getId());
            producerService.sendMessage(messageBody, Instant.now().toString(),
                    messageAttributes, responseQueueURL);
        } catch (JsonProcessingException e) {
            Logger.logInfo(e.getMessage());
            throw new ServicesException(1004);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void processResponseQueue(String data, MessageHeaders headers) {
        Long traceId = Long.valueOf(headers.get(Constants.FIELD_TRACE_ID).toString());
        OfflineSync offlineSync = offlineSyncRepository.findByIdAndIsDeletedFalse(traceId);
        try {
            SqsResponseDTO sqsResponseDTO = new SqsResponseDTO();
            String jsonData = objectMapper.writeValueAsString(sqsResponseDTO);
            String fileName = StringUtil.concatString(offlineSync.getRequestId(), Constants.FILE_EXTENSION_JSON);
            File jsonFile = new File(fileName);
            try (FileOutputStream fos = new FileOutputStream(jsonFile)) {
                fos.write(jsonData.getBytes());
            }
            helperService.uploadFileToS3(fileName, jsonFile);
            offlineSync.setStatus(Constants.SUCCESS);
        } catch (Exception e) {
            Logger.logInfo(e.getMessage());
            offlineSync.setStatus(Constants.FAILED);
            offlineSync.setErrorMessage(e.getMessage());
        } finally {
            offlineSyncRepository.save(offlineSync);
        }
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<OfflineSyncDTO> getOfflineSyncStatusList(RequestDTO request) throws ServicesException {
        ResponseListDTO<OfflineSyncDTO> response = new ResponseListDTO<>();
        OfflineSyncLog offlineSyncLog = logOfflineSync(request, Constants.STATUS_METHOD);
        try {
            ModelMapper mapper = new ModelMapper();
            List<OfflineSyncDTO> offlineSyncList = offlineSyncRepository.getOfflineSync(request.getRequestId(),
                            request.getUserId(), request.getTypes(), request.getStatuses())
                    .stream().map(offlineSync -> {
                        OfflineSyncDTO offlineSyncDTO = mapper.map(offlineSync, OfflineSyncDTO.class);
                        offlineSyncDTO.setData(request.isDataRequired() ? offlineSync.getRequestData() : null);
                        return offlineSyncDTO;
                    }).toList();
            response.setData(offlineSyncList);
            response.setTotalCount(Long.valueOf(offlineSyncList.size()));
            offlineSyncLog.setStatus(Constants.SUCCESS);
            offlineSyncLog.setResponseData(objectMapper.convertValue(response, Map.class));
        } catch (Exception e) {
            offlineSyncLog.setStatus(Constants.FAILED);
            offlineSyncLog.setErrorMessage(e.getMessage());
            Logger.logError(ErrorConstants.EXECUTION_EXCEPTION_MESSAGE + e);
            throw new ServicesException(1006, e.getMessage());
        } finally {
            offlineSyncLogRepository.save(offlineSyncLog);
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public OfflineSyncResponseDTO fetchSyncedData(RequestDTO request) {
        OfflineSyncResponseDTO offlineSyncResponse = new OfflineSyncResponseDTO();
        OfflineSyncLog offlineSyncLog = logOfflineSync(request, Constants.FETCH_METHOD);
        offlineSyncLog.setLastSyncTime(request.getLastSyncTime());
        String authToken = CommonUtil.getAuthToken();
        String client = CommonUtil.getClient();
        request.setCurrentSyncTime(new Date());
        try {
            if (Constants.NON_COMMUNITY.equalsIgnoreCase(request.getAppType())
                    && Objects.nonNull(request.getVillageIds()) && !request.getVillageIds().isEmpty()) {
                ExecutorService executor = Executors.newFixedThreadPool(Constants.INT_SIX);
                PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
                patientRequestDTO.setAppType(Constants.NON_COMMUNITY);
                patientRequestDTO.setVillageIds(request.getVillageIds().stream().map(Long::parseLong).toList());
                patientRequestDTO.setCurrentSyncTime(request.getCurrentSyncTime());
                patientRequestDTO.setLastSyncTime(request.getLastSyncTime());
                CompletableFuture<List<PatientDetailsDTO>> patients = CompletableFuture.supplyAsync(
                        () -> spiceServiceApiInterface.listPatientDetails(authToken, client, request), executor);
                CompletableFuture<List<FollowUpDTO>> screeningFollowUps = CompletableFuture.supplyAsync(
                        () -> spiceServiceApiInterface.getScreeningFollowUps(authToken, client, patientRequestDTO),
                        executor);
                CompletableFuture<List<FollowUpDTO>> assessmentFollowUps = CompletableFuture.supplyAsync(
                        () -> spiceServiceApiInterface.getAssessmentFollowUps(authToken, client, patientRequestDTO),
                        executor);
                CompletableFuture<List<FollowUpDTO>> lostToFollowUps = CompletableFuture.supplyAsync(
                        () -> spiceServiceApiInterface.getLostToFollowUps(authToken, client, patientRequestDTO),
                        executor);
                CompletableFuture<List<FollowUpDTO>> medicalReviewFollowUps = CompletableFuture.supplyAsync(
                        () -> spiceServiceApiInterface.getMedicalReviewFollowUps(authToken, client, patientRequestDTO),
                        executor);
                CompletableFuture<FollowUpCriteria> followUpCriteria = CompletableFuture.supplyAsync(
                        () -> spiceServiceApiInterface.getFollowUpCriteria(authToken, client, request), executor);
                CompletableFuture<Void> allOf = CompletableFuture.allOf(screeningFollowUps, assessmentFollowUps,
                        lostToFollowUps, medicalReviewFollowUps, followUpCriteria);
                allOf.thenRun(() -> {
                    try {
                        List<FollowUpDTO> followUps = new ArrayList<>();
                        followUps.addAll(screeningFollowUps.get());
                        followUps.addAll(assessmentFollowUps.get());
                        followUps.addAll(lostToFollowUps.get());
                        followUps.addAll(medicalReviewFollowUps.get());
                        offlineSyncResponse.setFollowUps(followUps);
                        offlineSyncResponse.setFollowUpCriteria(followUpCriteria.get());
                        offlineSyncResponse.setPatientDetails(patients.get());
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        throw new ServicesException(1006, e.getMessage());
                    } catch (ExecutionException e) {
                        throw new ServicesException(1006, e.getMessage());
                    }
                }).get();
                executor.shutdown();
            } else {
                ExecutorService executor = Executors.newFixedThreadPool(Constants.SEVEN);
                CompletableFuture<List<HouseholdDTO>> households = CompletableFuture.supplyAsync(() -> spiceServiceApiInterface.getHouseholdList(authToken,
                        client, request), executor);
                CompletableFuture<List<HouseholdMemberDTO>> members = CompletableFuture.supplyAsync(() -> spiceServiceApiInterface.getHouseholdMemberList(authToken,
                        client, request), executor);
                CompletableFuture<List<PregnancyInfo>> pregnancyInfos = CompletableFuture.supplyAsync(() -> spiceServiceApiInterface.getPregnancyInfoByVillages(authToken,
                        client, request), executor);
                CompletableFuture<List<FollowUpDTO>> followUps = CompletableFuture.supplyAsync(() -> spiceServiceApiInterface.getFollowUpList(authToken,
                        client, request), executor);
                CompletableFuture<FollowUpCriteria> followUpCriteria = CompletableFuture.supplyAsync(() -> spiceServiceApiInterface.getFollowUpCriteria(authToken,
                        client, request), executor);
                CompletableFuture<List<HouseholdMemberLinkDTO>> householdMemberLinks = CompletableFuture.supplyAsync(() -> spiceServiceApiInterface.getHouseholdMemberLinkList(authToken,
                        client, request), executor);
                CompletableFuture<List<AncResultDTO>> ancResults = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(isSmartAnc)) {
                    ancResults = CompletableFuture.supplyAsync(() -> cqlApiInterface.getAncResultByVillages(authToken,
                            client, request), executor);
                }
                CompletableFuture<Void> allOf = CompletableFuture.allOf(households, members, pregnancyInfos, followUps, householdMemberLinks, followUpCriteria, ancResults);
                CompletableFuture<List<AncResultDTO>> finalAncResults = ancResults;
                allOf.thenRun(() -> {
                    try {
                        offlineSyncResponse.setHouseholds(households.get());
                        offlineSyncResponse.setMembers(members.get());
                        offlineSyncResponse.setPregnancyInfos(pregnancyInfos.get());
                        offlineSyncResponse.setHouseholdMemberLinks(householdMemberLinks.get());
                        offlineSyncResponse.setFollowUps(followUps.get());
                        offlineSyncResponse.setFollowUpCriteria(followUpCriteria.get());
                        if (Boolean.TRUE.equals(isSmartAnc)) {
                            offlineSyncResponse.setAncResults(finalAncResults.get());
                        }

                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        throw new ServicesException(1006, e.getMessage());
                    } catch (ExecutionException e) {
                        throw new ServicesException(1006, e.getMessage());
                    }
                }).get();
                executor.shutdown();
            }
            offlineSyncResponse.setLastSyncTime(request.getCurrentSyncTime());
            offlineSyncLog.setStatus(Constants.SUCCESS);
            offlineSyncLog.setResponseData(objectMapper.convertValue(offlineSyncResponse, Map.class));
        } catch (IllegalArgumentException | ExecutionException e) {
            offlineSyncLog.setStatus(Constants.FAILED);
            offlineSyncLog.setErrorMessage(e.getMessage());
            Logger.logError(ErrorConstants.EXECUTION_EXCEPTION_MESSAGE + e);
            throw new ServicesException(1006, e.getMessage());
        } catch (InterruptedException e) {
            offlineSyncLog.setStatus(Constants.FAILED);
            offlineSyncLog.setErrorMessage(e.getMessage());
            Logger.logError(ErrorConstants.EXECUTION_EXCEPTION_MESSAGE + e);
            Thread.currentThread().interrupt();
            throw new ServicesException(1006, e.getMessage());
        } finally {
            offlineSyncLogRepository.save(offlineSyncLog);
        }
        return offlineSyncResponse;
    }

    /**
     * {@inheritDoc}
     */
    public void uploadSignatures(List<MultipartFile> signatureFiles, ProvenanceDTO provenance) {
        String authToken = CommonUtil.getAuthToken();
        String client = CommonUtil.getClient();
        List<CompletableFuture<Void>> futures = signatureFiles.stream()
                .map(file -> helperService.uploadConsentFormFileToS3(file)
                        .thenAccept(url -> {
                            String filename = file.getOriginalFilename();
                            int lastIndexOfDot = filename.lastIndexOf(Constants.DOT);
                            if (lastIndexOfDot != -Constants.ONE) {
                                filename = filename.substring(Constants.ZERO, lastIndexOfDot);
                            }
                            RequestDTO request = new RequestDTO();
                            request.setMemberId(filename);
                            request.setSignature(url);
                            request.setProvenance(provenance);
                            spiceServiceApiInterface.updateSignature(authToken, client, request);
                        })).toList();
        CompletableFuture<Void> allUploads = CompletableFuture.allOf(futures.toArray(new CompletableFuture[Constants.ZERO]));
        try {
            allUploads.get();
        } catch (InterruptedException e) {
            Logger.logError(e);
            Thread.currentThread().interrupt();
            throw new ServicesException(1006, e.getMessage());
        } catch (ExecutionException e) {
            Logger.logError(e);
            throw new ServicesException(1006, e.getMessage());
        }
    }

    /**
     * Logs an offline sync request for operations such as creation, fetching, and status retrieval.
     *
     * @param request    - Offline sync data, either as a Map or another object
     * @param methodName - Name of the method initiating the log
     * @return OfflineSyncLog object with populated fields
     */
    private OfflineSyncLog logOfflineSync(Object request, String methodName) {
        OfflineSyncLog offlineSyncLog = new OfflineSyncLog();
        offlineSyncLog.setStatus(Constants.IN_PROGRESS);
        Map<String, Object> requestData = request instanceof Map
                ? (Map<String, Object>) request
                : objectMapper.convertValue(request, Map.class);
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        offlineSyncLog.setRequestData(requestData);
        offlineSyncLog.setMethodName(methodName);
        offlineSyncLog.setSyncMode((String) requestData.get(Constants.SYNC_MODE));
        offlineSyncLog.setAppVersion((String) requestData.get(Constants.APP_VERSION_NAME));
        offlineSyncLog.setDeviceId((String) requestData.get(Constants.DEVICE_ID));
        return offlineSyncLogRepository.save(offlineSyncLog);
    }

}
