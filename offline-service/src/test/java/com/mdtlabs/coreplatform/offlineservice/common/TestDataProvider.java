package com.mdtlabs.coreplatform.offlineservice.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.util.StdDateFormat;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AncResultDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpCriteria;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpDetailDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberMapDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberSequenceDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FileDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.offlineservice.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.offlineservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.offlineservice.common.enumeration.CallStatus;
import com.mdtlabs.coreplatform.offlineservice.common.model.OfflineSync;
import com.mdtlabs.coreplatform.offlineservice.common.model.OfflineSyncLog;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.mock.web.MockMultipartFile;

import java.io.ByteArrayInputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * This class used to set testData for unit Testcases
 */
public class TestDataProvider {

    /**
     * Get Household Object For Test Data
     *
     * @return HouseholdDTO Object
     */
    public static HouseholdDTO getHouseholdData() {
        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setHouseholdNo(TestConstants.ONE);
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setReferenceId(TestConstants.ONE.toString());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);
        householdDTO.setHouseholdMembers(List.of(getHouseHoldMember()));
        householdDTO.setProvenance(getProvenance());
        return householdDTO;
    }

    /**
     * Get Household Member for TestData
     *
     * @return HouseholdMember data
     */
    public static HouseholdMemberDTO getHouseHoldMember() {
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setId(TestConstants.MEMBER_ID);
        householdMemberDTO.setGender(TestConstants.GENDER);
        householdMemberDTO.setHouseholdId(TestConstants.ONE.toString());
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setReferenceId(TestConstants.ONE.toString());
        householdMemberDTO.setHouseholdHeadRelationship(TestConstants.NAME);
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setProvenance(getProvenance());
        return householdMemberDTO;
    }

    /**
     * Get Offline Sync for TestData
     *
     * @return OfflineSync data
     */
    public static OfflineSyncDTO getOfflineSyncDTO() {
        OfflineSyncDTO offlineSyncDTO = new OfflineSyncDTO();
        offlineSyncDTO.setType(Constants.HOUSEHOLD);
        offlineSyncDTO.setFhirId(TestConstants.FHIR_ID);
        offlineSyncDTO.setStatus(Constants.SUCCESS);
        offlineSyncDTO.setReferenceId(TestConstants.ONE.toString());
        return offlineSyncDTO;
    }

    /**
     * Get S3 File for TestData
     *
     * @return S3File data
     */
    public static FileDTO getFile() {
        FileDTO fileDTO = new FileDTO();
        fileDTO.setInputStream(new ByteArrayInputStream(TestConstants.INPUT_BYTES));
        fileDTO.setContentLength(TestConstants.TWO);
        return fileDTO;
    }

    /**
     * Get Assessment Object For Test Data
     *
     * @return AssessmentDTO Object
     */
    public static AssessmentDTO getAssessmentData() {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setId("123");
        assessmentDTO.setEncounter(getEncounterDetailsDTO());
        assessmentDTO.getEncounter().setHouseholdId(TestConstants.HOUSEHOLD_ID);
        assessmentDTO.getEncounter().setMemberId(TestConstants.MEMBER_ID);
        assessmentDTO.getEncounter().setPatientId(TestConstants.PATIENT_ID);
        assessmentDTO.setAssessmentType(TestConstants.ICCM);
        assessmentDTO.setReferenceId(TestConstants.ONE.toString());
        assessmentDTO.setUpdatedAt(TestConstants.ONE);
        return assessmentDTO;
    }

    /**
     * Get EncounterDetailsDTO for TestData
     *
     * @return EncounterDetailsDTO data
     */
    public static EncounterDetailsDTO getEncounterDetailsDTO() {
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setHouseholdId(TestConstants.HOUSEHOLD_ID);
        encounterDetailsDTO.setMemberId(TestConstants.MEMBER_ID);
        encounterDetailsDTO.setPatientId(TestConstants.PATIENT_ID);
        return encounterDetailsDTO;
    }

    /**
     * Get offline sync For Test Data
     *
     * @param requestId   - Request ID
     * @param requestData - Request Data
     * @param requestTime - Request Time
     * @param type        - Type
     * @param status      - Status
     * @return OfflineSync
     */
    public static OfflineSync getOfflineSync(String requestId, Map<String, Object> requestData,
                                             Date requestTime, String type, String status) {
        OfflineSync offlineSync = new OfflineSync();
        offlineSync.setId(TestConstants.ONE);
        offlineSync.setRequestId(requestId);
        offlineSync.setReferenceId(TestConstants.ONE.toString());
        offlineSync.setType(type);
        offlineSync.setRequestData(requestData);
        offlineSync.setRequestTime(requestTime);
        offlineSync.setStatus(status);
        offlineSync.setAppVersion(TestConstants.APP_VERSION);
        offlineSync.setDeviceId(TestConstants.DEVICE_ID);
        return offlineSync;
    }

    /**
     * Get offline sync log For Test Data
     *
     * @param requestData - Request Data
     * @return OfflineSyncLog
     */
    public static OfflineSyncLog getOfflineSyncLog(Map<String, Object> requestData) {
        OfflineSyncLog offlineSyncLog = new OfflineSyncLog();
        offlineSyncLog.setStatus(Constants.IN_PROGRESS);
        offlineSyncLog.setRequestData(requestData);
        return offlineSyncLog;
    }

    /**
     * Get message headers For Test Data
     *
     * @param trackId - Track ID
     * @return MessageHeaders
     */
    public static MessageHeaders getMessageHeader(Long trackId) {
        Map<String, Object> headerParams = new HashMap<>();
        headerParams.put(Constants.FIELD_TRACE_ID, trackId);
        headerParams.put(Constants.FIELD_MESSAGE_GROUP_ID, TestConstants.ONE);
        return new MessageHeaders(headerParams);
    }

    /**
     * Serialize data
     *
     * @param data - Object data
     * @return String
     */
    public static String serializeData(Object data) throws JsonProcessingException {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.writeValueAsString(data);
    }

    /**
     * Convert to Hashmap
     *
     * @param data - Object data
     * @return String
     */
    public static Map<String, Object> convertToHashMap(Object data) {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        objectMapper.setDateFormat(new StdDateFormat());
        return objectMapper.convertValue(data, new TypeReference<>() {
        });
    }

    /**
     * Convert non-null fields to Hashmap
     *
     * @param data - Object data
     * @return String
     */
    public static Map<String, Object> convertNonNullFieldsToHashMap(Object data) {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        objectMapper.setDateFormat(new StdDateFormat());
        return objectMapper.convertValue(data, new TypeReference<>() {
        });
    }

    /**
     * Get FollowUpDTO for TestData
     *
     * @return FollowUpDTO data
     */
    public static FollowUpDTO getFollowUp() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setAttempts(TestConstants.ONE);
        followUpDTO.setCurrentPatientStatus(TestConstants.REFERRED);
        followUpDTO.setFollowUpDetails(getFollowUpDetailDTO());
        followUpDTO.setEncounterDate(new Date());
        followUpDTO.setEncounterId(TestConstants.STRING_ONE);
        followUpDTO.setEncounterType(TestConstants.ICCM);
        followUpDTO.setHouseholdId(TestConstants.STRING_ONE);
        followUpDTO.setId(TestConstants.ONE);
        followUpDTO.setIsCompleted(Boolean.TRUE);
        followUpDTO.setMemberId(TestConstants.STRING_ONE);
        followUpDTO.setNextVisitDate(new Date());
        followUpDTO.setPatientId(TestConstants.STRING_ONE);
        followUpDTO.setProvenance(getProvenance());
        followUpDTO.setPatientStatus(TestConstants.ON_TREATMENT);
        followUpDTO.setReason(TestConstants.MALARIA);
        followUpDTO.setReferredSiteId(TestConstants.STRING_ONE);
        followUpDTO.setSuccessfulAttempts(1);
        followUpDTO.setType(AppointmentType.HH_VISIT);
        followUpDTO.setVillageId(String.valueOf(Constants.ONE));
        followUpDTO.setUpdatedAt(1L);
        return followUpDTO;
    }

    /**
     * Get ProvenanceDTO for TestData
     *
     * @return ProvenanceDTO data
     */
    public static ProvenanceDTO getProvenance() {
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setModifiedDate(getFixedDate());
        provenanceDTO.setUserId(TestConstants.STRING_ONE);
        provenanceDTO.setOrganizationId(TestConstants.STRING_ONE);
        return provenanceDTO;
    }

    /**
     * Get fixed calender date
     *
     * @return Date
     */
    public static Date getFixedDate() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(2024, Calendar.JUNE, Constants.ONE, Constants.ONE, Constants.ONE, Constants.ONE);
        calendar.set(Calendar.MILLISECOND, 100);
        return calendar.getTime();
    }

    public static String getProvenanceString() {
        return "{\"userId\":\"1\",\"organizationId\":\"1\",\"modifiedDate\":\"2021-09-01T00:00:00.000+00:00\"}";
    }

    /**
     * Get FollowUpDetailDTO List  for TestData
     *
     * @return FollowUpDetailDTO list data
     */
    public static List<FollowUpDetailDTO> getFollowUpDetailDTO() {
        List<FollowUpDetailDTO> followUpDetailDTOS = new ArrayList<>();
        FollowUpDetailDTO followUpDetailDTO = new FollowUpDetailDTO();
        followUpDetailDTO.setAttempts(TestConstants.ONE);
        followUpDetailDTO.setCallDate(new Date());
        followUpDetailDTO.setPatientStatus(TestConstants.REFERRED);
        followUpDetailDTO.setStatus(CallStatus.SUCCESSFUL);
        followUpDetailDTOS.add(followUpDetailDTO);
        return followUpDetailDTOS;
    }


    /**
     * Get HouseholdMemberSequenceDTO Member for TestData
     *
     * @return HouseholdMemberSequenceDTO data
     */
    public static HouseholdMemberSequenceDTO getHouseholdMemberSequenceDTO() {
        HouseholdMemberSequenceDTO householdMemberSequenceDTO = new HouseholdMemberSequenceDTO();
        householdMemberSequenceDTO.setChiefdomCode(TestConstants.STRING_ONE);
        householdMemberSequenceDTO.setSequence(TestConstants.ONE);
        householdMemberSequenceDTO.setUserId(TestConstants.STRING_ONE);
        householdMemberSequenceDTO.setVillageCode(TestConstants.STRING_ONE);
        return householdMemberSequenceDTO;
    }

    /**
     * Get FollowUpCriteria for TestData
     *
     * @return FollowUpCriteria data
     */
    public static FollowUpCriteria getFollowUpCriteria() {
        FollowUpCriteria followUpCriteria = new FollowUpCriteria();
        followUpCriteria.setMalaria(TestConstants.INT_ONE);
        followUpCriteria.setPneumonia(TestConstants.INT_ONE);
        followUpCriteria.setDiarrhea(TestConstants.INT_ONE);
        followUpCriteria.setMuac(TestConstants.INT_ONE);
        followUpCriteria.setEscalation(TestConstants.INT_ONE);
        followUpCriteria.setAncVisit(TestConstants.INT_ONE);
        followUpCriteria.setReferral(TestConstants.INT_ONE);
        followUpCriteria.setPncVisit(TestConstants.INT_ONE);
        followUpCriteria.setChildVisit(TestConstants.INT_ONE);
        followUpCriteria.setSuccessfulAttempts(TestConstants.INT_ONE);
        followUpCriteria.setUnsuccessfulAttempts(TestConstants.INT_ZERO);
        return followUpCriteria;
    }

    /**
     * Get PregnancyInfo for TestData
     *
     * @return PregnancyInfo data
     */
    public static PregnancyInfo getPregnancyInfo() {
        Date date = new Date();
        PregnancyInfo pregnancyInfo = new PregnancyInfo();
        pregnancyInfo.setHouseholdMemberId(TestConstants.STRING_ONE);
        pregnancyInfo.setAncVisitNo(TestConstants.INT_ONE);
        pregnancyInfo.setLastMenstrualPeriod(date);
        pregnancyInfo.setEstimatedDeliveryDate(date);
        pregnancyInfo.setNoOfNeonates(TestConstants.INT_ONE);
        pregnancyInfo.setChildVisitNo(TestConstants.INT_ONE);
        pregnancyInfo.setDateOfDelivery(date);
        return pregnancyInfo;
    }

    /**
     * Get Mocked Signature File
     *
     * @return MockMultipartFile
     */
    public static MockMultipartFile getMockedSignatureFile() {
        return new MockMultipartFile(
                "signatureFiles",
                "signature.jpeg",
                "text/plain",
                "Sample Signature Data".getBytes()
        );
    }

    public static AncResultDTO getAncResultDTO() {
        AncResultDTO ancResultDTO = new AncResultDTO();
        ancResultDTO.setMemberId(TestConstants.MEMBER_ID);
        ancResultDTO.setPatientId(TestConstants.PATIENT_ID);
        ancResultDTO.setSmartAncContactDetails(TestConstants.IS_SMART_ANC);
        return ancResultDTO;
    }

    public static FollowUpDTO getScreeningFollowUp() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setId(TestConstants.FOLLOW_UP_ID);
        followUpDTO.setPatientId(TestConstants.PATIENT_ID);
        followUpDTO.setScreeningDateTime(new Date());
        followUpDTO.setNextVisitDate(new Date());
        followUpDTO.setVillageName(TestConstants.NAME);
        return followUpDTO;
    }

    public static FollowUpDTO getAssessmentFollowUp() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setId(TestConstants.FOLLOW_UP_ID);
        followUpDTO.setPatientId(TestConstants.PATIENT_ID);
        followUpDTO.setAssessmentDate(new Date());
        followUpDTO.setCurrentPatientStatus("Under Observation");
        followUpDTO.setOverDueCategories(List.of("BP Check", "Sugar Level"));
        return followUpDTO;
    }

    public static FollowUpDTO getLostToFollowUp() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setId(TestConstants.FOLLOW_UP_ID);
        followUpDTO.setPatientId(TestConstants.PATIENT_ID);
        followUpDTO.setIsCompleted(false);
        return followUpDTO;
    }

    public static FollowUpDTO getMedicalReviewFollowUp() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setId(TestConstants.FOLLOW_UP_ID);
        followUpDTO.setPatientId(TestConstants.PATIENT_ID);
        followUpDTO.setNextMedicalReviewDate(new Date());
        return followUpDTO;
    }

    public static HouseholdMemberMapDTO getHouseholdMemberMapDTO() {
        HouseholdMemberMapDTO householdMemberMapDTO = new HouseholdMemberMapDTO();
        HouseholdMemberDTO member = new HouseholdMemberDTO();
        member.setId(TestConstants.USER_ID);
        member.setName(TestConstants.NAME);
        member.setReferenceId(TestConstants.STRING_ONE);
        member.setHouseholdId(TestConstants.STRING_ONE);
        member.setHouseholdReferenceId(TestConstants.STRING_ONE);
        member.setPatientId(TestConstants.PATIENT_ID);
        member.setVillageId(TestConstants.STRING_ONE);
        member.setAssessments(List.of(getAssessmentData()));
        member.setProvenance(getProvenance());

        householdMemberMapDTO.setHouseholdId(TestConstants.HOUSEHOLD_ID);
        householdMemberMapDTO.setHouseholdMembers(List.of(member));

        return householdMemberMapDTO;
    }

    public static Message<String> getMessage(String payload) {
        //create headers for the message
        MessageHeaders headers = new MessageHeaders(Map.of(
                Constants.FIELD_TRACE_ID, UUID.randomUUID().toString(),
                TestConstants.FIELD_TIMESTAMP, Instant.now().toString()
        ));
        return MessageBuilder.createMessage(payload, headers);
    }

    public static FollowUpDTO getFollowUpDTO() {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setId(1L);
        followUpDTO.setHouseholdId("H001");
        followUpDTO.setPatientId("patientId1");
        followUpDTO.setEncounterId("E001");
        followUpDTO.setEncounterName("Routine Checkup");
        followUpDTO.setEncounterType("Screening");
        followUpDTO.setPatientStatus("Active");
        followUpDTO.setReason("Follow-up on health status");
        followUpDTO.setAttempts(3);
        followUpDTO.setVisits(2);
        followUpDTO.setSuccessfulAttempts(1);
        followUpDTO.setUnsuccessfulAttempts(2);
        followUpDTO.setCurrentPatientStatus("Under Observation");
        followUpDTO.setType(AppointmentType.SCREENED);
        followUpDTO.setNextVisitDate(new Date());
        followUpDTO.setEncounterDate(new Date());
        followUpDTO.setReferredSiteId("Site A");
        followUpDTO.setVillageId("V001");
        followUpDTO.setCalledAt(System.currentTimeMillis());
        followUpDTO.setProvenance(new ProvenanceDTO()); // Assuming you have a constructor or setters in ProvenanceDTO
        followUpDTO.setAppType("Mobile");
        followUpDTO.setName("John Doe");
        followUpDTO.setGender("Male");
        followUpDTO.setAge(33); // Set age directly or calculate it
        followUpDTO.setPhoneNumber("123-456-7890");
        followUpDTO.setCountyName("County A");
        followUpDTO.setSubCountyName("SubCounty B");
        followUpDTO.setCommunityHealthUnitName("Health Unit A");
        followUpDTO.setVillageName("Village A");
        followUpDTO.setLandmark("Near the river");
        followUpDTO.setReferAssessment(true);
        followUpDTO.setCallCompleted(false);
        followUpDTO.setCallInitiated(true);
        followUpDTO.setReferredDateSince(System.currentTimeMillis());
        followUpDTO.setRetryAttempts(2);
        followUpDTO.setScreeningDateTime(new Date());
        followUpDTO.setAssessmentDate(new Date());
        followUpDTO.setNextMedicalReviewDate(new Date());
        followUpDTO.setNextBpAssessmentDate(new Date());
        followUpDTO.setNextBgAssessmentDate(new Date());
        followUpDTO.setOverDueCategories(Arrays.asList("Category 1", "Category 2"));
        followUpDTO.setDueDate(new Date());
        followUpDTO.setCallRegisterId(1001L);
        followUpDTO.setReferredReasons(Arrays.asList("Reason 1", "Reason 2"));
        followUpDTO.setIdentityType("National ID");
        followUpDTO.setIdentityValue("N123456789");
        followUpDTO.setActive(true);
        followUpDTO.setDeleted(false);
        followUpDTO.setUpdatedAt(System.currentTimeMillis());
        return followUpDTO;
    }

    public static String getCreateOfflineSyncObject() {
        return """
                {
                    "requestId":"7c57f2a0-96e3-4517-b21b-3b1dd066e780",
                        "appVersionName":"2.1.0",
                        "appVersionCode":6,
                        "deviceId":"08ca76d0-ca8b-4832-8ecc-f6368fcc3f78",
                        "appType":"NON_COMMUNITY",
                        "followUps": [
                    {
                        "createdAt":1733133318235,
                            "createdBy":71,
                            "followUpDetails": [
                        {
                            "attempts":1,
                                "callDate":"2024-12-02T09:55:18.225Z",
                                "createdAt":1733133318235,
                                "createdBy":71,
                                "id":4543,
                                "isSynced":false,
                                "key":1,
                                "memberId":"159757",
                                "patientId":"159753",
                                "patientStatus":"Will visit facility",
                                "referredSiteId":"30",
                                "status":"SUCCESSFUL",
                                "type":"SCREENED",
                                "updatedAt":1733133318239,
                                "updatedBy":71
                        },
                        {
                            "attempts":2,
                                "callDate":"2024-12-02T09:57:59.124Z",
                                "createdAt":1733133479125,
                                "createdBy":71,
                                "id":4543,
                                "isSynced":false,
                                "key":3,
                                "memberId":"159757",
                                "patientId":"159753",
                                "patientStatus":"Will visit facility",
                                "referredSiteId":"30",
                                "status":"SUCCESSFUL",
                                "type":"SCREENED",
                                "updatedAt":1733133479127,
                                "updatedBy":71
                        }
                                  ],
                        "id":4543,
                            "isInitiated":false,
                            "memberId":"159757",
                            "patientId":"159753",
                            "provenance":{
                        "modifiedDate":"2024-12-02T10:14:31.422Z",
                                "organizationId":"48",
                                "spiceUserId":71,
                                "userId":"130"
                    },
                        "type":"SCREENED",
                            "updatedAt":1733133318239,
                            "updatedBy":71
                    },
                    {
                        "createdAt":1733133359106,
                            "createdBy":71,
                            "followUpDetails": [
                        {
                            "attempts":1,
                                "callDate":"2024-12-02T09:55:59.105Z",
                                "createdAt":1733133359106,
                                "createdBy":71,
                                "id":4539,
                                "isSynced":false,
                                "key":2,
                                "memberId":"159649",
                                "patientId":"159603",
                                "reason":"WRONG_NUMBER",
                                "referredSiteId":"30",
                                "status":"UNSUCCESSFUL",
                                "type":"SCREENED",
                                "updatedAt":1733133359114,
                                "updatedBy":71
                        }
                                  ],
                        "id":4539,
                            "isInitiated":false,
                            "memberId":"159649",
                            "patientId":"159603",
                            "provenance":{
                        "modifiedDate":"2024-12-02T10:14:31.45Z",
                                "organizationId":"48",
                                "spiceUserId":71,
                                "userId":"130"
                    },
                        "type":"SCREENED",
                            "updatedAt":1733133359114,
                            "updatedBy":71
                    }
                              ]
                }
                """;
    }

    public static Map<String, Object> getOfflineSyncObject() {
        try {
            return new ObjectMapper().readValue(getCreateOfflineSyncObject(),
                    new TypeReference<Map<String, Object>>() {});
        } catch (JsonProcessingException exception) {
            return null;
        }
    }

    public static HouseholdMemberLinkDTO getHouseholdMemberLinkDTO() {
        HouseholdMemberLinkDTO householdMemberLinkDTO = new HouseholdMemberLinkDTO();
        householdMemberLinkDTO.setMemberId(TestConstants.MEMBER_ID);
        householdMemberLinkDTO.setPatientId(TestConstants.PATIENT_ID);
        householdMemberLinkDTO.setProvenance(TestDataProvider.getProvenance());
        householdMemberLinkDTO.setVillageId(TestConstants.STRING_ONE);
        return householdMemberLinkDTO;
    }

    public static String getMemberAssessmentFollowUpData() {
        return """
                {
                  "assessments": [
                    {
                      "summary": {
                        "nextVisitDate": "2025-03-10T00:00:00+00:00"
                      },
                      "encounter": {
                        "endTime": "2024-12-01T10:30:00.000Z",
                        "latitude": 12.34567,
                        "memberId": "123456",
                        "referred": true,
                        "longitude": 76.54321,
                        "patientId": "987654321012345",
                        "startTime": "2024-12-01T09:00:00.000Z",
                        "provenance": {
                          "userId": "123456",
                          "spiceUserId": 7890,
                          "modifiedDate": "2024-12-01T09:45:00.000Z",
                          "organizationId": "54321"
                        },
                        "householdId": "654321",
                        "visitNumber": 2
                      },
                      "updatedAt": 1732295400000,
                      "villageId": "222",
                      "referenceId": 2,
                      "patientStatus": "Referred",
                      "assessmentType": "PNC",
                      "referredReasons": "High Risk Pregnancy",
                      "assessmentDetails": {
                        "pnc": {
                          "visitNo": 2,
                          "pncSigns": ["fever", "headache"],
                          "babyHealth": "Good",
                          "motherHealth": "Stable",
                          "feedingFrequency": "Every 3 hours",
                          "vaccinationStatus": "Up to Date"
                        }
                      },
                      "householdMemberId": "123456"
                    }
                  ],
                  "followUps": [
                    {
                      "id": 5455,
                      "householdId": "8559",
                      "memberId": "159500",
                      "patientId": "0890002800162",
                      "encounterId": "159506",
                      "encounterName": "OTHER_SYMPTOMS",
                      "encounterType": "ICCM",
                      "patientStatus": "Referred",
                      "reason": "Symptoms, Malaria",
                      "attempts": 0,
                      "successfulAttempts": 0,
                      "unsuccessfulAttempts": 0,
                      "isCompleted": false,
                      "type": "REFERRED",
                      "isWrongNumber": false,
                      "encounterDate": "2024-12-04T07:37:28+00:00",
                      "referredSiteId": "6182",
                      "villageId": "4",
                      "updatedAt": 0,
                      "name": "OTHER_SYMPTOMS",
                      "referAssessment": false,
                      "createdAt": "2024-12-04T07:37:52+00:00",
                      "noOfDueDays": 0,
                      "callRegisterId": 5455,
                      "callCompleted": false,
                      "callInitiated": false
                    }
                  ]
                }
                """;
    }

    public static HouseholdMemberDTO getHouseHoldMemberWithChildren() {
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setId(TestConstants.FHIR_ID);
        householdMemberDTO.setGender(TestConstants.GENDER);
        householdMemberDTO.setHouseholdId(TestConstants.ONE.toString());
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setReferenceId(TestConstants.ONE.toString());
        householdMemberDTO.setHouseholdHeadRelationship(TestConstants.NAME);
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setProvenance(getProvenance());
        householdMemberDTO.setChildren(List.of(getHouseHoldMember()));
        return householdMemberDTO;
    }
}
