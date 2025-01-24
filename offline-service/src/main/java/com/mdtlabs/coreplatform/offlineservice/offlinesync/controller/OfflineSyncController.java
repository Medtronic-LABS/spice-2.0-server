package com.mdtlabs.coreplatform.offlineservice.offlinesync.controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.zip.GZIPOutputStream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HealthFacilityDTO;
import jakarta.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.offlineservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncResponseDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.offlineservice.message.SuccessCode;
import com.mdtlabs.coreplatform.offlineservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.OfflineSyncService;


/**
 * <p>
 * This class is a controller class to perform operation offline synchronization
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 */
@RestController
@RequestMapping(value = "/offline-sync")
public class OfflineSyncController {

    ObjectMapper objectMapper;

    private OfflineSyncService offlineSyncService;

    private AdminServiceApiInterface adminServiceApiInterface;

    public OfflineSyncController(ObjectMapper objectMapper, OfflineSyncService offlineSyncService, AdminServiceApiInterface adminServiceApiInterface){
        this.offlineSyncService = offlineSyncService;
        this.objectMapper = objectMapper;
        this.adminServiceApiInterface =adminServiceApiInterface;
    }

    /**
     * Push the request to the request queue.
     *
     * @param request - Offline sync DTO
     * @return - Success message
     */
    @PostMapping("/create")
    public SuccessResponse<Boolean> createOfflineSync(@RequestBody Map<String, Object> request) {
        if (!Constants.NON_COMMUNITY.equalsIgnoreCase((String) request.get(Constants.APP_TYPE))) {
            updateOrganization(request);
        }
        offlineSyncService.createOfflineSync(request);
        return new SuccessResponse<>(SuccessCode.OFFLINE_SYNC_REQUEST_SAVE, HttpStatus.CREATED);
    }

    /**
     * Generate the response based on uuid.
     *
     * @param request - Request DTO
     * @return - Success message
     */
    @PostMapping("/generate-response")
    public SuccessResponse<Boolean> createResponse(@RequestBody RequestDTO request) {
        offlineSyncService.generateResponse(request);
        return new SuccessResponse<>(SuccessCode.OFFLINE_SYNC_GENERATE_RESPONSE, HttpStatus.CREATED);
    }

    /**
     * Get all status of the offline sync data.
     *
     * @param request - Request DTO
     * @return - Success message
     */
    @PostMapping("/status")
    public SuccessResponse<OfflineSyncDTO> getOfflineSyncStatusList(@RequestBody RequestDTO request) {
        ResponseListDTO<OfflineSyncDTO> responseList = offlineSyncService.getOfflineSyncStatusList(request);
        return new SuccessResponse<>(SuccessCode.OFFLINE_SYNC_GET_STATUS, responseList.getData(),
                responseList.getTotalCount(), HttpStatus.OK);
    }

    /**
     * Fetch synced data for Household, Household Member, ...etc.
     *
     * @param response - HTTP Servlet response
     * @param request  - Request DTO
     */
    @PostMapping("/fetch-synced-data")
    public void fetchSyncedData(HttpServletResponse response, @RequestBody RequestDTO request) throws IOException, ExecutionException, InterruptedException {
        OfflineSyncResponseDTO data = offlineSyncService.fetchSyncedData(request);
        response.setHeader(Constants.CONTENT_ENCODING_HEADER, Constants.GZIP);
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);

        try (GZIPOutputStream gzipOutputStream = new GZIPOutputStream(response.getOutputStream())) {
            objectMapper.writeValue(gzipOutputStream, data);
        }
    }

    /**
     * Upload the signature files to the S3 bucket.
     *
     * @param provenanceRequest - Provenance
     * @param signatureFiles    - Signature files
     */
    @PostMapping("/upload-signatures")
    public SuccessResponse<Boolean> uploadSignatures(@RequestParam(Constants.PROVENANCE) String provenanceRequest,
                                                     @RequestParam(Constants.SIGNATURE_FILE) List<MultipartFile> signatureFiles) throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        ProvenanceDTO provenance = mapper.readValue(provenanceRequest, ProvenanceDTO.class);
        offlineSyncService.uploadSignatures(signatureFiles, provenance);
        return new SuccessResponse<>(SuccessCode.OFFLINE_SYNC_SIGNATURE_UPLOAD, HttpStatus.OK);
    }

    /**
     * Upload organization data in provenance
     *
     * @param request - Request object
     */
    private void updateOrganization(Map<String, Object> request) {
        try {
            Map<Long, String> villageFacilityMap = new HashMap<>();
            List<Long> organizationIds = Objects.isNull(UserContextHolder.getUserDto().getOrganizationIds()) ? new ArrayList<>() :
                    UserContextHolder.getUserDto().getOrganizationIds().stream().toList();
            List<HealthFacilityDTO> healthFacilities = adminServiceApiInterface.getFacilityByTenants(CommonUtil.getAuthToken(),
                    CommonUtil.getClient(), organizationIds);
            healthFacilities.forEach(healthFacility ->
                    healthFacility.getLinkedVillages().forEach(village -> villageFacilityMap.put(village.getId(), healthFacility.getFhirId())));
            List<Map<String, Object>> households = (List<Map<String, Object>>) request.get(Constants.HOUSEHOLDS);
            updateOrganizationForHouseholds(households, villageFacilityMap);
            List<Map<String, Object>> householdMembers = (List<Map<String, Object>>) request.get(Constants.HOUSEHOLD_MEMBERS);
            updateOrganizationForHouseholdMembers(householdMembers, villageFacilityMap);
            List<Map<String, Object>> assessments = (List<Map<String, Object>>) request.get(Constants.ASSESSMENTS);
            updateOrganizationForAssessments(assessments, villageFacilityMap);
            List<Map<String, Object>> followUps = (List<Map<String, Object>>) request.get(Constants.FOLLOW_UPS);
            updateOrganizationForFollowups(followUps, villageFacilityMap);
        } catch (Exception e) {
            Logger.logError("Error occurred while updating organization");
            Logger.logError(e);
        }
    }
    /**
     * Upload organization data for household in provenance
     *
     * @param households         - households object
     * @param villageFacilityMap - villageFacilityMap
     */
    private void updateOrganizationForHouseholds(List<Map<String, Object>> households, Map<Long, String> villageFacilityMap) {
        if (Objects.nonNull(households)) {
            households.forEach(household -> {
                Map<String, Object> provenance = (Map<String, Object>) household.get(Constants.PROVENANCE);
                provenance.put(Constants.ORGANIZATION_ID, villageFacilityMap.get(parseVillageId(household.get(Constants.VILLAGE_ID))));

                String orgId = villageFacilityMap.get(parseVillageId(household.get(Constants.VILLAGE_ID)));
                if (Objects.nonNull(orgId)) {
                    provenance.put(Constants.ORGANIZATION_ID, orgId);
                }
                List<Map<String, Object>> householdMembers = (List<Map<String, Object>>) household.get(Constants.HOUSEHOLD_MEMBERS);
                updateOrganizationForHouseholdMembers(householdMembers, villageFacilityMap);
            });
        }
    }
    /**
     * Upload organization data for householdMember in provenance
     *
     * @param householdMembers   - householdMembers object
     * @param villageFacilityMap - villageFacilityMap
     */
    private void updateOrganizationForHouseholdMembers(List<Map<String, Object>> householdMembers, Map<Long, String> villageFacilityMap) {
        if (Objects.nonNull(householdMembers)) {
            householdMembers.forEach(householdMember -> {
                Map<String, Object> provenance = (Map<String, Object>) householdMember.get(Constants.PROVENANCE);
                String orgId = villageFacilityMap.get(parseVillageId(householdMember.get(Constants.VILLAGE_ID)));
                if (Objects.nonNull(orgId)) {
                    provenance.put(Constants.ORGANIZATION_ID, orgId);
                }
                List<Map<String, Object>> childMembers = (List<Map<String, Object>>) householdMember.get(Constants.CHILDREN);
                updateOrganizationForHouseholdMembers(childMembers, villageFacilityMap);
                List<Map<String, Object>> assessments = (List<Map<String, Object>>) householdMember.get(Constants.ASSESSMENTS);
                updateOrganizationForAssessments(assessments, villageFacilityMap);
            });
        }
    }
    /**
     * Upload organization data for assessment in provenance
     *
     * @param assessments        - assessments object
     * @param villageFacilityMap - villageFacilityMap
     */
    private void updateOrganizationForAssessments(List<Map<String, Object>> assessments, Map<Long, String> villageFacilityMap) {
        if (Objects.nonNull(assessments)) {
            assessments.forEach(assessment -> {
                Map<String, Object> provenance = (Map<String, Object>) ((Map<String, Object>) assessment.get(Constants.ENCOUNTER)).get(Constants.PROVENANCE);
                String orgId = villageFacilityMap.get(parseVillageId(assessment.get(Constants.VILLAGE_ID)));
                if (Objects.nonNull(orgId)) {
                    provenance.put(Constants.ORGANIZATION_ID, orgId);
                }
            });
        }
    }
    /**
     * Upload organization data for followUp in provenance
     *
     * @param followUps          - followUps object
     * @param villageFacilityMap - villageFacilityMap
     */
    private void updateOrganizationForFollowups(List<Map<String, Object>> followUps, Map<Long, String> villageFacilityMap) {
        if (Objects.nonNull(followUps)) {
            followUps.forEach(followUp -> {
                Map<String, Object> provenance = (Map<String, Object>) followUp.get(Constants.PROVENANCE);
                String orgId = villageFacilityMap.get(parseVillageId(followUp.get(Constants.VILLAGE_ID)));
                if (Objects.nonNull(orgId)) {
                    provenance.put(Constants.ORGANIZATION_ID, orgId);
                }
            });
        }
    }
    /**
     * Parse village id object
     *
     * @param villageObject - village object
     * @return village Id
     */
    private Long parseVillageId(Object villageObject) {
        return (villageObject instanceof Long longValue) ?
                longValue :
                Long.parseLong(String.valueOf(villageObject));
    }
}
