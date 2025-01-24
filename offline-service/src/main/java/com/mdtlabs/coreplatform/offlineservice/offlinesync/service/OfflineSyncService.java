package com.mdtlabs.coreplatform.offlineservice.offlinesync.service;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.OfflineSyncResponseDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.RequestDTO;

import org.springframework.messaging.MessageHeaders;

import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

public interface OfflineSyncService {

    /**
     * Push the request to the request queue.
     *
     * @param request - Offline sync DTO
     */
    public void createOfflineSync(Map<String, Object> request);

    /**
     * Process request queue
     *
     * @param data - Content of the request
     * @param headers - Headers of the request
     */
    public void processRequestQueue(String data, MessageHeaders headers);

    /**
     * Generate the response based on uuid.
     *
     * @param request - Request DTO
     */
    public void generateResponse(RequestDTO request);

    /**
     * Load messages from response queue
     *
     * @param data - Content of the request
     * @param headers - Headers of the request
     */
    public void processResponseQueue(String data, MessageHeaders headers);

    /**
     * Get offline sync status list
     *
     * @param request - Request DTO
     * @return List of offline sync
     */
    public ResponseListDTO<OfflineSyncDTO> getOfflineSyncStatusList(RequestDTO request);

    /**
     * Fetch synced data for Household, Household Member, ...etc.
     *
     * @param request - Request DTO
     * @return - synced data response
     */
    public OfflineSyncResponseDTO fetchSyncedData(RequestDTO request) throws ExecutionException, InterruptedException;

    /**
     * Upload the signature files to the S3 bucket.
     *
     * @param signatureFiles - Signature files
     * @param provenance - Provenance DTO
     */
    public void uploadSignatures(List<MultipartFile> signatureFiles, ProvenanceDTO provenance);

}
