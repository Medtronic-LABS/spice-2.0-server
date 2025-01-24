package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.Map;

/**
 * <p>
 * This is a DTO class for a offline sync.
 * </p>
 *
 */
@Data
public class OfflineSyncDTO {

    private String type;

    private Map<String, Object> data;

    private String requestId;

    private String referenceId;

    private String fhirId;

    private String status;

}