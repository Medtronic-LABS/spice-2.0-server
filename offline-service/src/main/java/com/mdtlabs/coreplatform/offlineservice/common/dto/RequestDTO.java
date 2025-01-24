package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for a common request.
 * </p>
 *
 */
@Data
public class RequestDTO {

    private String appType;

    private String requestId;

    private String patientId;

    private String memberId;

    private List<String> villageIds;

    private String villageId;

    private List<String> types;

    private List<String> statuses;

    private Long userId;

    private int skip;

    private int limit;

    private boolean isDataRequired;

    private Date currentSyncTime;

    private Date lastSyncTime;

    private String appVersionName;

    private String deviceId;

    private String signature;

    private ProvenanceDTO provenance;

}
