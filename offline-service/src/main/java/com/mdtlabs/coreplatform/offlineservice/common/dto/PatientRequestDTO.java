package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * This is a DTO class for PatientRequest entity.
 * </p>
 *
 * @author Nanthinee sugumar Created on Feb 28, 2024.
 */
@Data
public class PatientRequestDTO {

    private String ncdType;

    private String name;

    private String phoneNumber;

    private String id;

    private String idSystem;

    private String idCode;

    private String siteId;

    private String status;

    private String villageId;

    private int count;

    private List<String> villageNames;

    private String searchText;

    private int skip;

    private int limit;

    private List<Long> villageIds;

    private Long districtId;

    private String referencePatientId;

    private String appType;

    private String type;

    private Long countryId;

    private Long tenantId;

    private String memberReference;

    private List<Integer> remainingAttempts;

    private String dateRange;

    private Map<String, Date> customDate;

    private Date currentSyncTime;

    private Date lastSyncTime;

}

