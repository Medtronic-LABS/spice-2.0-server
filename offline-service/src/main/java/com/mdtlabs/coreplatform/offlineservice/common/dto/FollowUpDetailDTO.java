package com.mdtlabs.coreplatform.offlineservice.common.dto;

import com.mdtlabs.coreplatform.offlineservice.common.enumeration.CallStatus;
import lombok.Data;

import java.util.Date;

/**
 * <p>
 * This is a DTO class for follow up details information.
 * </p>
 *
 * @author Maria Antony Created on April 29, 2024.
 */
@Data
public class FollowUpDetailDTO {

    private Date callDate;

    private Integer duration;

    private CallStatus status;

    private String reason;

    private String otherReason;

    private String otherVisitedFacilityName;

    private String visitedFacilityId;

    private String patientStatus;

    private Long attempts;

    private String latitude;

    private String longitude;

    private boolean isInitiated;

}
