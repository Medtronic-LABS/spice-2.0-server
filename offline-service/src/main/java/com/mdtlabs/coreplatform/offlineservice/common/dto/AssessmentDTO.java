package com.mdtlabs.coreplatform.offlineservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for AssessmentDTO entity.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class AssessmentDTO {

    private String id;

    private String assessmentType;

    private String patientStatus;

    private AssessmentDetailsDTO assessmentDetails;

    private String referenceId;

    private Long followUpId;

    private String referredSiteId;

    private String referredReasons;

    private Date referredDate;

    private SummaryDTO summary;

    private EncounterDetailsDTO encounter;

    private String villageId;

    private long updatedAt;

}