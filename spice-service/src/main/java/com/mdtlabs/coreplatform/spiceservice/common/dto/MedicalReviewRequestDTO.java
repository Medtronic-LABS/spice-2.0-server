package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This is DTO class for medical review Request.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class MedicalReviewRequestDTO {

    private boolean isLatestRequired;

    private String patientId;

    private String patientReference;

    private String encounterId;

    private String assessmentName;

    private String type;

    private String childId;

    private String motherId;

    private String encounterReference;

    private List<String> diagnosisType;

    private String patientVisitId;
}
