package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This a request DTO for medical review Request details.
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

    private String villageId;

}
