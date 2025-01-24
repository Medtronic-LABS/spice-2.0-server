package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * This DTO class for frequency response details.
 *
 * @author Karthick M created on Aug 13, 2024
 */
@Data
public class TreatmentPlanResponseDTO {

    private String carePlanId;

    private String medicalReviewFrequency;

    private String bpCheckFrequency;

    private String bgCheckFrequency;

    private String hba1cCheckFrequency;

    private String choCheckFrequency;

    private Date nextMedicalReviewDate;

    private Date nextBpAssessmentDate;

    private Date nextBgAssessmentDate;
}
