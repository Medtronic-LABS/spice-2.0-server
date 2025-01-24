package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * This DTO class for Treatment plan.
 *
 * @author Karthick M created on Aug 13, 2024
 */
@Data
public class TreatmentPlanDTO {

    private String memberReference;

    private String patientReference;
 
    private ProvenanceDTO provenance;

    private List<FrequencyDTO> frequencies;

    private String cvdRiskLevel;

    private boolean isPregnancyAnc;

    private boolean isHba1c;

    private boolean isBGDefaultFrequency;

    private FrequencyDTO medicalReviewFrequency;

    private FrequencyDTO bpCheckFrequency;

    private FrequencyDTO bgCheckFrequency;

    private FrequencyDTO hba1cCheckFrequency;

    private FrequencyDTO choCheckFrequency;

    private String carePlanId;

    private Date nextMedicalReviewDate;

    private Date nextBpAssessmentDate;

    private Date nextBgAssessmentDate;
}


