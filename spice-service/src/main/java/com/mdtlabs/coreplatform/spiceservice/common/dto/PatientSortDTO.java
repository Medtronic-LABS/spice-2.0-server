package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 *  This is a DTO class for Patient Sort Details.
 *  This class contains various boolean fields that indicate different sorting criteria
 *  for patient data, such as red risk status, latest assessment, medical review due date,
 *  high or low blood pressure, high or low blood glucose, and assessment due date.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Sept 30, 2024
 */
@Data
public class PatientSortDTO {

    private Boolean isRedRisk;

    private Boolean isLatestAssessment;

    private Boolean isMedicalReviewDueDate;

    private Boolean isHighLowBp;

    private Boolean isHighLowBg;

    private Boolean isAssessmentDueDate;

    private Boolean isScreeningDueDate;
}
