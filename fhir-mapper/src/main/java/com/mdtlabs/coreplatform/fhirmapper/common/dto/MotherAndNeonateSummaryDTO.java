package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This is DTO which is to get both mother and neonate Summary information.
 */
@Data
public class MotherAndNeonateSummaryDTO {

    private GeneralMedicalReviewSummaryDTO motherDTO;

    private GeneralMedicalReviewSummaryDTO neonateDTO;

}
