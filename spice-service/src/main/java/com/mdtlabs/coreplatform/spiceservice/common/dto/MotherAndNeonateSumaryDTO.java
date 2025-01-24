package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Mother And Neonate Summary.
 * </p>
 *
 * @author jagan Created on Jul 11, 2024.
 */
@Data
public class MotherAndNeonateSumaryDTO {

    private GeneralMedicalReviewSummaryDTO motherDTO;

    private GeneralMedicalReviewSummaryDTO neonateDTO;

}
