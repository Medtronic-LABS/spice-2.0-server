package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for PNC child observation.
 *
 * @author Nandhakumar K created on Apr 22, 2024
 */
@Data
public class PncMedicalReviewDTO {

    private PncChildMedicalReviewDTO pncChild;

    private PncMotherMedicalReviewDTO pncMother;

    private HouseholdMemberDTO child;
}
