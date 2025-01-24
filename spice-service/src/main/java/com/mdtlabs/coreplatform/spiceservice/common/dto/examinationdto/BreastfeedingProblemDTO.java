package com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for BreastfeedingProblem ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class BreastfeedingProblemDTO {

    private Boolean anyBreastfeedingDifficulty;

    private Boolean lessThan8BreastfeedIn24hrs;

    private Boolean switchingBreastFrequently;

    private Boolean notIncreasingBFInIllness;

    private Boolean receivesOtherFoodsOrDrinks;

    private Boolean mouthUlcersOrThrush;

    private Boolean underweight;

    private String positioning;

    private String attachment;

    private String suckling;

    private String noFeedingProblem;

}
