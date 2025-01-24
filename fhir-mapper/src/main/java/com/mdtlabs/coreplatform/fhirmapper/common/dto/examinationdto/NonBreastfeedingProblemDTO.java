package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for nonBreastfeedingProblem ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class NonBreastfeedingProblemDTO {

    private Boolean inappropriateReplacementFeeds;

    private Boolean insufficientReplacementFeeds;

    private Boolean incorrectlyPreparedMilk;

    private Boolean useOfFeedingBottle;

    private Boolean feedFormHIVPositiveMother;

    private Boolean bottleFeeding;

    private Boolean lowWeightForAge;

    private String thrush;

}
