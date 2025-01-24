package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for GeneralDangerSigns Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Mar 05, 2024.
 */
@Data
public class GeneralDangerSignsDTO {

    private Boolean isUnusualSleepy;

    private Boolean isConvulsionPastFewDays;

    private Boolean isVomiting;

    private Boolean isBreastfeed;

}