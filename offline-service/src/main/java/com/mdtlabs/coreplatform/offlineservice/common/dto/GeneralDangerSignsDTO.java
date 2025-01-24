package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for GeneralDangerSigns Details.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class GeneralDangerSignsDTO {

    private Boolean isUnusualSleepy;

    private Boolean isConvulsionPastFewDays;

    private Boolean isVomiting;

    private Boolean isBreastfeed;

}