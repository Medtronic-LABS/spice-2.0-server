package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for APGAR Score.
 *
 * @author Nanthinee S created on May 28, 2024
 */
@Data
public class APGARScoreDTO {

    private int activity;

    private int pulse;

    private int grimace;

    private int appearance;

    private int respiration;

    private int oneMinuteTotalScore;

    private int fiveMinuteTotalScore;

    private int tenMinuteTotalScore;

}
