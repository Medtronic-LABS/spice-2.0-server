package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Diarrhoea.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class DiarrhoeaDTO {

    private Boolean hasDiarrhoea;

    private Boolean isBloodyDiarrhoea;

    private String otherSigns;

    private List<String> diarrhoeaSigns;

    private String zincDispensedStatus;

    private String orsDispensedStatus;

    private String jellyWaterDispensedStatus;

    private String sssDispensedStatus;

    private Long numberOfDaysDiarrhoea;

}