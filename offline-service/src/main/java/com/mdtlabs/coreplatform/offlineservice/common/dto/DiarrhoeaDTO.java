package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.List;

/**
 * <p>
 * This is a DTO class for Diarrhoea.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class DiarrhoeaDTO {

    private Boolean hasDiarrhoea;

    private Boolean isBloodyDiarrhoea;

    private List<String> diarrhoeaSigns;

    private String otherSigns;

    private String zincDispensedStatus;

    private String orsDispensedStatus;

    private String jellyWaterDispensedStatus;

    private String sssDispensedStatus;

    private Long numberOfDaysDiarrhoea;

}