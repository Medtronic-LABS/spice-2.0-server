package com.mdtlabs.coreplatform.offlineservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * This DTO class for ANC observation.
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class AncDTO {

    private Date lastMenstrualPeriod;

    private Date estimatedDeliveryDate;

    private Boolean isMalePartnerPresent;

    private Boolean sleepsUnderBedNet;

    private Boolean eatsMoreThanBefore;

    private Boolean takesIronFloatTablets;

    private Boolean takesFancidarTablets;

    private Boolean priorityPregnancy;

    private Boolean miscarriage;

    private Boolean birthPlanMade;

    private String placeOfDelivery;

    private String otherPlaceOfDelivery;

    private List<String> ancSigns;

    private String otherSigns;

    private String gestationalAge;

    private Boolean eats4GroupIronVitARichFoods;

    private Date nextVisitDate;

    private Boolean deathOfMother;

    private long visitNo;

}
