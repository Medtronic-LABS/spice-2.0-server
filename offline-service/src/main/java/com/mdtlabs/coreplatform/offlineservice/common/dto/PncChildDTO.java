package com.mdtlabs.coreplatform.offlineservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * This DTO class for PNC child observation.
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class PncChildDTO {

    private Boolean exclusivelyBreastfeeding;

    private Boolean fatherPresent;

    private String muac;

    private Boolean sleepsUnderBedNet;

    private Boolean takingMinimumMealsPerDay;

    private Boolean fedFrom4FoodGroups;

    private Boolean motherOrPartnerUsingFamilyPlanning;

    private Boolean deathOfBaby;

    private List<String> pncChildSigns;

    private String otherSigns;

    private Date plannedVisitDate;

    private Date actualVisitDate;

    private Boolean pentaOpvGiven;

    private Boolean measles1Given;

    private Boolean yellowFeverVacineGiven;

    private Boolean measles2Given;

    private Boolean postReferralFollowUpDone;

}
