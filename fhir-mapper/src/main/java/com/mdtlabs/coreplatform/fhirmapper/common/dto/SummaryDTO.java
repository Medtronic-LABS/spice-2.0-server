package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for IccmOtherDetails.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class SummaryDTO {

    private String notes;

    private Date nextVisitDate;

    private String referredSite;

    private String referredSiteId;

    private Boolean isTakenToClinic;

    private String malnutritionCondition;

    private String coughCondition;

    private String feverCondition;

    private String diarrheaCondition;

}
