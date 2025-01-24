package com.mdtlabs.coreplatform.offlineservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Iccm Other Details.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
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
