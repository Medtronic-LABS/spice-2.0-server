package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * This DTO class for screening log details.
 *
 * @author Gopinath created on Aug 21, 2024
 */
@Data
public class ScreeningLog {
    
    private String firstName;

    private String lastName;

    private String middleName;

    private String riskLevel;

    private String riskMessage;

    private boolean isRedRisk;

    private boolean isInitialReview;

    private Boolean IsPregnant;

    private String isPregnancyRisk;

    private String patientStatus;
}
