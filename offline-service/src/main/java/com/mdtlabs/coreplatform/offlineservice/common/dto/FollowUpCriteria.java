package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * <p>
 * This is a DTO class for follow up criteria.
 * </p>
 *
 * @author Maria Antony Created on May 16, 2024.
 */
@Data
public class FollowUpCriteria implements Serializable {

    private int malaria;

    private int pneumonia;

    private int diarrhea;

    private int muac;

    private int escalation;

    private int referral;

    private int ancVisit;

    private int pncVisit;

    private int childVisit;

    private int successfulAttempts;

    private int unsuccessfulAttempts;

    private int followupAttempts;

    private int screeningFollowupRemainingDays;

    private int assessmentFollowupRemainingDays;

    private int medicalReviewFollowupRemainingDays;

    private int lostToFollowupRemainingDays;

}
