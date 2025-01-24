package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Referral Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Mar 20, 2024.
 */
@Data
public class ReferralDetailsDTO {

    private String encounterId;

    private String type;

    private Date nextVisitDate;

    private String referredReason;

    private String assessmentName;

    private String referredSiteId;

    private String referredClinicianId;

    private String patientReference;

    private String patientId;

    private String householdId;

    private String memberId;

    private String category;

    private boolean referred;

    private boolean autoReferral;

    private ProvenanceDTO provenance;

    private String patientStatus;

    private String currentPatientStatus;

    private String villageId;

}
