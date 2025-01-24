package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.Objects;

import lombok.Data;
import org.hl7.fhir.r4.model.ResourceType;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

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

    private String referredSiteId;

    private String referredClinicianId;

    private String patientReference;

    private Date dateOfDelivery;

    private String patientId;

    private String memberId;

    private String category;

    private boolean referred;

    private boolean autoReferral;

    private ProvenanceDTO provenance;

    private String patientStatus;

    private String currentPatientStatus;

    private String encounterType;

    private String assessmentName;

    public String getPatientReference() {
        if (Objects.nonNull(this.patientReference) && !this.patientReference.contains(ResourceType.Patient.toString())) {
            this.patientReference = ResourceType.Patient + Constants.FORWARD_SLASH + this.patientReference;
        }
        return this.patientReference;
    }

    public String getEncounterId() {
        if (Objects.nonNull(this.encounterId) && !this.encounterId.contains(ResourceType.Encounter.toString())) {
            this.encounterId = ResourceType.Encounter + Constants.FORWARD_SLASH + this.encounterId;
        }
        return this.encounterId;
    }

}
