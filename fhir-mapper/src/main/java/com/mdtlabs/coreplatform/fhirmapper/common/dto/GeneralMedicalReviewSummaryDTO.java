package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for GeneralMedicalReviewSummary Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Mar 20, 2024.
 */
@Data
public class GeneralMedicalReviewSummaryDTO {

    private String id;

    private List<String> medicalSupplies;

    private String memberId;

    private String encounterType;

    private String cost;

    private String category;

    private Date nextVisitDate;

    private String patientStatus;

    private String patientReference;

    private ReferralDetailsDTO referralDetails;

    private ProvenanceDTO provenance;

    public EncounterDetailsDTO getEncounter() {
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setId(this.id);
        encounterDetailsDTO.setProvenance(this.provenance);
        encounterDetailsDTO.setMemberId(this.memberId);
        encounterDetailsDTO.setPatientReference(this.patientReference);
        return encounterDetailsDTO;
    }

}
