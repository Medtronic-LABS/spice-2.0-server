package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;

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

    private String householdId;

    private String patientId;

    private String villageId;

    private String encounterType;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private String cost;

    private Date nextVisitDate;

    private String patientStatus;

    private String patientReference;

    private AppointmentType type;

    private String reason;

    private String category;

    private ProvenanceDTO provenance;

    private ReferralDetailsDTO referralDetails;

}
