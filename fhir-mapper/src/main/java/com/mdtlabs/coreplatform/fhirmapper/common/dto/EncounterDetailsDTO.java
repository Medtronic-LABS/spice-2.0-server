package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Encounter Details entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class EncounterDetailsDTO {

    private String id;

    private String patientVisitId;

    private String patientReference;

    private String type;

    private boolean referred;

    private String patientId;

    private String patientStatus;

    private String memberId;

    private Date startTime;

    private Date endTime;

    private String householdId;

    private String diagnosisType;

    private Long followUpId;

    private ProvenanceDTO provenance;

    private Double latitude;

    private Double longitude;

    private int visitNumber;

    private boolean isPrescribed;

    private boolean isDispensed;

    private String villageId;

    private String signature;

    private boolean isLabTest;

    private String visitId;
}
