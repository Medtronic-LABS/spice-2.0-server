package com.mdtlabs.coreplatform.offlineservice.common.dto;

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

    private String patientReference;

    private boolean referred;

    private String patientId;

    private String patientStatus;

    private String memberId;

    private Date startTime;

    private Date endTime;

    private String householdId;

    private ProvenanceDTO provenance;

    private Double latitude;

    private Double longitude;

    private int visitNumber;

}
