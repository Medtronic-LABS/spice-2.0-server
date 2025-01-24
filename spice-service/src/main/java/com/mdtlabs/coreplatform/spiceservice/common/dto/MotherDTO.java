package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.List;

/**
 * <p>
 * This is a DTO class for Mother Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on April 24, 2024.
 */
@Data
public class MotherDTO {

    private String id;

    private List<String> signs;

    private String generalConditions;

    private List<String> riskFactors;

    private String stateOfPerineum;

    private String tear;

    private double ttDoseTaken;

    private List<String> status;

    private EncounterDetailsDTO encounter;

    private int visitNumber;

    private String patientReference;

    private String patientStatus;

    private LabourDTO labourDTO;

    private List<PrescriptionDTO> prescriptions;

    private List<LabTestDTO> investigations;

    private String neonateOutcome;
}
