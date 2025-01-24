package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Prescription request Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
public class PrescriptionRequestDTO {

    private EncounterDetailsDTO encounter;

    private List<PrescriptionDTO> prescriptions;

    private String signature;

    private String prescriptionId;

    private String discontinuedReason;

    private ProvenanceDTO provenance;

}
