package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;


/**
 * This DTO class for patient visit.
 *
 * @author Karthick M created on Aug 16, 2024
 */
@Data
public class PatientVisitDTO {
    
    private String patientReference;

    private ProvenanceDTO provenance;

    private Double latitude;

    private Double longitude;

	private String memberReference;

    private boolean isInitialReviewed;
}
