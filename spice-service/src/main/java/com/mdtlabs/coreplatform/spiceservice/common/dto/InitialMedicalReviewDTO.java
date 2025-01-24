package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Set;

/**
 * This class is a Data Transfer object for Red risk calculation.
 * 
 * @author Karthick M
 *
 */
@Data
public class InitialMedicalReviewDTO {
    
    private PatientStatusDTO diagnosis;

    private CurrentMedicationDetailsDTO currentMedications;
    
    private Set<MedicalReviewMetaDTO> comorbidities;
    
    private Set<MedicalReviewMetaDTO> complications;

    private Set<MedicalReviewMetaDTO> lifestyle;

    private Boolean isPregnant;

}
