package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Set;

/**
 * This class is a Data Transfer object for current medication Medical review request.
 * 
 * @author Karthick M
 *
 */
@Data
public class CurrentMedicationDetailsDTO {
    
    private Set<MedicalReviewMetaDTO> medications;

    private boolean isDrugAllergies;

    private boolean isAdheringCurrentMed;
    
    private String adheringMedComment;

    private String allergiesComment;
}
