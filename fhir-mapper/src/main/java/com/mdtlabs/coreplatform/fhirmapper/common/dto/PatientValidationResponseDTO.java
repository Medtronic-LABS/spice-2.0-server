package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PatientValidationResponseDTO {

    private boolean showAlert;
    
    private String message;

    private BioDataDTO patientDetails = null;
}
