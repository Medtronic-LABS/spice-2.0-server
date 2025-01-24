package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

@Data
public class PatientValidationResponseDTO {

    private boolean showAlert;

    private String message;

    private BioDataDTO patientDetails = null;
}
