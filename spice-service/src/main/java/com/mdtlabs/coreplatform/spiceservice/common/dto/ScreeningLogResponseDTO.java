package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;


/**
 * Response DTO for screening log.
 *
 */
@Data
public class ScreeningLogResponseDTO {

    private String patientFhirId;

    private String patientStatus;

    private String relatedPersonFhirId;

    private String relatedPersonStatus;

    private BioDataDTO bioDataDTO;

}