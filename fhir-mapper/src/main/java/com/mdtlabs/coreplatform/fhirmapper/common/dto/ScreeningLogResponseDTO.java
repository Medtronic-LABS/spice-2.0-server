package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * Response DTO for screening log.
 *
 */
@Data
public class ScreeningLogResponseDTO {

    private Map<String, List<String>> resourceIds;

    private String patientFhirId;

    private String patientStatus;

    private String relatedPersonFhirId;

    private String relatedPersonStatus;

    private BioDataDTO bioDataDTO;

}