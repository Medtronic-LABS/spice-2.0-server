package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class handles the common requests
 * 
 * @author Karthick M created on Feb 07, 2023
 */
@Data
public class CommonRequestDTO {

    private Long id;

    private Long patientTrackId;

    private Long countryId;

    private List<String> roleNames;
	
    private String searchTerm;

    private Long patientVisitId;

    private Long tenantId;

    private String menuName;

    public CommonRequestDTO(Long patientTrackId) {
        this.patientTrackId = patientTrackId;
    }

    public CommonRequestDTO() {
    }
}
