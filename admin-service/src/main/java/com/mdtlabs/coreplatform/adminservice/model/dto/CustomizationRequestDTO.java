package com.mdtlabs.coreplatform.adminservice.model.dto;


import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * <p>
 * This class is an common DTO used for customization requests.
 * </p>
 * 
 * @author Karthick M created on Feb 08, 2023
 */
@Data
public class CustomizationRequestDTO {

    @NotNull
    private Long countryId;

    private String type;

    private String category;

    private Long clinicalWorkflowId;

	private Long districtId;
	
	private Long tenantId;

    private Long cultureId;
}
