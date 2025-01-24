package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

/**
 * <p>
 * This class is an common DTO used for customization.
 * </p>
 * 
 * @author Karthick M created on Feb 08, 2023
 */
@Data
public class CustomizationDTO {

	private String type;

    private String category;

    private String formInput;

    private Long countryId;
    
    private long tenantId;
}
