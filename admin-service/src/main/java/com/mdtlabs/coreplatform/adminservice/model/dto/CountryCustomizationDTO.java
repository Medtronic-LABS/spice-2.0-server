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
public class CountryCustomizationDTO {

    private String type;

    private String category;

    private String formInput;

    private Long cultureId;

    private Long countryId;

    private boolean isDefault;

    private Long id;

    private Long tenantId;
}
