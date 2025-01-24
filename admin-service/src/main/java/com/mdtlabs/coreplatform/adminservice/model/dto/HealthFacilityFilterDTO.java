package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;


/**
 * This is a DTO class for request handling.
 * 
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class HealthFacilityFilterDTO {

    private Long id;
    
    private String name;

    private Long tenantId;

}
