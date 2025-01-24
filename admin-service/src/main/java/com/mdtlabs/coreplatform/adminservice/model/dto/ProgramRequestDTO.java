package com.mdtlabs.coreplatform.adminservice.model.dto;


import lombok.Data;

import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;

/**
 * This is a DTO class for program request entity.
 * 
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class ProgramRequestDTO {

    private Long id;

    private String name;

    private Long tenantId;

    private Country country;

    private Set<Long> healthFacilities;

    private Set<Long> deletedHealthFacilities;

    private Long regionTenantId;
    
    private boolean isActive;
}