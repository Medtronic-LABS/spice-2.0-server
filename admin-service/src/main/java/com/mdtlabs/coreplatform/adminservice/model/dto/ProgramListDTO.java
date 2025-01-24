package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

import java.util.Date;

/**
 * This is a DTO class for program list entity.
 * 
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class ProgramListDTO {
    private Long id;
    
    private String name;
    
    private Long tenantId;
    
    private boolean isActive;
    
    private Date createdAt;
}
