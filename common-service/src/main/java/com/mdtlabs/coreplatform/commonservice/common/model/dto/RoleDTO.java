package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import jakarta.persistence.Transient;
import lombok.Data;

import java.util.List;

@Data
public class RoleDTO {

    private Long id;

    private String name;
	
    private Long level;

    private String suiteAccessName;

    private List<String> appTypes;

    @Transient
    private String authority;
    
}
