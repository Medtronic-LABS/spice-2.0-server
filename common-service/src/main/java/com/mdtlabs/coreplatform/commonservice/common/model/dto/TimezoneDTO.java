package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

@Data
public class TimezoneDTO {
    
    private long id;

	private String offset;
	
    private String description;
}
