package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

import java.util.List;

@Data
public class DistrictRequestDTO {
	
    private Long id;

    private String name;
	
    private Long countryId;

	private Long tenantId;

    private List<UserRequestDTO> users;

    private Boolean isActive;

    private String status;

    private String reason;
}
