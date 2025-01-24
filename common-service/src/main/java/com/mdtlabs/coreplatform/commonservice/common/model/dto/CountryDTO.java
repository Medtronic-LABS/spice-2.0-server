package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class CountryDTO {

    private Long id;
	
	private String name;

	private String phoneNumberCode;

	private String unitMeasurement;

    private String regionCode;

    private List<String> appTypes;
    
    private Long tenantId;

    private Map<String, Object> displayValues;

}
