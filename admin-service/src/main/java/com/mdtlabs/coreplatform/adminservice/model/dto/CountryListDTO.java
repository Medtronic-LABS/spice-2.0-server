package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * This is an DTO class for Country entity.
 * 
 * @author Divya S
 *
 */
@Data
public class CountryListDTO {
	private long id;
	private String name;
	private int districtCount;
	private int chiefdomCount;
	private int healthFacilityCount;
	private List<String> appTypes;
	private Long tenantId;
	private Map<String, Object> displayValues;
}
