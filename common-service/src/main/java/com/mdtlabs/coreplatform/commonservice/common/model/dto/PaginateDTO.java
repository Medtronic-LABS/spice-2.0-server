package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

import java.util.Map;

@Data
public class PaginateDTO {

	private String searchTerm;

	private int limit;

	private int skip;

	private Map<String, String> sort;
}
