package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

/**
 * This is DTO class for organization entity.
 */
@Data
public class OrganizationDTO {

    private Long id;

	private Long formDataId;

	private String name;

	private Long sequence;

	private Long parentOrganizationId;

	private String formName;
}
