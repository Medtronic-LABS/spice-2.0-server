package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class ComplianceDTO {

	private Long id;

	private String name;

	private String otherCompliance;
}
