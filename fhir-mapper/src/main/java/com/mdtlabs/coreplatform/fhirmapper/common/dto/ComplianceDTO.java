package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;
import org.springframework.validation.annotation.Validated;

/**
 * This DTO class for Compliance.
 *
 * @author Gopinath R created on Aug 21, 2024
 */
@Data
@Validated
public class ComplianceDTO {

	private Long id;

	private String name;

	private String otherCompliance;

	private String value;
}
