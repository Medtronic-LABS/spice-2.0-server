package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.List;

/**
 * <p>
 * This is a DTO class for Fhir response entity.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
public class FhirResponseDTO {

    private String id;

    private String resourceType;

    private List<Object> entry;

}
