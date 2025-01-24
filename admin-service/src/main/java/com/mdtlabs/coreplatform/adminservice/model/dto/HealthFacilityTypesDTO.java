package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

import java.util.List;

/**
 * This is a DTO class for request handling.
 *
 * @author Premkalyan
 * @since Dec 16, 2024
 */
@Data
public class HealthFacilityTypesDTO {

    private Long id;
    
    private String name;

    private List<String> appTypes;

}
