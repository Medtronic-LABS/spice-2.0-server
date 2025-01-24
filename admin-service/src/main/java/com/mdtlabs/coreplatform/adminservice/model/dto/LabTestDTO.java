package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

/**
 * This is a DTO class for LabTest entity.
 *
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class LabTestDTO {

    private Long id;

    private String name;

    private String unit;

    private String genderValue;

    private Integer minValue;

    private Integer maxValue;

    private Long tenantId;

    private Long countryId;
}
