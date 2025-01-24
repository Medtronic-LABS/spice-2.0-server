package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for FHIR Code.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class FhirCodeDTO {

    private String name;

    private String system;

    private String code;

    private String display;

    public FhirCodeDTO(String system, String code, String display) {

        this.system = system;
        this.code = code;
        this.display = display;

    }
}
