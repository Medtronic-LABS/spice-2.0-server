package com.mdtlabs.coreplatform.spiceservice.common.dto;

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

    private String System;

    private String code;

    private String display;

    public FhirCodeDTO(String system, String code, String display) {

        this.System = system;
        this.code = code;
        this.display = display;

    }
}
