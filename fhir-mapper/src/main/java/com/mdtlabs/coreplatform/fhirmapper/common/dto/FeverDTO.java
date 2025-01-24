package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Fever Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class FeverDTO {

    private Boolean hasFever;

    private String act;

    private String rdtTest;

    private Long noOfDaysOfFever;

    private Double temperature;

}
