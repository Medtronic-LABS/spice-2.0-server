package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Cough.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class CoughDTO {

    private Boolean hasCough;

    private String amoxicillin;

    private Boolean chestInDrawing;

    private Long noOfDaysOfCough;

    private Long breathPerMinute;

}
