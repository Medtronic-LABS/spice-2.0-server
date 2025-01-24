package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Cough.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class CoughDTO {

    private Boolean hasCough;

    private String amoxicillin;

    private Boolean chestInDrawing;

    private Long noOfDaysOfCough;

    private Long breathPerMinute;

}
