package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Fever Details.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class FeverDTO {

    private Boolean hasFever;

    private String act;

    private String rdtTest;

    private Long noOfDaysOfFever;

    private Double temperature;

}
