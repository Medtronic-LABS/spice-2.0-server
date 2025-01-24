package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for BirthHistoryDTO.
 *
 * @author Yogeshwran M created on May 23, 2024
 */
@Data
public class BirthHistoryDTO {

    private Double birthWeight;

    private Integer gestationalAge;

    private String type;

    private String birthWeightCategory;

    private String gestationalAgeCategory;

    private Boolean haveBreathingProblem;
}
