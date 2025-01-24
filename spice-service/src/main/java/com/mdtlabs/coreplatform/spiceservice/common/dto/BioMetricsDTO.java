package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * This DTO class for Bio metrics details.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class BioMetricsDTO {

    private String gender;

    private Integer age;

    private Date dateOfBirth;

    private Double height;

    private Double weight;

    private Double bmi;

    private String bmiCategory;

    private Boolean isPregnant;

    private Boolean isRegularSmoker;

    private Boolean isphysicallyActive;

    private boolean isFamilyDiabetesHistory;

    private boolean isBeforeGestationalDiabetes;

}
