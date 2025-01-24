package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This class is a Data Transfer object for Patient observation details.
 * 
 * @author Karthick M
 *
 */
@Data
public class PatientObservationDetails {

    private Boolean isPregnant;

    private String gender;

    private Double glucoseValue;

    private String glucoseType;

    private Boolean isRegularSmoker;

    private Double avgDiastolic;

    private Double avgSystolic;

    private double bmi;

    private Integer age = 0;

    private Double height = 0d;

    private Double weight = 0d;

    private String riskLevel;

}
