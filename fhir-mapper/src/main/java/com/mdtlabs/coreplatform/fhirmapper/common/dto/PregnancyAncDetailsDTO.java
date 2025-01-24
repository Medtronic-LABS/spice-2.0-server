package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;

/**
 * <p>
 * This is a DTO class for Pregnancy Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
public class PregnancyAncDetailsDTO {

    private Date lastMenstrualPeriod;

    private Double height;

    private Double pulse;

    private Date estimatedDeliveryDate;

    private Integer gestationalAge;

    private Integer noOfFetus;

    private Integer gravida;

    private Integer parity;

    private String patientBloodGroup;

    private Double bmi;

    private Double systolic;

    private Double diastolic;

    private Double weight;

}
