package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;

/**
 * <p>
 * This is a DTO class for pregnancy information.
 * </p>
 *
 * @author Maria Antony Created on May 07, 2024.
 */
@Data
public class PregnancyInfo {

    private String householdMemberId;

    private Integer ancVisitNo;

    private Double weight;

    private Boolean isChildHaveBreathingIssues;

    private Date lastMenstrualPeriod;

    private Date estimatedDeliveryDate;

    private Integer pncVisitNo;

    private Date pncCreatedDate;

    private Date dateOfDelivery;

    private Boolean isDeliveryAtHome;

    private Integer noOfNeonates;

    private String neonatePatientId;

    private Integer childVisitNo;

    private Integer gestationalAge;

    private Integer ancMedicalReviewVisitNo;

    private Integer pncMotherMedicalReviewVisitNo;

    private Integer pncChildMedicalReviewVisitNo;

    private String neonateOutcome;

    private Boolean isNeonateDeathRecordedByPHU;

}