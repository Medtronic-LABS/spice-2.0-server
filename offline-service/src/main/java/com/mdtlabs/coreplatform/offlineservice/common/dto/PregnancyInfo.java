package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * This is a DTO class for pregnancy information.
 * </p>
 *
 * @author Maria Antony Created on May 07, 2024.
 */
@Data
public class PregnancyInfo implements Serializable {

    private String householdMemberId;

    private Integer ancVisitNo;

    private Date lastMenstrualPeriod;

    private Date estimatedDeliveryDate;

    private Integer pncVisitNo;

    private Date dateOfDelivery;

    private Boolean isDeliveryAtHome;

    private Integer noOfNeonates;

    private String neonatePatientId;

    private Integer childVisitNo;

    private Boolean isNeonateDeathRecordedByPHU;

}