package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;

/**
 * <p>
 * This is a DTO class for Labour Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on April 24, 2024.
 */
@Data
public class LabourDTO {

    private Date dateAndTimeOfDelivery;

    private Date dateAndTimeOfLabourOnset;

    private String deliveryType;

    private String deliveryBy;

    private String deliveryAt;

    private String deliveryAtOther;

    private String deliveryStatus;

    private Integer noOfNeoNates;

    private String neonatePatientId;

    private String deliveryByOther;
}
