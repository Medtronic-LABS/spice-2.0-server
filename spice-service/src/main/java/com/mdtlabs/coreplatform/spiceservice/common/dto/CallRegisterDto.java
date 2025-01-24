package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for follow up information.
 * </p>
 *
 * @author Nandhakumar Karthikeyan Created on Aug 12, 2024.
 */
@Data
public class CallRegisterDto {

    private String patientId;

    private String patientStatus;

    private boolean isCompleted;

    private Long createdBy;

    private Long updatedBy;

    private String encounterType;

    private String type;

    private String chwId;

    private long attempts;

    private long visits;

    private String villageId;

    private Long id;

    private String name;

    private String phoneNumber;

    private String callType;

}
