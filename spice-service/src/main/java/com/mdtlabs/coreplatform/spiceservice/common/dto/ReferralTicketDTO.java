package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for ReferralTicketDTO Details entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Mar 11, 2024.
 */
@Data
public class ReferralTicketDTO {

    private String id;

    private String referredBy;

    private String phoneNumber;

    private String referredTo;

    private String patientStatus;

    private String referredReason;

    private Date dateOfOnset;

    private Date referredDate;

    private List<Map<String, Object>> referredDates;

}
