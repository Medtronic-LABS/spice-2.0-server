package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;

/**
 * This DTO class for HouseholdMemberlnk.
 *
 * @author Denisha J created on Oct 22, 2024
 */
@Data
public class CallRegisterDetailDTO {

    private Date callDate;

    private int duration;

}
