package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * <p>
 * This is a DTO class for a Household member sequence
 * </p>
 *
 * @author Praveen created on Aprils 16, 2024
 */
@Data
public class HouseholdMemberSequenceDTO implements Serializable {

    private String chiefdomCode;

    private String villageCode;

    private String userId;

    private Long sequence;

}
