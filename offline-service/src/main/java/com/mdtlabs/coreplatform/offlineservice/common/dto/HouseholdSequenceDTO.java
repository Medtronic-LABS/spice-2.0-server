package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * <p>
 * This is a DTO class for a Household sequence
 * </p>
 *
 * @author Praveen created on Aprils 16, 2024
 */
@Data
public class HouseholdSequenceDTO implements Serializable {

    private String villageId;

    private Long sequence;

}
