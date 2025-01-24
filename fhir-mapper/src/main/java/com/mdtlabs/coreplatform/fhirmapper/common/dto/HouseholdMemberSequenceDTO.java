package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for a Household member sequence
 * </p>
 *
 * @author Praveen created on Aprils 16, 2024
 */
@Data
public class HouseholdMemberSequenceDTO {

    private String cheifdomCode;
    private String villageCode;
    private String userId;
    private Long sequence;

}
