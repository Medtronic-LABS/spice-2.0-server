package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * This is a DTO class for Household reference entity.
 * </p>
 *
 * @author Praveen created on March 20, 2024
 */
@Data
public class HouseholdReferenceDTO {

    private Map<String, List<String>> householdMembers;
    private Map<String, String> location;
    private Map<String, String> device;

}