package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * This is a DTO class for HouseholdMemberMap DTO.
 * </p>
 *
 * @author Praveen Created on July 22, 2024.
 */
@Data
public class HouseholdMemberMapDTO implements Serializable {

    private String householdId;

    private List<HouseholdMemberDTO> householdMembers = new ArrayList<>();
}
