package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for MotherNeonate Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on April 24, 2024.
 */
@Data
public class MotherNeonateDTO {

    private MotherDTO motherDTO;

    private HouseholdMemberDTO child;

    private NeonateDTO neonateDTO;

}
