package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

/**
 * This is a DTO class for request handling.
 *
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class DataRequestDTO {

    private Long countryId;

    private Long districtId;

    private Long healthFacilityId;

    private Long chiefdomId;

    private String searchTerm;
}
