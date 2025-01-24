package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

/**
 * This is a DTO class for Response handling.
 *
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class RegionDataDTO {

    private Long villageId;
    private String villageName;
    private String villageType;

    private Long countryId;
    private String countryName;

    private String districtName;
    private Long districtId;

    private String chiefdomName;
    private Long chiefdomId;

}
