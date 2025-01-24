package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * This is a DTO class for village entity.
 * 
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
@NoArgsConstructor
public class VillageDTO {

    private Long id;

    private String name;

    private String villagecode;

    private Long chiefdomId;

    private Long userId;

    private Long countryId;

    private Long districtId;

    private String chiefdomCode;

    private String districtCode;

    private long memberSequence;

    private long householdSequence;

    private String code;

    public VillageDTO(Long id, String name, Long userId) {
        this.name = name;
        this.id = id;
        this.userId = userId;
    }

}
