package com.mdtlabs.coreplatform.offlineservice.common.dto;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import lombok.Data;

import java.util.List;

/**
 * This is a DTO class for request handling.
 *
 * @author Praveen Created on 26 Sep 2024
 */
@Data
public class HealthFacilityDTO {

    private Long id;

    private String name;

    private List<VillageDTO> linkedVillages;

    private Long tenantId;

    private String fhirId;

}
