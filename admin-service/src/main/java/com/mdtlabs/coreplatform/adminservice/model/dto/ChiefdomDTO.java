package com.mdtlabs.coreplatform.adminservice.model.dto;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import lombok.Data;

import java.util.List;

/**
 * This is a DTO class for Chiefdom entity.
 *
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class ChiefdomDTO {

    private Long id;

    private String name;

    private String code;

    private Long tenantId;

    private Long countryId;

    private String districtName;

    private Long healthFacilityCount;

    private List<UserResponseDTO> users;

    private Long districtId;

    private Long districtTenantId;

    public ChiefdomDTO() {}

    public ChiefdomDTO(Long id, String name, Long tenantId) {
        super();
        this.id = id;
        this.name = name;
        this.tenantId = tenantId;
    }
}
