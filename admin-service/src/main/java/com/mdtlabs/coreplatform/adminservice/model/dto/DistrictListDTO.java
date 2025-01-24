package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

@Data
public class DistrictListDTO {
    public DistrictListDTO(long id, String name, Long tenantId) {
        super();
        this.id = id;
        this.name = name;
        this.tenantId = tenantId;
    }

    public DistrictListDTO() {
        super();
    }

    private long id;
    private String name;
    private int chiefdomCount;
    private int healthFacilityCount;
    private Long tenantId;
}
