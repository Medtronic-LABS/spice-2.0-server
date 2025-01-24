package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;

@Data
public class HealthFacilityRequestDTO {

    private Long id;
    
    private String name;

    private String type;

    private String phuFocalPersonName;

    private String phuFocalPersonNumber;
    
    private String address;

    private District district;

    private Chiefdom chiefdom;

    private String cityName;

    private String latitude;

    private String longitude;

    private String postalCode;

    private String language;

    private List<Long> linkedVillageIds;

    private List<Long> linkedSupervisorIds;

    private List<UserRequestDTO> users;

    private Long parentTenantId;

    private Long tenantId;

    private String fhirId;

    private Country country;

    private List<Long> clinicalWorkflowIds;

    private List<Long> customizedWorkflowIds;

    private Timezone timezone;

    private Long communityId;

    private Culture cultureId;

    private List<String> appTypes;

}
