package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * This is DTO class to get organization.
 */
@Data
@AllArgsConstructor
public class OrganizationDetailsDTO {

    private Long userId;

    private String formName;

    private Long chiefdomDistrictId;

    private String chiefdomDistrictName;

    private Long chiefdomDistrictTenantId;

    private Long chiefdomDistrictParentOrgId;

    private Long healthFacilityDistrictId;

    private String healthFacilityDistrictName;

    private Long healthFacilityDistrictTenantId;

    private Long healthFacilityDistrictParentOrgId;

    private Long healthFacilityChiefdomId;

    private String healthFacilityChiefdomName;

    private Long healthFacilityChiefdomTenantId;

    private Long healthFacilityChiefdomParentOrgId;

}
