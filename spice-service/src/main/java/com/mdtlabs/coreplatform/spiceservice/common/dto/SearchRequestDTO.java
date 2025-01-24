package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Set;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.PaginateDTO;

/**
 * This class is for search request DTO
 *
 * @author Nandhakumar Karthikeyan created on Aug 13, 2024
 */
@Data
public class SearchRequestDTO extends PaginateDTO {

    private Long id;

    private String name;

    private Long countryId;

    private Long healthFacilityId;

    private Long tenantId;

    private String userType;

    private boolean isUserBased;

    private boolean isAdmin;

    private String email;

    private Long healthFacilityTenantId;

    private Long chiefdomTenantId;

    private List<Long> tenantIds;

    private Set<Long> userIds;

    private Set<Long> villageIds;

    private Set<String> fhirIds;

    private List<Long> ids;

    private List<String> metaNames;

    private List<Long> workflowIds;

    private boolean isTenantBased;

    private String phoneNumber;

    private Long districtId;

    private Long userId;

    private String newPassword;

    private String oldPassword;

    private String username;

    private List<Long> healthFacilityLinkedVillages;

    private List<Long> linkedVillageIds;

    public SearchRequestDTO() {
    }

    public SearchRequestDTO(String name, Long countryId) {
        this.name = name;
        this.countryId = countryId;
    }
}
