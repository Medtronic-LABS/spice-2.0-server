package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;
import java.util.Set;

import lombok.Data;

@Data
public class SearchRequestDTO extends PaginateDTO {

	private Long id;

	private String name;

	private Long countryId;

	private String countryCode;

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

	private Long count;

	private List<String> appTypes;

	public SearchRequestDTO() {}

	public SearchRequestDTO(Long id) {
		this.id = id;
	}

	public SearchRequestDTO(Long id, Long count) {
        this.id = id;
		this.count = count;
    }

	public SearchRequestDTO(String name, Long countryId) {
		this.name = name;
		this.countryId = countryId;
	}



	private String roleType;

	private List<String> roleNames;

	private Boolean isSiteUsers;

	private Long parentRegionId;

    private boolean isNonNcdWorkflowEnabled;

	private Long cultureId;

	private String formName;

	private Long parentOrganizationId;

	private Long ignoreTenantId;

	private String roleName;

	private Boolean isTermsAndConditionsAccepted;
}
