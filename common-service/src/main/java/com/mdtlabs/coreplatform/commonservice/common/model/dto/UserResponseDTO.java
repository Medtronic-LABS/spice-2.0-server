package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import lombok.Data;
import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;

/**
 * This a DTO class for User entity for response.
 * 
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserResponseDTO {
    
    private Long id;

    private String firstName;

    private Set<Role> roles = new HashSet<>();

    private String middleName;

    private String lastName;

    private String gender;

    private String phoneNumber;

    private String username;

    private String countryCode;

    private Country country;

    private Set<Organization> organizations;

	private Long tenantId;

    private String fhirId;

	private Set<String> suiteAccess;

    private UserResponseDTO supervisor;

    private List<VillageDTO> villages;

    private Set<Long> healthFacilityIds;

	private CultureDTO culture;

    private Timezone timezone;

    private boolean isRedRisk;

    private String defaultRoleName;

    private List<OrganizationDTO> insightUserOrganization;

    private List<Organization> reportUserOrganization;

    private Integer insightId;

    private Boolean isTermsAndConditionsAccepted;
    
    private Designation designation;

    public List<RoleResponseDTO> getRoles() {
        ModelMapper mapper = new ModelMapper();
        if (Objects.isNull(roles)) {
            return new ArrayList<>();
        }
		return roles.stream().map(role -> mapper.map(role, RoleResponseDTO.class)).toList();
	}

    public List<OrganizationDTO> getOrganizations() {
        ModelMapper mapper = new ModelMapper();
        if (Objects.isNull(this.organizations)) {
            return new ArrayList<>();
        }
        return organizations.stream().map(org ->mapper.map(org, OrganizationDTO.class)).toList();
    }

    public CountryDTO getCountry() {
        if (!Objects.isNull(this.country)) {
            return new ModelMapper().map(this.country, CountryDTO.class);
        }
        return null;
    }

    public TimezoneDTO getTimezone() {
		ModelMapper modelMapper = new ModelMapper();
		if (Objects.isNull(timezone)) {
            return null;
        }
		return modelMapper.map(timezone, TimezoneDTO.class);
	}

    public boolean isRedRisk() {
        isRedRisk = roles.stream().anyMatch(role -> role.getName().equals(Constants.ROLE_RED_RISK_USER));
        return isRedRisk;
    }

    public String getDefaultRoleName() {
        List<String> roleNames = roles.stream().map(Role::getName).toList();
        defaultRoleName = roleNames.stream().filter(Constants.SPICE_WEB_ROLES.values()::contains).findFirst().orElse(null);
        defaultRoleName = Objects.isNull(defaultRoleName)
                ? roleNames.stream().filter(Constants.SPICE_MOBILE_ROLES.values()::contains).findFirst().orElse(null)
                : defaultRoleName;
        defaultRoleName = Objects.isNull(defaultRoleName)
                ? roleNames.stream().filter(Constants.SPICE_CFR_ROLES.values()::contains).findFirst().orElse(null)
                : defaultRoleName;
        return defaultRoleName;
    }
    
    public List<OrganizationDTO> getInsightUserOrganization() {
        ModelMapper mapper = new ModelMapper();
        if (Objects.isNull(this.insightUserOrganization)) {
            return new ArrayList<>();
        }
        return insightUserOrganization.stream().map(org ->mapper.map(org, OrganizationDTO.class)).toList();
    }

    public List<OrganizationDTO> getReportUserOrganization() {
        ModelMapper mapper = new ModelMapper();
        if (Objects.isNull(this.reportUserOrganization)) {
            return new ArrayList<>();
        }
        return reportUserOrganization.stream().map(org ->mapper.map(org, OrganizationDTO.class)).toList();
    }

    public CommonResponseDTO getDesignation() {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(designation)) {
            return new CommonResponseDTO();
        }
        return modelMapper.map(designation, CommonResponseDTO.class);
    }

}
