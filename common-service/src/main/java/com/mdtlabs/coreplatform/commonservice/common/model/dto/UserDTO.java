package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import lombok.Data;
import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;

@Data
public class UserDTO {
    private Long id;
	
	private String username;
	
	private Set<Role> roles = new HashSet<>();

	private boolean active; 
	
	private Date blockedDate;

	private String authorization;

	private long currentDate;

	private String firstName;

	private String lastName;

	private String phoneNumber;

	private Country country;

	private Timezone timezone;

	private Set<Organization> organizations = new HashSet<>();

	private Set<Organization> reportUserOrganization = new HashSet<> ();

	private Set<Organization> insightUserOrganization = new HashSet<> ();

    private String countryCode;

    private Date lastLoggedIn;
    
    private Long tenantId;
    
    private Boolean isSuperUser = false;

	private Boolean isJobUser = false;
    
	private Set<String> suiteAccess = new HashSet<>();

	private String fhirId;

	private String client;

	private CultureDTO culture;

	private Long cultureId;

	private Boolean isTermsAndConditionsAccepted;
	
	private Designation designation;

	public TimezoneDTO getTimezone() {
		return CommonUtil.getTimezone(timezone);
	}
	
	public List<RoleDTO> getRoles() {
	    if (Objects.isNull(roles)) {
            return new ArrayList<>();
        }
        return roles.stream().map(role -> new ModelMapper().map(role, RoleDTO.class)).toList();
	}

	public CountryDTO getCountry() {
		ModelMapper modelMapper = new ModelMapper();
		if (Objects.isNull(country)) {
			return null;
		}
		return modelMapper.map(country, CountryDTO.class);
	}

	@JsonIgnore
	public RoleDTO getDefaultRole() {
		ModelMapper modelMapper = new ModelMapper();
		List<String> roleNames = roles.stream().map(Role::getName).toList();
		Map<String,String> roleMap = new HashMap<>(Constants.SPICE_WEB_ROLES);
		roleMap.putAll(Constants.SPICE_MOBILE_ROLES);
		String roleName = roleNames.stream().filter(roleMap.values()::contains).findFirst().orElse(null);
		Role defaultRole =  roles.stream().filter(role -> role.getName().equals(roleName)).findFirst().orElse(null);
		return Objects.isNull(defaultRole) ? null : modelMapper.map(defaultRole, RoleDTO.class);
	}

	public CommonResponseDTO getDesignation() {
		ModelMapper modelMapper = new ModelMapper();
		if (Objects.isNull(designation)) {
			return new CommonResponseDTO();
		}
		return modelMapper.map(designation, CommonResponseDTO.class);
	}
}
