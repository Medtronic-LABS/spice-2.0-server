package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.List;

import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;

import lombok.Data;

@Data
public class UserContextDTO {

    private Long id;

    private String firstName;

    private String lastName;

    private String username;

    private String phoneNumber;

    private List<Role> roles = new ArrayList<>();

    private boolean active;

    private String authorization;

    private String cookie;

    private long currentDate;

    private long tenantId;

    private String countryCode;

    private Country country;

    private Boolean isSuperUser = false;

    private Boolean isJobUser = false;

    private Set<Long> organizationIds;

    private Set<String> suiteAccess = new HashSet<>();

    private String client;

    private String fhirId;

    private Timezone timezone;

    private Long cultureId;

    private CultureDTO culture;

    private Boolean isTermsAndConditionsAccepted;

    public TimezoneDTO getTimezone() {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(timezone)) {
            return null;
        }
        return modelMapper.map(timezone, TimezoneDTO.class);
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
}
