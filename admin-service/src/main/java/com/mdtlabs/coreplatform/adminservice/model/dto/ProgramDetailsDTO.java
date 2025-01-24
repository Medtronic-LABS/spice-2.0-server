package com.mdtlabs.coreplatform.adminservice.model.dto;


import lombok.Data;
import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;

import java.util.Set;
import java.util.stream.Collectors;

/**
 * This is a DTO class for program entity.
 * 
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class ProgramDetailsDTO {

    private Long id;
    
    private boolean isDeleted;

    private boolean isActive;

    private String name;

    private Country country;
    
    private Long tenantId;

    private Set<HealthFacility> healthFacilities;

    private Set<HealthFacility> deletedHealthFacilities;

    public Set<HealthFacilityDTO> getHealthFacilities(){
		return healthFacilities.stream().map(site -> 
            new ModelMapper().map(site, HealthFacilityDTO.class)
        ).collect(Collectors.toSet());
    }

    public Set<Long> getDeletedHealthFacilities() {
        return deletedHealthFacilities.stream().map(
            BaseEntity::getId
        ).collect(Collectors.toSet());
    }

    public CountryDTO getCountry() {
		return new ModelMapper().map(country, CountryDTO.class);
	}

}
