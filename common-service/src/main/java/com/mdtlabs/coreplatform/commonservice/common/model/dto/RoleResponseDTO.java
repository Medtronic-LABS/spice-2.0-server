package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryRole;

import lombok.Data;

/**
 * This is DTO class for  role response.
 * 
 * @author Karthick M created on May 29 2024.
 */
@Data
public class RoleResponseDTO {

    private Long id;

    private String name;
    
    private Long level;

    private String groupName;  

    private String displayName;

	private String suiteAccessName;

    private List<String> appTypes;
    
    @JsonIgnore
    private List<CountryRole> countryRoles;

    public String getDisplayName() {
        if (!Objects.isNull(UserContextHolder.getUserDto()) && !Objects.isNull(this.countryRoles) && !this.countryRoles.isEmpty() && Objects.nonNull(UserContextHolder.getUserDto().getCountry())) {
            return this.countryRoles.stream().filter(countryRole -> countryRole.getCountryId().equals(UserContextHolder.getUserDto().getCountry().getId())).map(CountryRole::getDisplayName).findFirst().orElse(this.displayName);
        }
        return this.displayName;
    }

    

}
