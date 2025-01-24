package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.util.List;

@Data
public class DistrictOrganizationDTO {
    private Long id;
    
    @NotBlank(message = ErrorConstants.DISTRICT_NAME_NOT_NULL)
    private String name;
    
    private Long tenantId;

    @NotNull(message = ErrorConstants.COUNTRY_ID_NOT_NULL)
    private Long countryId;
    
    @Size(min = 1, message = ErrorConstants.DISTRICT_USER_MIN_SIZE)
    @Valid
    private List<UserRequestDTO> users;
    
    private String countryCode;
    
    @NotNull(message = ErrorConstants.PARENT_ORG_ID_NOT_NULL)
    private Long parentOrganizationId;
}
