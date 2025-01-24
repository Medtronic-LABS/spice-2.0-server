package com.mdtlabs.coreplatform.adminservice.model.dto;


import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * This is a DTO class for District entity.
 *
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class DistrictDTO {

    private Long id;

    private String name;

    private String code;

    private Long tenantId;

    private List<UserResponseDTO> users;

    private Date UpdatedAt;

    private String status;

    private Long countryId;

}
