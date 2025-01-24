package com.mdtlabs.coreplatform.adminservice.model.dto;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

import lombok.Data;

import java.util.List;

/**
 * This is a DTO class for Chiefdom request DTO.
 * 
 * @author Gopinath R Created on 29 July 2024
 */
@Data
public class ChiefdomRequestDTO {

    private Long id;

    private String name;

    private Long tenantId;

    private Long countryId;

    private String searchTerm;

    private int limit;

    private int skip;

    private List<UserRequestDTO> users;

    private Long districtId;

    private List<Village> villages;

}
