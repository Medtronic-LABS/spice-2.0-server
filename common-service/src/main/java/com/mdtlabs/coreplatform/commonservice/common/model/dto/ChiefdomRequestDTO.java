package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

/**
 * This is a DTO class for Chiefdom entity.
 * 
 * @author Gopinath R Created on 29 July 2024
 */
@Data
public class ChiefdomRequestDTO {

    private Long id;

    private String name;

    private Long tenantId;

    private Long countryId;

    private List<UserRequestDTO> users;

    private Long districtId;

    private Long parentOrganizationId;

    private List<Village> villages;

}
