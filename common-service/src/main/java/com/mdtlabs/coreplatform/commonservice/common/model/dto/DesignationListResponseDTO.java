package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;
import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;

/**
 * <p>
 * This class is an Entity for Role Designation list details which contains necessary fields.
 * </p>
 *
 * @author Divya S created on Oct 18, 2024
 */
@Data
public class DesignationListResponseDTO {

    private Long id;

    private Long countryId;

    private Role role;

    private String name;

    public CommonResponseDTO getRole() {
        ModelMapper modelMapper = new ModelMapper();
        return modelMapper.map(role, CommonResponseDTO.class);
    }
}
