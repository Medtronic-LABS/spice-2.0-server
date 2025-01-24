package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

import java.util.List;

@Data
public class SupersetUserRequestDto {

    private Long id;

    private Boolean active;

    private String firstName;

    private String lastName;

    private String role;

    private String username;

    private List<Long> healthFacilities;

    private Integer supersetId;
}
