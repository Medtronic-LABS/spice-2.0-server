package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

@Data
public class SupersetLoginRequestDTO {

    private String username;

    private String password;

    private Boolean refresh;

    private String provider;
}
