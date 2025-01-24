package com.mdtlabs.coreplatform.dwarfurl.common.dto;

import lombok.Data;

/**
 * Data Transfer Object (DTO) for handling URL shortening requests.
 */
@Data
public class RequestDTO {

    private String url;
    private String app;
    private String env;
    private String token;
    private Long expiresAt;

}
