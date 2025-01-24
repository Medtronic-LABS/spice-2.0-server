package com.mdtlabs.coreplatform.userservice.model;

import lombok.Data;

@Data
public class RequestDTO {

    private String url;
    private Long expiresAt;
    private String app;
    private String env;
}
