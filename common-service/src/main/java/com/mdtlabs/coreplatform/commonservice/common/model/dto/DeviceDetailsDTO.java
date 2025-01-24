package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This class is an Request DTO class for device details.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 2026
 */
@Data
public class DeviceDetailsDTO {

    private Long id;

    private String name;

    private String type;

    private String model;

    private String version;

    private String aesKey;

    private String deviceId;

    private Long userId;

    private Long tenantId;

    private String rsaPublicKey;

    private String rsaPrivateKey;

    private String authTag;

    private Date lastLoggedIn;

    private String refId;
}
