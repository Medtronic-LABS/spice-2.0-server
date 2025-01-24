package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

/**
 * <p>
 * This class is an entity class for DeviceDetails table.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 2026
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_DEVICE_DETAILS)
public class DeviceDetails extends BaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.MODEL)
    private String model;

    @Column(name = FieldConstants.VERSION)
    private String version;

    @Column(name = FieldConstants.AES_KEY)
    private String aesKey;

    @Column(name = FieldConstants.DEVICE_ID)
    private String deviceId;

    @Column(name = FieldConstants.USER_ID)
    private Long userId;

    @Column(name = FieldConstants.TENANT_ID)
    private Long tenantId;

    @Column(name = FieldConstants.RSA_PUBLIC_KEY)
    private String rsaPublicKey;

    @Column(name = FieldConstants.RSA_PRIVATE_KEY)
    private String rsaPrivateKey;

    @Column(name = FieldConstants.AUTH_TAG)
    private String authTag;

    @Column(name = FieldConstants.LAST_LOGGED_IN)
    private Date lastLoggedIn;

    @Column(name = FieldConstants.REF_ID)
    private String refId;
}
