package com.mdtlabs.coreplatform.offlineservice.common.model;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.audit.RestrictAudit;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;
import org.hibernate.annotations.Type;

import java.util.Date;
import java.util.Map;

/**
 * <p>
 * This is a entity class for offline sync log.
 * </p>
 *
 * @author Praveen Created on July 03, 2024.
 */
@Data
@Entity
@RestrictAudit
@Table(name = TableConstants.TABLE_OFFLINE_SYNC_LOG)
public class OfflineSyncLog extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.REQUEST_DATA, columnDefinition = "jsonb")
    @Type(value = JsonBinaryType.class)
    private Map<String, Object> requestData;

    @Column(name = FieldConstants.APP_VERSION)
    private String appVersion;

    @Column(name = FieldConstants.STATUS)
    private String status;

    @Column(name = FieldConstants.ERROR_MESSAGE)
    private String errorMessage;

    @Column(name = FieldConstants.LAST_SYNC_TIME)
    private Date lastSyncTime;

    @Column(name = FieldConstants.DEVICE_ID)
    private String deviceId;

    @Column(name = FieldConstants.RESPONSE_DATA)
    @Type(value = JsonBinaryType.class)
    private Map<String, Object> responseData;

    @Column(name = FieldConstants.METHOD_NAME)
    private String methodName;

    @Column(name = FieldConstants.SYNC_MODE)
    private String syncMode;

}
