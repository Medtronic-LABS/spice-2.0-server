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

@Data
@Entity
@RestrictAudit
@Table(name = TableConstants.TABLE_OFFLINE_SYNC)
public class OfflineSync extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.REQUEST_DATA, columnDefinition = "jsonb")
    @Type(value = JsonBinaryType.class)
    private Map<String, Object> requestData;

    @Column(name = FieldConstants.REQUEST_ID)
    private String requestId;

    @Column(name = FieldConstants.REQUEST_TIME)
    private Date requestTime;

    @Column(name = FieldConstants.REFERENCE_ID)
    private String referenceId;

    @Column(name = FieldConstants.FHIR_ID)
    private String fhirId;

    @Column(name = FieldConstants.STATUS)
    private String status;

    @Column(name = FieldConstants.RETRY_ATTEMPTS)
    private Integer retryAttempts = 0;

    @Column(name = FieldConstants.ERROR_MESSAGE)
    private String errorMessage;

    @Column(name = FieldConstants.APP_VERSION)
    private String appVersion;

    @Column(name = FieldConstants.DEVICE_ID)
    private String deviceId;

}
