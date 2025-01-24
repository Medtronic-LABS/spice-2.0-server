package com.mdtlabs.coreplatform.cqlservice.model.entity;

import java.io.Serial;
import java.util.Map;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;
import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.cqlservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.cqlservice.constants.TableConstants;

/**
 * <p>
 * The CQL Result class represents a CqlResult entity with properties such as results, resourceId, memberId,
 * and patientId.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024
 */
@Entity
@Table(name = TableConstants.TABLE_CQL_RESULT)
@Data
public class CqlResult extends BaseEntity {

    @Serial
    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.RESOURCE_ID)
    private String resourceId;

    @Column(name = FieldConstants.MEMBER_ID)
    private String memberId;

    @Column(name = FieldConstants.PATIENT_ID)
    private String patientId;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.RESULTS, columnDefinition = "jsonb")
    private Map<String, Object> results;

    @Column(name = FieldConstants.VILLAGE_ID)
    private String villageId;

    @Column(name = FieldConstants.IS_LATEST)
    private Boolean isLatest;
}