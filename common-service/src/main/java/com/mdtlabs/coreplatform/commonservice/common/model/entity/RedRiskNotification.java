package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

/**
 * <p>
 * This class is for RedRiskNotification entity
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Aug 20 2024
 */
@Data
@Table(name = TableConstants.TABLE_RED_RISK_NOTIFICATION)
@Entity
public class RedRiskNotification extends TenantBaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.ENCOUNTER_ID)
    private String encounterId;

    @Column(name = FieldConstants.MEMBER_ID)
    private String memberId;

    @Column(name = FieldConstants.PATIENT_ID)
    private String patientId;

    @Column(name = FieldConstants.STATUS)
    private String status;


}
