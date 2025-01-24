package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;

/**
 * <p>
 * This class is an Entity for patient transfer details which contains necessary fields.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram A created on Oct 07, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_PATIENT_TRANSFER)
public class PatientTransfer extends TenantBaseEntity {

    private static final long serialVersionUID = 1L;

    @JoinColumn(name = FieldConstants.TRANSFER_TO)
    @ManyToOne(fetch = FetchType.LAZY)
    private User transferTo;

    @JoinColumn(name = FieldConstants.TRANSFER_BY)
    @ManyToOne(fetch = FetchType.LAZY)
    private User transferBy;

    @JoinColumn(name = FieldConstants.TRANSFER_SITE)
    @ManyToOne(fetch = FetchType.LAZY)
    private HealthFacility transferSite;

    @JoinColumn(name = FieldConstants.OLD_SITE)
    @ManyToOne(fetch = FetchType.LAZY)
    private HealthFacility oldSite;

    @Column(name = FieldConstants.TRANSFER_REASON)
    private String transferReason;

    @Column(name = FieldConstants.TRANSFER_REJECT_REASON)
    private String rejectReason;

    @Column(name = FieldConstants.OLD_PROGRAM_ID)
    private Long oldProgramId;

    @Column(name = FieldConstants.PATIENT_FHIR_ID)
    private String patientFhirId;

    @Column(name = FieldConstants.MEMBER_ID)
    private String memberId;

    @Enumerated(EnumType.STRING)
    @Column(name = FieldConstants.TRANSFER_STATUS)
    private PatientTransferStatus transferStatus;

    @Column(name = FieldConstants.IS_SHOW)
    private boolean isShow = true;
}