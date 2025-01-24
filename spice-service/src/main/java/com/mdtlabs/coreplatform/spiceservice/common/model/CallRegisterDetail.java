package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.Date;

import com.mdtlabs.coreplatform.spiceservice.common.enumeration.CallStatus;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

/**
 * <p>
 * This is a entity class for Call Register Details.
 * </p>
 *
 * @author Yogeshwaran mohan Created on Mar 26, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.CALL_REGISTER_DETAIL)
public class CallRegisterDetail extends BaseEntity {

    @Column(name = FieldConstants.CALL_DATE)
    private Date callDate;

    @Column(name = FieldConstants.DURATION)
    private Integer duration;

    @Enumerated(EnumType.STRING)
    @Column(name = FieldConstants.STATUS)
    private CallStatus status;

    @Column(name = FieldConstants.REASON)
    private String reason;

    @Column(name = FieldConstants.OTHER_REASON)
    private String otherReason;

    @Column(name = FieldConstants.PATIENT_STATUS)
    private String patientStatus;

    @Column(name = FieldConstants.ATTEMPTS)
    private int attempts;

    @Column(name = FieldConstants.CALL_REGISTER_ID)
    private Long callRegisterId;

    @Column(name = FieldConstants.LATITUDE)
    private String latitude;

    @Column(name = FieldConstants.LONGITUDE)
    private String longitude;

    @Column(name = FieldConstants.VISITED_FACILITY_ID)
    private String visitedFacilityId;

    @Column(name = FieldConstants.OTHER_VISITED_FACILITY_NAME)
    private String otherVisitedFacilityName;

}
