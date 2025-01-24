package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.Date;
import java.util.List;

import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;

/**
 * <p>
 * This is a entity class for Call Register.
 * </p>
 *
 * @author Yogeshwaran mohan Created on Mar 26, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.CALL_REGISTER)
public class CallRegister extends BaseEntity {

    @Column(name = FieldConstants.PATIENT_STATUS)
    private String patientStatus;

    @Column(name = FieldConstants.REASON)
    private String reason;

    @Column(name = FieldConstants.HOUSEHOLD_ID)
    private String householdId;

    @Column(name = FieldConstants.MEMBER_ID)
    private String memberId;

    @Column(name = FieldConstants.ENCOUNTER_TYPE)
    private String encounterType;

    @Column(name = FieldConstants.NEXT_VISIT_DATE)
    private Date nextVisitDate;

    @Enumerated(EnumType.STRING)
    @Column(name = FieldConstants.TYPE)
    private AppointmentType type;

    @Column(name = FieldConstants.REFERRED_SITE_ID)
    private String referredSiteId;

    @Column(name = FieldConstants.ENCOUNTER_DATE)
    private Date encounterDate;

    @Column(name = FieldConstants.ENCOUNTER_ID)
    private String encounterId;

    @Column(name = FieldConstants.ENCOUNTER_NAME)
    private String encounterName;

    @Column(name = FieldConstants.PATIENT_ID)
    private String patientId;

    @Column(name = FieldConstants.ATTEMPTS)
    private int attempts;

    @Column(name = FieldConstants.VISITS)
    private long visits;

    @Column(name = FieldConstants.IS_WRONG_NUMBER)
    private Boolean isWrongNumber;

    @Column(name = FieldConstants.IS_COMPLETED)
    private Boolean isCompleted;

    @Column(name = FieldConstants.VILLAGE_ID)
    private String villageId;

    @Column(name = FieldConstants.IS_INITIATED)
    private Boolean isInitiated;

    @Column(name = FieldConstants.SCREENING_DATE_TIME)
    private Date screeningDateTime;

    @Column(name = FieldConstants.NEXT_BP_ASSESSMENT_DATE)
    private Date nextBPAssessmentDate;

    @Column(name = FieldConstants.NEXT_BG_ASSESSMENT_DATE)
    private Date nextBGAssessmentTime;

    @Enumerated(EnumType.STRING)
    @Column(name = FieldConstants.FIRST_INTERACTION_MODE)
    private InteractionMode firstInteractionMode;

    @Enumerated(EnumType.STRING)
    @Column(name = FieldConstants.LAST_INTERACTION_MODE)
    private InteractionMode lastInteractionMode;

    @Column(name = FieldConstants.CALLED_AT)
    private Long calledAt;

    @OneToMany
    @JoinColumn(name = FieldConstants.CALL_REGISTER_ID)
    private List<CallRegisterDetail> callRegisterDetail;

    @Column(name = FieldConstants.NEXT_MEDICAL_REVIEW_DATE)
    private Date nextMedicalReviewDate;

    @Column(name = FieldConstants.USER_ID)
    private Long userId;
}
