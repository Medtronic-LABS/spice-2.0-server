package com.mdtlabs.coreplatform.spiceservice.common.dto;

import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for follow up information.
 * </p>
 *
 * @author Maria Antony Created on April 29, 2024.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FollowUpDTO implements Serializable {

    private Long id;

    private String householdId;

    private String memberId;

    private String patientId;

    private String encounterId;

    private String encounterName;

    private String encounterType;

    private String patientStatus;

    private String reason;

    private long attempts;

    private long visits;

    private long successfulAttempts;

    private long unsuccessfulAttempts;

    private String currentPatientStatus;

    private List<FollowUpDetailDTO> followUpDetails;

    private Boolean isCompleted;

    private AppointmentType type;

    private Boolean isWrongNumber;

    private Date nextVisitDate;

    private Date encounterDate;

    private String referredSiteId;

    private String villageId;

    private InteractionMode firstInteractionMode;

    private InteractionMode lastInteractionMode;

    private Long calledAt;

    private ProvenanceDTO provenance;

    private String appType;

    private Boolean isInitiated;

    private String name;

    private String gender;

    private Date dateOfBirth;

    private Integer age;

    private String phoneNumber;

    private String countyName;

    private String subCountyName;

    private String communityHealthUnitName;

    private String villageName;

    private String landmark;

    private boolean referAssessment;

    private boolean isCallCompleted;

    private boolean isCallInitiated;

    private long referredDateSince;

    private long createdAt;

    private Integer retryAttempts;

    private Date screeningDateTime;

    private Date assessmentDate;

    private Date nextMedicalReviewDate;

    private Date nextBpAssessmentDate;

    private Date nextBgAssessmentDate;

    private List<String> overDueCategories;

    private Date dueDate;

    private Long callRegisterId;

    private List<String> referredReasons;

    private String identityType;

    private String identityValue;

    private boolean isActive;

    private boolean isDeleted;

    private long updatedAt;

    public FollowUpDTO(String memberId, AppointmentType type) {
        this.memberId = memberId;
        this.type = type;
    }

}
