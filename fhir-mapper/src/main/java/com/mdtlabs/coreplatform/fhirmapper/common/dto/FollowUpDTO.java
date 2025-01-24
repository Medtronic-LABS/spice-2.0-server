package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for follow up information.
 * </p>
 *
 * @author Maria Antony Created on July 18, 2024.
 */
@Data
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

    private Date nextVisitDate;

    private Date encounterDate;

    private String referredSiteId;

    private String villageId;

    private String name;

    private String gender;

    private Integer age;

    private List<String> diagnosis;

    private String phoneNumber;

    private String countyName;

    private String subCountyName;

    private String communityHealthUnitName;

    private String villageName;

    private String landmark;

    private boolean referAssessment;

    private boolean isCallCompleted;

    private boolean isCallInitiated;

    private Long referredDateSince;

    private Date createdAt;

    private Integer retryAttempts;

    private Date screeningDateTime;

    private Date assessmentDate;

    private Date nextMedicalReviewDate;

    private Date nextBpAssessmentDate;

    private Date nextBgAssessmentDate;

    private List<String> overDueCategories;

    private int noOfDueDays;

    private Date dueDate;

    private Long callRegisterId;

}
