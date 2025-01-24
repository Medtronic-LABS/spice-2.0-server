package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for HouseholdMember entity.
 * </p>
 *
 * @author Gopinath Created on Feb 14, 2024.
 */
@Data
public class HouseholdMemberDTO implements Serializable {

    private String id;

    private String referenceId;

    private String householdReferenceId;

    private String name;

    private String householdId;

    private String phoneNumber;

    private String phoneNumberCategory;

    private String patientId;

    private String gender;

    private String initial;

    private String signature;

    private String village;

    private String villageId;

    private String householdHeadRelationship;

    private List<AssessmentDTO> assessments;

    private Date dateOfBirth;

    private List<HouseholdMemberDTO> children;

    private String type;

    private Boolean isChild;

    private String motherPatientId;

    private String version;

    private Date lastUpdated;

    private ProvenanceDTO provenance;

    private Boolean isPregnant;

    private Boolean isActive;

    private Boolean assignHousehold;

}
