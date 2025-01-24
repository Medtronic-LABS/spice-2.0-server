package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Householdmember entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class HouseholdMemberDTO {

    private String id;

    private String referenceId;

    private String name;

    private String householdId;

    private String phoneNumber;

    private String phoneNumberCategory;

    private String patientId;

    private String patientReference;

    private String gender;

    private String initial;

    private String signature;

    private String village;

    private String villageId;

    private String householdHeadRelationship;

    private Date dateOfBirth;

    private String version;

    private Date lastUpdated;

    private ProvenanceDTO provenance;

    private Boolean isChild;

    private String motherPatientId;

    private Boolean isPregnant;

    private Boolean isActive;

    private String memberId;

    private Boolean assignHousehold;

    private List<HouseholdMemberDTO> children;
}
