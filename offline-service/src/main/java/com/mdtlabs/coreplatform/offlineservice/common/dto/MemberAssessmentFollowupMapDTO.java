package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * This is a DTO class for HouseholdMember entity.
 * </p>
 *
 * @author Praveen Created on July 22, 2024.
 */
@Data
public class MemberAssessmentFollowupMapDTO implements Serializable {

    private String householdMemberId;

    private List<AssessmentDTO> assessments = new ArrayList<>();

    private List<FollowUpDTO> followUps = new ArrayList<>();

}
