package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * This DTO class for offline Sync response.
 *
 * @author Praveen created on Aprils 10, 2024
 */
@Data
public class OfflineSyncResponseDTO implements Serializable {

    private List<HouseholdDTO> households;

    private List<HouseholdMemberDTO> members;

    private List<HouseholdSequenceDTO> householdSequence;

    private List<HouseholdMemberSequenceDTO> householdMemberSequence;

    private List<PregnancyInfo> pregnancyInfos;

    private List<PatientDetailsDTO> patientDetails;

    private List<FollowUpDTO> followUps;

    private List<HouseholdMemberLinkDTO> householdMemberLinks;

    private List<AncResultDTO> ancResults;

    private FollowUpCriteria followUpCriteria;

    private Date lastSyncTime;

}
