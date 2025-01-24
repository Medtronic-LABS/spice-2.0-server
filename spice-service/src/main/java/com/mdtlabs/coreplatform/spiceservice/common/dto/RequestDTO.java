package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for Request entity.
 * </p>
 */
@Data
@NoArgsConstructor
public class RequestDTO {

    private String appType;

    private String id;

    private List<String> villageIds;

    private String villageId;

    private String gender;

    private Date deliveryDate;

    private Boolean isPregnant;

    private String patientStatus;

    private Boolean closeReferralTicket;

    private List<String> assessmentType;

    private String assessmentId;

    private String visitType;

    private String patientId;

    private String ticketId;

    private String ticketType;

    private String category;

    private String reason;

    private int skip;

    private int limit;

    private ProvenanceDTO provenance;

    private Date currentSyncTime;

    private Date lastSyncTime;

    private String type;

    private Boolean isActive = true;

    private String patientReference;

    private String memberReference;

    private String encounterId;

    private String childId;

    private boolean isPreviousHistory;

    private String memberId;

    private String neonateId;

    private String labourId;

    private String motherId;

    private String prescriptionId;

    private String motherPatientId;

    private String closedReason;

    private String closedEncounterType;

    private String signature;

    private String encounterType;

    private boolean isLatestRequired;

    public List<String> diagnosisType;

    private String comments;

    private String roleName;
    
    private String patientVisitId;

    private Long healthFacilityId;

    private String transferSite;
    
    private String menuName;

    private String identityValue;

    private String identityType;

    private Boolean isFromPatientUpdate;

    private String signatureUrl;

    private Integer sortOrder;

    public RequestDTO(ProvenanceDTO provenance, String memberId, String signatureUrl) {
        this.provenance = provenance;
        this.memberId = memberId;
        this.signatureUrl = signatureUrl;
    }

}
