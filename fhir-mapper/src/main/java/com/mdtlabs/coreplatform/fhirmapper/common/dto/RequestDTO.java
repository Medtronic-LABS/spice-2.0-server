package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

/**
 * <p>
 * This is a DTO class for Request entity.
 * </p>
 */
@Data
public class RequestDTO {
    
    private String patientVisitId;

    private String id;

    private List<String> villageIds;

    private String villageId;

    private String gender;

    private Date deliveryDate;

    private Boolean isPregnant;

    private String patientStatus;

    private Boolean closeReferralTicket;

    private List<String> assessmentType;

    private String ticketType;

    private List<String> ticketStatuses;

    private String encounterType;

    private String category;

    private String reason;

    private String otherReason;

    private String closedReason;

    private String closedEncounterType;

    private String assessmentId;

    private String visitType;

    private String patientId;

    private String ticketId;

    private int skip;

    private int limit;

    private ProvenanceDTO provenance;

    private Date currentSyncTime;

    private Date lastSyncTime;

    private String type;

    private Boolean isActive = true;

    private String patientReference;

    private String encounterId;

    private String memberId;

    private String memberReference;

    private String childId;

    private String neonateId;

    private String labourId;

    private String motherId;

    private String prescriptionId;

    private boolean isPreviousHistory;

    private String motherPatientId;

    private Boolean updatePriorityStatus;

    private boolean isAncStarted;

    private String priority;

    private String fhirId;

    private String signature;

    private List<String> diagnosisType;

    private String roleName;

    private String comments;

    private boolean isNutritionHistoryRequired;

    private boolean isNutritionist;

    private Long healthFacilityId;

    private String transferSite;
    
    private String menuName;

    private String identityValue;

    private String identityType;

    private Boolean isFromPatientUpdate;

    private String signatureUrl;

    private String siteFhirId;

    private Integer sortOrder;

    public RequestDTO() {
    }

    public RequestDTO(String patientReference, String type) {
        this.patientReference = patientReference;
        this.type = type;
    }

    public RequestDTO(String patientReference, List<String> diagnosisType) {
        this.patientReference = patientReference;
        this.diagnosisType = diagnosisType;
    }

    public Integer getSortOrder() {
        return Objects.isNull(sortOrder) ? Constants.ONE : sortOrder;
    }
}
