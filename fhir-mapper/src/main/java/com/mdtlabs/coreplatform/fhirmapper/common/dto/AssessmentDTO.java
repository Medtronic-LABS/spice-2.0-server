package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

/**
 * <p>
 * This is a DTO class for AssessmentDTO entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class AssessmentDTO {

    private String id;

    private String assessmentType;

    private String patientStatus;

    private AssessmentDetailsDTO assessmentDetails;

    private String referralTicketType;

    private String referralTicketVisitType;

    private String referredReasons;

    private FollowUpDTO followUp;

    private SummaryDTO summary;

    private EncounterDetailsDTO encounter;

    private String ancContactDetails;

    private String villageId;

    private BpLogDTO bpLog;

    private GlucoseLogDTO glucoseLog;

    private String patientId;

    private String assessmentOrganizationId;

    private BioDataDTO bioData;

    private BioMetricsDTO bioMetrics;

    private Date assessmentTakenOn;

    private Map<String, String> suicideScreener;

    private Map<String, String> substanceAbuse;

    private MentalHealthDTO phq4;

    private Boolean isReferAssessment;

    private Date dateOfBirth;

    private String memberReference;

    private PregnancyDetailsDTO pregnancyAnc;

    private List<SymptomDTO> ncdSymptoms;

    private List<ComplianceDTO> compliance;

    private Map<String, String> assessmentLocation;

    private double cageAid;

    private String riskLevel;

    private String riskMessage;

    private String cvdRiskLevel;

    private String cvdRiskScoreDisplay;

    private Double cvdRiskScore;

    private Double temperature;

    private MentalHealthDTO phq9;

    private MentalHealthDTO gad7;

    private Long siteId;

    private Long userId;

    private String patientReference;

    private String riskColorCode;

    private AssessmentTreatmentPlanDTO treatmentPlanResponse;

    private List<String> provisionalDiagnosis;

    private boolean isOldRecord;

    private Date nextBpAssessmentDate;

    private Date nextBgAssessmentDate;

    private Date nextMedicalReviewDate;

    private String type;

    public String getRiskColorCode() {
        if (!StringUtils.isBlank(this.riskLevel)) {
            return Constants.RISK_COLOR_CODE.get(this.riskLevel);
        }
        return null;
    }

}