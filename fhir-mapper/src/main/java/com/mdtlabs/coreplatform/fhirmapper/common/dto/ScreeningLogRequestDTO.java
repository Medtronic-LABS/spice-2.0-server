package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * This DTO class for Screening log details.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class ScreeningLogRequestDTO {
        private String fhirId;
        private String appVersion;
        private String latitude;
        private double cageAid;
        private Map<String, String> suicideScreener;
        private String type;
        private String otherType;
        private BpLogDTO bpLog;
        private String suicidalIdeation;
        private String cvdRiskLevel;
        private BioMetricsDTO bioMetrics;
        private PregnancyDetailsDTO pregnancyAnc;
        private String cvdRiskScoreDisplay;
        private double cvdRiskScore;
        private Boolean isReferAssessment;
        private double deviceInfoId;
        private BioDataDTO bioData;
        private String longitude;
        private Map<String, String> substanceAbuse;
        private MentalHealthDTO phq4;
        private String unitMeasurement;
        private List<String> referredReasons;
        private Long siteId;
        private Long userId;
        private String category;
        private GlucoseLogDTO glucoseLog;
        private Date screeningDateTime;
        private String memberReference;
        private String countryId;
        private Map<String, String> hivHistory;
        private Map<String, String> generalHealth;
        private Map<String, String> tbSymptoms;
        private Map<String, String> stdScreening;
        private Map<String, String> risksOfHIVInfection;
        private Map<String, String>  hivRiskBehaviours;
        private String villageId;
}
