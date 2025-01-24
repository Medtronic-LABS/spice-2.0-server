package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * This DTO class for BP log.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class BpLogDTO {

    private double avgSystolic;

    private Boolean isBeforeHtnDiagnosis;

    private double avgDiastolic;

    private String avgBloodPressure;

    private List<BpLogDetailsDTO> bpLogDetails;

    private Integer avgPulse;

    private Double temperature;

    private String cvdRiskLevel;

    private Double cvdRiskScore;

    private String cvdRiskScoreDisplay;

    private Boolean isRegularSmoker;

    private String type;

    private String patientId;

    private String riskLevel;

    private Date bpTakenOn;

    private Date createdAt;

    private String assessmentOrganizationId;

    private String relatedPersonFhirId;

    private String encounterId;

    private List<String> symptoms;

    private ProvenanceDTO provenance;

    private BioDataDTO bioData;

    private BioMetricsDTO bioMetrics;
}
