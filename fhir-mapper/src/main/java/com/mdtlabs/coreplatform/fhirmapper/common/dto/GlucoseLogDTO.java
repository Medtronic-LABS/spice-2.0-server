package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * This DTO class for Glucose log details.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class GlucoseLogDTO implements Cloneable {
    private String createdBy;

    private String updatedBy;

    private String diabetesOtherSymptoms;

    private String glucoseUnit;

    private Date glucoseDateTime;

    private String glucoseType;

    private List<DiabetesDTO> diabetes;

    private Double glucoseValue;

    private Boolean isBeforeDiabetesDiagnosis;

    private Date lastMealTime;

    private Double hba1c;

    private String hba1cUnit;

    private Date hba1cDateTime;

    private String type;

    private Date bgTakenOn;

    private Date createdAt;

    private String patientId;

    private String relatedPersonFhirId;

    private String assessmentOrganizationId;

    private String encounterId;

    private List<String> symptoms;

    private ProvenanceDTO provenance;

    private BioMetricsDTO bioMetricsDTO;

    private BioDataDTO bioDataDTO;

    @Override
    public GlucoseLogDTO clone() {
        try {
            return (GlucoseLogDTO) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }
}
