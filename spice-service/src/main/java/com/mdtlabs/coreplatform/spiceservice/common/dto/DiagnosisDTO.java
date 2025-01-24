package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This is a Diagnosis DTO class for Patient diagnosis.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class DiagnosisDTO {

    private String patientId;

    private String patientReference;

    private List<DiseaseDTO> diseases;

    private ProvenanceDTO provenance;

    private String type;

    private String otherNotes;

    private String value;

    @Data
    public static class DiseaseDTO {

        private Long diseaseCategoryId;

        private Long diseaseConditionId;

        private String diseaseCategory;

        private String notes;

        private String diseaseCondition;

        private String type;

    }
}
