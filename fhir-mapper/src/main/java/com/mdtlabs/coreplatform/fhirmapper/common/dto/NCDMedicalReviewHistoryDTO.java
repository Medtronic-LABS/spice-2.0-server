package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * This is a DTO class for Patient summary history.
 *
 * @author Divya S created on Oct 15, 2024
 */
@Data
public class NCDMedicalReviewHistoryDTO {

    private String patientVisitId;

    private List<Map<String, Object>> history = new ArrayList<>();

    private MedicalReviewSummaryDTO medicalReview = new MedicalReviewSummaryDTO();

    @Data
    public static class MedicalReviewSummaryDTO {

        private List<PhysicalExamDTO> physicalExams = new ArrayList<>();

        private List<String> complaints = new ArrayList<>();

        private List<String> notes = new ArrayList<>();

        private List<String> prescriptions = new ArrayList<>();

        private List<String> investigations = new ArrayList<>();

    }
}
