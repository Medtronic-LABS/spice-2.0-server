package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * This is a response DTO class for under five years ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class IccmResponseDTO {

    private String clinicalNotes;

    private String presentingComplaints;

    private List<String> systemicExamination;

    private String systemicExaminationNotes;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private Map<String, List<ExaminationQA>> examination;

    private Map<String, String> examinationDisplayNames;

    private String patientStatus;

    private List<PrescriptionDTO> prescriptions;

    private List<Map<String, String>> summaryStatus;

    private List<LabTestDTO> investigations;

    @Data
    public static class ExaminationQA {

        private String title;

        private Object value;
    }

}
