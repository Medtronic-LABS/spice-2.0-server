package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for GeneralMedicalReviewSummary Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Mar 20, 2024.
 */
@Data
public class GeneralMedicalReviewSummaryDetailsDTO {

    private String id;

    private List<String> presentingComplaints;

    private String presentingComplaintsNotes;

    private String patientStatus;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private List<String> systemicExaminations;

    private String systemicExaminationsNotes;

    private String clinicalNotes;

    private List<PrescriptionDTO> prescriptions;

    private List<Map<String, String>> summaryStatus;

    private List<LabTestDTO> investigations;
}
