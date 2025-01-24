package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * This DTO class for PNC mother observation.
 *
 * @author Nandhakumar K created on Apr 22, 2024
 */
@Data
public class PncMotherMedicalReviewDTO {

    private String id;

    private int visitNumber;

    private String patientReference;

    private String patientStatus;

    private Boolean isMotherAlive;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private String breastCondition;

    private String breastConditionNotes;

    private String involutionsOfTheUterus;

    private String involutionsOfTheUterusNotes;

    private List<String> presentingComplaints;

    private String presentingComplaintsNotes;

    private List<String> systemicExaminations;

    private String systemicExaminationsNotes;

    private String clinicalNotes;

    private EncounterDetailsDTO encounter;

    private List<PrescriptionDTO> prescriptions;

    private List<Map<String, String>> summaryStatus;

    private List<LabTestDTO> investigations;

    private LabourDTO labourDTO;

    private String neonateOutcome;
}
