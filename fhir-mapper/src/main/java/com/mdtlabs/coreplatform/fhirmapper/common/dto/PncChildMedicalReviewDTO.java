package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class for PNC child observation.
 *
 * @author Nandhakumar K created on Apr 22, 2024
 */
@Data
public class PncChildMedicalReviewDTO {

    private String id;

    private int visitNumber;

    private String patientReference;

    private Boolean isChildAlive;

    private String patientStatus;

    private List<String> presentingComplaints;

    private String presentingComplaintsNotes;

    private List<String> physicalExaminations;

    private String physicalExaminationNotes;

    private String congenitalDetect;

    private String cordExamination;

    private Boolean breastFeeding;

    private Boolean exclusiveBreastFeeding;

    private String clinicalNotes;

    private EncounterDetailsDTO encounter;

    private List<PrescriptionDTO> prescriptions;

}
