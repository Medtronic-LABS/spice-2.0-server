package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * This is a DTO class for Medical Review Pregnancy Summary Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
public class MedicalReviewPregnancySummaryDetailsDTO {

    private String id;

    private List<String> presentingComplaints;

    private String presentingComplaintsNotes;

    private List<String> obstetricExaminations;

    private String obstetricExaminationNotes;

    private int visitNumber;

    private String patientStatus;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private String memberId;

    private String clinicalNotes;

    private Double weight;

    private Double pulse;

    private Double diastolic;

    private Double systolic;

    private Double bmi;

    private Double fundalHeight;

    private Double fetalHeartRate;

    private Date dateOfReview;

    private List<PrescriptionDTO> prescriptions;

    private List<Map<String, String>> summaryStatus;

    private Double height;

    private List<LabTestDTO> investigations;
}


