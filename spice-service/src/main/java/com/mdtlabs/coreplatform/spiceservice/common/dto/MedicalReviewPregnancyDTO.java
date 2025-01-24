package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for Medical Review Pregnancy.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
public class MedicalReviewPregnancyDTO {

    private String id;

    private List<String> presentingComplaints;

    private String presentingComplaintsNotes;

    private List<String> obstetricExaminations;

    private String obstetricExaminationNotes;

    private String clinicalNotes;

    private String patientReference;

    private String patientId;

    private Double fundalHeight;

    private Double fetalHeartRate;

    private double visitNumber;

    private PregnancyAncDetailsDTO pregnancyDetails;

    private List<String> pregnancyHistory;

    private String pregnancyHistoryNotes;

    private boolean deliveryKit;

    private EncounterDetailsDTO encounter;
}
