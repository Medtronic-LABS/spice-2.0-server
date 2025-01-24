package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Medical Review Pregnancy Details.
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

    private String patientId;

    private int visitNumber;

    private Double fundalHeight;

    private Double fetalHeartRate;

    private PregnancyAncDetailsDTO pregnancyDetails;

    private List<String> pregnancyHistory;

    private String pregnancyHistoryNotes;

    private boolean deliveryKit;

    private EncounterDetailsDTO encounter;

}
