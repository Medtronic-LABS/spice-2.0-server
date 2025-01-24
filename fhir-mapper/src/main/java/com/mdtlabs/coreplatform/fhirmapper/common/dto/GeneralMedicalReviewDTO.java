package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for GeneralMedical Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Mar 20, 2024.
 */
@Data
public class GeneralMedicalReviewDTO {

    private String id;

    private List<String> presentingComplaints;

    private String presentingComplaintsNotes;

    private List<String> systemicExaminations;

    private String systemicExaminationsNotes;

    private String clinicalNotes;

    private EncounterDetailsDTO encounter;

}
