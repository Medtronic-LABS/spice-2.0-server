package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is a DTO class for Psychology Details.
 * </p>
 *
 * @author Shrikanth Created on September 27, 2024.
 */
@Data
public class PsychologyDTO {

    private List<String> clinicianNotes;

    private List<NoteAssessment> counselorAssessments;

    private Long id;

    private String clinicianNote;

    private Long patientVisitId;

    private String patientReference;

    private Long memberReference;

    private String referredBy;

    private String referredByDisplay;

    private Date referredDate;

    private String counselorAssessment;

    private Long counselorAssessmentId;

    private Date assessedDate;

    private String assessedBy;

    private String assessedByDisplay;

    private ProvenanceDTO provenance;
}
