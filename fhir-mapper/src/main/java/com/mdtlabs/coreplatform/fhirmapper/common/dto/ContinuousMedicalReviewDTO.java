package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Set;

/**
 * This class is a Data Transfer object for Continuous Medical review request.
 * 
 * @author Karthick M
 *
 */
@Data
public class ContinuousMedicalReviewDTO  {

	private Set<MedicalReviewMetaDTO> physicalExams;

	private Set<MedicalReviewMetaDTO> complaints;

	private Set<MedicalReviewMetaDTO> comorbidities;
    
    private Set<MedicalReviewMetaDTO> complications;

	private String complaintComments;

	private String physicalExamComments;

	private String clinicalNote;
}
