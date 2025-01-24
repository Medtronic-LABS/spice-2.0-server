package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * This class is a Data Transfer object for NCD medical review request.
 * 
 * @author Karthick M
 *
 */
@Data
public class NCDMedicalReviewDTO  {

	private InitialMedicalReviewDTO initialMedicalReview;

	private ContinuousMedicalReviewDTO continuousMedicalReview;

	private Boolean isPregnant;

	private String qrCode;
	
	private Date nextMedicalReviewDate;

	private String patientReference;

	private String encounterReference;

	private String patientStatus;

	private ProvenanceDTO provenance;

	private String memberReference;


}
