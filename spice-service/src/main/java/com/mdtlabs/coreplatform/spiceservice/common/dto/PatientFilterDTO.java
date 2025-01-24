package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Patient Filter Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class PatientFilterDTO {

    private List<String> patientStatus;

    private List<String> visitDate;

    private String enrollmentStatus;

    private String medicalReviewDate;

    private String assessmentDate;

    private Boolean isRedRiskPatient;

    private String cvdRiskLevel;

    private String labTestReferredOn;

    private String prescriptionReferredOn;

}