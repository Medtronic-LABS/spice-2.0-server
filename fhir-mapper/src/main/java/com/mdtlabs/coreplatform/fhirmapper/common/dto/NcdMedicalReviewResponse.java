package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * This class is a Data Transfer object for NCD medical review response.
 * 
 * @author Karthick M
 *
 */
@Data
public class NcdMedicalReviewResponse {

   
    private Boolean isSigned;

    private String medicalReviewFrequency;

    private Boolean isPregnant;

    private Boolean isInitialPregnancyReview;

    private Date lastMenstrualPeriodDate;

    private Date estimatedDeliveryDate;

    private Set<String> physicalExams = new LinkedHashSet<>();

    private Set<String> complaints = new LinkedHashSet<>();

    private String physicalExamComments;

    private String compliantComments;

    private Date reviewedAt;

    private String clinicalNote;

    private Set<String> comorbities = new LinkedHashSet<>();

    private Set<String> complications = new LinkedHashSet<>();

    private List<PrescriptionDTO> prescriptions = new ArrayList<>();

    private List<String> investigations = new ArrayList<>();

    private ConfirmDiagnosisDTO confirmDiagnosis;
}
