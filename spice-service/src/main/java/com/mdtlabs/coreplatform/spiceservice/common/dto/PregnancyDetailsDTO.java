package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude;

import com.mdtlabs.coreplatform.spiceservice.common.dto.pregnancy.PregnancySymptomDTO;

/**
 * <p>
 * This is a DTO class for Pregnancy Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PregnancyDetailsDTO {

    private Boolean isPregnant;

    private Date lastMenstrualPeriod;

    private Long ancVisitAssessment;

    private Long pncVisitAssessment;

    private Long childHoodVisitAssessment;

    private Integer ancVisitMedicalReview;

    private Integer pncVisitMedicalReview;

    private Integer childHoodVisitMedicalReview;

    private String patientStatus;

    private Double height;

    private Double pulse;

    private Date estimatedDeliveryDate;

    private String gestationalAge;

    private String noOfFetus;

    private String gravida;

    private String parity;

    private String patientBloodGroup;

    private Double bmi;

    private String smartAncContactDetails;

    private Date dateOfDelivery;

    private Integer noOfNeonates;

    private String neonatePatientId;

    private String type;

    private Double temperature;

    private String neonatalOutcomes;

    private String maternalOutcomes;

    private List<Map<String, String>> diagnosis;

    private Date actualDeliveryDate;

    private Double weight;

    private Boolean isOnTreatment;

    private Date diagnosisTime;

    private String patientReference;

    private String memberReference;

    private Boolean isPregnancyAnc;

    private ProvenanceDTO provenance;

    private List<PregnancySymptomDTO> pregnancySymptoms;

    private Boolean attendedAncClinic;

    private String pregnancyOtherSymptoms;

    private Boolean isIronFolateProvided;

    private Boolean isInterestedToEnroll;

    private Boolean isMosquitoNetProvided;

    private Boolean isIptDrugProvided;

    private Boolean isPregnancyRisk;

    private NcdPatientStatus ncdPatientStatus;

    private Boolean isDangerSymptoms;

    private String patientVisitId;
}
