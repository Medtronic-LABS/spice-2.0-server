package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

/**
 * <p>
 * This is a DTO class for Patient Entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Apr 01, 2024.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PatientDetailsDTO {

    private String id;

    private String name;

    private Date birthDate;

    private String patientId;

    private String nationalId;

    private String gender;

    private String village;

    private String villageId;

    private String landmark;

    private String phoneNumber;

    private String relationship;

    private String memberId;

    private String chwName;

    private String chwPhoneNumber;

    private String houseHoldId;

    private Long houseHoldNumber;

    private String motherPatientId;

    private String pregnancyStatus;

    private Boolean isPregnant;

    private PregnancyDetailsDTO pregnancyDetails;

    private String phoneNumberCategory;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private Boolean isActive;

    // africa
    private Date dateOfBirth;
    
    private int age;

    private Double height;

    private Double weight;

    private Double bmi;

    private String bmiCategory;

    private Double avgSystolic;

    private Double avgDiastolic;

    private String avgBloodPressure;

    private Double avgPulse;

    private Boolean isRegularSmoker;

    private String identityType;

    private String identityValue;

    private String firstName;

    private String middleName;

    private String lastName;

    private String enrollmentAt;

    private String occupation;

    private String patientStatus;

    private String createdAt;

    private String updatedAt;

    private String programId;

    private String createdBy;

    private String updatedBy;

    private String cageAid;

    private String riskMessage;

    private String cvdRiskLevel;

    private String cvdRiskScoreDisplay;

    private String cvdRiskScore;

    private String phq4RiskLevel;

    private String phq4score;

    private String phq4FirstScore;

    private String phq4SecondScore;

    private String gad7Score;

    private String gad7RiskLevel;

    private String phq9Score;

    private String phq9RiskLevel;

    private String suicidalIdeation;

    private String glucoseUnit;

    private Double glucoseValue;

    private String glucoseType;

    private Date glucoseDateTime;

    private List<DiabetesDTO> diabetes;

    private String diabetesOtherSymptoms;

    private Date lastMealTime;

    private Double hba1c;

    private String hba1cUnit;

    private Date hba1cDateTime;

    private List<BpLogDetailsDTO> bpLogDetails;

    private Boolean isPhq9;

    private Boolean redRiskPatient;

    private Boolean initialMedicalReview;

    private Boolean isHtnDiagnosis;

    private Boolean isDiabetesDiagnosis;

    private Boolean isGestationalDiabetes;

    private String nextBpAssessmentDate;

    private String nextBgAssessmentDate;

    private Date nextMedicalReviewDate;

    private Boolean isReferred;

    private boolean isInitialReviewed;

    private ConfirmDiagnosisDTO confirmDiagnosis;

    private PrescriberDetailsDTO prescribedDetails;

    private List<String> mentalHealthLevels;

    private String riskColorCode;

    private String riskLevel;

    private List<String> provisionalDiagnosis;

    private String insuranceId;

    private Boolean insuranceStatus;

    private String insuranceType;

    private String otherInsurance;

    private List<String> referredReasons;

    public String getRiskColorCode() {
        if (!StringUtils.isBlank(this.riskLevel) && Objects.equals(Constants.HIGH, this.riskLevel)) {
            return Constants.RISK_COLOR_CODE.get(this.riskLevel);
        }
        return null;
    }
}
