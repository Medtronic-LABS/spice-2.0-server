package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * This is a DTO class for Patient Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Apr 01, 2024.
 */
@Data
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

    private String memberId;

    private String chwName;

    private String childPatientId;

    private String chwPhoneNumber;

    private String houseHoldId;

    private String relationship;

    private Long houseHoldNumber;

    private Boolean isPregnant;

    private PregnancyDetailsDTO pregnancyDetails;

    private String phoneNumberCategory;

    private String pregnancyStatus;

    private Boolean isActive;

    private Date dateOfBirth;

    private Integer age;

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

    private String diabetesOtherSymptoms;

    private Date lastMealTime;

    private Double hba1c;

    private String hba1cUnit;

    private Date hba1cDateTime;

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

    private List<String> mentalHealthLevels;

    private String riskColorCode;

    private String riskLevel;

    private List<String> provisionalDiagnosis;

    private String insuranceId;

    private Boolean insuranceStatus;

    private String insuranceType;

    private String otherInsurance;

    private List<String> referredReasons;

    public int getAge() {
        if (Objects.isNull(this.age) && Objects.nonNull(this.dateOfBirth)) {
            Calendar birthCalendar = Calendar.getInstance();
            Calendar currentCalendar = Calendar.getInstance();
            birthCalendar.setTime(this.dateOfBirth);
            currentCalendar.setTime(new Date());
            this.age = currentCalendar.get(Calendar.YEAR) - birthCalendar.get(Calendar.YEAR);
        }
        return Objects.requireNonNullElse(this.age, 0);
    }
}
