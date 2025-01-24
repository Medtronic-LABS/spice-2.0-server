package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * This is DTO  class for response for enrollment.
 * 
 * @author Karthick M created on Aug 05, 2024
 */
@Data
public class EnrollmentResponseDTO {
    
    private Date dateOfEnrollment;

    private String name;

    private String gender;

    private Integer age;

    private String programId;

    private String nationalId;

    private String phoneNumber;

    private String facilityName;

    private Date dateOfBirth;

    private String id;

    private Boolean isActive;

    private String patientStatus;

    private String identityType;

    private String identityValue;

    private String village;

    private String villageId;

    private String memberReference;

    private String firstName;

    private String middleName;

    private String lastName;

    private String levelOfEducation;

    private String memberId;

    private String patientId;

    private AssessmentTreatmentPlanDTO treatmentPlanResponse;
}
