package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Patient Entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class PatientDTO {

    private String requestFrom;

    private String id;

    private String name;

    private String nationalId;

    private Date birthDate;

    private String patientId;

    private String gender;

    private String village;

    private String villageId;

    private String phoneNumber;

    private String phoneNumberCategory;

    private Long age;

    private String performer;

    private String memberReference;

    private String memberId;

    private String fhirUrl;

    private String motherPatientId;

    private String CHW;

    private String householdId;

    private Boolean isPregnant;

    private Boolean isActive;

    private List<String> diagnosisType;

    private String identityType;

    private String identityValue;

    private String patientStatus;

    private Date dateOfBirth;

    private String chw;

    private String HHId;

    private String type;

}