package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

/**
 * <p>
 * This is a DTO class for Patient Entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Apr 01, 2024.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PatientDTO {
    private String id;

    private String name;

    private String nationalId;

    private Date birthDate;

    private String patientId;

    private String gender;

    private String village;

    private String phoneNumber;

    private Long age;

    private String Performer;

    private String memberReference;

    private String memberId;

    private String fhirUrl;

    private String chw;

    private String HHId;

    private Boolean isPregnant;

    private String type;

    private List<String> diagnosisType;

    private String identityType;

    private String identityValue;

    private String patientStatus;

    private Date dateOfBirth;

    private String villageId;

    private String phoneNumberCategory;

    private String motherPatientId;

    private String householdId;

    private Boolean isActive;

}
