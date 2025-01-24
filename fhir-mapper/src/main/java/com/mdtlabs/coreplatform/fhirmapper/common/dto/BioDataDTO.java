package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Objects;

import lombok.Data;

/**
 * This DTO class for bio data details.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class BioDataDTO {

    private String identityType;

    private String identityValue;

    private String nationalId;

    private String firstName;

    private String middleName;

    private String lastName;

    private String initial;

    private String phoneNumber;

    private String phoneNumberCategory;

    private String landmark;

    private String occupation;

    private String levelOfEducation;

    private String insuranceId;

    private Boolean insuranceStatus;

    private String insuranceType;

	private DataDTO program;

	private DataDTO district;

	private DataDTO country;

	private DataDTO chiefdom;

    private String otherInsurance;

    private DataDTO village;

    private Boolean isFromPatientUpdate;

    private String memberReference;

    private String healthFacilityName;

    private String patientReference;

    private boolean isDuplicatePatient;

    private BioDataDTO patientDetails = null;

    private String patientStatus;

    private String programId;

    private String siteName;

    private String gender;

    private String siteFhirId;

    private String districtId;

    private String chiefdomId;

    private String relatedPersonStatus;

    private String otherVillage;

    private String encounterReference;

    @Data
    public static class DataDTO {

        private Long id;

        private String name;

        private String otherText;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName.toUpperCase();
    }

    public void setMiddleName(String middleName) {
        this.middleName = Objects.nonNull(middleName) ? middleName.toUpperCase() : null;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName.toUpperCase();
    }
}

