package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This class is a Data Transfer object for Red risk calculation.
 * 
 * @author Karthick S
 *
 */
@Data
public class RedRiskDTO {

    private int comorbiditiesCount;

    private String diabetesDiagControlledType;

    private String diabetesPatientType;

    private PatientObservationDetails patientObservationDetails;
    
    public RedRiskDTO(int comorbiditiesCount, String diabetesDiagControlledType, String diabetesPatientType,
            PatientObservationDetails patientObservationDetails) {
        this.comorbiditiesCount = comorbiditiesCount;
        this.diabetesDiagControlledType = diabetesDiagControlledType;
        this.diabetesPatientType = diabetesPatientType;
        this.patientObservationDetails = patientObservationDetails;
    }

    public RedRiskDTO() {
    }
}