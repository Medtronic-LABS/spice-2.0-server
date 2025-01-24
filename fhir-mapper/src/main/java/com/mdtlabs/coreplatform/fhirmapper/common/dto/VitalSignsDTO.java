package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import org.hl7.fhir.r4.model.Observation;

@Data
public class VitalSignsDTO {
    private String relatedPersonId;
    private ProvenanceDTO provenanceDTO;
    private Observation bpObservation;
    private Observation bgObservation;
    private Observation temperatureObservation;
    private Observation heightObservation;
    private Observation weightObservation;
    private Observation bmiObservation;
    private Observation regularSmokerObservation;
    private Observation suicideObservation;
    private Observation substanceAbuseObservation;
    private Observation pregnancyObservation;
    private Observation mentalHealthObservation;
    private Observation redRiskObservation;
    private String screenedLandmark;
}
