package com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir;

import lombok.Data;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.RelatedPerson;

@Data
public class SearchPersonDetailsDTO {
    private RelatedPerson relatedPerson;
    private Patient patient;
    private Observation bpObservation;
    private Observation bgObservation;
    private Observation bmiObservation;
    private Observation temperatureObservation;
    private Observation heightObservation;
    private Observation weightObservation;
    private Observation suicideScreenerObservation;
    private Observation substanceAbuseObservation;
    private Observation regularSmokerObservation;
    private Observation pregnancyObservation;
    private Observation mentalHealthObservation;
    private Observation redRiskObservation;
    private Observation vitalSignsObservation;
    private QuestionnaireResponse phq4;
    private QuestionnaireResponse phq9;
    private QuestionnaireResponse gad7;
}
