package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import org.hl7.fhir.r4.model.QuestionnaireResponse;

import java.util.Map;

@Data
public class MentalHealthObservationDTO {

    private Map<String, QuestionnaireResponse> questionnaireResponses;

    private Map<String, QuestionnaireResponse> existingMentalHealthDetails;

    private Map<String, String> mentalRiskDetails;

    private String relatedPersonId;
}
