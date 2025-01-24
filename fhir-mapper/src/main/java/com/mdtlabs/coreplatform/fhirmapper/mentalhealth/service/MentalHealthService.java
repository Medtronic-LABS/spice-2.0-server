package com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service;

import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.RelatedPerson;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * This is an interface to perform any actions in mentalHealth related entities.
 *
 * @author Niraimathi S created on Sept 06 07, 2024
 */
public interface MentalHealthService {

    /**
     * <p>
     * This method is used to add a new mental health for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     */
    void createMentalHealth(AssessmentDTO request);

    /**
     * <p>
     * This method is used to create Questionnaire response to save mental health details of a patient.
     * </p>
     *
     * @param requestDTO    {@link AssessmentDTO} -  Request Object with mental health questions, answers and Scores.
     * @param patient       {@link Patient} - Patient FHIR object with patient details
     * @param relatedPerson {@link RelatedPerson} - Related person data
     * @param encounter     {@link Encounter} - Encounter details
     * @param bundle        {@link Bundle} - Resource bundle
     * @param provenanceDTO {@link ProvenanceDTO} - Provenance Details
     * @return A map of {@link QuestionnaireResponse} type with all available mental health questionnaire response
     * values
     */
    Map<String, QuestionnaireResponse> createQuestionnaireResponse(AssessmentDTO requestDTO, Patient patient,
            RelatedPerson relatedPerson, Encounter encounter, Bundle bundle, ProvenanceDTO provenanceDTO);

    MentalHealthDTO getMentalHealthDetails(RequestDTO request);

    void createMentalHealthCondition(AssessmentDTO mentalHealth);


    AssessmentDTO getMentalHealthCondition(AssessmentDTO request);
}
