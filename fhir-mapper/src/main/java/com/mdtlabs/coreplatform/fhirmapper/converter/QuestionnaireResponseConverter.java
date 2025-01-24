package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

/**
 * <p>
 * Converts to FHIR Questionnaire response based on given metal
 * health questions and answers
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class QuestionnaireResponseConverter {
    public final FhirUtils fhirUtils;

    public final RestApiUtil restApiUtil;

    public final CommonConverter commonConverter;

    @Autowired
    public QuestionnaireResponseConverter(FhirUtils fhirUtils, RestApiUtil restApiUtil,
                                          CommonConverter commonConverter) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.commonConverter = commonConverter;
    }

    /**
     * <p>
     * Process and create mental health observation based on given details
     * </p>
     *
     * @param mentalHealthObservationDTO  The mental health details of the patient
     *
     * @return {@link Observation} Created FHIR Observation entity.
     */
    public Observation processMentalHealthDetails(MentalHealthObservationDTO mentalHealthObservationDTO) {
        String mentalHealthSearch = String.format(Constants.MENTAL_HEALTH_OBSERVATION_QUERY,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL + Constants.VERTICAL_BAR + Constants.OBSERVATION_MENTAL_HEALTH,
                FhirConstants.RELATED_PERSON + Constants.FORWARD_SLASH + mentalHealthObservationDTO.getRelatedPersonId());
        Bundle bundle = restApiUtil.getBatchRequest(mentalHealthSearch);
        Map<String, QuestionnaireResponse> existingMentalHealthDetails = new HashMap<>();
        Observation mentalHealthObservation = null;

        if (Objects.nonNull(bundle) && !bundle.getEntry().isEmpty()) {
            mentalHealthObservation = setMentalHealthDetails(bundle, existingMentalHealthDetails);
            mentalHealthObservationDTO.setExistingMentalHealthDetails(existingMentalHealthDetails);
        }
        return createOrUpdateMentalHealthObservation(mentalHealthObservationDTO, mentalHealthObservation);
    }

    /**
     * <p>
     * Create or update mental health observation based on given details
     * </p>
     *
     * @param mentalHealthDTO  The mental health details of the patient
     * @param observation      The FHIR Observation entity
     *
     * @return {@link Observation} Created FHIR Observation entity.
     */
    private Observation createOrUpdateMentalHealthObservation(MentalHealthObservationDTO mentalHealthDTO,
                                                              Observation observation) {
        if (Objects.isNull(observation)) {
            observation = new Observation();
            observation.setStatus(Observation.ObservationStatus.FINAL);
            observation.setCode(fhirUtils.setCodes(MetaCodeConstants.MENTAL_HEALTH_KEY));
            observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                    .setValue(Constants.OBSERVATION_MENTAL_HEALTH);
            if (Objects.nonNull(mentalHealthDTO.getRelatedPersonId())) {
                observation.addPerformer(new Reference(
                        String.format(FhirConstants.RELATED_PERSON_ID, mentalHealthDTO.getRelatedPersonId())));
            } else {
                observation.addPerformer(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
            }
        }
        observation.setEffective(new DateTimeType(new Date()));
        observation.setHasMember(new ArrayList<>());
        setPhq4MentalHealthReference(mentalHealthDTO, observation);
        setPhq9MentalHealthReference(mentalHealthDTO, observation);
        setGad7MentalHealthReference(mentalHealthDTO, observation);
        Map<String, String> existingMentalRiskDetails = getMentalHealthRiskDetails(observation);
        for (Map.Entry<String, String> entry: mentalHealthDTO.getMentalRiskDetails().entrySet()) {
            existingMentalRiskDetails.put(entry.getKey(), entry.getValue());
        }
        setMentalHealthRiskDetails(observation, existingMentalRiskDetails);
        commonConverter.setObservationText(observation, MetaCodeConstants.MENTAL_HEALTH_DETAILS);
        return observation;
    }

    /**
     * <p>
     * Sets phq4 details reference in mental health observation entity
     * </p>
     *
     * @param mentalHealthDTO  The mental health details of the patient
     * @param observation      The FHIR Observation entity
     */
    private void setPhq4MentalHealthReference(MentalHealthObservationDTO mentalHealthDTO,
                                           Observation observation) {
        if (mentalHealthDTO.getQuestionnaireResponses().containsKey(Constants.PHQ4)) {
             observation.addHasMember(new Reference(FhirConstants.PHQ4_QUESTIONNAIRERESPONSE_IDENTIFIER_URL));
        } else if (Objects.nonNull(mentalHealthDTO.getExistingMentalHealthDetails())
                && mentalHealthDTO.getExistingMentalHealthDetails().containsKey(Constants.PHQ4)) {
            observation.addHasMember(new Reference(String.format(FhirConstants.QUESTIONNAIRE_RESPONSE_ID,
                    mentalHealthDTO.getExistingMentalHealthDetails().get(Constants.PHQ4).getIdPart())));
        }
    }

    /**
     * <p>
     * Sets phq9 details reference in mental health observation entity
     * </p>
     *
     * @param mentalHealthDTO  The mental health details of the patient
     * @param observation      The FHIR Observation entity
     */
    private void setPhq9MentalHealthReference(MentalHealthObservationDTO mentalHealthDTO,
                                              Observation observation) {
        if (mentalHealthDTO.getQuestionnaireResponses().containsKey(Constants.PHQ9)) {
            observation.addHasMember(new Reference(FhirConstants.PHQ9_QUESTIONNAIRERESPONSE_IDENTIFIER_URL));
        } else if (Objects.nonNull(mentalHealthDTO.getExistingMentalHealthDetails())
                && mentalHealthDTO.getExistingMentalHealthDetails().containsKey(Constants.PHQ9)) {
            observation.addHasMember(new Reference(String.format(FhirConstants.QUESTIONNAIRE_RESPONSE_ID,
                    mentalHealthDTO.getExistingMentalHealthDetails().get(Constants.PHQ9).getIdPart())));
        }
    }

    /**
     * <p>
     * Sets gad7 details reference in mental health observation entity
     * </p>
     *
     * @param mentalHealthDTO  The mental health details of the patient
     * @param observation      The FHIR Observation entity
     */
    private void setGad7MentalHealthReference(MentalHealthObservationDTO mentalHealthDTO,
                                              Observation observation) {
        if (mentalHealthDTO.getQuestionnaireResponses().containsKey(Constants.GAD7)) {
            observation.addHasMember(new Reference(FhirConstants.GAD7_QUESTIONNAIRERESPONSE_IDENTIFIER_URL));
        } else if (Objects.nonNull(mentalHealthDTO.getExistingMentalHealthDetails())
                && mentalHealthDTO.getExistingMentalHealthDetails().containsKey(Constants.GAD7)) {
            observation.addHasMember(new Reference(String.format(FhirConstants.QUESTIONNAIRE_RESPONSE_ID,
                    mentalHealthDTO.getExistingMentalHealthDetails().get(Constants.GAD7).getIdPart())));
        }
    }

    /**
     * <p>
     * Sets patient mental health details from FHIR bundle entity
     * </p>
     *
     * @param bundle                   The FHIR Bundle entity
     * @param mentalHealthDetails      Map of patient phq4, phq9 and gad7 details
     */
    private Observation setMentalHealthDetails(Bundle bundle,
                                        Map<String, QuestionnaireResponse> mentalHealthDetails) {
        Observation mentalHealthObservation = null;
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (entry.getResource() instanceof QuestionnaireResponse questionnaireResponse) {
                if (questionnaireResponse.getIdentifier().getValue().equals(Constants.PHQ4)) {
                    mentalHealthDetails.put(Constants.PHQ4, questionnaireResponse);
                } else if (questionnaireResponse.getIdentifier().getValue().equals(Constants.PHQ9)) {
                    mentalHealthDetails.put(Constants.PHQ9, questionnaireResponse);
                } else if (questionnaireResponse.getIdentifier().getValue().equals(Constants.GAD7)) {
                    mentalHealthDetails.put(Constants.GAD7, questionnaireResponse);
                }
            } else if (entry.getResource() instanceof Observation observation) {
                mentalHealthObservation = observation;
            }
        }
        return mentalHealthObservation;
    }

    /**
     * Converts to FHIR Questionnaire Response entity based on given PHQ4
     * questions and answers
     *
     * @param mentalHealthDTO     The mental health details to convert
     * @param date        The mental health questions recorded time
     *
     * @return Converted FHIR Questionnaire Response entity.
     */
    public QuestionnaireResponse createMentalHealthQuestionnaireResponse(MentalHealthDTO mentalHealthDTO, Date date, String type) {
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();
        if (Objects.nonNull(mentalHealthDTO.getQuestionnaireId())) {
            Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.QUESTIONNAIRE_RESPONSE_ID, mentalHealthDTO.getQuestionnaireId(), type));
            if (Objects.nonNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
                for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                    if (entry.getResource() instanceof QuestionnaireResponse response) {
                        questionnaireResponse = response;
                        questionnaireResponse.setItem(new ArrayList<>());
                    }
                }
            }
        } else {
            questionnaireResponse.setStatus(QuestionnaireResponse.QuestionnaireResponseStatus.COMPLETED);
            questionnaireResponse.setIdentifier(new Identifier().setSystem(FhirIdentifierConstants.MENTAL_HEALTH_SYSTEM_URL).setValue(
                    type));
        }
        questionnaireResponse.setAuthoredElement(new DateTimeType(date));
        for (MentalHealthDetailsDTO mentalHealthDetailsDTO : mentalHealthDTO.getMentalHealthDetails()) {
            String question = mentalHealthDetailsDTO.getQuestion();
            questionnaireResponse.addItem()
                    .setText(question)
                    .addAnswer(new QuestionnaireResponse.QuestionnaireResponseItemAnswerComponent()
                            .setValue(new StringType(mentalHealthDetailsDTO.getAnswer())));
        }
        return questionnaireResponse;
    }

    /**
     * Creates and populates a QuestionnaireResponse object for an HIV questionnaire based on input data.
     * This method takes a map of request parameters and updates the provided QuestionnaireResponse object with relevant values.
     *
     * @param request a Map of key-value pairs where each entry represents a question identifier and its corresponding answer.
     *        This data will be used to populate the QuestionnaireResponse with specific answers for the HIV questionnaire.
     * @param questionnaireResponse an existing QuestionnaireResponse object that will be updated with the data from the request map.
     *        This object represents the response structure for the HIV questionnaire.
     * @return a populated QuestionnaireResponse object containing the responses from the request map, suitable for inclusion in a FHIR Bundle.
     */
    public QuestionnaireResponse createHIVQuestionnaireResponse(Map<String, String> request, QuestionnaireResponse questionnaireResponse) {
        if (!request.isEmpty()) {
            for (Map.Entry<String, String> entry : request.entrySet()) {
                String question = entry.getKey();
                questionnaireResponse.addItem()
                        .setText(question)
                        .addAnswer(new QuestionnaireResponse.QuestionnaireResponseItemAnswerComponent()
                                .setValue(new StringType(entry.getValue())));
            }
        }
        return questionnaireResponse;
    }

    /**
     * Creates a basic QuestionnaireResponse by setting foundational information such as the date and type.
     * This method initializes or updates the provided QuestionnaireResponse with essential metadata.
     *
     * @param questionnaireResponse the QuestionnaireResponse object that will be populated with basic information.
     *        This object represents a general response structure for a questionnaire.
     * @param date the Date object representing when the questionnaire response was completed or recorded.
     *        This value is set in the response to indicate its completion date.
     * @param type a String indicating the type or category of the questionnaire (e.g., "HIV", "General Health").
     *        This value is used to identify or categorize the questionnaire response.
     * @return the updated QuestionnaireResponse with basic details, which can then be further populated with specific responses.
     */
    public QuestionnaireResponse createBasicQuestionnaireResponse(QuestionnaireResponse questionnaireResponse, Date date, String type) {
        questionnaireResponse.setStatus(QuestionnaireResponse.QuestionnaireResponseStatus.COMPLETED);
        questionnaireResponse.setAuthoredElement(new DateTimeType(date));
        questionnaireResponse.setIdentifier(new Identifier().setSystem(FhirIdentifierConstants.HIV_SYSTEM_URL).setValue(
                type));
        return questionnaireResponse;
    }

    /**
     * Set Questionnaire Response source details using patient
     * and related person
     *
     * @param questionnaireResponse     Fhir Questionnaire Response entity
     * @param patient                   Fhir Patient entity
     * @param relatedPerson             Fhir Related Person entity
     *
     */
    public void setReference(QuestionnaireResponse questionnaireResponse, Patient patient,
            RelatedPerson relatedPerson) {
        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getIdPart())) {
            questionnaireResponse.setSource(
                new Reference(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart())));
        } else if (Objects.nonNull(relatedPerson)) {
            questionnaireResponse.setSource(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
        }
        if(Objects.nonNull(patient)  && Objects.nonNull(patient.getIdPart())) {
            questionnaireResponse.setSubject(
                    new Reference(String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
        } else if (Objects.nonNull(patient)) {
            questionnaireResponse.setSubject(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));

        }
    }

    /**
     * <p>
     * Get patient mental health risk details from Fhir Observation entity
     * </p>
     *
     * @param observation  Fhir Observation Response entity
     *
     */
    public Map<String, String> getMentalHealthRiskDetails(Observation observation) {
        Map<String, String> mentalRiskDetails = new HashMap<>();
        if (!observation.getComponent().isEmpty()) {
            for (Observation.ObservationComponentComponent component: observation.getComponent()) {
                mentalRiskDetails.put(component.getCode().getText(), component.getValueStringType().getValue());
            }
        }
        return mentalRiskDetails;
    }

    /**
     * <p>
     * Set patient mental health risk details in Fhir observation entity
     * </p>
     *
     * @param observation         Fhir Observation Response entity
     * @param mentalRiskDetails   Map of patient mental risk details
     *
     */
    public void setMentalHealthRiskDetails(Observation observation, Map<String, String> mentalRiskDetails) {
        observation.setComponent(new ArrayList<>());
        for (Map.Entry<String, String> entry : mentalRiskDetails.entrySet()) {
            Observation.ObservationComponentComponent
                    riskComponent = new Observation.ObservationComponentComponent();
            riskComponent.getCode().setText(entry.getKey());
            riskComponent.setValue(new StringType(entry.getValue()));
            observation.addComponent(riskComponent);
        }
    }
}
