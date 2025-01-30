package com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.QuestionnaireResponseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.MentalHealthService;

/**
 * <p>
 * This class implements the mental health service class and contains business logic for the operations of mental health
 * entity.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 07, 2023
 */
@Service
public class MentalHealthServiceImpl implements MentalHealthService {

    private final RestApiUtil restApiUtil;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final QuestionnaireResponseConverter questionnaireResponseConverter;
    private final EncounterConverter encounterConverter;
    private final CommonConverter commonConverter;
    private final VitalSignsConverter vitalSignsConverter;

    public final FhirUtils fhirUtils;

    private final AssessmentService assessmentService;

    public MentalHealthServiceImpl(QuestionnaireResponseConverter questionnaireResponseConverter,
            EncounterConverter encounterConverter, CommonConverter commonConverter, RestApiUtil restApiUtil,
            VitalSignsConverter vitalSignsConverter, FhirUtils fhirUtils, @Lazy AssessmentService assessmentService) {
        this.questionnaireResponseConverter = questionnaireResponseConverter;
        this.encounterConverter = encounterConverter;
        this.commonConverter = commonConverter;
        this.restApiUtil = restApiUtil;
        this.vitalSignsConverter = vitalSignsConverter;
        this.fhirUtils = fhirUtils;
        this.assessmentService = assessmentService;
    }

    /**
     * {@inheritDoc}
     */
    public void createMentalHealth(AssessmentDTO mentalHealth) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url = String.format(Constants.GET_MEMBER_ID, mentalHealth.getMemberReference());
        Bundle relatedPersonBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();
        Patient patient = Objects.isNull(mentalHealth.getPatientId())
                ? null
                : restApiUtil.getPatientById(
                        StringUtil.concatString(fhirServerUrl, FhirConstants.PATIENT, Constants.FORWARD_SLASH,
                                mentalHealth.getPatientId()));
        if (Objects.nonNull(patient) || Objects.nonNull(relatedPerson)) {
            Encounter encounter;
            if (Objects.nonNull(mentalHealth.getEncounter()) && Objects.nonNull(mentalHealth.getEncounter().getId())) {
                encounter = restApiUtil.getEncounterById(mentalHealth.getEncounter().getId());
            } else {
                encounter = encounterConverter.createEncounter(patient, relatedPerson, null, null,
                        mentalHealth.getAssessmentTakenOn());
                encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL)
                        .setValue(Constants.ENCOUNTER_TYPE_MENTAL_HEALTH);
                commonConverter.setEncounterDetailsInBundle(bundle, encounter, FhirConstants.ENCOUNTER_IDENTIFIER_URL, mentalHealth.getEncounter().getProvenance());
            }
            Map<String, QuestionnaireResponse> questionnaireResponses = createQuestionnaireResponse(mentalHealth,
                    patient, relatedPerson, encounter, bundle, mentalHealth.getEncounter().getProvenance());
            Observation mentalHealthObservation = createMentalHealthObservation(mentalHealth, relatedPerson,
                    questionnaireResponses, encounter);
            setObservationDetails(bundle, patient, null, mentalHealthObservation,
                    FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL, mentalHealth.getEncounter().getProvenance());
            setVitalSigns(patient, relatedPerson, bundle, mentalHealthObservation, mentalHealth.getEncounter().getProvenance());
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        }
    }

    /**
     * <p>
     * This method is used to set Vital Sign information of Mental health.
     * </p>
     *
     * @param patient                {@link Patient} - Patient details
     * @param relatedPerson          {@link RelatedPerson} - Related person details
     * @param bundle                 {@link Bundle} - Bundle Object with all entities
     */
    private void setVitalSigns(Patient patient, RelatedPerson relatedPerson, Bundle bundle,
            Observation mentalHealthObservation, ProvenanceDTO provenanceDTO) {
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(relatedPerson.getIdPart());
        vitalSignsDTO.setMentalHealthObservation(mentalHealthObservation);
        Observation vitalSignsObservation = vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO, bundle);
        commonConverter.setObservationText(vitalSignsObservation, FhirConstants.VITAL_SIGNS);
        setObservationDetails(bundle, patient, null, vitalSignsObservation,
                FhirConstants.VITAL_SIGNS_IDENTIFIER_URL, provenanceDTO);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, QuestionnaireResponse> createQuestionnaireResponse(AssessmentDTO requestDTO, Patient patient,
            RelatedPerson relatedPerson, Encounter encounter, Bundle bundle, ProvenanceDTO provenanceDTO) {
        Map<String, QuestionnaireResponse> questionnaireResponses = new HashMap<>();
        QuestionnaireResponse questionnaireResponse = null;
        if (Objects.nonNull(requestDTO.getPhq4())) {
            questionnaireResponse = questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(
                    requestDTO.getPhq4(), requestDTO.getAssessmentTakenOn(), Constants.PHQ4);
            questionnaireResponseConverter.setReference(questionnaireResponse, patient, relatedPerson);
            commonConverter.setQuestionnaireResponseReference(questionnaireResponse, null, encounter);
            questionnaireResponse.setAuthor(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    requestDTO.getEncounter().getProvenance().getOrganizationId())));
            boolean isUpdate = Objects.nonNull(questionnaireResponse.getIdPart()) ? Boolean.TRUE : Boolean.FALSE;
            commonConverter.setQuestionnarieDetailsInBundle(bundle, questionnaireResponse,
                    FhirConstants.PHQ4_QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO, isUpdate);
            questionnaireResponses.put(Constants.PHQ4, questionnaireResponse);
        }

        if (Objects.nonNull(requestDTO.getPhq9())) {
            questionnaireResponse = questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(
                    requestDTO.getPhq9(), requestDTO.getAssessmentTakenOn(), Constants.PHQ9);
            questionnaireResponseConverter.setReference(questionnaireResponse, patient, relatedPerson);
            commonConverter.setQuestionnaireResponseReference(questionnaireResponse, null, encounter);
            questionnaireResponse.setAuthor(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    requestDTO.getEncounter().getProvenance().getOrganizationId())));
            boolean isUpdate = Objects.nonNull(questionnaireResponse.getIdPart()) ? Boolean.TRUE : Boolean.FALSE;
            commonConverter.setQuestionnarieDetailsInBundle(bundle, questionnaireResponse,
                    FhirConstants.PHQ9_QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO, isUpdate);
            questionnaireResponses.put(Constants.PHQ9, questionnaireResponse);
        }

        if (Objects.nonNull(requestDTO.getGad7())) {
            questionnaireResponse = questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(
                    requestDTO.getGad7(), requestDTO.getAssessmentTakenOn(), Constants.GAD7);
            questionnaireResponseConverter.setReference(questionnaireResponse, patient, relatedPerson);
            commonConverter.setQuestionnaireResponseReference(questionnaireResponse, null, encounter);
            questionnaireResponse.setAuthor(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    requestDTO.getEncounter().getProvenance().getOrganizationId())));
            boolean isUpdate = Objects.nonNull(questionnaireResponse.getIdPart()) ? Boolean.TRUE : Boolean.FALSE;

            commonConverter.setQuestionnarieDetailsInBundle(bundle, questionnaireResponse,
                    FhirConstants.GAD7_QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO, isUpdate);
            questionnaireResponses.put(Constants.GAD7, questionnaireResponse);
        }
        return questionnaireResponses;
    }

    /**
     * <p>
     * This method is used to create patient mental health details observation entity.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} - Request Object with mental health details
     * @param relatedPerson {@link RelatedPerson} - The Fhir RelatedPerson entity
     * @param questionnaireResponses
     * {@link Map<String, QuestionnaireResponse>} - Map of Fhir QuestionnaireResponse entity
     *
     * @return A {@link Observation} Entity with Risk details.
     */
    private Observation createMentalHealthObservation(AssessmentDTO assessmentDTO, RelatedPerson relatedPerson,
                                                     Map<String, QuestionnaireResponse> questionnaireResponses, Encounter encounter) {
        MentalHealthObservationDTO mentalHealthObservationDTO = new MentalHealthObservationDTO();
        mentalHealthObservationDTO.setRelatedPersonId(relatedPerson.getIdPart());
        mentalHealthObservationDTO.setQuestionnaireResponses(questionnaireResponses);
        Map<String, String> mentalRiskDetails = new HashMap<>();
        Observation mentalHealthObservation = null;

        if (Objects.nonNull(assessmentDTO.getPhq4())) {
            mentalRiskDetails
                    .put(FhirConstants.PHQ4_SCORE, String.valueOf(assessmentDTO.getPhq4().getScore()));
            mentalRiskDetails
                    .put(FhirConstants.PHQ4_RISK_LEVEL, assessmentDTO.getPhq4().getRiskLevel());
        }
        if (Objects.nonNull(assessmentDTO.getPhq9())) {
            mentalRiskDetails
                    .put(FhirConstants.PHQ9_SCORE, String.valueOf(assessmentDTO.getPhq9().getScore()));
            mentalRiskDetails
                    .put(FhirConstants.PHQ9_RISK_LEVEL, assessmentDTO.getPhq9().getRiskLevel());
        }
        if (Objects.nonNull(assessmentDTO.getGad7())) {
            mentalRiskDetails
                    .put(FhirConstants.GAD7_SCORE, String.valueOf(assessmentDTO.getGad7().getScore()));
            mentalRiskDetails
                    .put(FhirConstants.GAD7_RISK_LEVEL, assessmentDTO.getGad7().getRiskLevel());
        }
        if (!mentalRiskDetails.isEmpty()) {
            mentalHealthObservationDTO.setMentalRiskDetails(mentalRiskDetails);
            mentalHealthObservation = questionnaireResponseConverter
                    .processMentalHealthDetails(mentalHealthObservationDTO, encounter, null);
        }
        return mentalHealthObservation;
    }

    /**
     * Set observation details in FHIR bundle resource
     *
     * @param bundle        {@link Bundle} -   The FHIR bundle resource
     * @param patient       {@link Patient} -  The FHIR Patient entity
     * @param relatedPerson {@link RelatedPerson} - The FHIR Related Person entity
     * @param observation   {@link Observation} - The FHIR Observation entity
     * @param identifierUrl {@link String} The Observation identifier url
     */
    private void setObservationDetails(Bundle bundle, Patient patient, RelatedPerson relatedPerson,
            Observation observation, String identifierUrl, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(observation)) {
            commonConverter.setObservationReference(observation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, observation, identifierUrl, provenanceDTO);
        }
    }

    /**
     * {@inheritDoc}
     */
    public MentalHealthDTO getMentalHealthDetails(RequestDTO request) {
        MentalHealthDTO mentalHealthDTO = new MentalHealthDTO();
        mentalHealthDTO.setMentalHealthDetails(new ArrayList<>());
        if (Objects.isNull(request.getMemberReference())) {
            throw new BadRequestException(2004);
        }
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.QUESTIONNAIRE_RESPONSE_BY_MEMBER_ID, request.getMemberReference(), request.getType()));
        if (Objects.nonNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
            for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                if (entry.getResource() instanceof QuestionnaireResponse response) {
                    mentalHealthDTO.setQuestionnaireId(response.getIdPart());
                    mentalHealthDTO.setEncounterId(fhirUtils.getIdFromReference(response.getEncounter().getReference()));
                    if (response.hasItem()) {
                        response.getItem().forEach(
                                item -> {
                                    MentalHealthDetailsDTO mentalHealthDetailsDTO = new MentalHealthDetailsDTO();
                                    mentalHealthDetailsDTO.setQuestion(item.getText());
                                    if (item.hasAnswer() && Objects.nonNull(item.getAnswer().getFirst())
                                            && Objects.nonNull(item.getAnswer().getFirst().getValue())) {
                                        mentalHealthDetailsDTO.setAnswer(item.getAnswer().getFirst().getValue().toString());
                                    }
                                    mentalHealthDTO.getMentalHealthDetails().add(mentalHealthDetailsDTO);
                                }
                        );

                    }
                }
            }
        }
        return mentalHealthDTO;
    }

    /**
     * {@inheritDoc}
     */
    public void createMentalHealthCondition(AssessmentDTO mentalHealth) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url = String.format(Constants.GET_MEMBER_ID, mentalHealth.getMemberReference());
        Bundle relatedPersonBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();
        Patient patient = Objects.isNull(mentalHealth.getPatientId())
                ? null
                : restApiUtil.getPatientById(
                StringUtil.concatString(fhirServerUrl, FhirConstants.PATIENT, Constants.FORWARD_SLASH,
                        mentalHealth.getPatientId()));
        if (Objects.nonNull(patient) || Objects.nonNull(relatedPerson)) {
            Encounter encounter;
            if (Objects.nonNull(mentalHealth.getEncounter()) && Objects.nonNull(mentalHealth.getEncounter().getId())) {
                encounter = restApiUtil.getEncounterById(mentalHealth.getEncounter().getId());
            } else {
                encounter = encounterConverter.createEncounter(patient, relatedPerson, null, null,
                        mentalHealth.getAssessmentTakenOn());
                encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL)
                        .setValue(Constants.ENCOUNTER_TYPE_MENTAL_HEALTH);
                commonConverter.setEncounterDetailsInBundle(bundle, encounter, FhirConstants.ENCOUNTER_IDENTIFIER_URL, mentalHealth.getEncounter().getProvenance());
            }
            Observation substanceAbuse = assessmentService.createSubstanceAbuseObservation(mentalHealth, encounter);
            Observation suicideScreener = assessmentService.createSuicideObservation(mentalHealth, encounter);
            assessmentService.setObservationDetails(bundle, suicideScreener, FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL,
                    mentalHealth, patient, relatedPerson, mentalHealth.getEncounter().getProvenance());
            assessmentService.setObservationDetails(bundle, substanceAbuse, FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL,
                    mentalHealth, patient, relatedPerson, mentalHealth.getEncounter().getProvenance());

            if ((Objects.nonNull(suicideScreener) && Objects.isNull(suicideScreener.getIdPart())) || (Objects.nonNull(substanceAbuse) && Objects.isNull(substanceAbuse.getIdPart()))) {
                VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
                vitalSignsDTO.setRelatedPersonId(relatedPerson.getIdPart());
                vitalSignsDTO.setSuicideObservation(suicideScreener);
                vitalSignsDTO.setSubstanceAbuseObservation(substanceAbuse);
                Observation vitalSignsObservation = vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO, bundle);
                commonConverter.setObservationText(vitalSignsObservation, FhirConstants.VITAL_SIGNS);
                setObservationDetails(bundle, patient, null, vitalSignsObservation,
                        FhirConstants.VITAL_SIGNS_IDENTIFIER_URL, mentalHealth.getEncounter().getProvenance());
            }
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        }
    }


    /**
     * {@inheritDoc}
     */
    public AssessmentDTO getMentalHealthCondition(AssessmentDTO request) {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        if (Objects.isNull(request.getMemberReference())) {
            throw new BadRequestException(2004);
        }
        String url = String.format(Constants.GET_LATEST_OBSERVATION, request.getType(), StringUtil.concatString(FhirConstants.RELATED_PERSON, Constants.FORWARD_SLASH, request.getMemberReference()));
        Bundle observationBundle = restApiUtil.getBatchRequest(url);
        if (Objects.nonNull(observationBundle.getEntry()) && !observationBundle.getEntry().isEmpty()) {
            for (Bundle.BundleEntryComponent entry : observationBundle.getEntry()) {
                if (entry.getResource() instanceof Observation response) {
                    Map<String, String> responseMap = new HashMap<>();
                    responseMap.put(Constants.OBSERVATION_ID, response.getIdPart());
                    EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
                    encounterDetailsDTO.setId(fhirUtils.getIdFromReference(response.getEncounter().getReference()));
                    assessmentDTO.setEncounter(encounterDetailsDTO);
                    if (response.hasComponent()) {
                        response.getComponent().forEach(component -> {
                            if (Objects.nonNull(component.getCode().getText())) {
                                responseMap.put(component.getCode().getText(), component.getValueCodeableConcept().getText());
                            }
                        });
                    }

                    if (Objects.equals(MetaCodeConstants.SUICIDE_SCREENER_KEY, request.getType())) {
                        assessmentDTO.setSuicideScreener(responseMap);
                    } else if (Objects.equals(MetaCodeConstants.SUBSTANCE_ABUSE_KEY, request.getType())) {
                        assessmentDTO.setSubstanceAbuse(responseMap);
                    }
                }
            }
        }
        return assessmentDTO;
    }
}
