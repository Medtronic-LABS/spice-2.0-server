package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import javax.xml.bind.DatatypeConverter;

import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO.DataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RiskDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

/**
 * <p>
 * This common converter helps to convert common FHIR entity's like component
 * codeableConcept, dateTimeType and value quantity.
 * Helps to add all FHIR entity's in specified FHIR Bundle resource
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class CommonConverter {

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final PatientStatusConverter patientStatusConverter;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    @Autowired
    public CommonConverter(FhirUtils fhirUtils, RestApiUtil restApiUtil,
                           PatientStatusConverter patientStatusConverter,
                           FhirAssessmentMapper fhirAssessmentMapper) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.patientStatusConverter = patientStatusConverter;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
    }

    /**
     * Converts to FHIR Basic Observations like height, weight, bmi based on
     * given bio data and biometrics details
     *
     * @param bioMetricsDTO The biometrics details of the patient.
     * @param date          The observation recorded date.
     * @param type          type of the observation.
     * @return Converted FHIR Observation entity.
     */
    public Observation createBasicObservation(BioMetricsDTO bioMetricsDTO,
                                              Date date, String type) {
        Observation observation = new Observation();
        observation.setStatus(Observation.ObservationStatus.FINAL);

        switch (type) {
            case FhirConstants.HEIGHT -> {
                observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                        .setValue(Constants.OBSERVATION_HEIGHT);
                observation.setCode(fhirUtils.setCodes(MetaCodeConstants.HEIGHT_KEY));
                Quantity heightQuantity = new Quantity();
                heightQuantity.setValue(bioMetricsDTO.getHeight());
                heightQuantity.setUnit(FhirConstants.CM_CODE);
                observation.setValue(heightQuantity);
            }
            case FhirConstants.WEIGHT -> {
                observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                        .setValue(Constants.OBSERVATION_WEIGHT);
                observation.setCode(fhirUtils.setCodes(MetaCodeConstants.WEIGHT_KEY));
                Quantity weightQuantity = new Quantity();
                weightQuantity.setValue(bioMetricsDTO.getWeight());
                weightQuantity.setUnit(FhirConstants.KG_CODE);
                observation.setValue(weightQuantity);
            }
            case FhirConstants.BMI -> {
                observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                        .setValue(Constants.OBSERVATION_BMI);
                observation.setCode(fhirUtils.setCodes(MetaCodeConstants.BMI_KEY));
                Quantity bmiQuantity = new Quantity();
                bmiQuantity.setValue(bioMetricsDTO.getBmi());
                bmiQuantity.setUnit(FhirConstants.KG_PER_M2_CODE);
                observation.setValue(bmiQuantity);

                if (Objects.nonNull(bioMetricsDTO.getBmiCategory())) {
                    Observation.ObservationComponentComponent
                            bmiComponent = new Observation.ObservationComponentComponent();
                    bmiComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.BMI_CATEGORY_KEY));
                    bmiComponent.setValue(new StringType(bioMetricsDTO.getBmiCategory()));
                    observation.addComponent(bmiComponent);
                }
            }
            default -> Logger.logInfo(type);
        }

        if (Objects.nonNull(date)) {
            observation.setEffective(new DateTimeType(date));
        } else {
            observation.setEffective(new DateTimeType(new Date()));
        }
        return observation;
    }

    /**
     * Converts to FHIR Basic Observations like weight, height and temperature.
     *
     * @param value         The value of the basic details like weight, height and temperature.
     * @param createdAt     details taken date from the patient
     * @param metaCode      metadata code
     * @param quantityCode  quantity code
     * @param narrativeCode narrative code
     * @return Converted FHIR Observation entity.
     */
    public Observation createBasicObservation(Double value, Date createdAt, String metaCode,
                                              String quantityCode, String narrativeCode, String type) {
        Observation observation = new Observation();
        observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(type);
        observation.setText(getNarrative(narrativeCode));
        observation.setStatus(Observation.ObservationStatus.FINAL);
        observation.setCode(fhirUtils.setCodes(metaCode));
        Quantity quantity = new Quantity();
        quantity.setValue(value);
        quantity.setUnit(quantityCode);
        observation.setValue(quantity);
        if (Objects.nonNull(createdAt)) {
            observation.setEffective(new DateTimeType(createdAt));
        } else {
            observation.setEffective(new DateTimeType(new Date()));
        }
        return observation;
    }

    /**
     * Converts to FHIR Observation based on given regular smoker details
     * Create regular smoker observation entity
     *
     * @param date The observation recorded date.
     * @return Converted FHIR Observation entity.
     */
    public Observation createRegularSmokerObservation(Boolean isRegularSmoker, Date date) {
        Observation observation = new Observation();
        observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).
                setValue(Constants.OBSERVATION_REGULAR_SMOKER);
        observation.setStatus(Observation.ObservationStatus.FINAL);
        observation.setCode(fhirUtils.setCodes(MetaCodeConstants.REGULAR_SMOKER_KEY));
        observation.getValueCodeableConcept().addCoding()
                .setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                .setCode(Boolean.TRUE.equals(isRegularSmoker) ? Constants.YES_CODE : Constants.NO_CODE)
                .setDisplay(Boolean.TRUE.equals(isRegularSmoker) ? Constants.YES : Constants.NO);
        if (Objects.nonNull(date)) {
            observation.setEffective(new DateTimeType(date));
        } else {
            observation.setEffective(new DateTimeType(new Date()));
        }
        return observation;
    }

    /**
     * <p>
     * Converts to FHIR Observation based on given patient risk details
     * </p>
     *
     * @param riskDetailsRequestDTO The patient risk details to convert.
     * @param date                  The observation recorded date.
     * @return Converted FHIR Observation entity.
     */
    public Observation createRiskDetailsObservation(
            RiskDetailsRequestDTO riskDetailsRequestDTO,
            Date date, String key) {
        Observation riskObservation = new Observation();
        riskObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_RISK_LEVEL);
        riskObservation.setStatus(Observation.ObservationStatus.FINAL);
        riskObservation.setCode(fhirUtils.setCodes(key));

        for (Map.Entry<String, String> entry :
                riskDetailsRequestDTO.getRiskDetails().entrySet()) {
            Observation.ObservationComponentComponent
                    riskComponent = new Observation.ObservationComponentComponent();
            riskComponent.getCode().setText(entry.getKey());
            riskComponent.setValue(new StringType(entry.getValue()));
            riskObservation.addComponent(riskComponent);
        }
        if (Objects.nonNull(date)) {
            riskObservation.setEffective(new DateTimeType(date));
        } else {
            riskObservation.setEffective(new DateTimeType(new Date()));
        }
        return riskObservation;
    }

    /**
     * Converts to FHIR Observation based on given suicide screener details
     *
     * @param suicideScreener The suicide screener questions and answers.
     * @param date            The observation recorded date.
     * @return Converted FHIR Observation entity.
     */
    public Observation createSuicideScreenerObservation(
            Map<String, String> suicideScreener,
            Date date) {
        Observation suicideScreenerObservation = new Observation();
        suicideScreenerObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_SUICIDE_SCREENER);
        suicideScreenerObservation.setStatus(Observation.ObservationStatus.FINAL);
        suicideScreenerObservation.setCode(
                fhirUtils.setCodes(MetaCodeConstants.SUICIDE_SCREENER_KEY));

        for (Map.Entry<String, String> entry : suicideScreener.entrySet()) {
            setSuicideOrSubstanceComponent(suicideScreenerObservation, entry);
        }
        if (Objects.nonNull(date)) {
            suicideScreenerObservation.setEffective(new DateTimeType(date));
        } else {
            suicideScreenerObservation.setEffective(new DateTimeType(new Date()));
        }
        return suicideScreenerObservation;
    }

    /**
     * Converts to FHIR Observation based on given substance abuse details
     *
     * @param substanceAbuse The substance abuse questions and answers.
     * @param cageAid        The used substance id
     * @param date           The observation recorded date.
     * @return Converted FHIR Observation entity.
     */
    public Observation createSubstanceAbuseObservation(
            Map<String, String> substanceAbuse,
            Double cageAid,
            Date date) {
        Observation substanceAbuseObservation = new Observation();
        substanceAbuseObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_SUBSTANCE_ABUSE);
        substanceAbuseObservation.setStatus(Observation.ObservationStatus.FINAL);
        substanceAbuseObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.SUBSTANCE_ABUSE_KEY));
        substanceAbuseObservation.setValue(new StringType(cageAid.toString()));

        for (Map.Entry<String, String> entry : substanceAbuse.entrySet()) {
            setSuicideOrSubstanceComponent(substanceAbuseObservation, entry);
        }
        if (Objects.nonNull(date)) {
            substanceAbuseObservation.setEffective(new DateTimeType(date));
        } else {
            substanceAbuseObservation.setEffective(new DateTimeType(new Date()));
        }
        return substanceAbuseObservation;
    }

    /**
     * Sets patient managing organization details based on given organization
     *
     * @param patient      The FHIR Patient entity.
     * @param organization The FHIR Organization entity.
     */
    public void setPatientOrganization(Patient patient, Organization organization) {
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getIdPart())) {
            patient.setManagingOrganization(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID, organization.getIdPart())));
        }
    }

    /**
     * Sets encounter organization details based on given organization
     *
     * @param encounter    The FHIR Encounter entity.
     * @param organization The FHIR Organization entity.
     */
    public void setEncounterOrganization(Encounter encounter, Organization organization) {
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getIdPart())) {
            encounter.setServiceProvider(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID, organization.getIdPart())));
        }
    }

    /**
     * Sets location organization details based on given organization
     *
     * @param location     The FHIR Location entity.
     * @param organization The FHIR Organization entity.
     */
    public void setLocationOrganization(Location location, Organization organization) {
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getIdPart())) {
            location.setManagingOrganization(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID, organization.getIdPart())));
        }
    }

    /**
     * Sets Questionnaire Response organization and encounter details based on given organization
     *
     * @param questionnaireResponse The FHIR Questionnaire Response entity.
     * @param organization          The FHIR Organization entity.
     * @param encounter             The FHIR Encounter entity.
     */
    public void setQuestionnaireResponseReference(QuestionnaireResponse questionnaireResponse,
                                                  Organization organization, Encounter encounter) {
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getIdPart())) {
            questionnaireResponse.setAuthor(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID, organization.getIdPart())));
        }
        if (Objects.nonNull(encounter) && Objects.nonNull(encounter.getIdPart())) {
            questionnaireResponse.setEncounter(new Reference(
                    String.format(FhirConstants.ENCOUNTER_ID, encounter.getIdPart())));
        } else if (Objects.nonNull(encounter)) {
            questionnaireResponse.setEncounter(new Reference(FhirConstants.ENCOUNTER_IDENTIFIER_URL));
        }
    }

    /**
     * Sets Observation encounter & organization details based on given organization
     * & encounter details
     *
     * @param observation  The FHIR Questionnaire Response entity.
     * @param organization The FHIR Organization entity.
     * @param encounter    The FHIR Encounter entity.
     */
    public void setObservationEncounterAndOrganization(Observation observation, Organization organization,
                                                       Encounter encounter) {
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getIdPart())) {
            observation.addPerformer(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID, organization.getIdPart())));
        }

        if (Objects.nonNull(encounter) && Objects.nonNull(encounter.getIdPart())) {
            observation.setEncounter(new Reference(
                    String.format(FhirConstants.ENCOUNTER_IDENTIFIER_URL, encounter.getIdPart())));
        } else if (Objects.nonNull(encounter)) {
            observation.setEncounter(new Reference(FhirConstants.ENCOUNTER_IDENTIFIER_URL));
        }
    }

    /**
     * Sets Observation reference details based on given patient
     * & related person details
     *
     * @param observation   The FHIR Observation entity.
     * @param patient       The FHIR Patient entity.
     * @param relatedPerson The FHIR Related Person entity.
     */
    public void setObservationReference(Observation observation, Patient patient,
                                        RelatedPerson relatedPerson) {
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            observation.setSubject(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));

            if (Objects.nonNull(observation.getPerformer()) && !observation.getPerformer().stream().map(Reference::getReference).toList().contains(StringUtil.concatString(FhirConstants.PATIENT, Constants.FORWARD_SLASH, patient.getIdPart()))) {

                observation.addPerformer(new Reference(
                        String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
            }
        } else if (Objects.nonNull(patient)) {
            observation.setSubject(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
            observation.addPerformer(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
        }

        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getIdPart())) {
            if (!observation.getPerformer().stream().map(Reference::getReference).toList().contains(StringUtil.concatString(FhirConstants.RELATED_PERSON, Constants.FORWARD_SLASH, relatedPerson.getIdPart()))) {
                observation.addPerformer(new Reference(
                        String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart())));
            }
        } else if (Objects.nonNull(relatedPerson)) {
            observation.addPerformer(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
        }
    }

    /**
     * Sets Service request reference details based on given patient
     * & related person details
     *
     * @param serviceRequest The FHIR Service Request entity.
     * @param patient        The FHIR Patient entity.
     * @param relatedPerson  The FHIR Related Person entity.
     */
    public void setServiceRequestReference(ServiceRequest serviceRequest, Patient patient,
                                           RelatedPerson relatedPerson, Encounter encounter) {
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            serviceRequest.setSubject(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
        } else if (Objects.nonNull(patient)) {
            serviceRequest.setSubject(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
        }
        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getIdPart())) {
            serviceRequest.addPerformer(new Reference(
                    String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart())));
        } else if (Objects.nonNull(relatedPerson)) {
            serviceRequest.addPerformer(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
        }
        if (Objects.nonNull(encounter) && Objects.nonNull(encounter.getIdPart())) {
            serviceRequest.setEncounter(new Reference(
                    String.format(FhirConstants.ENCOUNTER_IDENTIFIER_URL, encounter.getIdPart())));
        } else if (Objects.nonNull(encounter)) {
            serviceRequest.setEncounter(new Reference(FhirConstants.ENCOUNTER_IDENTIFIER_URL));
        }
    }

    /**
     * Sets Observation details in given bundle using
     * observation identifier
     *
     * @param bundle        The FHIR bundle entity.
     * @param observation   The FHIR Observation entity.
     * @param identifier    The Observation identifier.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    public void setObservationDetailsInBundle(Bundle bundle, Observation observation,
                                              String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(observation) && Objects.nonNull(observation.getIdPart())) {
            fhirUtils.setBundle(String.format(FhirConstants.OBSERVATION_ID, observation.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, observation.getIdPart()),
                    Bundle.HTTPVerb.PUT, observation, bundle, provenanceDTO);
        } else if (Objects.nonNull(observation)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Observation),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    observation, bundle, provenanceDTO);
        }
    }

    /**
     * <p>
     * Sets Service Request details in given bundle using
     * service request identifier
     * </p>
     *
     * @param bundle         The FHIR bundle entity.
     * @param serviceRequest The FHIR Service Request entity.
     * @param identifier     The Service request identifier.
     * @param provenanceDTO  The ProvenanceDTO entity to store performers information
     */
    public void setServiceRequestInBundle(Bundle bundle, ServiceRequest serviceRequest,
                                          String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(serviceRequest) && Objects.nonNull(serviceRequest.getIdPart())) {
            fhirUtils.setBundle(String.format(FhirConstants.SERVICE_REQUEST_ID, serviceRequest.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, serviceRequest.getIdPart()),
                    Bundle.HTTPVerb.PUT, serviceRequest, bundle, provenanceDTO);
        } else if (Objects.nonNull(serviceRequest)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    serviceRequest, bundle, provenanceDTO);
        }
    }

    /**
     * Sets Location details in given bundle using
     * location identifier
     *
     * @param bundle        The FHIR bundle entity.
     * @param location      The FHIR Location entity.
     * @param identifier    The Location identifier.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    public void setLocationDetailsInBundle(Bundle bundle, Location location,
                                           String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(location)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Location),
                            Constants.FORWARD_SLASH, identifier), identifier,
                    Bundle.HTTPVerb.POST, location, bundle, provenanceDTO);
        }
    }

    /**
     * Sets patient details in given bundle using
     * patient identifier
     *
     * @param bundle        The FHIR bundle entity.
     * @param patient       The FHIR patient entity.
     * @param identifier    The patient identifier.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    public void setPatientDetailsInBundle(Bundle bundle, Patient patient,
                                          String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            fhirUtils.setBundle(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, patient.getIdPart()),
                    Bundle.HTTPVerb.PUT, patient, bundle, provenanceDTO);
        } else if (Objects.nonNull(patient)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    patient, bundle, provenanceDTO);
        }
    }

    /**
     * Sets related person details in given bundle using
     * related person identifier
     *
     * @param bundle        The FHIR bundle entity.
     * @param relatedPerson The FHIR related person entity.
     * @param identifier    The related person identifier.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    public void setRelatedPersonDetailsInBundle(Bundle bundle, RelatedPerson relatedPerson,
                                                String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(relatedPerson)
                && Objects.nonNull(relatedPerson.getIdPart())) {
            fhirUtils.setBundle(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, relatedPerson.getIdPart()),
                    Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);
        } else if (Objects.nonNull(relatedPerson)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    relatedPerson, bundle, provenanceDTO);
        }
    }

    /**
     * Sets the patient details in the provided bundle, including relevant information
     * such as patient identifier, provenance details, country information, and user country ID.
     * This method ensures that the provided data is correctly organized within the bundle for further processing.
     *
     * @param bundle        The {@link Bundle} object where patient details are to be set.
     * @param patient       The {@link Patient} object containing the patient's details to be added to the bundle.
     * @param identifier    The unique identifier for the patient.
     * @param provenanceDTO The {@link ProvenanceDTO} containing provenance information to be included.
     * @param countryId     The ID representing the country associated with the patient or the context.
     * @param userCountryId The country ID associated with the user or the user's context.
     */
    public void setPatientDetailsInBundle(Bundle bundle, Patient patient,
                                          String identifier, ProvenanceDTO provenanceDTO, String countryId, String userCountryId) {
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart()) && countryId.equals(userCountryId)) {
            fhirUtils.setBundle(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, patient.getIdPart()),
                    Bundle.HTTPVerb.PUT, patient, bundle, provenanceDTO);
        } else if (Objects.nonNull(patient)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    patient, bundle, provenanceDTO);
        }
    }

    /**
     * Sets the related person details in the provided bundle, including relevant information
     * such as related person identifier, provenance details, country information, and user country ID.
     * This method ensures that the related person's data is properly organized within the bundle for further use.
     *
     * @param bundle        The {@link Bundle} object where related person details are to be set.
     * @param relatedPerson The {@link RelatedPerson} object containing the related person's details to be added.
     * @param identifier    The unique identifier for the related person.
     * @param provenanceDTO The {@link ProvenanceDTO} containing provenance information to be included.
     * @param countryId     The ID representing the country associated with the related person or context.
     * @param userCountryId The country ID associated with the user or the user's context.
     */
    public void setRelatedPersonDetailsInBundle(Bundle bundle, RelatedPerson relatedPerson,
                                                String identifier, ProvenanceDTO provenanceDTO, String countryId, String userCountryId) {
        if (Objects.nonNull(relatedPerson)
                && Objects.nonNull(relatedPerson.getIdPart()) && userCountryId.equals(countryId)) {
            fhirUtils.setBundle(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, relatedPerson.getIdPart()),
                    Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);
        } else if (Objects.nonNull(relatedPerson)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    relatedPerson, bundle, provenanceDTO);
        }
    }

    /**
     * Sets encounter details in given bundle using
     * encounter identifier
     *
     * @param bundle        The FHIR bundle entity.
     * @param encounter     The FHIR encounter entity.
     * @param identifier    The patient identifier.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    public void setEncounterDetailsInBundle(Bundle bundle, Encounter encounter,
                                            String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(encounter)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    encounter, bundle, provenanceDTO);
        }
    }

    /**
     * Sets Questionnaire Response details in given bundle using
     * questionnaire response identifier
     *
     * @param bundle                The FHIR bundle entity.
     * @param questionnaireResponse The FHIR questionnaire response entity.
     * @param identifier            The questionnaire response identifier.
     * @param provenanceDTO         The ProvenanceDTO entity to store performers information
     */
    public void setQuestionnarieDetailsInBundle(Bundle bundle,
                                                QuestionnaireResponse questionnaireResponse,
                                                String identifier, ProvenanceDTO provenanceDTO, boolean isUpdate) {
        if (Objects.nonNull(questionnaireResponse)) {
            if (isUpdate) {
                fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.QuestionnaireResponse),
                                Constants.FORWARD_SLASH, questionnaireResponse.getIdPart()), identifier, Bundle.HTTPVerb.PUT,
                        questionnaireResponse, bundle, provenanceDTO);
            } else {
                fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.QuestionnaireResponse),
                                Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                        questionnaireResponse, bundle, provenanceDTO);
            }
        }
    }

    /**
     * Populates a Bundle with details from a given QuestionnaireResponse, Identifier, and Provenance data.
     * This method is used specifically for setting HIV-related questionnaire details in the provided Bundle.
     *
     * @param bundle                the FHIR Bundle to be populated with data. This is the output where the questionnaire details will be added.
     * @param questionnaireResponse the QuestionnaireResponse object containing responses to the HIV questionnaire.
     *                              This object holds the data that will be extracted and placed into the bundle.
     * @param identifier            a String identifier that uniquely represents this specific questionnaire instance, used for tracking or referencing purposes.
     * @param provenanceDTO         a ProvenanceDTO object that holds information about the origin, authorship, or source of the questionnaire data.
     *                              This is used to provide traceability and context for the information being bundled.
     */
    public void setHIVQuestionnarieDetailsInBundle(Bundle bundle,
                                                   QuestionnaireResponse questionnaireResponse,
                                                   String identifier, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(questionnaireResponse)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.QuestionnaireResponse),
                            Constants.FORWARD_SLASH, identifier), identifier, Bundle.HTTPVerb.POST,
                    questionnaireResponse, bundle, provenanceDTO);
        }
    }

    /**
     * Sets Resource details in given bundle using resource identifier url
     *
     * @param bundle        FHIR bundle
     * @param resource      FHIR resource
     * @param identifierUrl FHIR identifier url for the given resource
     * @param resIdUrl      FHIR res id url for the given resource
     * @param resType       FHIR resource type
     * @param provenanceDTO {@link ProvenanceDTO} object
     */
    public void setResourceInBundle(Bundle bundle, Resource resource, String identifierUrl, String resIdUrl,
                                    String resType, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(resource) && Objects.nonNull(resource.getIdPart())) {
            fhirUtils.setBundle(String.format(resIdUrl, resource.getIdPart()), StringUtil.concatString(
                    Constants.FHIR_BASE_URL, resource.getIdPart()), Bundle.HTTPVerb.PUT, resource, bundle, provenanceDTO);
        } else if (Objects.nonNull(resource)) {
            fhirUtils.setBundle(StringUtil.concatString(resType, Constants.FORWARD_SLASH, identifierUrl),
                    identifierUrl, Bundle.HTTPVerb.POST, resource, bundle, provenanceDTO);
        }
    }

    /**
     * Sets observation text details using given text
     *
     * @param observation The FHIR Observation entity.
     * @param text        The observation text details
     */
    public void setObservationText(Observation observation, String text) {
        Narrative narrative = new Narrative();
        narrative.setStatus(Narrative.NarrativeStatus.GENERATED);
        narrative.setDivAsString("<div xmlns=\"http://www.w3.org/1999/xhtml\">" + text + "</div>");
        observation.setText(narrative);
    }

    /**
     * Sets text details using given text and return the narrative
     *
     * @param text The text details to set into the narrative
     * @return Narrative
     */
    public Narrative getNarrative(String text) {
        Narrative narrative = new Narrative();
        narrative.setStatus(Narrative.NarrativeStatus.GENERATED);
        narrative.setDivAsString("<div xmlns=\"http://www.w3.org/1999/xhtml\">" + text + "</div>");
        return narrative;
    }

    /**
     * <p>
     * Sets observation code details using FHIR CodeableConcept
     * </p>
     *
     * @param observation     The FHIR observation entity
     * @param codeableConcept The FHIR codeable concept entity
     */
    public void setObservationCode(Observation observation, CodeableConcept codeableConcept) {
        for (Coding coding : codeableConcept.getCoding()) {
            observation.getCode().addCoding(coding);
        }
    }

    /**
     * <p>
     * Sets observation note details using given type and value
     * </p>
     *
     * @param observation The FHIR Observation entity.
     * @param type        The observation note type
     * @param value       The observation note value
     */
    public void setObservationNote(Observation observation, String type, String value) {
        List<Annotation> notes = observation.getNote();
        Annotation annotation = new Annotation();
        annotation.setText(type + Constants.HIGHFIN + value);
        notes.add(annotation);
        observation.setNote(notes);
    }

    /**
     * Calculate the date of birth based on given age and created date
     *
     * @param recordedAt The age recorded date and time
     * @param age        The age used to created date of birth
     */
    public Date calculateBirthDate(Date recordedAt, Integer age) {
        Date birthDate = null;
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy");

        try {
            Date date = simpleDateFormat.parse(recordedAt.toString());

            Calendar calendar = Calendar.getInstance();
            calendar.setTime(date);
            calendar.add(Calendar.YEAR, Integer.parseInt("-" + age));
            birthDate = calendar.getTime();
        } catch (ParseException e) {
            Logger.logInfo("Invalid date format");
        }
        return birthDate;
    }

    /**
     * Sets patient link component details in patient based
     * on given related person details
     *
     * @param patient       The FHIR Patient entity.
     * @param relatedPerson The FHIR Related Person entity.
     */
    public void setPatientLinkComponentReference(Patient patient,
                                                 RelatedPerson relatedPerson) {
        Patient.PatientLinkComponent patientLinkComponent = new Patient.PatientLinkComponent();
        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getIdPart())) {
            patientLinkComponent.setOther(new Reference(
                    String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart())));
        } else if (Objects.nonNull(relatedPerson)) {
            patientLinkComponent.setOther(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
        }
        patient.addLink(patientLinkComponent);
    }

    /**
     * <p>
     * Sets the reference link between patient and related person
     * <p/>
     *
     * @param patient       - The FHIR Patient entity.
     * @param relatedPerson - The FHIR RelatedPerson entity.
     */
    public void setPatientAndRelatedPersonLink(Patient patient, RelatedPerson relatedPerson) {
        this.setPatientLinkComponentReference(patient, relatedPerson);
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            relatedPerson.setPatient(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
        } else if (Objects.nonNull(patient)) {
            relatedPerson.setPatient(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
        }
    }

    /**
     * Set Suicide or Substance Component to FHIR observation entity based on suicideQuestion.
     *
     * @param observation The FHIR Observation entity.
     * @param entry       The Suicide question.
     */
    public void setSuicideOrSubstanceComponent(Observation observation,
                                               Map.Entry<String, String> entry) {
        fhirAssessmentMapper.createObservationComponent(Constants.YES.equalsIgnoreCase(entry.getValue()),
                entry.getKey(), observation.getComponent());
        Logger.logInfo(entry.getKey());
    }

    /**
     * <p>
     * Get FHIR Contact Point Use entity based on
     * given mobile number category
     * </p>
     *
     * @param phoneNumberCategory The category of the mobile number.
     * @return The FHIR ContactPointUse entity
     */
    public ContactPoint.ContactPointUse getFhirPhoneNumberCategory(String phoneNumberCategory) {
        switch (phoneNumberCategory) {
            case Constants.PERSONAL -> {
                return ContactPoint.ContactPointUse.MOBILE;
            }
            case Constants.FAMILY_MEMBER -> {
                return ContactPoint.ContactPointUse.HOME;
            }
            case Constants.FRIEND -> {
                return ContactPoint.ContactPointUse.TEMP;
            }
            default -> {
                Logger.logInfo(phoneNumberCategory);
                return null;
            }
        }
    }

    /**
     * <p>
     * Set the phone number category based on the FHIR number category response.
     * <p/>
     *
     * @param phoneNumberCategory - FHIR phone number category.
     * @param bioDataDTO          - Used to set the phone number category in bia data.
     */
    public void setPhoneNumberCategory(String phoneNumberCategory, BioDataDTO bioDataDTO) {
        switch (phoneNumberCategory) {
            case Constants.MOBILE -> bioDataDTO.setPhoneNumberCategory(Constants.PERSONAL);
            case Constants.HOME -> bioDataDTO.setPhoneNumberCategory(Constants.FAMILY_MEMBER);
            case Constants.TEMP -> bioDataDTO.setPhoneNumberCategory(Constants.FRIEND);
            default -> Logger.logInfo(phoneNumberCategory);
        }
    }

    /**
     * <p>
     * Convert the related person object to patient resource
     * <p/>
     *
     * @param relatedPerson - Related person details
     * @param tenantId      - Patient organization id
     * @return {@ScreeningLogRequestDTO} - Converted screening log details
     */
    public ScreeningLogRequestDTO setPatientDetails(RelatedPerson relatedPerson, String tenantId) {
        BioDataDTO bioDataDTO = new BioDataDTO();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();
        relatedPerson.getIdentifier().stream()
                .filter(identifier -> FhirConstants.STATUS.equals(identifier.getSystem()))
                .findFirst()
                .ifPresent(identifier -> bioDataDTO.setIdentityValue(identifier.getValue()));
        List<StringType> names = relatedPerson.getName().getFirst().getGiven();
        if (names.size() > Constants.TWO) {
            bioDataDTO.setFirstName(names.getFirst().getValue());
            bioDataDTO.setLastName(names.getLast().getValue());
            bioDataDTO.setMiddleName(names.get(Constants.TWO).getValue());
        } else if (names.size() == Constants.TWO) {
            bioDataDTO.setFirstName(names.getFirst().getValue());
            bioDataDTO.setLastName(names.getLast().getValue());
        }
        bioDataDTO.setPhoneNumber(relatedPerson.getTelecom().getFirst().getValue());
        if (Objects.nonNull(relatedPerson.getTelecom().getFirst().getUse())) {
            setPhoneNumberCategory(relatedPerson.getTelecom().getFirst().getUse().name().toLowerCase(),
                    bioDataDTO);
        }
        bioMetricsDTO.setGender(relatedPerson.getGender().name());
        ScreeningLogRequestDTO screeningLogRequestDTO = new ScreeningLogRequestDTO();
        screeningLogRequestDTO.setBioMetrics(bioMetricsDTO);
        screeningLogRequestDTO.setBioData(bioDataDTO);
        return screeningLogRequestDTO;
    }

    /**
     * <p>
     * Set the condition resource in bundle
     * <p/>
     *
     * @param bundle        - Fhir bundle resource.
     * @param condition     - Condition resource used to store patient decease information.
     * @param identifier    - Patient condition identifier URL.
     * @param isUpdate      - Used to define whether patient condition update or create.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    public void setConditionInBundle(Bundle bundle, Condition condition,
                                     String identifier, Boolean isUpdate,
                                     ProvenanceDTO provenanceDTO) {
        if (!Boolean.TRUE.equals(isUpdate)) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Condition),
                            Constants.FORWARD_SLASH, identifier), identifier,
                    Bundle.HTTPVerb.POST, condition, bundle, provenanceDTO);
        } else {
            fhirUtils.setBundle(StringUtil.concatString(FhirConstants.CONDITION,
                            Constants.FORWARD_SLASH, condition.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, condition.getIdPart()),
                    Bundle.HTTPVerb.PUT, condition, bundle, provenanceDTO);
        }
    }


    /* Creates HumanName where patients name details added.
     *
     * Creates Contact point where patients telecom details added.
     *
     * @param phoneNumber
     * @param phoneNumberCategory
     * @return ContactPoint
     */
    public ContactPoint createContactPoint(String phoneNumber, String phoneNumberCategory) {
        ContactPoint contactPoint = new ContactPoint();
        contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
        contactPoint.setValue(phoneNumber);
        contactPoint.setUse(getFhirPhoneNumberCategory(phoneNumberCategory.toLowerCase()));
        return contactPoint;
    }

    /**
     * Creates HumanName where patients name details added.
     *
     * @param firstName
     * @param middleName
     * @param lastName
     * @return HumanName
     */
    public HumanName createHumanName(String firstName, String middleName, String lastName, String initial) {
        HumanName name = new HumanName();
        name.setText(StringUtil.concatString(firstName, lastName));
        if (Objects.nonNull(initial)) {
            name.setPrefix(List.of(new StringType(initial)));
        }
        List<StringType> givenNames = new ArrayList<>();
        givenNames.add(new StringType(firstName));
        if (!Objects.isNull(middleName)) {
            givenNames.add(new StringType(middleName));
        }
        givenNames.add(new StringType(lastName));
        name.setGiven(givenNames);
        return name;
    }


    /*
     * Creates Attachment resource for qrcode.
     */
    public Attachment createAttachment(String qrCode) {
        return new Attachment().setData(DatatypeConverter.parseBase64Binary(qrCode)).setCreation(new Date());
    }

    /**
     * Create or update the address in patient
     *
     * @param addresses  list of old address.
     * @param bioDataDTO bioDataDTO
     */
    public List<Address> createOrUpdateAddress(List<Address> addresses, BioDataDTO bioDataDTO) {
        if (Objects.nonNull(addresses) && !addresses.isEmpty()) {
            addresses.forEach(address -> {
                if (Address.AddressUse.HOME.equals(address.getUse())) {
                    updateAddress(address, bioDataDTO.getCountry(), bioDataDTO.getDistrict(), bioDataDTO.getChiefdom(),
                            bioDataDTO.getVillage(), bioDataDTO.getOtherVillage());
                } else if (Address.AddressUse.TEMP.equals(address.getUse()) && Objects.nonNull(bioDataDTO.getLandmark())) {
                    address.setText(bioDataDTO.getLandmark());
                }
            });
            return addresses;
        } else {
            List<Address> newAddressList = new ArrayList<>();
            newAddressList.add(createAddress(bioDataDTO.getCountry(),
                    bioDataDTO.getDistrict(), bioDataDTO.getChiefdom(),
                    bioDataDTO.getVillage(), bioDataDTO.getOtherVillage()));
            newAddressList.add(new Address().setUse(Address.AddressUse.TEMP).setText(bioDataDTO.getLandmark()));
            return newAddressList;
        }
    }

    /**
     * Creates Address where patients address details added.
     *
     * @param country  country details
     * @param district district details
     * @param chiefdom chiefdom detasils
     * @param village  village details
     */
    public Address createAddress(DataDTO country, DataDTO district, DataDTO chiefdom, DataDTO village, String otherVillage) {
        Address address = new Address();
        address.setUse(AddressUse.HOME);
        if (!Objects.isNull(country)) {
            address.setCountry(country.getName());
        }
        if (!Objects.isNull(district)) {
            address.setDistrict(district.getName());
        }
        if (!Objects.isNull(chiefdom)) {
            address.setCity(chiefdom.getName());
        }
        if (!Objects.isNull(village)) {
            address.setText(Objects.nonNull(otherVillage) && village.getName().equals(Constants.OTHER) ? otherVillage :
                    village.getName());
        }
        return address;
    }

    /**
     * Update Address where patients address details added.
     *
     * @param country  country details
     * @param district district details
     * @param chiefdom chiefdom detasils
     * @param village  village details
     * @return address
     */
    public Address updateAddress(Address address, DataDTO country, DataDTO district, DataDTO chiefdom,
                                 DataDTO village, String otherVillage) {
        if (!Objects.isNull(country)) {
            address.setCountry(country.getName());
        }
        if (!Objects.isNull(district)) {
            address.setDistrict(district.getName());
        }
        if (!Objects.isNull(chiefdom)) {
            address.setCity(chiefdom.getName());
        }
        if (!Objects.isNull(village)) {
            address.setText(Objects.nonNull(otherVillage) && village.getName().equals(Constants.OTHER) ? otherVillage :
                    village.getName());
        }
        return address;
    }

    /**
     * <p>
     * Get FHIR Resource from bundle using given identifier
     * </p>
     *
     * @param bundle     The Fhir bundle entity.
     * @param identifier The observation identifier url.
     * @return Fhir Resource entity
     */
    public Optional<Resource> getResourceFromBundleByIdentifier(Bundle bundle, String identifier) {
        return bundle.getEntry().stream()
                .filter(entry -> identifier.equals(entry.getFullUrl()))
                .map(Bundle.BundleEntryComponent::getResource)
                .findFirst();
    }

    /**
     * <p>
     * Get FHIR Observation from bundle using given identifier
     * </p>
     *
     * @param bundle     The Fhir bundle entity.
     * @param identifier The observation identifier url.
     * @return Fhir observation entity
     */
    public Observation getObservationFromBundleByIdentifier(Bundle bundle, String identifier) {
        Optional<Resource> resourceOpt = this.getResourceFromBundleByIdentifier(bundle, identifier);

        if (resourceOpt.isPresent() && resourceOpt.get() instanceof Observation observation) {
            return observation;
        } else {
            return null;
        }
    }

    /**
     * <p>
     * Get FHIR Location from bundle using given identifier
     * </p>
     *
     * @param bundle     The Fhir bundle entity.
     * @param identifier The Location identifier url.
     * @return Fhir Location entity
     */
    public Location getLocationFromBundle(Bundle bundle, String identifier) {
        Optional<Resource> resourceOpt = this.getResourceFromBundleByIdentifier(bundle, identifier);
        if (resourceOpt.isPresent() && resourceOpt.get() instanceof Location location) {
            return location;
        } else {
            return null;
        }
    }

    /**
     * <p>
     * Get FHIR QuestionnaireResponse from bundle using given identifier
     * </p>
     *
     * @param bundle     The Fhir bundle entity.
     * @param identifier The QuestionnaireResponse identifier url.
     * @return Fhir QuestionnaireResponse entity
     */
    public QuestionnaireResponse getQuestionnaireResponseFromBundle(Bundle bundle, String identifier) {
        Optional<Resource> resourceOpt = this.getResourceFromBundleByIdentifier(bundle, identifier);

        if (resourceOpt.isPresent() && resourceOpt.get() instanceof QuestionnaireResponse
                questionnaireResponse) {
            return questionnaireResponse;
        } else {
            return null;
        }
    }

    /**
     * <p>
     * Get FHIR Location from bundle using given identifier
     * </p>
     *
     * @param bundle     The Fhir bundle entity.
     * @param identifier The observation identifier url.
     * @return Fhir Location entity
     */
    public Location getLocationFromBundleByIdentifier(Bundle bundle, String identifier) {
        Optional<Resource> resourceOpt = this.getResourceFromBundleByIdentifier(bundle, identifier);
        if (resourceOpt.isPresent() && resourceOpt.get() instanceof Location location) {
            return location;
        } else {
            return null;
        }
    }

    public void setConditionMetaDetails(Condition condition, Encounter encounter, Patient patient,
                                        RelatedPerson relatedPerson) {

        // set encounter details
        if (Objects.nonNull(encounter) && Objects.nonNull(encounter.getIdPart())) {
            condition.setEncounter(new Reference(
                    String.format(FhirConstants.ENCOUNTER_ID, encounter.getIdPart())));
        } else if (Objects.nonNull(encounter)) {
            condition.setEncounter(new Reference(FhirConstants.ENCOUNTER_IDENTIFIER_URL));
        }

        // set patient details
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            condition.setSubject(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
        }

        // set related person information
        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getIdPart())) {
            condition.setAsserter(new Reference(
                    String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart())));
        }

    }

    /**
     * <p>
     * Creates patient red risk observation based on provided risk level and risk message.
     * </p>
     *
     * @param riskLevel       The patient red risk level
     * @param riskMessage     The patient red risk message
     * @param memberReference The Fhir related person reference
     * @return Created FHIR Observation entity
     */
    public Observation createRiskLevelObservation(String riskLevel, String riskMessage, String memberReference) {
        Observation riskObservation = null;
        Bundle resultBundle = restApiUtil.getBatchRequest(String.format(Constants.GET_RED_RISK_DETAILS,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL
                        + Constants.VERTICAL_BAR + Constants.OBSERVATION_RISK_LEVEL, memberReference));
        if (!resultBundle.getEntry().isEmpty()) {
            riskObservation = (Observation) resultBundle.getEntry().getFirst().getResource();
            riskObservation.setValue(new StringType(riskLevel));
        } else {
            riskObservation = new Observation();
            riskObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                    .setValue(Constants.OBSERVATION_RISK_LEVEL);
            riskObservation.setStatus(Observation.ObservationStatus.FINAL);
            riskObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.RISK_LEVEL));
            riskObservation.setValue(new StringType(riskLevel));
            if (Objects.nonNull(memberReference)) {
                riskObservation.addPerformer(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, memberReference));
            }
        }
        if (Objects.nonNull(riskMessage)) {
            riskObservation.setComponent(new ArrayList<>());
            Observation.ObservationComponentComponent component = new Observation.ObservationComponentComponent();
            component.setCode(fhirUtils.setCodes(FhirConstants.RISK_MESSAGE));
            component.setValue(new StringType(riskMessage));
            riskObservation.addComponent(component);
        }
        this.setObservationText(riskObservation, MetaCodeConstants.RED_RISK_DETAILS);
        return riskObservation;
    }

    /**
     * <p>
     * Create referral ticket for a patient based on
     * given reason and organization
     * </p>
     *
     * @param requestOrgId   The id of the requesting organization
     * @param userId         The id of the practitioner
     * @param referredReason It contains the reason to why this patient is referred
     * @param date           referred date time
     * @return ServiceRequest FHIR entity
     */
    public ServiceRequest createServiceRequest(String requestOrgId, String userId,
                                               String referredReason, Date date) {
        ServiceRequest serviceRequest = new ServiceRequest();
        serviceRequest.addIdentifier()
                .setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                .setValue(Constants.REFERRED);
        serviceRequest.setIntent(ServiceRequest.ServiceRequestIntent.ORDER);
        serviceRequest.setPriority(ServiceRequest.ServiceRequestPriority.URGENT);
        serviceRequest.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
        serviceRequest.setAuthoredOn(date);
        serviceRequest.setPatientInstruction(referredReason);
        //Add current organization
        serviceRequest.setRequester(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Organization),
                        Constants.FORWARD_SLASH, requestOrgId)));
        //Add related person
        serviceRequest.addPerformer(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Practitioner),
                        Constants.FORWARD_SLASH, userId)));
        return serviceRequest;
    }

    /**
     * Updates the risk level of a patient. If the patient already has a risk level identifier, it updates the value.
     * Otherwise, it adds a new identifier with the given risk level.
     *
     * @param riskLevel The new risk level to be set for the patient.
     * @param patient   The Patient object whose risk level needs to be updated.
     */
    public void updateRiskLevel(String riskLevel, Patient patient) {
        boolean hasRiskIdentifier = false;
        List<Identifier> identifiers = patient.getIdentifier();
        for (Identifier identifier : identifiers) {
            if (FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL.equals(identifier.getSystem())) {
                identifier.setValue(riskLevel);
                hasRiskIdentifier = true;
            }
        }
        if (!hasRiskIdentifier) {
            Identifier identifier = new Identifier();
            identifier.setSystem(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL);
            identifier.setValue(riskLevel);
            patient.addIdentifier(identifier);
        }
    }

    /**
     * Updates the risk level of a patient. If the patient already has a risk level identifier, it updates the value.
     * Otherwise, it adds a new identifier with the given risk level.
     *
     * @param riskLevel     The new risk level to be set for the patient.
     * @param relatedPerson The Patient object whose risk level needs to be updated.
     */
    public void updateRiskLevel(String riskLevel, RelatedPerson relatedPerson) {
        boolean hasRiskIdentifier = false;
        List<Identifier> identifiers = relatedPerson.getIdentifier();
        for (Identifier identifier : identifiers) {
            if (FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL.equals(identifier.getSystem())) {
                identifier.setValue(riskLevel);
                hasRiskIdentifier = true;
            }
        }
        if (!hasRiskIdentifier) {
            Identifier identifier = new Identifier();
            identifier.setSystem(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL);
            identifier.setValue(riskLevel);
            relatedPerson.addIdentifier(identifier);
        }
    }

    /**
     * <p>
     * Used to create or update the patient status
     * <p/>
     *
     * @param patientStatusDto               - The patient status details.
     * @param diabetesConditionReference     - The patient diabetes condition details.
     * @param hypertensionConditionReference - The patient diabetes condition details.
     * @param isUpdate                       - The reference for condition update or create.
     */
    public void createOrUpdateDiabetesAndHypertensionPatientStatus(PatientStatusDTO patientStatusDto, Bundle bundle,
                                                                   AtomicReference<Condition> diabetesConditionReference,
                                                                   AtomicReference<Condition> hypertensionConditionReference, Boolean isUpdate) {
        if (Objects.nonNull(patientStatusDto.getNcdPatientStatus())) {
            Condition diabetesCondition = patientStatusConverter.createDiabetesStatus(patientStatusDto,
                    diabetesConditionReference.get());
            Condition hypertensionCondition = patientStatusConverter.createHypertensionStatus(patientStatusDto,
                    hypertensionConditionReference.get());
            patientStatusConverter.setReference(diabetesCondition, patientStatusDto.getPatientReference(),
                    patientStatusDto.getPatientVisitId());
            patientStatusConverter.setReference(hypertensionCondition, patientStatusDto.getPatientReference(),
                    patientStatusDto.getPatientVisitId());
            this.setConditionInBundle(bundle, diabetesCondition,
                    FhirConstants.PATIENT_STATUS_DIABETES_IDENTIFIER_URL, isUpdate, patientStatusDto.getProvenance());
            this.setConditionInBundle(bundle, hypertensionCondition,
                    FhirConstants.PATIENT_STATUS_HYPERTENSION_IDENTIFIER_URL, isUpdate, patientStatusDto.getProvenance());
        }
    }

    /**
     * <p>
     * This function retrieves and returns a confirmed disease  condition for a specific patient.
     * </p>
     *
     * @param patientId {@link String} It is used to retrieve information and data related to that particular patient.
     * @param type      {@link String} It refers to the type of condition being queried for verification status.
     * @return {@link Condition} This represents the confirmed non-communicable disease condition for a specific patient
     * based on the provided patient ID and type.
     */
    public Condition getConfirmDiagnosis(String patientId, String type) {
        AtomicReference<Condition> confirmedNCDCondition = new AtomicReference<>();
        String url = String.format(Constants.CONDITION_QUERY_VERIFICATION_STATUS_WITH_IDENTIFIER, patientId, type);
        Bundle resultBundle = restApiUtil.getBatchRequest(url);
        resultBundle.getEntry().stream().map(Bundle.BundleEntryComponent::getResource)
                .filter(Condition.class::isInstance)
                .map(Condition.class::cast)
                .forEach(confirmedNCDCondition::set
                );
        return confirmedNCDCondition.get();
    }

    /**
     * <p>
     * This function updates the known status of a patient's non-communicable disease condition in a FHIR bundle.
     * </p>
     *
     * @param patientStatusDto {@link PatientStatusDTO} It contains patient status information
     * @param bundle           {@link Bundle} It is used to group related resources together, such as patient
     *                         information, conditions, medications, etc., for easier management and transmission within a FHIR
     *                         system.
     * @param condition        {@link Condition} It represents a medical condition related to a patient.
     * @param isUpdate         {@link Boolean} It indicates whether the update operation should be performed or not.
     */
    public void updateNCDKnownStatus(PatientStatusDTO patientStatusDto, Bundle bundle,
                                     Condition condition, Boolean isUpdate) {
        condition = patientStatusConverter.createConfirmedNCDStatus(patientStatusDto,
                condition);
        patientStatusConverter.setReference(condition, patientStatusDto.getPatientReference(),
                patientStatusDto.getPatientVisitId());
        this.setConditionInBundle(bundle, condition,
                FhirConstants.PATIENT_DIAGNOSIS_IDENTIFIER_URL, isUpdate, patientStatusDto.getProvenance());
    }

    /**
     * <p>
     * This function updates the known status of a patient's mental health condition in a FHIR bundle.
     * </p>
     *
     * @param patientStatusDto      {@link PatientStatusDTO} It contains patient status information
     * @param bundle                {@link Bundle} It is used to group related resources together, such as patient
     *                              information, conditions, medications, etc., for easier management and transmission within a FHIR
     *                              system.
     * @param mentalHealthCondition {@link Condition} It represents a medical condition related to a patient.
     * @param substanceCondition    {@link Condition} It represents a medical condition related to a patient.
     */
    public void createOrUpdateMentalHealthPatientStatus(PatientStatusDTO patientStatusDto, Bundle bundle, AtomicReference<Condition> mentalHealthCondition, AtomicReference<Condition> substanceCondition) {
        Condition mentalHealth = Objects.nonNull(mentalHealthCondition.get()) ? mentalHealthCondition.get() : new Condition();
        Condition substance = Objects.nonNull(substanceCondition.get()) ? substanceCondition.get() : new Condition();
        String uuid;

        if (Objects.nonNull(patientStatusDto.getMentalHealthStatus()) && Objects.nonNull(patientStatusDto.getMentalHealthStatus().getStatus())) {
            patientStatusConverter.setMentalHealthStatus(mentalHealth, patientStatusDto.getMentalHealthStatus(), Constants.MENTAL_HEALTH_STATUS);
        }
        if (Objects.nonNull(patientStatusDto.getSubstanceUseStatus()) && Objects.nonNull(patientStatusDto.getSubstanceUseStatus().getStatus())) {
            patientStatusConverter.setMentalHealthStatus(substance, patientStatusDto.getSubstanceUseStatus(), Constants.SUBSTANCE_DISORDER);
        }
        if (mentalHealth.hasVerificationStatus()) {
            uuid = fhirUtils.getUniqueId();
            patientStatusConverter.setReference(mentalHealth, patientStatusDto.getPatientReference(),
                    patientStatusDto.getPatientVisitId());
            this.setConditionInBundle(bundle, mentalHealth,
                    StringUtil.concatString(Constants.FHIR_BASE_URL, uuid), Objects.nonNull(mentalHealthCondition.get()), patientStatusDto.getProvenance());
        }
        if (substance.hasVerificationStatus()) {
            uuid = fhirUtils.getUniqueId();
            patientStatusConverter.setReference(substance, patientStatusDto.getPatientReference(),
                    patientStatusDto.getPatientVisitId());
            this.setConditionInBundle(bundle, substance,
                    StringUtil.concatString(Constants.FHIR_BASE_URL, uuid), Objects.nonNull(substanceCondition.get()), patientStatusDto.getProvenance());
        }
    }

    /**
     * <p>
     * This function updates the known status of a patient's non-communicable disease condition in a FHIR bundle.
     * </p>
     *
     * @param patientStatusDto {@link PatientStatusDTO} It contains patient status information
     * @param bundle           {@link Bundle} It is used to group related resources together, such as patient
     *                         information, conditions, medications, etc., for easier management and transmission within a FHIR
     *                         system.
     * @param condition        {@link Condition} It represents a medical condition related to a patient.
     * @param isUpdate         {@link Boolean} It indicates whether the update operation should be performed or not.
     */
    public void updateMentalHealthKnownStatus(PatientStatusDTO patientStatusDto, Bundle bundle,
                                              Condition condition, Boolean isUpdate) {
        String uuid = fhirUtils.getUniqueId();
        condition = patientStatusConverter.createConfirmedMentalHealthStatus(patientStatusDto,
                condition);
        patientStatusConverter.setReference(condition, patientStatusDto.getPatientReference(),
                patientStatusDto.getPatientVisitId());
        this.setConditionInBundle(bundle, condition,
                StringUtil.concatString(Constants.FHIR_BASE_URL, uuid), isUpdate, patientStatusDto.getProvenance());
    }
}
