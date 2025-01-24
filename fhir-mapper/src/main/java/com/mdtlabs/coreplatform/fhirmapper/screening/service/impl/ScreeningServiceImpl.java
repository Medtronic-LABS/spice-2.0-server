package com.mdtlabs.coreplatform.fhirmapper.screening.service.impl;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import static java.util.Objects.isNull;
import static java.util.stream.Collectors.groupingBy;

import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MessageConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.PatientStatusConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodGlucoseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodPressureConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.LocationConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.QuestionnaireResponseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.RelatedPersonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.screening.service.ScreeningService;

/**
 * <p>
 * Service implementation for handling screening related operations.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Service
public class ScreeningServiceImpl implements ScreeningService {
    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;
    private final FhirUtils fhirUtils;
    private final RestApiUtil restApiUtil;
    private final RelatedPersonConverter relatedPersonConverter;
    private final PatientConverter patientConverter;
    private final QuestionnaireResponseConverter questionnaireResponseConverter;
    private final BloodPressureConverter bloodPressureConverter;
    private final BloodGlucoseConverter bloodGlucoseConverter;
    private final PregnancyConverter pregnancyConverter;
    private final EncounterConverter encounterConverter;
    private final LocationConverter locationConverter;
    private final CommonConverter commonConverter;
    private final VitalSignsConverter vitalSignsConverter;
    private final SymptomConverter symptomConverter;

    @Autowired
    public ScreeningServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil,
                                RelatedPersonConverter relatedPersonConverter, PatientConverter patientConverter,
                                QuestionnaireResponseConverter questionnaireResponseConverter,
                                BloodPressureConverter bloodPressureConverter, BloodGlucoseConverter bloodGlucoseConverter,
                                PregnancyConverter pregnancyConverter, EncounterConverter encounterConverter,
                                LocationConverter locationConverter, CommonConverter commonConverter,
                                VitalSignsConverter vitalSignsConverter, SymptomConverter symptomConverter) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.relatedPersonConverter = relatedPersonConverter;
        this.patientConverter = patientConverter;
        this.questionnaireResponseConverter = questionnaireResponseConverter;
        this.bloodPressureConverter = bloodPressureConverter;
        this.bloodGlucoseConverter = bloodGlucoseConverter;
        this.pregnancyConverter = pregnancyConverter;
        this.encounterConverter = encounterConverter;
        this.locationConverter = locationConverter;
        this.commonConverter = commonConverter;
        this.vitalSignsConverter = vitalSignsConverter;
        this.symptomConverter = symptomConverter;
    }


    /**
     * {@inheritDoc}
     */
    public BioDataDTO processScreeningLog(ScreeningLogRequestDTO request) {
        fhirUtils.initiateCodesMap();
        Organization organization = new Organization();
        organization.setId(String.valueOf(request.getSiteId()));
        Patient patient = null;
        RelatedPerson relatedPerson = null;
        String identifier = request.getBioData().getIdentityValue();
        String relatedPersonSearch = String.format(Constants.RELATED_PERSON_QUERY,
                FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID + Constants.VERTICAL_BAR + identifier,
                FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL + Constants.VERTICAL_BAR + request.getCountryId());
        Bundle responseBundle = restApiUtil.getBatchRequest(relatedPersonSearch);

        for (Bundle.BundleEntryComponent entry : responseBundle.getEntry()) {
            if (entry.getResource() instanceof Patient patientResource) {
                patient = patientResource;
                Logger.logInfo(MessageConstants.EXIST_SCREENED_RELATED_PERSON_LOG);
            } else if (entry.getResource() instanceof RelatedPerson relatedPersonResource) {
                Logger.logInfo(MessageConstants.EXIST_SCREENED_PATIENT_LOG);
                relatedPerson = relatedPersonResource;
            }
        }
        return createScreeningLogInFhir(request, organization, patient, relatedPerson);
    }

    /**
     * <p>
     * Create screening log details and saved in FHIR Server
     * </p>
     *
     * @param request       The screening log request DTO received in the request body.
     * @param organization  The screened organization details
     * @param patient       The screened patient details
     * @param relatedPerson The screened related person details
     * @return A String containing the created screening log fhir details.
     */
    private BioDataDTO createScreeningLogInFhir(ScreeningLogRequestDTO request,
                                                Organization organization, Patient patient,
                                                RelatedPerson relatedPerson) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String userCountryId = String.valueOf(UserContextHolder.getUserDto().getCountry().getId());
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(request.getUserId().toString());
        provenanceDTO.setOrganizationId(request.getSiteId().toString());
        provenanceDTO.setModifiedDate(request.getScreeningDateTime());
        Location location = createLocation(request, organization);
        if (!isPatientEnrolled(patient)) {
            patient = createPatient(request, patient, organization);
            relatedPerson = createRelatedPerson(request, patient, relatedPerson);
        }
        Encounter encounter = createEncounter(patient, relatedPerson, location, organization, request);
        ServiceRequest serviceRequest = createReferralTicket(request, patient, relatedPerson, encounter);
        List<ServiceRequest> serviceRequests = updateOldReferralTicket(request, patient);
        Observation bpLogObservation = createBpLogObservation(request, encounter, organization);
        Observation glucoseLogObservation = createGlucoseLogObservation(request, encounter, organization);
        Observation heightObservation = createHeightObservation(request, encounter, organization);
        Observation weightObservation = createWeightObservation(request, encounter, organization);
        Observation bmiObservation = createBmiObservation(request, encounter, organization);
        Observation suicideScreener = createSuicideObservation(request, encounter, organization);
        Observation symptomObservation = createSymptomObservation(request.getGlucoseLog(), encounter);
        Observation substanceAbuse = createSubstanceAbuseObservation(request, encounter, organization);
        Observation regularSmoker = createRegularSmokerObservation(request, encounter, organization);
        Observation pregnancyObservation = createPregnancyObservation(request, encounter, organization, relatedPerson,
                patient, bundle);
        QuestionnaireResponse questionnaireResponse = createQuestionnaireResponse(request, organization,
                patient, relatedPerson, encounter);
        Observation mentalHealthObservation = createMentalHealthObservation(request, relatedPerson,
                questionnaireResponse);
        QuestionnaireResponse hivQuestionnaireResponse = createHIVQuestionnaireResponse(request, organization,
                patient, relatedPerson, encounter);
        ResponseEntity<FhirResponseDTO> responseEntity;
        Map<String, List<String>> responseWithFhirIds;

        commonConverter.setPatientDetailsInBundle(bundle, patient,
                FhirConstants.PATIENT_IDENTIFIER_URL, provenanceDTO, request.getCountryId(), userCountryId);
        commonConverter.setLocationDetailsInBundle(bundle, location,
                FhirConstants.LOCATION_IDENTIFIER_URL, provenanceDTO);
        commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPerson,
                FhirConstants.RELATED_PERSON_IDENTIFIER_URL, provenanceDTO, request.getCountryId(), userCountryId);
        commonConverter.setEncounterDetailsInBundle(bundle, encounter,
                FhirConstants.ENCOUNTER_IDENTIFIER_URL, provenanceDTO);
        commonConverter.setQuestionnarieDetailsInBundle(bundle, questionnaireResponse,
                FhirConstants.PHQ4_QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO, Boolean.FALSE);
        commonConverter.setHIVQuestionnarieDetailsInBundle(bundle, hivQuestionnaireResponse,
                FhirConstants.HIV_QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, bpLogObservation,
                FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, glucoseLogObservation,
                FhirConstants.BLOOD_SUGAR_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, symptomObservation,
                FhirConstants.SYMPTOM_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, pregnancyObservation,
                FhirConstants.PREGNANCY_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, heightObservation,
                FhirConstants.HEIGHT_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, weightObservation,
                FhirConstants.WEIGHT_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, bmiObservation,
                FhirConstants.BMI_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, suicideScreener,
                FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, substanceAbuse,
                FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, relatedPerson, regularSmoker,
                FhirConstants.REGULAR_SMOKER_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, patient, null, mentalHealthObservation,
                FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL, provenanceDTO);
        setServiceRequestDetails(bundle, serviceRequest,
                FhirConstants.REFERRAL_TICKER_IDENTIFIER_URL, provenanceDTO);
        setOldServiceRequestDetails(bundle, serviceRequests);
        Observation vitalSignsObservation = createVitalSignsObservation(request, bundle, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, patient, null, vitalSignsObservation,
                FhirConstants.VITAL_SIGNS_IDENTIFIER_URL, provenanceDTO);
        responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.nonNull(responseEntity) && Objects.nonNull(responseEntity.getBody())) {
            responseWithFhirIds = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
            request.getBioData().setMemberReference(Objects.nonNull(responseWithFhirIds.get(FhirConstants.RELATED_PERSON)) ? responseWithFhirIds.get(FhirConstants.RELATED_PERSON).getFirst() : null);
            request.getBioData().setPatientReference(Objects.nonNull(responseWithFhirIds.get(FhirConstants.PATIENT)) ? responseWithFhirIds.get(FhirConstants.PATIENT).getFirst() : null);
            request.getBioData().setEncounterReference(Objects.nonNull(responseWithFhirIds.get(FhirConstants.ENCOUNTER)) ? responseWithFhirIds.get(FhirConstants.ENCOUNTER).getFirst() : null);
        }
        if (Objects.nonNull(patient)) {
            List<Identifier> patientIdentifiers = patient.getIdentifier().stream().filter(
                    identifier -> identifier.getSystem().contains(FhirConstants.PATIENT_STATUS)).toList();
            if (!patientIdentifiers.isEmpty()) {
                patientIdentifiers.stream().findFirst().ifPresent(identifier ->
                        request.getBioData().setPatientStatus(identifier.getValue()));
            }
        }
        List<Identifier> relatedPersonIdentifiers = relatedPerson.getIdentifier().stream().filter(
                identifier -> identifier.getSystem().contains(FhirConstants.PATIENT_STATUS)).toList();
        if (!relatedPersonIdentifiers.isEmpty()) {
            relatedPersonIdentifiers.stream().findFirst().ifPresent(identifier ->
                    request.getBioData().setRelatedPersonStatus(identifier.getValue()));
        }
        return request.getBioData();
    }

    /**
     * Checks if the given patient is enrolled.
     *
     * @param patient The FHIR Patient entity to check.
     * @return true if the patient is enrolled, false otherwise.
     */
    private boolean isPatientEnrolled(Patient patient) {
        if (Objects.nonNull(patient)) {
            return patient.getIdentifier().stream().anyMatch(identifier -> identifier.getSystem()
                    .equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL) && identifier.getValue()
                    .equals(FhirConstants.ENROLLED));
        }
        return false;
    }

    /**
     * Creates a symptom observation based on the given glucose log data transfer object and encounter.
     *
     * @param glucoseLogDTO the data transfer object containing glucose log details
     * @param encounter     the encounter associated with the observation
     * @return an Observation object representing the symptom observation created
     */
    private Observation createSymptomObservation(GlucoseLogDTO glucoseLogDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(glucoseLogDTO.getDiabetes())) {
            observation = symptomConverter.createSymptomObservation(glucoseLogDTO, glucoseLogDTO.getBgTakenOn());
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.SYMPTOM);
        }
        return observation;
    }

    /**
     * Process and create FHIR Location entity based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param organization The FHIR organization entity
     * @return Converted FHIR Location entity.
     */
    private Location createLocation(ScreeningLogRequestDTO requestDTO, Organization organization) {
        Location location = null;
        if (Objects.nonNull(requestDTO.getBioData().getLandmark())) {
            location = locationConverter.createLocation(requestDTO.getBioData().getLandmark(),
                    requestDTO.getLongitude(), requestDTO.getLatitude());
            commonConverter.setLocationOrganization(location, organization);
        }
        return location;
    }

    /**
     * Process and create FHIR Patient entity based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param patient      The FHIR Patient entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Patient entity.
     */
    private Patient createPatient(ScreeningLogRequestDTO requestDTO, Patient patient,
                                  Organization organization) {
        if (Boolean.TRUE.equals(requestDTO.getIsReferAssessment())
                || (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart()))) {
            patient = patientConverter.createPatient(patient, requestDTO.getBioData(),
                    requestDTO.getBioMetrics(), requestDTO.getBioMetrics().getDateOfBirth());
            boolean identifierExists = patient.getIdentifier().stream()
                    .anyMatch(identifier -> FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL
                            .equals(identifier.getSystem()));
            if (!identifierExists) {
                patient.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                        .setValue(FhirConstants.SCREENED);
            }
            commonConverter.setPatientOrganization(patient, organization);
        }
        if (Objects.nonNull(requestDTO.getIsReferAssessment()) && Objects.nonNull(patient)) {
            patient.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)) {
                    identifier.setValue(Boolean.TRUE.equals(requestDTO.getIsReferAssessment()) ? Constants.YES : Constants.NO);
                }
            });
        }
        return patient;
    }

    /**
     * Process and create FHIR Service request entity based on given
     * screening log details
     *
     * @param requestDTO    The screening log details
     * @param patient       The FHIR Patient entity
     * @param relatedPerson The FHIR Related person entity
     * @param encounter     The FHIR Encounter entity
     * @return Converted FHIR Patient entity.
     */
    private ServiceRequest createReferralTicket(ScreeningLogRequestDTO requestDTO, Patient patient,
                                                RelatedPerson relatedPerson, Encounter encounter) {
        ServiceRequest serviceRequest = null;
        if (Boolean.TRUE.equals(requestDTO.getIsReferAssessment())
                && Objects.nonNull(requestDTO.getReferredReasons())
                && !requestDTO.getReferredReasons().isEmpty()) {
            String reason = String.join(Constants.COMMA, requestDTO.getReferredReasons());
            serviceRequest = commonConverter.createServiceRequest(requestDTO.getSiteId().toString(),
                    requestDTO.getUserId().toString(), reason, requestDTO.getScreeningDateTime());
            if (Objects.nonNull(serviceRequest)) {
                commonConverter.setServiceRequestReference(serviceRequest, patient, relatedPerson, encounter);
            }
        }
        return serviceRequest;
    }

    /**
     * Update FHIR Service request entity based on given
     * screening log details
     *
     * @param requestDTO The screening log details
     * @param patient    The FHIR Patient entity
     * @return Converted FHIR Patient entity.
     */
    private List<ServiceRequest> updateOldReferralTicket(ScreeningLogRequestDTO requestDTO, Patient patient) {
        List<ServiceRequest> serviceRequests = new ArrayList<>();
        if (Boolean.TRUE.equals(requestDTO.getIsReferAssessment()) && Objects.nonNull(requestDTO.getReferredReasons())
                && !requestDTO.getReferredReasons().isEmpty() && Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.SERVICE_REQUEST_BY_PATIENT_ID_QUERY,
                    patient.getIdPart()));
            for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
                ServiceRequest oldServiceRequest = (ServiceRequest) component.getResource();
                oldServiceRequest.setStatus(ServiceRequest.ServiceRequestStatus.COMPLETED);
                serviceRequests.add(oldServiceRequest);
            }
        }
        return serviceRequests;
    }

    /**
     * Process and create FHIR Related Person entity based on given
     * screening log details
     *
     * @param requestDTO    The screening log details
     * @param patient       The FHIR Patient entity
     * @param relatedPerson The FHIR Related Person entity
     * @return Converted FHIR Patient entity.
     */
    private RelatedPerson createRelatedPerson(ScreeningLogRequestDTO requestDTO, Patient patient,
                                              RelatedPerson relatedPerson) {
        relatedPerson = relatedPersonConverter.createRelatedPerson(relatedPerson,
                requestDTO.getBioData(), requestDTO.getBioMetrics(),
                requestDTO.getBioMetrics().getDateOfBirth(),
                requestDTO.getSiteId().toString(), requestDTO.getCountryId(),
                Boolean.TRUE.equals(requestDTO.getIsReferAssessment()) ? Constants.YES : Constants.NO);
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            relatedPerson.setPatient(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
        } else if (Objects.nonNull(patient)) {
            relatedPerson.setPatient(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
        }
        if (Objects.nonNull(patient) && patient.getLink().isEmpty()) {
            commonConverter.setPatientLinkComponentReference(patient, relatedPerson);
        }
        return relatedPerson;
    }

    /**
     * Process and create FHIR Encounter entity based on given
     * screening log details
     *
     * @param patient       The FHIR Patient entity
     * @param relatedPerson The FHIR Related Person entity
     * @param location      The FHIR Location entity
     * @param organization  The FHIR Organization entity
     * @param requestDTO    The screening log details
     * @return Converted FHIR Encounter entity.
     */
    private Encounter createEncounter(Patient patient, RelatedPerson relatedPerson,
                                      Location location, Organization organization,
                                      ScreeningLogRequestDTO requestDTO) {
        Encounter encounter = encounterConverter.createEncounter(patient, relatedPerson, location,
                requestDTO.getCategory(), requestDTO.getScreeningDateTime());
        encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL)
                .setValue(FhirConstants.SCREENING);
        encounterConverter.setEncounterClassHistory(encounter, requestDTO.getType());

        if (requestDTO.getType().equalsIgnoreCase(MetaCodeConstants.OTHER)
                && Objects.nonNull(requestDTO.getOtherType())) {
            CodeableConcept codeableConcept = fhirUtils.setCodes(MetaCodeConstants.OTHER_KEY);
            codeableConcept.setText(requestDTO.getOtherType());
            encounter.setType(List.of(codeableConcept));
        }
        commonConverter.setEncounterOrganization(encounter, organization);
        return encounter;
    }

    /**
     * Process and create FHIR vital signs observation based on given
     * bundle details
     *
     * @param request       The ScreeningLogRequestDTO entity it contains screened details
     * @param bundle        The FHIR bundle entity it contains patient vital observations.
     * @param relatedPerson The FHIR RelatedPerson entity.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     * @return Converted FHIR Observation entity.
     */
    private Observation createVitalSignsObservation(ScreeningLogRequestDTO request, Bundle bundle,
                                                    RelatedPerson relatedPerson,
                                                    ProvenanceDTO provenanceDTO) {
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(relatedPerson.getIdPart());
        vitalSignsDTO.setProvenanceDTO(provenanceDTO);
        vitalSignsDTO.setBpObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL));
        vitalSignsDTO.setBgObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.BLOOD_SUGAR_IDENTIFIER_URL));
        vitalSignsDTO.setHeightObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.HEIGHT_IDENTIFIER_URL));
        vitalSignsDTO.setWeightObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.WEIGHT_IDENTIFIER_URL));
        vitalSignsDTO.setBmiObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.BMI_IDENTIFIER_URL));
        vitalSignsDTO.setRegularSmokerObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.REGULAR_SMOKER_IDENTIFIER_URL));
        vitalSignsDTO.setSubstanceAbuseObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL));
        vitalSignsDTO.setSuicideObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL));
        vitalSignsDTO.setPregnancyObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.PREGNANCY_IDENTIFIER_URL));
        vitalSignsDTO.setMentalHealthObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL));
        vitalSignsDTO.setRedRiskObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.RED_RISK_OBSERVATION_IDENTIFIER_URL));
        if (Objects.nonNull(request.getBioData().getLandmark())) {
            vitalSignsDTO.setScreenedLandmark(request.getBioData().getLandmark());
        }
        Observation observation = vitalSignsConverter
                .createOrUpdateVitalSigns(vitalSignsDTO, bundle);
        commonConverter.setObservationText(observation, FhirConstants.VITAL_SIGNS);
        return observation;
    }

    /**
     * Process and create FHIR bp log observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createBpLogObservation(ScreeningLogRequestDTO requestDTO,
                                               Encounter encounter, Organization organization) {
        Map<String, String> cvdRiskDetails = new HashMap<>();
        Observation observation = null;
        if (Objects.nonNull(requestDTO.getBpLog())) {
            requestDTO.getBpLog().setType(Constants.ASSESSMENT_TYPE_SCREENING);
            if (Objects.nonNull(requestDTO.getCvdRiskScore())) {
                cvdRiskDetails.put(FhirConstants.CVD_RISK_SCORE,
                        String.valueOf(requestDTO.getCvdRiskScore()));
            }
            if (Objects.nonNull(requestDTO.getCvdRiskLevel())) {
                cvdRiskDetails.put(FhirConstants.CVD_RISK_LEVEL,
                        requestDTO.getCvdRiskLevel());
            }
            if (Objects.nonNull(requestDTO.getCvdRiskScoreDisplay())) {
                cvdRiskDetails.put(FhirConstants.CVD_RISK_SCORE_DISPLAY,
                        requestDTO.getCvdRiskScoreDisplay());
            }
            observation = bloodPressureConverter
                    .createBloodPressureObservation(requestDTO.getBpLog(), requestDTO.getScreeningDateTime(),
                            cvdRiskDetails);
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, FhirConstants.BLOOD_PRESSURE);
        }
        return observation;
    }

    /**
     * Process and create FHIR glucose log observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createGlucoseLogObservation(ScreeningLogRequestDTO requestDTO,
                                                    Encounter encounter, Organization organization) {
        Observation observation = null;
        if (Objects.nonNull(requestDTO.getGlucoseLog())) {
            observation = bloodGlucoseConverter
                    .createBloodGlucoseObservation(requestDTO.getGlucoseLog());
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, FhirConstants.BLOOD_GLUCOSE);
        }
        return observation;
    }

    /**
     * Process and create FHIR bp log observation based on given
     * screening log details
     *
     * @param requestDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     * @param organization  The FHIR Organization entity
     * @param relatedPerson The FHIR related person entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createPregnancyObservation(ScreeningLogRequestDTO requestDTO,
                                                   Encounter encounter, Organization organization,
                                                   RelatedPerson relatedPerson, Patient patient, Bundle bundle) {
        Observation observation = null;
        PregnancyDetailsDTO pregnancyDetailsDTO = requestDTO.getPregnancyAnc();
        if (Objects.nonNull(pregnancyDetailsDTO)) {
            ProvenanceDTO provenanceDTO = new ProvenanceDTO();
            provenanceDTO.setUserId(requestDTO.getUserId().toString());
            provenanceDTO.setModifiedDate(requestDTO.getScreeningDateTime());
            provenanceDTO.setOrganizationId(requestDTO.getSiteId().toString());
            pregnancyDetailsDTO.setProvenance(provenanceDTO);
            if (Objects.nonNull(pregnancyDetailsDTO.getLastMenstrualPeriod())) {
                Date estimatedDeliveryDate = DateUtil.addDate(pregnancyDetailsDTO.getLastMenstrualPeriod(), 280);
                pregnancyDetailsDTO.setEstimatedDeliveryDate(estimatedDeliveryDate);
            }
            if (Objects.nonNull(pregnancyDetailsDTO.getPregnancySymptoms())
                    && !pregnancyDetailsDTO.getPregnancySymptoms().isEmpty()) {
                if (pregnancyDetailsDTO.getPregnancySymptoms().size() == 1
                        && pregnancyDetailsDTO.getPregnancySymptoms().getFirst().getName().equals(Constants.NO_SYMPTOMS)) {
                    pregnancyDetailsDTO.setIsPregnancyRisk(Boolean.FALSE);
                } else {
                    pregnancyDetailsDTO.setIsPregnancyRisk(Boolean.TRUE);
                    createRedRiskObservation(relatedPerson, patient, bundle, provenanceDTO);
                }
            } else if (Objects.nonNull(requestDTO.getBioMetrics())
                    && (requestDTO.getBioMetrics().getAge() < 18 || requestDTO.getBioMetrics().getAge() > 35)) {
                pregnancyDetailsDTO.setIsPregnancyRisk(Boolean.TRUE);
            }
            String url = String.format(Constants.FETCH_LATEST_OBSERVATION_QUERY, FhirConstants.PREGNANCY.toLowerCase(),
                    Observation.ObservationStatus.PRELIMINARY.name().toLowerCase(), relatedPerson.getIdPart());
            Bundle pregnancyBundle = restApiUtil.getBatchRequest(url);
            if (pregnancyBundle.getEntry().isEmpty()) {
                observation = pregnancyConverter.createPregnancyObservation(pregnancyDetailsDTO);
            } else {
                Observation pregnancyObservation = (Observation) pregnancyBundle.getEntry().getFirst().getResource();
                observation = pregnancyConverter.updatePregnancyObservation(pregnancyObservation, pregnancyDetailsDTO);
            }
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
        }
        return observation;
    }

    /**
     * <p>
     * Create patient red risk observation based on given
     * assessment details
     * </p>
     *
     * @param relatedPerson The Fhir RelatedPerson entity
     */
    private void createRedRiskObservation(RelatedPerson relatedPerson, Patient patient, Bundle bundle,
                                          ProvenanceDTO provenanceDTO) {
        Observation redRiskObservation = commonConverter.createRiskLevelObservation(Constants.HIGH,
                null, null);
        setPatientRiskDetailsInBundle(redRiskObservation, patient, relatedPerson, provenanceDTO, bundle);
        commonConverter.updateRiskLevel(Constants.HIGH, patient);
        commonConverter.updateRiskLevel(Constants.HIGH, relatedPerson);

    }


    /**
     * <p>
     * Sets patient mental health and red risk details in bundle
     * </p>
     *
     * @param redRiskObservation The FHIR Observation entity
     * @param patient            The FHIR Patient entity.
     * @param relatedPerson      The FHIR RelatedPerson entity.
     * @param provenanceDTO      The ProvenanceDTO entity to store performers information
     * @param bundle             The FHIR bundle entity it contains patient vital observations
     */
    private void setPatientRiskDetailsInBundle(Observation redRiskObservation,
                                               Patient patient, RelatedPerson relatedPerson,
                                               ProvenanceDTO provenanceDTO, Bundle bundle) {
        if (Objects.nonNull(redRiskObservation)) {
            commonConverter.setObservationReference(redRiskObservation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, redRiskObservation,
                    FhirConstants.RED_RISK_OBSERVATION_IDENTIFIER_URL, provenanceDTO);
        }
    }

    /**
     * Process and create FHIR height observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createHeightObservation(ScreeningLogRequestDTO requestDTO,
                                                Encounter encounter, Organization organization) {
        Observation observation = null;
        if (Objects.nonNull(requestDTO.getBioMetrics().getHeight())) {
            observation = commonConverter
                    .createBasicObservation(requestDTO.getBioMetrics(),
                            requestDTO.getScreeningDateTime(), FhirConstants.HEIGHT);
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, FhirConstants.HEIGHT);
        }
        return observation;
    }

    /**
     * Process and create FHIR weight observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createWeightObservation(ScreeningLogRequestDTO requestDTO,
                                                Encounter encounter, Organization organization) {
        Observation observation = null;
        if (Objects.nonNull(requestDTO.getBioMetrics().getWeight())) {
            observation = commonConverter
                    .createBasicObservation(requestDTO.getBioMetrics(),
                            requestDTO.getScreeningDateTime(), FhirConstants.WEIGHT);
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, FhirConstants.WEIGHT);
        }
        return observation;
    }

    /**
     * Process and create FHIR BMI observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createBmiObservation(ScreeningLogRequestDTO requestDTO,
                                             Encounter encounter, Organization organization) {
        Observation observation = null;
        if (Objects.nonNull(requestDTO.getBioMetrics().getBmi())) {
            observation = commonConverter
                    .createBasicObservation(requestDTO.getBioMetrics(),
                            requestDTO.getScreeningDateTime(), FhirConstants.BMI);
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, FhirConstants.BMI);
        }
        return observation;
    }

    /**
     * Process and create FHIR regular smoker observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createRegularSmokerObservation(ScreeningLogRequestDTO requestDTO,
                                                       Encounter encounter, Organization organization) {
        Observation observation = null;
        observation = commonConverter.createRegularSmokerObservation(requestDTO.getBioMetrics().getIsRegularSmoker(), requestDTO.getScreeningDateTime());
        commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
        commonConverter.setObservationText(observation, FhirConstants.REGULAR_SMOKER);
        return observation;
    }

    /**
     * Process and create FHIR substance abuse observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createSubstanceAbuseObservation(ScreeningLogRequestDTO requestDTO,
                                                        Encounter encounter, Organization organization) {
        Observation observation = null;
        if (Objects.nonNull(requestDTO.getSubstanceAbuse()) && !requestDTO.getSubstanceAbuse().isEmpty()) {
            observation = commonConverter.createSubstanceAbuseObservation(requestDTO.getSubstanceAbuse(),
                    requestDTO.getCageAid(),
                    requestDTO.getScreeningDateTime());
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, MetaCodeConstants.SUBSTANCE_ABUSE);
            if (Objects.nonNull(requestDTO.getCageAid())) {
                observation.setValue(new StringType(String.valueOf(requestDTO.getCageAid())));
            }
        }
        return observation;
    }

    /**
     * Process and create FHIR suicide screener observation based on given
     * screening log details
     *
     * @param requestDTO   The screening log details
     * @param encounter    The FHIR Encounter entity
     * @param organization The FHIR Organization entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createSuicideObservation(ScreeningLogRequestDTO requestDTO,
                                                 Encounter encounter,
                                                 Organization organization) {
        Observation observation = null;
        if (!StringUtils.isBlank(requestDTO.getSuicidalIdeation())
                && Objects.nonNull(requestDTO.getSuicideScreener())
                && !requestDTO.getSuicideScreener().isEmpty()) {
            observation = commonConverter.createSuicideScreenerObservation(requestDTO.getSuicideScreener(),
                    requestDTO.getScreeningDateTime());
            observation.getValueCodeableConcept().addCoding()
                    .setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                    .setCode((Constants.YES.equalsIgnoreCase(requestDTO.getSuicidalIdeation()) ? Constants.YES_CODE : Constants.NO_CODE))
                    .setDisplay(Constants.YES.equalsIgnoreCase(requestDTO.getSuicidalIdeation()) ? Constants.YES : Constants.NO);
            commonConverter.setObservationEncounterAndOrganization(observation, organization, encounter);
            commonConverter.setObservationText(observation, MetaCodeConstants.SUICIDAL_SCREENER);
        }
        return observation;
    }

    /**
     * <p>
     * Create patient mental health observation based on given
     * screening log details
     * </p>
     *
     * @param requestDTO            The screening log details
     * @param relatedPerson         The Fhir RelatedPerson entity
     * @param questionnaireResponse The Fhir QuestionnaireResponse entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createMentalHealthObservation(ScreeningLogRequestDTO requestDTO,
                                                      RelatedPerson relatedPerson,
                                                      QuestionnaireResponse questionnaireResponse) {
        MentalHealthObservationDTO mentalHealthObservationDTO = new MentalHealthObservationDTO();
        Map<String, String> riskDetails = new HashMap<>();
        Map<String, QuestionnaireResponse> questionnaireResponses = new HashMap<>();
        Observation observation = null;

        if (Objects.nonNull(requestDTO.getPhq4())) {
            riskDetails.put(FhirConstants.PHQ4_RISK_LEVEL,
                    requestDTO.getPhq4().getRiskLevel());
            riskDetails.put(FhirConstants.PHQ4_SCORE,
                    String.valueOf(requestDTO.getPhq4().getScore()));
            if (!Objects.isNull(requestDTO.getPhq4().getMentalHealthDetails())) {
                for (MentalHealthDetailsDTO mentalHealthDetails : requestDTO.getPhq4().getMentalHealthDetails()) {
                    if (Constants.ONE == mentalHealthDetails.getDisplayOrder()
                            || Constants.TWO == mentalHealthDetails.getDisplayOrder()) {
                        requestDTO.getPhq4().setFirstScore(requestDTO.getPhq4().getFirstScore() + mentalHealthDetails.getScore());
                    } else if (Constants.THREE == mentalHealthDetails.getDisplayOrder()
                            || Constants.FOUR == mentalHealthDetails.getDisplayOrder()) {
                        requestDTO.getPhq4().setSecondScore(requestDTO.getPhq4().getSecondScore() + mentalHealthDetails.getScore());
                    }
                }
            }
            riskDetails.put(FhirConstants.PHQ4_FIRST_SCORE,
                    String.valueOf(requestDTO.getPhq4().getFirstScore()));
            riskDetails.put(FhirConstants.PHQ4_SECOND_SCORE,
                    String.valueOf(requestDTO.getPhq4().getSecondScore()));
        }
        if (Objects.nonNull(requestDTO.getPhq4()) && !riskDetails.isEmpty()) {
            questionnaireResponses.put(Constants.PHQ4, questionnaireResponse);
            mentalHealthObservationDTO.setMentalRiskDetails(riskDetails);
            mentalHealthObservationDTO.setRelatedPersonId(relatedPerson.getIdPart());
            mentalHealthObservationDTO.setQuestionnaireResponses(questionnaireResponses);
            observation = questionnaireResponseConverter.processMentalHealthDetails(mentalHealthObservationDTO);
        }
        return observation;
    }

    /**
     * Process and create FHIR questionnaire response based on given
     * screening log details
     *
     * @param requestDTO    The screening log details
     * @param organization  The FHIR Organization entity
     * @param patient       The FHIR Patient entity
     * @param relatedPerson The FHIR Related Person entity
     * @param encounter     The FHIR Encounter entity
     * @return Converted FHIR Questionnaire Response entity.
     */
    private QuestionnaireResponse createQuestionnaireResponse(ScreeningLogRequestDTO requestDTO,
                                                              Organization organization, Patient patient,
                                                              RelatedPerson relatedPerson,
                                                              Encounter encounter) {
        QuestionnaireResponse questionnaireResponse = null;
        if (Objects.nonNull(requestDTO.getPhq4())) {
            questionnaireResponse = questionnaireResponseConverter
                    .createMentalHealthQuestionnaireResponse(requestDTO.getPhq4(), requestDTO.getScreeningDateTime(), Constants.PHQ4);
            questionnaireResponseConverter.setReference(questionnaireResponse, patient, relatedPerson);
            commonConverter.setQuestionnaireResponseReference(questionnaireResponse, organization, encounter);
        }
        return questionnaireResponse;
    }

    /**
     * Creates a new QuestionnaireResponse for an HIV screening, using information from various FHIR resources
     * such as Organization, Patient, RelatedPerson, and Encounter. This method helps build a comprehensive
     * response specific to an HIV screening scenario.
     *
     * @param requestDTO    a ScreeningLogRequestDTO containing the screening details and any related metadata.
     *                      This object holds data required to populate the QuestionnaireResponse for the HIV screening.
     * @param organization  the Organization responsible for conducting the screening.
     *                      This represents the facility or healthcare organization linked to the HIV questionnaire.
     * @param patient       the Patient object representing the individual being screened for HIV.
     *                      This is the primary subject of the questionnaire response.
     * @param relatedPerson a RelatedPerson who may be involved in the patient's care (e.g., caregiver, family member).
     *                      This provides context or additional relationship data for the HIV questionnaire response.
     * @param encounter     an Encounter object indicating the specific visit or event during which the HIV screening took place.
     *                      This helps associate the questionnaire with a particular clinical encounter.
     * @return a populated QuestionnaireResponse tailored to the HIV screening context, which can then be added to a FHIR Bundle or used in further processing.
     */
    private QuestionnaireResponse createHIVQuestionnaireResponse(ScreeningLogRequestDTO requestDTO,
                                                                 Organization organization, Patient patient,
                                                                 RelatedPerson relatedPerson,
                                                                 Encounter encounter) {
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();
        boolean isQuestionnaireResponse = false;
        if (Objects.nonNull(requestDTO.getGeneralHealth())) {
            isQuestionnaireResponse = true;
            questionnaireResponse = questionnaireResponseConverter
                    .createHIVQuestionnaireResponse(requestDTO.getGeneralHealth(), questionnaireResponse);
        }
        if (Objects.nonNull(requestDTO.getTbSymptoms())) {
            isQuestionnaireResponse = true;
            questionnaireResponse = questionnaireResponseConverter
                    .createHIVQuestionnaireResponse(requestDTO.getTbSymptoms(), questionnaireResponse);
        }
        if (Objects.nonNull(requestDTO.getStdScreening())) {
            isQuestionnaireResponse = true;
            questionnaireResponse = questionnaireResponseConverter
                    .createHIVQuestionnaireResponse(requestDTO.getStdScreening(), questionnaireResponse);
        }
        if (Objects.nonNull(requestDTO.getHivRiskBehaviours())) {
            isQuestionnaireResponse = true;
            questionnaireResponse = questionnaireResponseConverter
                    .createHIVQuestionnaireResponse(requestDTO.getHivRiskBehaviours(), questionnaireResponse);
        }
        if (Objects.nonNull(requestDTO.getHivHistory())) {
            isQuestionnaireResponse = true;
            questionnaireResponse = questionnaireResponseConverter
                    .createHIVQuestionnaireResponse(requestDTO.getHivHistory(), questionnaireResponse);
        }
        if (Objects.nonNull(requestDTO.getRisksOfHIVInfection())) {
            isQuestionnaireResponse = true;
            questionnaireResponse = questionnaireResponseConverter
                    .createHIVQuestionnaireResponse(requestDTO.getRisksOfHIVInfection(), questionnaireResponse);
        }
        if (isQuestionnaireResponse) {
            questionnaireResponse = questionnaireResponseConverter.createBasicQuestionnaireResponse(questionnaireResponse, requestDTO.getScreeningDateTime(), Constants.HIV);
            questionnaireResponseConverter.setReference(questionnaireResponse, patient, relatedPerson);
            commonConverter.setQuestionnaireResponseReference(questionnaireResponse, organization, encounter);
        }
        return isQuestionnaireResponse ? questionnaireResponse : null;
    }

    /**
     * Set observation details in FHIR bundle resource
     *
     * @param bundle        The FHIR bundle resource
     * @param patient       The FHIR Patient entity
     * @param relatedPerson The FHIR Related Person entity
     * @param observation   The FHIR Observation entity
     * @param identifierUrl The Observation identifier url
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     */
    private void setObservationDetails(Bundle bundle, Patient patient,
                                       RelatedPerson relatedPerson,
                                       Observation observation,
                                       String identifierUrl, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(observation)) {
            commonConverter.setObservationReference(observation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, observation, identifierUrl, provenanceDTO);
        }
    }

    /**
     * Set Service request details in FHIR bundle resource
     *
     * @param bundle         The FHIR bundle resource
     * @param serviceRequest The FHIR Service Request entity
     * @param identifierUrl  The Service request identifier url
     * @param provenanceDTO  The ProvenanceDTO entity to store performers information
     */
    private void setServiceRequestDetails(Bundle bundle,
                                          ServiceRequest serviceRequest,
                                          String identifierUrl, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(serviceRequest)) {
            commonConverter.setServiceRequestInBundle(bundle, serviceRequest, identifierUrl,
                    provenanceDTO);
        }
    }

    /**
     * Set old service request details in FHIR bundle resource
     *
     * @param bundle          The FHIR bundle resource
     * @param serviceRequests List of FHIR Service Request entity
     */
    private void setOldServiceRequestDetails(Bundle bundle, List<ServiceRequest> serviceRequests) {
        if (!serviceRequests.isEmpty()) {
            for (ServiceRequest serviceRequest : serviceRequests) {
                commonConverter.setServiceRequestInBundle(bundle, serviceRequest, serviceRequest.getIdPart(), null);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public ScreeningLog getScreeningLog(ScreeningLogRequestDTO requestDTO) {
        ScreeningLog screeningLog = new ScreeningLog();
        AtomicReference<Observation> riskObservation = new AtomicReference<>();
        String url = String.format(Constants.GET_MEMBER_ID, requestDTO.getFhirId());
        Bundle relatedPersonBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();
        String observationUrl = String.format(Constants.SCREENING_SEARCH_PARAM,
                StringUtil.concatString(MetaCodeConstants.RISK_LEVEL, Constants.COMMA, MetaCodeConstants.PREGNANCY_KEY),
                StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                        relatedPerson.getIdPart()));
        Bundle observationBundle = restApiUtil.getBatchRequest(observationUrl);
        observationBundle.getEntry().forEach(entry -> {
            Observation observation = (Observation) entry.getResource();
            if (observation.getCode().getText().equals(MetaCodeConstants.PREGNANCY_KEY) &&
                    Observation.ObservationStatus.PRELIMINARY.equals(observation.getStatus())) {
                screeningLog.setIsPregnant(true);
            }
            if (observation.getCode().getText().equals(MetaCodeConstants.RISK_LEVEL)) {
                riskObservation.set(observation);
            }
        });
        if (!Objects.isNull(riskObservation.get())) {
            screeningLog.setRiskLevel(riskObservation.get().getValueStringType().getValue());
            riskObservation.get().getComponent().forEach(component -> {
                if (component.getCode().getText().equals(FhirConstants.RISK_LEVEL)) {
                    screeningLog.setRiskLevel(component.getValue().toString());
                }
                if (component.getCode().getText().equals(FhirConstants.RISK_MESSAGE)) {
                    screeningLog.setRiskMessage(component.getValue().toString());
                }
            });
        }

        relatedPerson.getIdentifier().stream()
                .filter(identifier -> FhirConstants.STATUS.equals(identifier.getSystem()))
                .findFirst()
                .ifPresent(identifier -> screeningLog.setPatientStatus(identifier.getValue()));
        return screeningLog;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DashboardDetails getPatientCountOfUsers(DashboardDetailsRequestDTO requestDTO) {
        if (isNull(requestDTO.getUserId())) {
            throw new BadRequestException(1016);
        }
        Map<String, String> dateRange = getDateFilter(requestDTO);
        String url = String.format(Constants.PROVENANCE_INCLUDES_RELATED_PERSON_AND_ENCOUNTER, Constants.USER_PRACTIONER.concat(requestDTO.getUserId()),
                dateRange.get(Constants.START_DATE), dateRange.get(Constants.END_DATE));
        Logger.logInfo("The url to fetch dashboard count is: " + url);
        Bundle bundle = restApiUtil.getBatchRequest(url);
        String investigationUrl = String.format(Constants.PROVENANCE_INCLUDES_DIAGNOSTIC_REPORT, Constants.USER_PRACTIONER.concat(requestDTO.getUserId()),
                dateRange.get(Constants.START_DATE), dateRange.get(Constants.END_DATE));
        Logger.logInfo("The url to fetch investigation dashboard count is: " + investigationUrl);
        Bundle putBundle = restApiUtil.getBatchRequest(investigationUrl);
        return getCountOfPatients(bundle, putBundle);
    }

    /**
     * <p>
     * Method to calculate date filter for followup based on date range.
     * </p>
     *
     * @param requestDTO - Contains data about a patient request.
     * @return - Returns map contains start and end date.
     */
    private Map<String, String> getDateFilter(DashboardDetailsRequestDTO requestDTO) {
        Map<String, String> dateFilter = new HashMap<>();
        String userTimezone = UserContextHolder.getUserDto().getTimezone().getOffset();
        if (!Objects.isNull(requestDTO.getSortField())) {
            if (Constants.TODAY.equals(requestDTO.getSortField())) {
                dateFilter.put(Constants.START_DATE, DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Boolean.FALSE));
                dateFilter.put(Constants.END_DATE, DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Boolean.TRUE));
            } else if (Constants.YESTERDAY.equals(requestDTO.getSortField())) {
                dateFilter.put(Constants.START_DATE, DateUtil.getUserTimezoneTime(userTimezone, Constants.ONE, Boolean.FALSE));
                dateFilter.put(Constants.END_DATE, DateUtil.getUserTimezoneTime(userTimezone, Constants.ONE, Boolean.TRUE));
            } else if (Constants.THIS_WEEK.equals(requestDTO.getSortField())) {
                String startDate = DateUtil.getStartDayOfWeekByUserTimeZone(userTimezone);
                String endDate = DateUtil.convertDateToStringInFHIRFormat(
                        DateUtil.addDateWithTimezone(DateUtil.formatDate(startDate), Constants.INT_SEVEN, userTimezone));
                dateFilter.put(Constants.START_DATE, startDate);
                dateFilter.put(Constants.END_DATE, endDate);
            } else if (Constants.THIS_MONTH.equals(requestDTO.getSortField())) {
                dateFilter.put(Constants.START_DATE, DateUtil.getStartDayOfMonthByUserTimeZone(userTimezone));
                dateFilter.put(Constants.END_DATE, DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Boolean.TRUE));
            }
        } else if (!Objects.isNull(requestDTO.getCustomDate())
                && !Objects.isNull(requestDTO.getCustomDate().get(Constants.START_DATE))
                && !Objects.isNull(requestDTO.getCustomDate().get(Constants.END_DATE))) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(requestDTO.getCustomDate().get(Constants.START_DATE));
            dateFilter.put(Constants.START_DATE, DateUtil.getISOString(calendar));
            calendar.setTime(requestDTO.getCustomDate().get(Constants.END_DATE));
            dateFilter.put(Constants.END_DATE, DateUtil.getISOString(calendar));
        } else {
            throw new BadRequestException(1015);
        }
        return dateFilter;
    }

    /**
     * Get the patient count details from FHIR bundle resource
     *
     * @param bundle The FHIR bundle resource
     * @return patient count details entity.
     */
    public DashboardDetails getCountOfPatients(Bundle bundle, Bundle putBundle) {
        DashboardDetails dashboardDetails = new DashboardDetails();
        AtomicLong screened = new AtomicLong();
        AtomicLong assessed = new AtomicLong();
        AtomicLong enrolled = new AtomicLong();
        AtomicLong referred = new AtomicLong();
        long investigated = 0L;
        AtomicLong dispensed = new AtomicLong();
        AtomicLong lifestyleCount = new AtomicLong();
        AtomicLong notesCount = new AtomicLong();

        if (!putBundle.getEntry().isEmpty()) {
            Map<ResourceType, List<Resource>> resources = putBundle.getEntry().stream()
                    .map(Bundle.BundleEntryComponent::getResource).collect(groupingBy(Resource::getResourceType));
            List<DiagnosticReport> diagnosticReports = resources.getOrDefault(ResourceType.DiagnosticReport, List.of()).stream().map(DiagnosticReport.class::cast).toList();
            investigated = Long.parseLong(String.valueOf(diagnosticReports.size()));
        }
        if (!bundle.getEntry().isEmpty()) {
            Map<ResourceType, List<Resource>> resources = bundle.getEntry().stream()
                    .map(Bundle.BundleEntryComponent::getResource).collect(groupingBy(Resource::getResourceType));
            List<RelatedPerson> relatedPersons = resources.getOrDefault(ResourceType.RelatedPerson, List.of()).stream().map(RelatedPerson.class::cast).toList();
            List<Encounter> encounters = resources.getOrDefault(ResourceType.Encounter, List.of()).stream().map(Encounter.class::cast).toList();
            List<Observation> observations = resources.getOrDefault(ResourceType.Observation, List.of()).stream().map(Observation.class::cast).toList();
            List<MedicationDispense> medicationDispenses = resources.getOrDefault(ResourceType.MedicationDispense, List.of()).stream().map(MedicationDispense.class::cast).toList();
            medicationDispenses.forEach(medicationDispense ->
                medicationDispense.getIdentifier().forEach(identifier -> {
                    if (identifier.getSystem().contains(Constants.PRESCRIPTION_FILLED_DAYS) && !identifier.getValue().equals(String.valueOf(Constants.ZERO))) {
                        dispensed.addAndGet(1);
                    }
                })
            );
            List<RelatedPerson> uniqueRelatedPersons = relatedPersons.stream()
                    .collect(Collectors.toMap(
                            RelatedPerson::getIdPart,
                            person -> person,
                            (existing, replacement) -> existing))
                    .values()
                    .stream()
                    .toList();

            uniqueRelatedPersons.forEach(relatedPerson ->
                relatedPerson.getIdentifier().forEach(identifier -> {
                    if (identifier.getSystem().contains(Constants.IS_PATIENT_REFERRED) &&
                            Constants.YES.equalsIgnoreCase(identifier.getValue())) {
                        referred.addAndGet(1);
                    }
                })
            );

            observations.forEach(observation ->
                observation.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                            Constants.NUTRITION_LIFESTYLE.equals(identifier.getValue())) {
                        lifestyleCount.addAndGet(1);
                    } else if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                            Constants.PSYCHOLOGY_ASSESSMENT.equals(identifier.getValue())) {
                        notesCount.addAndGet(1);
                    }
                })
            );

            encounters.forEach(encounter ->
                encounter.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                            FhirConstants.SCREENING.equals(identifier.getValue())) {
                        screened.addAndGet(1);
                    } else if (FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                            PatientStatusConstants.ASSESSMENT.equals(identifier.getValue())) {
                        assessed.addAndGet(1);
                    } else if (FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                            PatientStatusConstants.ENROLLMENT.equals(identifier.getValue())) {
                        enrolled.addAndGet(1);
                    }
                })
            );
            dashboardDetails.setScreened(screened.get());
            dashboardDetails.setAssessed(assessed.get());
            dashboardDetails.setRegistered(enrolled.get());
            dashboardDetails.setReferred(referred.get());
            dashboardDetails.setDispensed(dispensed.get());
            dashboardDetails.setInvestigated(investigated);
            dashboardDetails.setNutritionistLifestyleCount(lifestyleCount.get());
            dashboardDetails.setPsychologicalNotesCount(notesCount.get());
        }
        return dashboardDetails;
    }
}
