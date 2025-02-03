package com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.service.PatientNutritionLifestyleService;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

/**
 * <p>
 * This is a service class for PatientNutritionLifestyle entity.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@Service
public class PatientNutritionLifestyleServiceImpl implements PatientNutritionLifestyleService {

    private final FhirUtils fhirUtils;
    private final RestApiUtil restApiUtil;
    private final UserService userService;
    private final PatientService patientService;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    @Autowired
    public PatientNutritionLifestyleServiceImpl(FhirUtils fhirUtils,
                                                RestApiUtil restApiUtil, UserService userService,
                                                PatientService patientService) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.userService = userService;
        this.patientService = patientService;
    }

    private static final String LIFESTYLE_LIST_QUERY =
            ResourceType.Observation + Constants.QUESTION_MARK + Observation.SP_IDENTIFIER + Constants.EQUAL_SYMBOL
                    + FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL + Constants.VERTICAL_BAR + Constants.OBSERVATION_NUTRITION_LIFESTYLE
                    + Constants.AND + Observation.SP_SUBJECT + Constants.EQUAL_SYMBOL + Constants.STRING_FORMAT_SPECIFIER
                    + Constants.AND + Constants.PARAM_SORT_DESC + Constants.EQUAL_SYMBOL + Constants.PARAM_LASTUPDATED;

    private final String lifeStyleListAssessed = LIFESTYLE_LIST_QUERY
            + Constants.AND + Observation.SP_STATUS + Constants.EQUAL_SYMBOL + Observation.ObservationStatus.FINAL.toCode()
            + Constants.COMMA + Observation.ObservationStatus.AMENDED.toCode();

    private final String lifeStyleListNotViewed = LIFESTYLE_LIST_QUERY
            + Constants.AND + Observation.SP_STATUS + Constants.EQUAL_SYMBOL + Observation.ObservationStatus.AMENDED.toCode();

    private final String lifeStyleListHistory = LIFESTYLE_LIST_QUERY
            + Constants.AND + Observation.SP_STATUS + Constants.COLON + Constants.NOT
            + Constants.EQUAL_SYMBOL + Observation.ObservationStatus.CANCELLED.toCode();

    private final String lifeStyleListNotAssessed = LIFESTYLE_LIST_QUERY
            + Constants.AND + Observation.SP_STATUS + Constants.EQUAL_SYMBOL + Observation.ObservationStatus.REGISTERED.toCode();

    private static final String LIFESTYLE_BY_ID_QUERY =
            ResourceType.Observation + Constants.QUESTION_MARK + Observation.SP_RES_ID + Constants.EQUAL_SYMBOL + Constants.STRING_FORMAT_SPECIFIER;

    /**
     * {@inheritDoc}
     */
    public PatientNutritionLifestyle addPatientNutritionLifestyle(PatientNutritionLifestyle patientNutritionLifestyle) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String uuid = fhirUtils.getUniqueId();
        if (Objects.isNull(patientNutritionLifestyle.getPatientReference())) {
            Patient patient =
                    patientService.createPatientByMemberReference(patientNutritionLifestyle.getMemberReference(),
                            patientNutritionLifestyle.getProvenance(), bundle);
            patientNutritionLifestyle.setPatientReference(patient.getId());
        }
        Observation observation = convertToObservation(patientNutritionLifestyle, new Observation());

        String fullUrl = Constants.FHIR_BASE_URL.concat(uuid);
        String url =
                String.valueOf(ResourceType.Observation).concat(Constants.FORWARD_SLASH)
                        .concat(Constants.FHIR_BASE_URL)
                        .concat(uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle, patientNutritionLifestyle.getProvenance());
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));

        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        patientNutritionLifestyle.setId(response.get(String.valueOf(ResourceType.Observation)).get(Constants.ZERO));
        if (response.containsKey(String.valueOf(ResourceType.Patient))) {
            patientNutritionLifestyle.setPatientReference(response.get(String.valueOf(ResourceType.Patient)).get(Constants.ZERO));
        }

        return patientNutritionLifestyle;
    }

    /**
     * {@inheritDoc}
     */
    public List<PatientNutritionLifestyle> getPatientNutritionLifeStyleList(RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getPatientReference())) {
            throw new DataNotFoundException(2003);
        }
        List<PatientNutritionLifestyle> patientNutritionLifestyles = new ArrayList<>();

        String url;
        if (requestDTO.isNutritionist()) {
            url = lifeStyleListNotAssessed;
        } else if (requestDTO.isNutritionHistoryRequired()) {
            url = lifeStyleListAssessed;
        } else {
            url = lifeStyleListHistory;
        }
        Bundle bundle = restApiUtil.getBatchRequest(String.format(url, requestDTO.getPatientReference()));
        if (Objects.nonNull(bundle) && !bundle.getEntry().isEmpty()) {
            for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                if (entry.getResource() instanceof Observation observation) {
                    patientNutritionLifestyles.add(convertToPatientNutritionLifestyle(observation));
                }
            }
        }
        return patientNutritionLifestyles;
    }

    /**
     * <p>
     * Retrieves the count of not viewed lifestyle entries for a given patient.
     * </p>
     *
     * @param patientId the ID of the patient
     * @return the count of not viewed lifestyle entries
     */
    public int getNotViewedLifestyleCount(Long patientId) {
        if (Objects.isNull(patientId)) {
            throw new DataNotFoundException(2003);
        }
        Bundle bundle = restApiUtil.getBatchRequest(String.format(lifeStyleListNotViewed, patientId));
        return Objects.nonNull(bundle) ? bundle.getEntry().size() : Constants.ZERO;
    }

    /**
     * {@inheritDoc}
     */
    public PatientNutritionLifestyleUpdateDTO updatePatientNutritionLifestyle(PatientNutritionLifestyleUpdateDTO patientNutritionLifestyleUpdateDTO) {
        for (PatientNutritionLifestyle patientNutritionLifestyle : patientNutritionLifestyleUpdateDTO.getLifestyles()) {
            if (Objects.isNull(patientNutritionLifestyle.getId())) {
                throw new DataNotFoundException(1009);
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getIsNutritionist())) {
                patientNutritionLifestyle.setIsNutritionist(patientNutritionLifestyleUpdateDTO.getIsNutritionist());
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getProvenance())) {
                patientNutritionLifestyle.setProvenance(patientNutritionLifestyleUpdateDTO.getProvenance());
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getAssessedBy())) {
                patientNutritionLifestyle.setAssessedBy(patientNutritionLifestyleUpdateDTO.getAssessedBy());
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getReferredDate())) {
                patientNutritionLifestyle.setReferredDate(patientNutritionLifestyleUpdateDTO.getReferredDate());
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getAssessedDate())) {
                patientNutritionLifestyle.setAssessedDate(patientNutritionLifestyleUpdateDTO.getAssessedDate());
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getMemberReference())) {
                patientNutritionLifestyle.setMemberReference(patientNutritionLifestyleUpdateDTO.getMemberReference());
            }
            if (Objects.nonNull(patientNutritionLifestyleUpdateDTO.getPatientReference())) {
                patientNutritionLifestyle.setPatientReference(patientNutritionLifestyleUpdateDTO.getPatientReference());
            }
            Bundle bundle = restApiUtil.getBatchRequest(String.format(LIFESTYLE_BY_ID_QUERY,
                    patientNutritionLifestyle.getId()));
            Bundle postBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            if (Objects.nonNull(bundle) && !bundle.getEntry().isEmpty()) {
                for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                    if (entry.getResource() instanceof Observation observation) {
                        patientNutritionLifestyle.setVisitId(patientNutritionLifestyleUpdateDTO.getPatientVisitId());
                        convertToObservation(patientNutritionLifestyle, observation);
                        fhirUtils.setBundle(String.valueOf(ResourceType.Observation).concat(Constants.FORWARD_SLASH)
                                        .concat(observation.getIdPart()),
                                Constants.EMPTY_SPACE,
                                Bundle.HTTPVerb.PUT,
                                observation,
                                postBundle,
                                patientNutritionLifestyle.getProvenance());
                    }
                }
            }
        }
        return patientNutritionLifestyleUpdateDTO;
    }

    /**
     * {@inheritDoc}
     */
    public PatientNutritionLifestyle removePatientNutritionLifestyle(PatientNutritionLifestyle patientNutritionLifestyle) {
        if (Objects.isNull(patientNutritionLifestyle.getId())) {
            throw new DataNotFoundException(1009);
        }
        Bundle bundle = restApiUtil.getBatchRequest(String.format(LIFESTYLE_BY_ID_QUERY, patientNutritionLifestyle.getId()));
        Bundle postBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (entry.getResource() instanceof Observation observation) {
                observation.setStatus(Observation.ObservationStatus.CANCELLED);
                fhirUtils.setBundle(String.valueOf(ResourceType.Observation).concat(Constants.FORWARD_SLASH)
                                .concat(observation.getIdPart()), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, observation,
                        postBundle, patientNutritionLifestyle.getProvenance());

            }
        }
        return patientNutritionLifestyle;
    }

    /**
     * <p>
     * Adds notes to the given observation based on the PatientNutritionLifestyle details.
     * </p>
     *
     * @param observation               the Observation object
     * @param patientNutritionLifestyle the PatientNutritionLifestyle object containing the note details
     */
    private void addNotes(Observation observation, PatientNutritionLifestyle patientNutritionLifestyle) {
        Annotation note;
        if (Boolean.TRUE.equals(patientNutritionLifestyle.getIsNutritionist())) {
            observation.setStatus(Observation.ObservationStatus.AMENDED);
        } else {
            observation.setStatus(Observation.ObservationStatus.REGISTERED);
        }
        if (Objects.nonNull(patientNutritionLifestyle.getClinicianNote())) {
            note = new Annotation();
            note.setAuthor(new StringType(Constants.CLINICIAN_NOTE));
            note.setText(patientNutritionLifestyle.getClinicianNote());
            note.setTime(patientNutritionLifestyle.getReferredDate());
            observation.getNote().add(note);
        }
        if (Objects.nonNull(patientNutritionLifestyle.getOtherNote())) {
            note = new Annotation();
            note.setAuthor(new StringType(Constants.OTHER_NOTE));
            note.setText(patientNutritionLifestyle.getOtherNote());
            note.setTime(patientNutritionLifestyle.getAssessedDate());
            observation.getNote().add(note);
        }
        if (Objects.nonNull(patientNutritionLifestyle.getLifestyleAssessment())) {
            note = new Annotation();
            note.setAuthor(new StringType(Constants.LIFESTYLE_ASSESSMENT));
            note.setText(patientNutritionLifestyle.getLifestyleAssessment());
            note.setTime(patientNutritionLifestyle.getReferredDate());
            observation.getNote().add(note);
        }
    }

    /**
     * <p>
     * Converts a PatientNutritionLifestyle object to an Observation object.
     * </p>
     *
     * @param patientNutritionLifestyle the PatientNutritionLifestyle object to be converted
     * @param observation               the Observation object to be populated
     * @return the populated Observation object
     */
    private Observation convertToObservation(PatientNutritionLifestyle patientNutritionLifestyle,
                                             Observation observation) {
        if (Objects.isNull(observation.getId())) {
            Identifier identifier = new Identifier();
            identifier.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL);
            identifier.setValue(Constants.OBSERVATION_NUTRITION_LIFESTYLE);
            observation.getIdentifier().add(identifier);
        }

        observation.setCode(fhirUtils.setCodes(MetaCodeConstants.NUTRITION_LIFESTYLE));

        observation.setSubject(new Reference(StringUtil.concatString(ResourceType.Patient.toString(),
                Constants.FORWARD_SLASH,
                patientNutritionLifestyle.getPatientReference())));
        observation.setEncounter(new Reference(StringUtil.concatString(ResourceType.Encounter.toString(),
                Constants.FORWARD_SLASH,
                patientNutritionLifestyle.getVisitId())));
        if (Objects.nonNull(patientNutritionLifestyle.getAssessedBy())) {
            observation.setFocus(List.of(new Reference(StringUtil.concatString(ResourceType.Practitioner.toString(),
                    Constants.FORWARD_SLASH,
                    patientNutritionLifestyle.getAssessedBy()))));
        }

        if (Objects.isNull(observation.getId())) {
            for (String lifestyle : patientNutritionLifestyle.getLifestyles()) {
                Observation.ObservationComponentComponent observationComponent =
                        new Observation.ObservationComponentComponent();
                observationComponent.setCode(fhirUtils.setCodes(lifestyle));
                observationComponent.setValue(new StringType(lifestyle));
                observation.getComponent().add(observationComponent);
            }
        } else if (Objects.nonNull(patientNutritionLifestyle.getLifestyles())) {
            observation.getComponent().clear();
            for (String lifestyle : patientNutritionLifestyle.getLifestyles()) {
                Observation.ObservationComponentComponent observationComponent =
                        new Observation.ObservationComponentComponent();
                observationComponent.setCode(fhirUtils.setCodes(lifestyle));
                observationComponent.setValue(new StringType(lifestyle));
                observation.getComponent().add(observationComponent);
            }
        }

        if (Objects.nonNull(patientNutritionLifestyle.getReferredDate())) {
            observation.setIssued(patientNutritionLifestyle.getReferredDate());
        }
        if (Objects.nonNull(patientNutritionLifestyle.getAssessedDate())) {
            observation.setEffective(new DateTimeType(patientNutritionLifestyle.getAssessedDate()));
        }
        List<Reference> performer = new ArrayList<>();
        if (Objects.nonNull(patientNutritionLifestyle.getReferredBy())) {
            performer.add(new Reference(StringUtil.concatString(ResourceType.Practitioner.toString(),
                    Constants.FORWARD_SLASH,
                    patientNutritionLifestyle.getReferredBy())));
        }
        performer.add(new Reference(StringUtil.concatString(ResourceType.RelatedPerson.toString(),
                Constants.FORWARD_SLASH,
                patientNutritionLifestyle.getMemberReference())));
        performer.add(new Reference(StringUtil.concatString(ResourceType.Organization.toString(),
                Constants.FORWARD_SLASH,
                patientNutritionLifestyle.getProvenance().getOrganizationId())));
        observation.setPerformer(performer);

        addNotes(observation, patientNutritionLifestyle);
        if (patientNutritionLifestyle.isViewed()) {
            observation.setStatus(Observation.ObservationStatus.FINAL);
        }
        return observation;
    }

    /**
     * <p>
     * Converts an Observation object to a PatientNutritionLifestyle object.
     * </p>
     *
     * @param observation the Observation object to be converted
     * @return the populated PatientNutritionLifestyle object
     */
    private PatientNutritionLifestyle convertToPatientNutritionLifestyle(Observation observation) {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        patientNutritionLifestyle.setId(observation.getIdPart());
        patientNutritionLifestyle.setPatientReference(fhirUtils.getIdFromReference(observation.getSubject()
                .getReference()));
        patientNutritionLifestyle.setVisitId(fhirUtils.getIdFromReference(observation.getEncounter()
                .getReference()));
        patientNutritionLifestyle.setReferredDate(observation.getIssued());
        patientNutritionLifestyle.setAssessedDate(observation.getEffectiveDateTimeType().getValue());
        patientNutritionLifestyle.setLifestyles(observation.getComponent().stream()
                .map(component -> fhirUtils.getText(component.getCode().getText())).collect(Collectors.toSet()));
        for (Reference reference : observation.getPerformer()) {
            if (reference.getReference().contains(ResourceType.Practitioner.toString())) {
                String practitionerId = reference.getReferenceElement().getIdPart();
                patientNutritionLifestyle.setReferredBy(practitionerId);
                patientNutritionLifestyle.setReferredByDisplay(getUserFullNameById(practitionerId));
            }
            if (reference.getReference().contains(ResourceType.RelatedPerson.toString())) {
                patientNutritionLifestyle.setMemberReference(reference.getReferenceElement().getIdPart());
            }
        }
        if (Objects.nonNull(observation.getFocus()) && !observation.getFocus().isEmpty()) {
            patientNutritionLifestyle.setAssessedBy(observation.getFocus().getFirst().getReferenceElement().getIdPart());
            patientNutritionLifestyle.setAssessedByDisplay(
                    getUserFullNameById(patientNutritionLifestyle.getAssessedBy()));
        }

        if (Objects.nonNull(observation.getNote()) && !observation.getNote().isEmpty()) {
            for (Annotation note : observation.getNote()) {
                if (Constants.OTHER_NOTE.equals(note.getAuthorStringType().getValue())) {
                    patientNutritionLifestyle.setOtherNote(note.getText());
                } else if (Constants.CLINICIAN_NOTE.equals(note.getAuthorStringType().getValue())) {
                    patientNutritionLifestyle.setClinicianNote(note.getText());
                } else if (Constants.LIFESTYLE_ASSESSMENT.equals(note.getAuthorStringType().getValue())) {
                    patientNutritionLifestyle.setLifestyleAssessment(note.getText());
                }
            }
        }
        if (Observation.ObservationStatus.FINAL.equals(observation.getStatus())) {
            patientNutritionLifestyle.setViewed(true);
        }
        return patientNutritionLifestyle;
    }

    /**
     * <p>
     * Retrieves the full name of a user by their ID.
     * </p>
     *
     * @param userId the ID of the user
     * @return the full name of the user
     */
    private String getUserFullNameById(String userId) {
        Practitioner practitioner = userService.getUserById(userId);
        String userName = null;
        if (Objects.nonNull(practitioner)
                && Objects.nonNull(practitioner.getName())
                && Objects.nonNull(practitioner.getName().getFirst())) {
            userName = practitioner.getName().getFirst().getText();
        }
        return userName;
    }

}
