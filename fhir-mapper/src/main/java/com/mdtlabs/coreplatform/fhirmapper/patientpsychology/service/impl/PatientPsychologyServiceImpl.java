package com.mdtlabs.coreplatform.fhirmapper.patientpsychology.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.HttpServerErrorException;

import static java.util.Objects.isNull;
import static java.util.Objects.nonNull;

import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.patientpsychology.service.PatientPsychologyService;

/**
 * Service interface for managing psychology notes.
 * <p>
 * Defines operations for creating and retrieving patient psychology,
 * including specialized summaries for mother and neonate. This interface abstracts the
 * underlying data access and business logic for general medical review management.
 * </p>
 *
 * @author Bala Ashwanth N created on Nov 15, 2024
 */
@Service
public class PatientPsychologyServiceImpl implements PatientPsychologyService {

    private final RestApiUtil restApiUtil;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final FhirUtils fhirUtils;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    public PatientPsychologyServiceImpl(RestApiUtil restApiUtil, FhirAssessmentMapper fhirAssessmentMapper
            , FhirUtils fhirUtils) {
        this.restApiUtil = restApiUtil;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirUtils = fhirUtils;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PsychologyDTO savePatientPsychology(PsychologyDTO request) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Encounter encounter = restApiUtil.getEncounterById(String.valueOf(request.getPatientVisitId()));
        if (isNull(encounter)) {
            // have to set code
            throw new BadRequestException(1032);
        }
        EncounterDetailsDTO encounterDetailsDTO = convertEncounterToEncounterDetailsDTO(encounter);
        encounterDetailsDTO.setPatientReference(StringUtil.concatString(ResourceType.Patient.toString(),
                Constants.FORWARD_SLASH, request.getPatientReference()));
        if (!CollectionUtils.isEmpty(request.getClinicianNotes())) {
            request.getClinicianNotes().forEach(clinicianNote -> {
                Observation observation = fhirAssessmentMapper.createNoteObservation(encounterDetailsDTO,
                        request.getProvenance(), Constants.OBSERVATION_PSYCHOLOGY_NOTES, Constants.PSYCHOLOGY, clinicianNote,
                        false, null);
                fhirAssessmentMapper.addObservationToBundle(observation, bundle, request.getProvenance());
            });
        } else if (!CollectionUtils.isEmpty(request.getCounselorAssessments())) {
            request.getCounselorAssessments().forEach(counselorAssessment -> {
                if (StringUtil.isNotBlank(counselorAssessment.getCounselorAssessment())) {
                    Observation observation = fhirAssessmentMapper.createNoteObservation(encounterDetailsDTO,
                            request.getProvenance(), Constants.OBSERVATION_PSYCHOLOGY_ASSESSMENT, Constants.PSYCHOLOGY,
                            counselorAssessment.getCounselorAssessment(), true,
                            counselorAssessment.getId());
                    fhirAssessmentMapper.addObservationToBundle(observation, bundle, request.getProvenance());
                }
            });
        } else if (StringUtil.isNotBlank(request.getCounselorAssessment())) {
            Observation observation = fhirAssessmentMapper.createNoteObservation(encounterDetailsDTO,
                    request.getProvenance(), Constants.OBSERVATION_PSYCHOLOGY_ASSESSMENT, Constants.PSYCHOLOGY,
                    request.getCounselorAssessment(), true, null);
            fhirAssessmentMapper.addObservationToBundle(observation, bundle, request.getProvenance());
        } else {
            throw new Validation(1037);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        PsychologyDTO responseDTO = new PsychologyDTO();
        responseDTO.setMemberReference(request.getMemberReference());
        responseDTO.setPatientVisitId(request.getPatientVisitId());
        return responseDTO;
    }

    /**
     * {@inheritDoc}
     */
    public List<PsychologyDTO> getPatientPsychologyByRelatedPersonId(PsychologyDTO request) {
        List<PsychologyDTO> psychologyDTOS = new ArrayList<>();
        if (isNull(request.getMemberReference()) || isNull(request.getProvenance()) ||
                StringUtil.isBlank(request.getProvenance().getUserId())) {
            throw new Validation(1033);
        }
        String url = String.format(Constants.GET_PSYCHOLOGY_BY_IDENTIFIER_TYPE_AND_RELATED_PERSON_ID
                , FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.concat(Constants.VERTICAL_BAR)
                        .concat(Constants.OBSERVATION_PSYCHOLOGY_NOTES), FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL
                        .concat(Constants.VERTICAL_BAR).concat(Constants.OBSERVATION_PSYCHOLOGY_ASSESSMENT),
                request.getMemberReference());
        Bundle bundle = null;
        try {
            bundle = restApiUtil.getBatchRequest(url);
        } catch (HttpServerErrorException httpServerErrorException) {
            Logger.logError(httpServerErrorException);
            throw new DataNotFoundException(1035);
        }
        if (nonNull(bundle) && !CollectionUtils.isEmpty(bundle.getEntry())) {
            List<Observation> observations = bundle.getEntry().stream().map(Bundle.BundleEntryComponent::getResource)
                    .filter(resource -> ResourceType.Observation.equals(resource.getResourceType()))
                    .map(Observation.class::cast).filter(observation -> !Observation.ObservationStatus.CANCELLED
                            .equals(observation.getStatus())).toList();
            List<Practitioner> practitioners = bundle.getEntry().stream().map(Bundle.BundleEntryComponent::getResource)
                    .filter(resource -> ResourceType.Practitioner.equals(resource.getResourceType()))
                    .map(Practitioner.class::cast).toList();
            List<Observation> noteObservations = observations.stream().filter(observation -> Constants.OBSERVATION_PSYCHOLOGY_NOTES
                    .equals(observation.getIdentifier().getFirst().getValue())).toList();
            List<Observation> assessmentObservations = observations.stream().filter(observation ->
                            Constants.OBSERVATION_PSYCHOLOGY_ASSESSMENT.equals(observation.getIdentifier().getFirst().getValue()))
                    .toList();
            setAssessedAndReferredObservations(assessmentObservations, noteObservations, psychologyDTOS, practitioners);

            if (!CollectionUtils.isEmpty(noteObservations)) {
                for (Observation noteObservation : noteObservations) {
                    PsychologyDTO psychologyDTO = new PsychologyDTO();
                    psychologyDTO.setClinicianNote(noteObservation.getNote().getFirst().getText());
                    psychologyDTO.setId(StringUtil.isNotBlank(noteObservation.getIdPart()) ?
                            Long.parseLong(noteObservation.getIdPart()) : null);
                    psychologyDTO.setReferredDate(noteObservation.getIssued());
                    noteObservation.getPerformer().stream().filter(reference ->
                                    nonNull(reference) && reference.getReference().startsWith(ResourceType.Practitioner.name()))
                            .findFirst().ifPresent(reference -> setUserByPractitionerId(reference.getReference()
                                            .split(Constants.FORWARD_SLASH)[1], practitioners, psychologyDTO,
                                    true, false));
                    psychologyDTOS.add(psychologyDTO);
                }
            }
        }
        return psychologyDTOS;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PsychologyDTO removePsychologyDataById(PsychologyDTO request) {
        if (isNull(request.getId())) {
            throw new Validation(1009);
        }
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.GET_OBSERVATION_BY_DERIVED_FROM,
                request.getId()));
        if (Objects.nonNull(bundle) && !CollectionUtils.isEmpty(bundle.getEntry()) &&
                ResourceType.Observation.equals(bundle.getEntry().getFirst().getResource().getResourceType())) {
            Observation observation = (Observation) bundle.getEntry().getFirst().getResource();
            if (Objects.nonNull(observation)) {
                updateObservationStatus(observation.getIdPart(), Observation.ObservationStatus.CANCELLED);
            }
        }
        updateObservationStatus(String.valueOf(request.getId()), Observation.ObservationStatus.CANCELLED);
        return request;
    }

    /**
     * <p>
     * Used to set observation status for an observation
     * </p>
     */
    private void updateObservationStatus(String id, Observation.ObservationStatus observationStatus) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Observation observation = restApiUtil.getObservationById(id);
        observation.setStatus(observationStatus);
        fhirUtils.setBundle(String.valueOf(ResourceType.Observation).concat(Constants.FORWARD_SLASH)
                .concat(observation.getIdPart()), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, observation, bundle);
        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * <p>
     * Used to convert encounter to encounterDTO
     * </p>
     *
     * @param encounter - contains the value of encounter
     * @return {@link EncounterDetailsDTO} returns the encounter dto
     */
    private EncounterDetailsDTO convertEncounterToEncounterDetailsDTO(Encounter encounter) {
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();

        encounterDetailsDTO.setId(StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                Constants.FORWARD_SLASH, encounter.getIdPart()));

        if (!CollectionUtils.isEmpty(encounter.getParticipant()) &&
                nonNull(encounter.getParticipant().getFirst().getIndividual())) {
            encounterDetailsDTO.setMemberId(encounter.getParticipant().getFirst().getIndividual().getReference());
        }

        return encounterDetailsDTO;
    }

    /**
     * <p>
     * used to get user by practitioner id
     * </p>
     */
    private void setUserByPractitionerId(String practitionerId, List<Practitioner> practitioners,
                                         PsychologyDTO psychologyDTO, boolean isReferred, boolean isAssessed) {
        if (StringUtil.isNotBlank(practitionerId) && !CollectionUtils.isEmpty(practitioners)
                && !CollectionUtils.isEmpty(practitioners.getFirst().getName())
                && !CollectionUtils.isEmpty(practitioners.getFirst().getName().getFirst().getGiven())
                && nonNull(practitioners.getFirst().getName().getFirst().getFamily())) {
            practitioners.stream().filter(practitioner ->
                    practitionerId.equals(practitioner.getIdPart())).findFirst().ifPresent(practitioner -> {
                if (!CollectionUtils.isEmpty(practitioner.getName())
                        && !CollectionUtils.isEmpty(practitioner.getName().getFirst().getGiven())
                        && nonNull(practitioner.getName().getFirst().getFamily())) {
                    String username = (practitioner.getName().getFirst().getGiven().getFirst().asStringValue()
                            .concat(Constants.EMPTY_SPACE).concat(practitioner.getName().getFirst().getFamily()));
                    if (isReferred) {
                        psychologyDTO.setReferredByDisplay(username);
                        psychologyDTO.setReferredBy(practitioner.getIdPart());
                    }
                    if (isAssessed) {
                        psychologyDTO.setAssessedByDisplay(username);
                        psychologyDTO.setAssessedBy(practitioner.getIdPart());
                    }

                }
            });
        }
    }

    /**
     * <p>
     * Used to set observations from noteAssesments
     * </p>
     *
     * @param assessmentObservations - contains the value of assessment observations where noteObservations may required
     * @param noteObservations       - this contains clinical notes referred by provider
     * @param psychologyDTOS         - this is where the clinical notes is present
     */
    private void setAssessedAndReferredObservations(List<Observation> assessmentObservations,
                                                    List<Observation> noteObservations,
                                                    List<PsychologyDTO> psychologyDTOS,
                                                    List<Practitioner> practitioners) {
        assessmentObservations.forEach(observation -> {
            List<String> notes = observation.getNote().stream().map(Annotation::getText).toList();
            PsychologyDTO psychologyDTO = new PsychologyDTO();
            if (!CollectionUtils.isEmpty(observation.getDerivedFrom())) {
                psychologyDTO.setCounselorAssessment(notes.getFirst());
                psychologyDTO.setCounselorAssessmentId(nonNull(observation.getIdPart()) ?
                        Long.parseLong(observation.getIdPart()) : null);
                psychologyDTO.setAssessedDate(observation.getIssued());
                observation.getPerformer().stream().filter(reference -> nonNull(reference)
                                && reference.getReference().startsWith(ResourceType.Practitioner.name())).findFirst()
                        .ifPresent(reference -> setUserByPractitionerId(reference.getReference()
                                        .split(Constants.FORWARD_SLASH)[1], practitioners, psychologyDTO,
                                false, true));
                String id = getIdFromDerivedFrom(observation);
                Observation foundObservation = null;
                for (Observation noteObservation : noteObservations) {
                    if (noteObservation.getIdPart().equals(id)) {
                        foundObservation = noteObservation;
                        psychologyDTO.setClinicianNote(noteObservation.getNote().getFirst().getText());
                        psychologyDTO.setId(Long.parseLong(noteObservation.getIdPart()));
                        psychologyDTO.setReferredDate(noteObservation.getIssued());
                        noteObservation.getPerformer().stream().filter(reference -> nonNull(reference)
                                        && reference.getReference().startsWith(ResourceType.Practitioner.name()))
                                .findFirst().ifPresent(reference -> setUserByPractitionerId(reference.getReference()
                                                .split("/")[1], practitioners, psychologyDTO,
                                        true, false));
                        break;
                    }
                }
                if (nonNull(foundObservation)) {
                    noteObservations.remove(foundObservation);
                }
            } else {
                psychologyDTO.setCounselorAssessment(notes.getFirst());
                psychologyDTO.setId(nonNull(observation.getIdPart()) ? Long.parseLong(observation.getIdPart()) : null);
                psychologyDTO.setReferredDate(observation.getIssued());
                psychologyDTO.setAssessedDate(observation.getIssued());
                observation.getPerformer().stream().filter(reference -> nonNull(reference)
                                && reference.getReference().startsWith(ResourceType.Practitioner.name()))
                        .findFirst().ifPresent(reference ->
                                setUserByPractitionerId(reference.getReference().split(Constants.FORWARD_SLASH)[1],
                                        practitioners, psychologyDTO, true, true)
                        );
            }
            psychologyDTOS.add(psychologyDTO);
        });
    }

    /**
     * <p>
     * Used to get id from Derived from observations from noteAssessments
     * </p>
     */
    private String getIdFromDerivedFrom(Observation observation) {
        return observation.getDerivedFrom().getFirst().getReference().split(Constants.FORWARD_SLASH)[1];
    }
}
