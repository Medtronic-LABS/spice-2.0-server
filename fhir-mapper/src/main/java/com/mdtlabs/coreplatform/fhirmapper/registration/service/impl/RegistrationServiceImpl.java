package com.mdtlabs.coreplatform.fhirmapper.registration.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import javax.sql.DataSource;

import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CarePlan;
import org.hl7.fhir.r4.model.Coverage;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MessageConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.PatientStatusConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentTreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanFrequencyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.fhirmapper.registration.service.RegistrationService;

/**
 * <p>
 * This class is a service class to perform operation on Resgistration.
 * </p>
 *
 * @author Karthick M created on Aug 05, 2024
 */
@Service
public class RegistrationServiceImpl implements RegistrationService {

    private final RestApiUtil restApiUtil;
    private final FhirUtils fhirUtils;
    private final CommonConverter commonConverter;
    private final PatientConverter patientConverter;
    private final PregnancyConverter pregnancyConverter;
    private final VitalSignsConverter vitalSignsConverter;
    private final OrganizationRepository organizationRepository;
    private final DataSource dataSource;
    private final AdminServiceApiInterface adminServiceApiInterface;
    private final PatientTreatmentPlanService patientTreatmentPlanService;

    private static final String GET_MEMBER_BY_IDENTITY_VALUE_QUERY = "RelatedPerson?identifier=%s|%s&identifier=%s|%s";
    private static final String GET_PATIENT_BY_IDENTITY_VALUE_QUERY = "Patient?identifier=%s|%s&identifier=%s|%s";
    private static final String GET_PATIENT_BY_ID_QUERY = "Patient?_id=%s";
    private static final String GET_MEMBER_BY_ID_QUERY = "RelatedPerson?_id=%s";
    private static final String GET_MEMBER_BY_IDENTITY_TYPE_AND_VALUE_QUERY = "RelatedPerson?identifier=%s|%s";

    public RegistrationServiceImpl(RestApiUtil restApiUtil, FhirUtils fhirUtils,
                                   CommonConverter commonConverter, PregnancyConverter pregnancyConverter,
                                   VitalSignsConverter vitalSignsConverter, PatientConverter patientConverter,
                                   OrganizationRepository organizationRepository, DataSource dataSource,
                                   AdminServiceApiInterface adminServiceApiInterface,
                                   PatientTreatmentPlanService patientTreatmentPlanService) {
        this.restApiUtil = restApiUtil;
        this.fhirUtils = fhirUtils;
        this.commonConverter = commonConverter;
        this.patientConverter = patientConverter;
        this.pregnancyConverter = pregnancyConverter;
        this.vitalSignsConverter = vitalSignsConverter;
        this.organizationRepository = organizationRepository;
        this.dataSource = dataSource;
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.patientTreatmentPlanService = patientTreatmentPlanService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EnrollmentResponseDTO registerPatientAndMember(EnrollmentRequestDTO request) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        EnrollmentResponseDTO response = new EnrollmentResponseDTO();
        RelatedPerson member = createMember(request, bundle);
        Patient patient = createPatient(request, member, bundle);
        Encounter encounter = new Encounter();
        encounter.setStatus(Encounter.EncounterStatus.FINISHED);
        encounter.setIdentifier(List.of(new Identifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue(PatientStatusConstants.ENROLLMENT)));
        encounter.setPeriod(new Period().setStartElement(new DateTimeType(request.getProvenance().getModifiedDate())));
        encounter.setStatus(Encounter.EncounterStatus.FINISHED);
        encounter.setServiceProvider(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Organization), Constants.FORWARD_SLASH,
                        request.getProvenance().getOrganizationId())));
        linkPatientToRelatedPerson(member, request);
        linkPatientAndRelatedPersonToEncounter(encounter, request);
        if (Objects.nonNull(request.getBioMetrics()) && Objects.nonNull(request.getBioMetrics().getIsPregnant())) {
            PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
            pregnancyDetailsDTO.setProvenance(request.getProvenance());
            pregnancyDetailsDTO.setIsPregnant(request.getBioMetrics().getIsPregnant());
            if (!Boolean.TRUE.equals(pregnancyDetailsDTO.getIsPregnancyAnc())) {
                pregnancyDetailsDTO.setIsPregnancyRisk(Boolean.FALSE);
            }
            String url = String.format(Constants.FETCH_LATEST_OBSERVATION_QUERY, FhirConstants.PREGNANCY.toLowerCase(),
                    Observation.ObservationStatus.PRELIMINARY.name().toLowerCase(), request.getMemberId());
            Bundle pregnancyBundle = restApiUtil.getBatchRequest(url);
            Observation pregnancyObservation = null;
            if (pregnancyBundle.getEntry().isEmpty()) {
                pregnancyObservation = pregnancyConverter.createPregnancyObservation(pregnancyDetailsDTO);
            } else {
                Observation oldPregnancyObservation = (Observation) pregnancyBundle.getEntry().getFirst().getResource();
                pregnancyObservation = pregnancyConverter.updatePregnancyObservation(oldPregnancyObservation,
                        pregnancyDetailsDTO);
            }
            pregnancyObservation.addPerformer(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    request.getProvenance().getOrganizationId())));
            pregnancyObservation.addPerformer(new Reference(StringUtil.concatString(
                    String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, request.getMemberId())));
            pregnancyObservation.setSubject(new Reference(request.getPatientId()));
            commonConverter.setObservationDetailsInBundle(bundle, pregnancyObservation,
                    FhirConstants.PREGNANCY_IDENTIFIER_URL, request.getProvenance());
            Observation vitalSignsObservation = createVitalSignsObservation(bundle, request,
                    pregnancyDetailsDTO.getProvenance());
            setObservationDetails(bundle, vitalSignsObservation, request, FhirConstants.VITAL_SIGNS_IDENTIFIER_URL,
                    pregnancyDetailsDTO.getProvenance());
        }
        commonConverter.setEncounterDetailsInBundle(bundle, encounter,
                FhirConstants.ENCOUNTER_IDENTIFIER_URL, request.getProvenance());
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(
                fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(bundle));

        setPatientId(request, responseEntity, response);
        return constructResponse(patient, request, response);
    }

    /**
     * Process and create FHIR vital signs observation based on given
     * bundle details
     *
     * @param bundle        The FHIR bundle entity it contains patient vital observations.
     * @param request       EnrollmentRequestDTO instance.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     * @return Converted FHIR Observation entity.
     */
    private Observation createVitalSignsObservation(Bundle bundle, EnrollmentRequestDTO request,
                                                    ProvenanceDTO provenanceDTO) {
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(request.getMemberId());
        vitalSignsDTO.setProvenanceDTO(provenanceDTO);
        vitalSignsDTO.setPregnancyObservation(commonConverter.getObservationFromBundleByIdentifier(
                bundle, FhirConstants.PREGNANCY_IDENTIFIER_URL));
        Observation observation = vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO, bundle);
        commonConverter.setObservationText(observation, FhirConstants.VITAL_SIGNS);
        return observation;
    }

    /**
     * Set observation details in FHIR bundle resource
     *
     * @param bundle        The FHIR bundle resource
     * @param observation   The FHIR Observation entity
     * @param identifierUrl The Observation identifier url
     */
    private void setObservationDetails(Bundle bundle, Observation observation, EnrollmentRequestDTO request,
                                       String identifierUrl, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(observation)) {
            commonConverter.setObservationDetailsInBundle(bundle, observation, identifierUrl, provenanceDTO);
            observation.addPerformer(new Reference(StringUtil.concatString(String.valueOf(
                    ResourceType.RelatedPerson), Constants.FORWARD_SLASH, request.getMemberId())));
            observation.setSubject(new Reference(request.getPatientId()));
            observation.addPerformer(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    provenanceDTO.getOrganizationId())));
        }
    }

    /**
     * Links a patient to a related person
     *
     * @param relatedPerson
     * @param request
     */
    private void linkPatientToRelatedPerson(RelatedPerson relatedPerson, EnrollmentRequestDTO request) {
        relatedPerson.setPatient(new Reference(request.getPatientId()));
    }

    /**
     * Links a patient to a related person
     *
     * @param encounter
     * @param request
     */
    private void linkPatientAndRelatedPersonToEncounter(Encounter encounter, EnrollmentRequestDTO request) {
        encounter.setSubject(new Reference(request.getPatientId()));
        Encounter.EncounterParticipantComponent
                participantComponent = new Encounter.EncounterParticipantComponent();
        participantComponent.setIndividual(new Reference(String.format(FhirConstants.RELATED_PERSON_ID, request.getMemberId())));
        encounter.addParticipant(participantComponent);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PatientValidationResponseDTO isPatientPresentInRelatedPerson(BioDataDTO request) {
        String url = String.format(GET_MEMBER_BY_IDENTITY_TYPE_AND_VALUE_QUERY, FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID, request.getIdentityValue());
        Bundle bundle = restApiUtil.getBatchRequest(url);
        if (!bundle.getEntry().isEmpty()) {
            RelatedPerson relatedPerson = (RelatedPerson) bundle.getEntry().getFirst().getResource();
            if (relatedPerson.getIdentifier().stream().anyMatch(id -> Objects.equals(id.getValue(), Constants.ENROLLED))) {
                throw new Validation(4000);
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    public PatientValidationResponseDTO validatePatient(BioDataDTO request) {
        if (Objects.isNull(request.getIdentityValue())) {
            throw new DataNotAcceptableException(20100);
        }
        PatientValidationResponseDTO patientValidationResponse = new PatientValidationResponseDTO();
        Bundle basicDetailBundle = restApiUtil.getBatchRequest(String.format(
                Constants.FHIR_RESOURCE_MEMBER + Constants.NAME_AND_TELECOM_PARAM + Constants.PATIENT_IDENTIFIER
                        + Constants.VERTICAL_BAR + Constants.STRING_FORMAT_SPECIFIER, request.getFirstName(),
                request.getLastName(), request.getPhoneNumber(), FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL,
                request.getCountry().getId()));

        RelatedPerson relatedPersonByNationalId = restApiUtil.getRelatedPerson(String.format(
                Constants.FHIR_RESOURCE_MEMBER + Constants.IDENTIFIER_PARAM + Constants.VERTICAL_BAR
                        + Constants.STRING_FORMAT_SPECIFIER + Constants.AND + Constants.IDENTIFIER_PARAM
                        + Constants.VERTICAL_BAR + Constants.STRING_FORMAT_SPECIFIER,
                FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID, request.getIdentityValue(),
                FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL, request.getCountry().getId()) + Constants.AND + Constants.PATIENT_ACTIVE_STATUS);

        Map<String, RelatedPerson> relatedPersonList = getRelatedPersons(request, basicDetailBundle);
        if (!relatedPersonList.isEmpty()) {
            if ((Objects.nonNull(relatedPersonByNationalId)) && !Objects.equals(relatedPersonByNationalId.getIdPart(),
                    request.getMemberReference())) {
                relatedPersonList.put(relatedPersonByNationalId.getIdPart(), relatedPersonByNationalId);
            }
            if (getDuplicateRecordFromEncounter(relatedPersonList, patientValidationResponse)) {
                return null;
            }

        } else if (Objects.nonNull(relatedPersonByNationalId) && !Objects.equals(relatedPersonByNationalId.getIdPart(),
                request.getMemberReference())) {
            setValidatePatientResponse(relatedPersonByNationalId, patientValidationResponse);
        }
        return patientValidationResponse;
    }

    /**
     * <p>
     * To get duplicate record details from encounter resource.
     * </p>
     *
     * @param relatedPersonList         {@link Map} - Collection of Related person details
     * @param patientValidationResponse {@link PatientValidationResponseDTO} - Response object with duplicate record
     *                                  details
     * @return a {@link Boolean} type value
     */
    private boolean getDuplicateRecordFromEncounter(Map<String, RelatedPerson> relatedPersonList,
                                                    PatientValidationResponseDTO patientValidationResponse) {
        if (Objects.nonNull(relatedPersonList) && !relatedPersonList.isEmpty()) {
            RelatedPerson duplicateRelatedPerson;
            String relatedPersonIds = String.join(",", relatedPersonList.keySet());
            Bundle encounterBundle = restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_MEMBER_ID,
                    FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL, FhirConstants.SCREENING, relatedPersonIds));
            if (Objects.nonNull(encounterBundle) && !encounterBundle.getEntry().isEmpty()) {
                String memberIdAsString = fhirUtils.getRelatedPersonIdFromEncounter(
                        fhirUtils.getOldestEncounter(encounterBundle));
                if (Objects.isNull(memberIdAsString)) {
                    return true;
                }
                duplicateRelatedPerson = relatedPersonList.getOrDefault(memberIdAsString, null);
                if (Objects.nonNull(duplicateRelatedPerson)) {
                    setValidatePatientResponse(duplicateRelatedPerson, patientValidationResponse);
                }
            } else {
                List<RelatedPerson> enrolledMembers = relatedPersonList.values().stream()
                        .filter(entry -> entry.getIdentifier().stream()
                                .anyMatch(identifier ->
                                        Objects.equals(identifier.getSystem(), FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                                                && Objects.equals(identifier.getValue(), Constants.ENROLLED))).toList();
                if (!enrolledMembers.isEmpty()) {
                    setValidatePatientResponse(enrolledMembers.getFirst(), patientValidationResponse);
                }
            }
        }
        return false;
    }

    /**
     * <p>
     * Gets collection of Related persons from the given bundle.
     * </p>
     *
     * @param request           {@link BioDataDTO} - Patient data to validate
     * @param basicDetailBundle {@link Bundle} - contains Related person entries.
     * @return A {@link Map} collection of converted Related person values.
     */
    private Map<String, RelatedPerson> getRelatedPersons(BioDataDTO request, Bundle basicDetailBundle) {
        return basicDetailBundle.getEntry().stream()
                .map(entry -> (RelatedPerson) entry.getResource()).filter(entry -> request.getFirstName().equalsIgnoreCase(entry.getName().getFirst().getGiven().getFirst().toString())
                        && request.getLastName().equalsIgnoreCase(entry.getName().getFirst().getGiven().getLast().toString())
                        && !Objects.equals(entry.getIdPart(), request.getMemberReference()))
                .collect(Collectors.toMap(Resource::getIdPart, relatedPerson -> relatedPerson));
    }

    /**
     * <p>
     * This method sets the patient validation response with duplicate record values.
     * </p>
     *
     * @param relatedPerson             {@link RelatedPerson} - Duplicate Record
     * @param patientValidationResponse {@link PatientValidationResponseDTO} - Response object
     */
    private void setValidatePatientResponse(RelatedPerson relatedPerson,
                                            PatientValidationResponseDTO patientValidationResponse) {
        BioDataDTO patientDetail = new BioDataDTO();
        patientDetail.setMemberReference(relatedPerson.getIdPart());
        patientDetail.setFirstName(String.valueOf(relatedPerson.getName().getFirst().getGiven().getFirst()));
        patientDetail.setLastName(String.valueOf(relatedPerson.getName().getFirst().getGiven().getLast()));
        patientDetail.setPhoneNumber(relatedPerson.getTelecom().getFirst().getValue());
        patientDetail.setGender(String.valueOf(relatedPerson.getGender()));
        patientDetail.setNationalId(relatedPerson.getIdentifier().stream()
                .filter(identifier -> identifier.getSystem()
                        .equals(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)).map(Identifier::getValue)
                .findFirst().orElse(null));
        patientDetail.setIdentityValue(relatedPerson.getIdentifier().stream()
                .filter(identifier -> identifier.getSystem()
                        .equals(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)).map(Identifier::getValue)
                .findFirst().orElse(null));
        patientDetail.setPatientStatus(relatedPerson.getIdentifier().stream()
                .filter(identifier -> identifier.getSystem()
                        .equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)).map(Identifier::getValue)
                .findFirst().orElse(null));
        Map<String, Object> siteDetails = restApiUtil.getSiteDetails(
                relatedPerson.getPatient().getReference());
        Patient patient = (Patient) siteDetails.get(FhirConstants.PATIENT);
        String organizationId = relatedPerson.getIdentifier().stream()
                .filter(identifier -> identifier.getSystem()
                        .equals(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)).map(Identifier::getValue)
                .findFirst().orElse(null);
        Organization organization = null;
        Bundle orgBundle = restApiUtil.getBatchRequest(String.format(Constants.ORGANIZATION_PARAMS, organizationId));
        if (!orgBundle.getEntry().isEmpty()) {
            for (Bundle.BundleEntryComponent entry : orgBundle.getEntry()) {
                if (entry.getResource().getResourceType().equals(ResourceType.Organization)) {
                    organization = (Organization) entry.getResource();
                }
            }
        }
        String siteName = Objects.nonNull(organization) ? organization.getName() : Constants.EMPTY_STRING;
        String siteFhirId = Objects.nonNull(organization) ? organization.getIdPart() : Constants.EMPTY_STRING;
        AtomicReference<String> programId = new AtomicReference<>(Constants.EMPTY_STRING);
        if (Objects.nonNull(patient)) {
            patient.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)) {
                    programId.set(identifier.getValue());
                }
            });
        }
        patientDetail.setProgramId(Objects.nonNull(programId.get()) ? String.valueOf(programId.get()) : null);
        patientDetail.setSiteName(siteName);
        patientDetail.setSiteFhirId(siteFhirId);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setFhirId(patientDetail.getSiteFhirId());
        HealthFacilityRequestDTO healthFacilityDTO =
                adminServiceApiInterface.getHealthFacilityByFhirId(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), requestDTO);
        patientDetail.setDistrictId(healthFacilityDTO.getDistrict().getId().toString());
        patientDetail.setChiefdomId(healthFacilityDTO.getChiefdom().getId().toString());
        patientValidationResponse.setPatientDetails(patientDetail);
    }

    /**
     * <p>
     * Validates a patient's enrollment status and throws Data conflict exception if the Patient is already
     * enrolled.
     * </p>
     *
     * @param relatedPerson {@link RelatedPerson} - Related person details
     */
    private void validateEnrollmentStatus(RelatedPerson relatedPerson) {
        if (Objects.nonNull(relatedPerson) && relatedPerson.getIdentifier().stream().anyMatch(
                identifier -> identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                        && identifier.getValue().equals(FhirConstants.ENROLLED))) {
            Logger.logError("Patient already Enrolled.");
            Map<String, Object> siteDetails = restApiUtil.getSiteDetails(relatedPerson.getPatient().getReference());
            Organization organization = (Organization) siteDetails.get(FhirConstants.ORGANIZATION);
            Patient patient = (Patient) siteDetails.get(FhirConstants.PATIENT);
            String siteName = Objects.nonNull(organization) ? organization.getName() : Constants.EMPTY_STRING;
            if (Objects.nonNull(patient)) {
                String programId = patient.getIdentifier().stream().filter(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)).toList().getFirst().getValue();
                throw new DataConflictException(20101, programId, siteName);
            }
        }

    }

    /**
     * Create a patient resource and updated is patient is present.
     *
     * @param request
     * @param transcationBundle
     * @return Patient resource
     */
    private Patient createPatient(EnrollmentRequestDTO request, RelatedPerson member, Bundle transcationBundle) {
        Bundle bundle = null;
        if (Objects.isNull(request.getPatientId())) {
            bundle = restApiUtil.getBatchRequest(String.format(GET_PATIENT_BY_IDENTITY_VALUE_QUERY, FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID, request.getBioData().getIdentityValue(),
                    FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL, request.getBioData().getCountry().getId()).concat(Constants.PATIENT_ACTIVE_STATUS));
        } else {
            bundle = restApiUtil.getBatchRequest(String.format(GET_PATIENT_BY_ID_QUERY, request.getPatientId()).concat(Constants.PATIENT_ACTIVE_STATUS));
        }
        updateVirtualId(request);
        Patient patient;
        if (!Objects.isNull(bundle) && !bundle.getEntry().isEmpty()) {
            patient = (Patient) bundle.getEntry().getFirst().getResource();
            if (patient.getIdentifier().stream().anyMatch(id -> Objects.equals(id.getValue(), Constants.ENROLLED))) {
                throw new Validation(4000);
            }
            patientConverter.mapPatient(patient, request, Constants.PATIENT_CREATION_TYPE);
            createCoverage(request, patient.getIdPart(), transcationBundle);
            fhirUtils.setBundle(StringUtil.concatString(ResourceType.Patient.toString(), Constants.FORWARD_SLASH,
                            patient.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, patient.getIdPart()),
                    Bundle.HTTPVerb.PUT, patient, transcationBundle, request.getProvenance());
            member.setIdentifier(patient.getIdentifier());
            request.setPatientId(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()));
            request.setPatientReference(patient.getIdPart());
        } else {
            patient = new Patient();
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            String url = StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
            patientConverter.mapPatient(patient, request, Constants.PATIENT_CREATION_TYPE);
            createCoverage(request, fullUrl, transcationBundle);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, patient, transcationBundle, request.getProvenance());
            request.setPatientId(url);
        }
        member.setIdentifier(patient.getIdentifier());
        return patient;
    }

    /**
     * Create a Member resource or updating Member if it already present.
     *
     * @param request
     * @param transcationBundle
     * @return RelatedPerson resource
     */
    private RelatedPerson createMember(EnrollmentRequestDTO request, Bundle transcationBundle) {
        Bundle bundle = null;
        if (Objects.isNull(request.getId())) {
            bundle = restApiUtil.getBatchRequest(String.format(GET_MEMBER_BY_IDENTITY_VALUE_QUERY, FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID, request.getBioData().getIdentityValue(),
                    FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL, request.getBioData().getCountry().getId()).concat(Constants.PATIENT_ACTIVE_STATUS));
        } else {
            bundle = restApiUtil.getBatchRequest(String.format(GET_MEMBER_BY_ID_QUERY, request.getId()).concat(Constants.PATIENT_ACTIVE_STATUS));
        }
        if (!Objects.isNull(bundle) && !bundle.getEntry().isEmpty()) {
            RelatedPerson member = (RelatedPerson) bundle.getEntry().getFirst().getResource();
            validateEnrollmentStatus(member);
            patientConverter.mapRelatedPerson(member, request, Constants.PATIENT_CREATION_TYPE);
            fhirUtils.setBundle(StringUtil.concatString(ResourceType.RelatedPerson.toString(), Constants.FORWARD_SLASH, member.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, member.getIdPart()),
                    Bundle.HTTPVerb.PUT, member, transcationBundle, request.getProvenance());
            request.setMemberId(member.getIdPart());

            return member;
        } else {
            RelatedPerson member = new RelatedPerson();
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            String url = StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, fullUrl);
            patientConverter.mapRelatedPerson(member, request, "FROM_CREATE_PATIENT");
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, member, transcationBundle, request.getProvenance());
            request.setMemberReference(fullUrl);
            request.setMemberId(fullUrl);
            return member;
        }
    }

    /**
     * Creates Coverage resource and maps the values form request.
     *
     * @param request
     * @param patientReference
     * @param bundle
     */
    private void createCoverage(EnrollmentRequestDTO request, String patientReference, Bundle bundle) {
        if (!Objects.isNull(request.getBioData().getInsuranceStatus())) {
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            String url = StringUtil.concatString(String.valueOf(ResourceType.Coverage), Constants.FORWARD_SLASH, fullUrl);
            Coverage coverage = new Coverage();
            coverage.setBeneficiary(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, patientReference)));
            if (Boolean.TRUE.equals(request.getBioData().getInsuranceStatus())) {
                coverage.setStatus(Coverage.CoverageStatus.ACTIVE);
                coverage.setSubscriberId(request.getBioData().getInsuranceId());
                if (Objects.nonNull(request.getBioData().getInsuranceType())) {
                    coverage.setType(request.getBioData().getInsuranceType().equalsIgnoreCase(MetaCodeConstants.OTHER) ?
                            fhirUtils.createCodeableConcept(MetaCodeConstants.OTHER + Constants.HIGHFIN + request.getBioData().getOtherInsurance()) :
                            fhirUtils.createCodeableConcept(request.getBioData().getInsuranceType()));
                }
            } else {
                coverage.setStatus(Coverage.CoverageStatus.CANCELLED);
            }
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, coverage, bundle, request.getProvenance());
        }
    }

    /**
     * Constructs response from registration.
     *
     * @param patient
     * @param request
     * @return EnrollmentResponseDTO.
     */
    private EnrollmentResponseDTO constructResponse(Patient patient, EnrollmentRequestDTO request, EnrollmentResponseDTO response) {
        response.setDateOfEnrollment(request.getDateOfEnrollment());
        response.setAge(request.getBioMetrics().getAge());
        if (Objects.nonNull(request.getBioData().getMiddleName())) {
            response.setName(request.getBioData().getFirstName() + Constants.EMPTY_SPACE +
                    request.getBioData().getMiddleName() + Constants.EMPTY_SPACE + request.getBioData().getLastName());
            response.setMiddleName(request.getBioData().getMiddleName());
        } else {
            response.setName(request.getBioData().getFirstName() +
                    Constants.EMPTY_SPACE + request.getBioData().getLastName());
        }
        response.setNationalId(request.getBioData().getIdentityValue());
        response.setProgramId(request.getVirtualId());
        response.setPhoneNumber(request.getBioData().getPhoneNumber());
        response.setGender(patient.getGender().getDisplay());
        CarePlan carePlan = patientTreatmentPlanService.getCarePlanForPatient(response.getMemberId());
        if (Objects.nonNull(carePlan) && carePlan.hasIntent() && Objects.equals(CarePlan.CarePlanIntent.PLAN, carePlan.getIntent()) ||
                carePlan.hasMeta() && Objects.nonNull(carePlan.getMeta().getVersionId()) &&
                        Constants.ONE < Integer.parseInt(carePlan.getMeta().getVersionId())) {
            response.setTreatmentPlanResponse(mapAssessmentTreatmentPlan(carePlan));
        }
        return response;
    }

    /**
     * <p>
     * Sets the patient ID and member ID of the registered patient.
     * </p>
     *
     * @param response       the response of the patient to be returned is given
     * @param request        the patient request is given
     * @param responseEntity the saved response entity is given
     */
    private void setPatientId(EnrollmentRequestDTO request, ResponseEntity<FhirResponseDTO> responseEntity, EnrollmentResponseDTO response) {
        if (Objects.nonNull(responseEntity) && Objects.nonNull(responseEntity.getBody())) {
            Map<String, List<String>> fhirIdsFromResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());

            response.setMemberId(
                    Objects.nonNull(request.getId()) ? request.getId() :
                            fhirIdsFromResponse.getOrDefault(String.valueOf(ResourceType.RelatedPerson), new ArrayList<>())
                                    .stream().findFirst().orElse(null)
            );
            response.setPatientId(
                    Objects.nonNull(request.getPatientReference()) ? request.getPatientReference() :
                            fhirIdsFromResponse.getOrDefault(String.valueOf(ResourceType.Patient), new ArrayList<>())
                                    .stream().findFirst().orElse(null)
            );
        }
    }

    /**
     * Updates patient virtual Id.
     *
     * @param request the patient request is given
     * @return Long - virtual Id
     */
    private void updateVirtualId(EnrollmentRequestDTO request) {
        SimpleJdbcCall jdbcCall = new SimpleJdbcCall(dataSource).withFunctionName(Constants.UPDATE_VIRTUAL_ID);
        com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization organization = organizationRepository.findByFormDataIdAndFormName(request.getBioData().getCountry().getId(), Constants.FORM_NAME_COUNTRY);
        if (Objects.isNull(organization)) {
            throw new DataNotFoundException();
        }
        SqlParameterSource in = new MapSqlParameterSource().addValue(Constants.INPUT_TENANT_ID, organization.getId());
        Long virtualId = jdbcCall.executeFunction(Long.class, in);
        if (Constants.MINUS_ONE == virtualId) {
            throw new Validation(1205);
        }
        request.setVirtualId(virtualId.toString());
    }

    /**
     * {@inheritDoc}
     */
    public boolean updateMemberSignature(RequestDTO requestDTO) {
        ResponseEntity<FhirResponseDTO> response = null;
        if (Objects.nonNull(requestDTO.getMemberId())) {
            Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            Bundle responseBundle = restApiUtil.getBatchRequest(String.format(Constants.RELATED_PERSON_QUERY_ID, requestDTO.getMemberId()));

            for (Bundle.BundleEntryComponent entry : responseBundle.getEntry()) {
                if (entry.getResource() instanceof RelatedPerson relatedPersonResource) {
                    Logger.logInfo(MessageConstants.EXIST_SCREENED_PATIENT_LOG);
                    Attachment attachment = new Attachment().setUrl(requestDTO.getSignatureUrl());
                    if (relatedPersonResource.hasPhoto()) {
                        relatedPersonResource.getPhoto().add(attachment);
                    } else {
                        relatedPersonResource.addPhoto(attachment);
                    }
                    commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPersonResource,
                            null, requestDTO.getProvenance());
                }
            }
            response = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(bundle));
        }
        return Objects.nonNull(response);
    }

    /**
     * <p>
     * Sets the AssessmentTreatmentPlanDTO from the care plan.
     * </p>
     *
     * @param carePlan The care plan of the given patient ID is given
     * @return The AssessmentTreatmentPlanDTO is constructed and returned
     */
    private AssessmentTreatmentPlanDTO mapAssessmentTreatmentPlan(CarePlan carePlan) {
        AssessmentTreatmentPlanDTO treatmentPlanDTO = new AssessmentTreatmentPlanDTO();
        treatmentPlanDTO.setCarePlanId(carePlan.getIdPart());
        for (CarePlan.CarePlanActivityComponent carePlanActivityComponent : carePlan.getActivity()) {
            CarePlan.CarePlanActivityDetailComponent detailComponent = carePlanActivityComponent.getDetail();
            TreatmentPlanFrequencyDTO treatmentPlanFrequencyDTO = new TreatmentPlanFrequencyDTO();

            switch (detailComponent.getCode().getText()) {
                case Constants.FREQUENCY_BP_CHECK:
                    treatmentPlanFrequencyDTO.setLabel(Constants.BP_CHECK_FREQUENCY);
                    treatmentPlanFrequencyDTO.setValue(detailComponent.getDescription());
                    treatmentPlanDTO.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
                    break;

                case Constants.FREQUENCY_BG_CHECK:
                    treatmentPlanFrequencyDTO.setLabel(Constants.BG_CHECK_FREQUENCY);
                    treatmentPlanFrequencyDTO.setValue(detailComponent.getDescription());
                    treatmentPlanDTO.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
                    break;

                case Constants.FREQUENCY_MEDICAL_REVIEW:
                    treatmentPlanFrequencyDTO.setLabel(Constants.MEDICAL_REVIEW_FREQUENCY);
                    treatmentPlanFrequencyDTO.setValue(detailComponent.getDescription());
                    treatmentPlanDTO.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
                    break;

                case Constants.FREQUENCY_HBA1C_CHECK:
                    treatmentPlanFrequencyDTO.setLabel(Constants.HBA1C_CHECK_FREQUENCY);
                    treatmentPlanFrequencyDTO.setValue(detailComponent.getDescription());
                    treatmentPlanDTO.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
                    break;

                case Constants.FREQUENCY_CHO_CHECK:
                    treatmentPlanFrequencyDTO.setLabel(Constants.CHO_CHECK_FREQUENCY);
                    treatmentPlanFrequencyDTO.setValue(detailComponent.getDescription());
                    treatmentPlanDTO.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
                    break;

                default:
                    break;
            }
        }
        return treatmentPlanDTO;
    }
}
