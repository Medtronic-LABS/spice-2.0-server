package com.mdtlabs.coreplatform.fhirmapper.mapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import io.micrometer.common.util.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.DomainResource;
import org.hl7.fhir.r4.model.Dosage;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Timing;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.APGARScoreDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.CommonUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

/**
 * <p>
 * mapper file is utilized to convert requests into FHIR resources.
 * </p>
 *
 * @author Nandhakumar created on Jun 30, 2022
 */
@Component
public class FhirAssessmentMapper {

    private final FhirUtils fhirUtils;

    private final UserService userService;

    private final RedisTemplate<String, Map<String, List<MetaDataDTO>>> redisTemplate;

    private final RestApiUtil restApiUtil;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    public FhirAssessmentMapper(FhirUtils fhirUtils, UserService userService, RedisTemplate<String, Map<String,
            List<MetaDataDTO>>> redisTemplate, RestApiUtil restApiUtil) {
        this.fhirUtils = fhirUtils;
        this.userService = userService;
        this.redisTemplate = redisTemplate;
        this.restApiUtil = restApiUtil;
    }

    /**
     * Map FHIR Patient from SPICE PatientDTO
     *
     * @param patient            FHIR patient object
     * @param householdMemberDTO SPICE household member dto.
     * @return FHIR Patient Object
     */
    public Patient setPatient(Patient patient, HouseholdMemberDTO householdMemberDTO, String userId) {
        Address address = new Address();
        address.setCity(householdMemberDTO.getVillage());
        patient.setActive(Objects.isNull(householdMemberDTO.getIsActive()) || householdMemberDTO.getIsActive());
        patient.setBirthDate(householdMemberDTO.getDateOfBirth());
        ContactPoint contactPoint = new ContactPoint();
        contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
        contactPoint.setUse(getContactPointUse(householdMemberDTO.getPhoneNumberCategory()));
        contactPoint.setValue(householdMemberDTO.getPhoneNumber());
        patient.setTelecom(List.of(contactPoint));
        patient.setAddress(List.of(address));

        patient.setGender(Enumerations.AdministrativeGender.fromCode(householdMemberDTO.getGender()));
        HumanName name = new HumanName();
        name.setText(householdMemberDTO.getName());
        patient.setName(List.of(name));

        if (patient.getGeneralPractitioner().isEmpty()) {
            //Created UserId
            patient.addGeneralPractitioner(new Reference(
                    StringUtil.concatString(String.valueOf(ResourceType.Practitioner), Constants.FORWARD_SLASH,
                            userId)));
        }

        if (patient.getLink().isEmpty()) {
            Patient.PatientLinkComponent patientLink = new Patient.PatientLinkComponent();
            patientLink.setOther(new Reference(
                    StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                            householdMemberDTO.getId())));
            patientLink.setType(Patient.LinkType.SEEALSO);
            patient.addLink(patientLink);
        }

        if (patient.getIdentifier().isEmpty()) {
            patient.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)
                    .setValue(householdMemberDTO.getPatientId());
            patient.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                    .setValue(householdMemberDTO.getVillageId());
        } else {
            patient.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)) {
                    identifier.setValue(householdMemberDTO.getPatientId());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    identifier.setValue(householdMemberDTO.getVillageId());
                }
            });
        }

        setPregnancyStatus(householdMemberDTO.getIsPregnant(), patient);
        if (Boolean.TRUE.equals(householdMemberDTO.getIsPregnant())) {
            setPatientOverallStatus(patient.getIdentifier(), Constants.PREGNANCY);
        }
        return patient;
    }

    /**
     * Get patient details by patient reference
     *
     * @param identifiers   identifiers
     * @param patientStatus patient status
     */
    private void setPatientOverallStatus(List<Identifier> identifiers, String patientStatus) {
        if (identifiers.stream().noneMatch(
                identifier -> identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL))) {
            identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                    .setValue(patientStatus));
        } else {
            if (patientStatus != null) {
                for (Identifier identifier : identifiers) {
                    if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())) {
                        String iccmStatus = !Objects.isNull(identifier.getValue()) ?
                                extractStatus(identifier.getValue(), Constants.PREGNANCY_STATUS) : null;
                        identifier.setValue(setIccmStatus(iccmStatus, Constants.PREGNANCY));
                    }
                }
            }
        }
    }

    /**
     * Concat Both Iccmstatus and regular status
     *
     * @param iccmStatus IccmStatus
     * @param status     Regular status
     * @return status
     */
    public String setIccmStatus(String iccmStatus, String status) {
        return !Objects.isNull(iccmStatus) && !iccmStatus.isEmpty() ?
                StringUtil.concatString(status, Constants.COMMA, Constants.EMPTY_SPACE,
                        iccmStatus) : status;
    }

    /***
     * Covert patient status to display format
     *
     * @param status patient status
     * @return status value
     */
    public String changePatientStatusToDisplayFormat(String status) {
        String mappedStatus = null;
        if (!Objects.isNull(status)) {
            String iccmStatus = extractStatus(status, Constants.PREGNANCY_STATUS);
            String pregnantStatus = extractStatus(status, Constants.PATIENT_STATUS_LIST);
            if (StringUtils.isEmpty(iccmStatus) && !Objects.isNull(pregnantStatus)) {
                mappedStatus = Constants.PATIENT_STATUS_DISPLAY_NAMES.get(pregnantStatus.toLowerCase());
            } else if (StringUtils.isEmpty(pregnantStatus) && !Objects.isNull(iccmStatus)) {
                mappedStatus = Constants.PATIENT_STATUS_DISPLAY_NAMES.get(iccmStatus.toLowerCase());
            } else {
                mappedStatus = setIccmStatus(Objects.nonNull(iccmStatus) ? Constants.PATIENT_STATUS_DISPLAY_NAMES.get(iccmStatus.toLowerCase()) : null,
                        Objects.nonNull(pregnantStatus) ? Constants.PATIENT_STATUS_DISPLAY_NAMES.get(pregnantStatus.toLowerCase()) : null);
            }
        }
        return mappedStatus;
    }


    /**
     * Get ICCM status from current status
     *
     * @param status status
     * @return currentIccm status
     */
    public String extractStatus(String status, List<String> skipStatus) {
        String[] currentStatus = status.split(Constants.COMMA);
        String extractedStatus = Constants.EMPTY_STRING;
        for (String statusValue : currentStatus) {
            if (!skipStatus.contains(statusValue)) {
                extractedStatus = statusValue;
                break;
            }
        }
        return extractedStatus.replace(Constants.EMPTY_SPACE, Constants.EMPTY_STRING);
    }

    /**
     * get Summary patient status Details
     *
     * @param patient patient Object
     * @return patient status list
     */
    public List<Map<String, String>> getSummaryStatus(Patient patient) {
        List<Map<String, String>> statusList = new ArrayList<>();
        Identifier identifier = patient.getIdentifier().stream()
                .filter(value -> value.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL))
                .findFirst().orElse(null);
        String pregnancyStatus = null;
        if (Objects.nonNull(identifier) && Objects.nonNull(identifier.getValue())) {
            pregnancyStatus = extractStatus(identifier.getValue(),
                    Constants.PATIENT_STATUS_LIST);
        }
        String onTreateMentDisplayName = Constants.PATIENT_STATUS_DISPLAY_NAMES.get(Constants.ON_TREATMENT.toLowerCase());
        String recoveredDisplayName = Constants.PATIENT_STATUS_DISPLAY_NAMES.get(Constants.RECOVERED.toLowerCase());
        pregnancyStatus = !Objects.isNull(pregnancyStatus) ?
                Constants.PATIENT_STATUS_DISPLAY_NAMES.get(pregnancyStatus.toLowerCase()) : pregnancyStatus;

        statusList.add(Map.of(Constants.VALUE, Constants.ON_TREATMENT, Constants.NAME,
                Objects.isNull(pregnancyStatus) ? onTreateMentDisplayName :
                        StringUtil.concatString(pregnancyStatus, Constants.COMMA, Constants.EMPTY_SPACE,
                                onTreateMentDisplayName)));
        statusList.add(Map.of(Constants.VALUE, Constants.RECOVERED, Constants.NAME,
                Objects.isNull(pregnancyStatus) ? recoveredDisplayName :
                        StringUtil.concatString(pregnancyStatus, Constants.COMMA, Constants.EMPTY_SPACE,
                                recoveredDisplayName)));
        return statusList;
    }

    /**
     * Maps a given phone number category to a corresponding
     * ContactPointUse enum value.
     *
     * @param phoneNumberCategory The `phoneNumberCategory` parameter is a string that represents the
     *                            category of a phone number. It can have values like "PERSONAL", "FAMILY_MEMBER", or "FRIEND".
     * @return Returns the ContactPointUse enum value associated with the given phoneNumberCategory.
     */
    private ContactPoint.ContactPointUse getContactPointUse(String phoneNumberCategory) {
        Map<String, ContactPoint.ContactPointUse> map = new HashMap<>();
        map.put(Constants.PERSONAL, ContactPoint.ContactPointUse.MOBILE);
        map.put(Constants.FAMILY_MEMBER, ContactPoint.ContactPointUse.HOME);
        map.put(Constants.FRIEND, ContactPoint.ContactPointUse.TEMP);
        return map.get(phoneNumberCategory);
    }

    /**
     * update Encounter status based
     *
     * @param id            EncounterId
     * @param provenanceDTO Providence Details
     * @param status        patient Status
     */
    public Encounter updateEncounterStatusDetails(String id, ProvenanceDTO provenanceDTO, String status,
                                                  Bundle updateBundle) {
        Encounter encounter = null;
        Bundle bundle = getEncounterDetails(id, Boolean.FALSE);
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (String.valueOf(ResourceType.Encounter).equals(entry.getResource().getResourceType().toString())) {
                encounter = (Encounter) entry.getResource();
                encounter.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue(status);
                String url = StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH,
                        id);
                String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, id);
                fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.PUT, encounter, updateBundle, provenanceDTO);
            }
        }
        return encounter;
    }

    /**
     * Get Encounter using EncounterId
     *
     * @param id id
     * @return Bundle Object
     */
    public Bundle getEncounterDetails(String id, boolean includeObservation) {
        String url = String.format(Constants.GET_ENCOUNTER_ID, id);
        if (includeObservation) {
            url = StringUtil.concatString(url, Constants.REV_INCLUDE_ENCOUNTER_OBSERVATION);
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Set the pregnancy status of a Patient or a RelatedPerson resource.
     * It checks if the resource already has a pregnancy status extension. If it does, it updates the value.
     * If it doesn't, it creates a new extension and adds it to the resource.
     *
     * @param status   The pregnancy status to be set. It is a Boolean value.
     * @param resource The FHIR resource (Patient or RelatedPerson) to which the pregnancy status is to be set.
     * @return The FHIR resource with the pregnancy status set.
     */
    public DomainResource setPregnancyStatus(Boolean status, DomainResource resource) {
        if (resource instanceof Patient || resource instanceof RelatedPerson) {
            boolean hasPregnantExtension = false;
            List<Extension> extensions = resource.getExtension();
            for (Extension extension : extensions) {
                if (FhirIdentifierConstants.IS_PREGNANT_EXTENSION_URL.equals(extension.getUrl())) {
                    extension.setValue(new StringType(fhirUtils.convertPregnancyType(status)));
                    hasPregnantExtension = true;
                }
            }
            if (!hasPregnantExtension) {
                Extension extension = createIsPregnantExtension();
                extension.setValue(new StringType(fhirUtils.convertPregnancyType(status)));
                resource.addExtension(extension);
            }
        }
        return resource;
    }

    /**
     * Create a new FHIR extension for indicating pregnancy status.
     *
     * @return The newly created FHIR extension.
     */
    private Extension createIsPregnantExtension() {
        Extension extension = new Extension();
        extension.setUrl(FhirIdentifierConstants.IS_PREGNANT_EXTENSION_URL);
        return extension;
    }

    /**
     * Create FHIR Encounter for Assessment
     *
     * @param encounterDetailsDTO AssessmentDetails
     * @param bundle              Bundle Object
     */
    public String createEncounter(EncounterDetailsDTO encounterDetailsDTO, Bundle bundle, String encounterType,
                                  String partOfEncounter) {
        String uuid = fhirUtils.getUniqueId();
        String id = StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL,
                uuid);
        Encounter encounter = setEncounter(new Encounter(), encounterDetailsDTO, encounterType, Boolean.TRUE);
        if (!Objects.isNull(partOfEncounter)) {
            encounter.setPartOf(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH, partOfEncounter)));
        }
        fhirUtils.setBundle(id,
                StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                Bundle.HTTPVerb.POST,
                encounter,
                bundle,
                encounterDetailsDTO.getProvenance());
        return id;
    }

    /**
     * Update FHIR Encounter for Assessment
     *
     * @param encounterDetailsDTO AssessmentDetails
     * @param bundle              Bundle Object
     */
    public void updateEncounter(EncounterDetailsDTO encounterDetailsDTO, Bundle bundle, String encounterType,
                                String partOfEncounter) {
        String url = String.format(Constants.GET_ENCOUNTER_ID, encounterDetailsDTO.getId());
        Bundle encounterBundle = restApiUtil.getBatchRequest(url);
        encounterBundle.getEntry().forEach(resource -> {
            Encounter encounter = (Encounter) resource.getResource();
            encounter = setEncounter(encounter, encounterDetailsDTO, encounterType, Boolean.FALSE);
            if (!Objects.isNull(partOfEncounter)) {
                encounter.setPartOf(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH, partOfEncounter)));
            }
            fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_ENCOUNTER, Constants.FORWARD_SLASH, encounterDetailsDTO.getId()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, encounterDetailsDTO.getId()),
                    Bundle.HTTPVerb.PUT, encounter, bundle, encounterDetailsDTO.getProvenance());
        });


    }

    /**
     * Create Observation for yes or no questions
     *
     * @param encounterDetailsDTO Assessment Details
     * @param name                QuestionName
     * @param value               answer value
     * @param identifier          observation unique value
     * @param bundle              Bundle Object
     */
    public void createObservation(EncounterDetailsDTO encounterDetailsDTO,
                                  String name,
                                  Boolean value,
                                  String identifier,
                                  Bundle bundle) {
        if (!Objects.isNull(value)) {
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            String url = StringUtil.concatString(String.valueOf(ResourceType.Observation),
                    Constants.FORWARD_SLASH,
                    Constants.FHIR_BASE_URL,
                    uuid);
            Observation observation = setObservation(encounterDetailsDTO,
                    name,
                    identifier);
            setValuesToObservation(observation, null, value, null);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle, encounterDetailsDTO.getProvenance());
        }
    }

    /**
     * Create Observation for String answer questions
     *
     * @param encounterDetailsDTO Assessment Details
     * @param name                QuestionName
     * @param identifier          unique value
     * @param valueString         answer value
     * @param bundle              Bundle Object with observation
     */
    public String createObservation(EncounterDetailsDTO encounterDetailsDTO, String name, String identifier,
                                    String valueString, Bundle bundle) {
        String url = Constants.EMPTY_STRING;
        if (!Objects.isNull(valueString)) {
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                    Constants.FHIR_BASE_URL, uuid);
            Observation observation = setObservation(encounterDetailsDTO, name, identifier);
            setValuesToObservation(observation, valueString, null, null);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle,
                    encounterDetailsDTO.getProvenance());
        }
        return url;
    }

    /**
     * Adds  observation to the bundle.
     *
     * @param observation   FHIR observation
     * @param bundle        Bundle Object
     * @param provenanceDTO provenance Details
     * @return String
     */
    public String addObservationToBundle(Observation observation, Bundle bundle, ProvenanceDTO provenanceDTO) {
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Observation),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL,
                uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle, provenanceDTO);
        return url;
    }


    /**
     * Create Birth History for child
     *
     * @param signs               List of signs
     * @param weight              Weight Value
     * @param gestationalAge      Age
     * @param encounterDetailsDTO EncounterDetails
     * @param bundle              Bundle Object
     */
    public void createBirthHistory(List<String> signs, Double weight, Integer gestationalAge,
                                   EncounterDetailsDTO encounterDetailsDTO, Bundle bundle) {
        Map<String, List<MetaDataDTO>> valuesMap = redisTemplate.opsForValue().get(Constants.META);
        BirthHistoryDTO birthHistoryDTO = new BirthHistoryDTO();
        if (valuesMap != null) {
            List<String> symptomsByCategory = valuesMap.get(Constants.SYMPTOM).stream()
                    .filter(metaData -> Constants.BREATHING.equalsIgnoreCase(metaData.getDiseaseType())).map(symptom -> String.valueOf(symptom.getValue()))
                    .toList();
            if (Objects.nonNull(signs)) {
                birthHistoryDTO.setHaveBreathingProblem(signs.stream().anyMatch(symptomsByCategory::contains));
            }
        }
        if (!Objects.isNull(weight)) {
            birthHistoryDTO.setBirthWeight(weight);
            birthHistoryDTO.setBirthWeightCategory(CommonUtils.birthWeight(weight));
        }

        if (!Objects.isNull(gestationalAge)) {
            birthHistoryDTO.setGestationalAge(gestationalAge);
            birthHistoryDTO.setGestationalAgeCategory(CommonUtils.gestationalAge(gestationalAge));
        }
        createVitalObservation(bundle, encounterDetailsDTO, Constants.BIRTH_HISTORY, birthHistoryDTO,
                encounterDetailsDTO.getPatientReference());
    }


    /**
     * Set patientBasic details using Observation
     *
     * @param values Observation map
     * @return PregnancyDetailsDTO patientDetails
     */
    public PregnancyDetailsDTO setPatientBasicDetails(Map<String, Observation> values) {
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setLastMenstrualPeriod(!Objects.isNull(values.get(Constants.LAST_MENSTRUAL_PERIOD)) ? values.get(Constants.LAST_MENSTRUAL_PERIOD)
                .getValueDateTimeType().getValue() : null);
        return pregnancyDetailsDTO;
    }

    /**
     * update Previous encounter details
     *
     * @param identifier Assessment Details
     * @param bundle     Bundle Object
     */
    public void updatePreviousEncounters(List<String> identifier, Bundle bundle, ProvenanceDTO provenanceDTO, String patientReference) {
        String requestUrl = String.format(Constants.UPDATE_ENCOUNTER_PARAMS, FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL, identifier, StringUtil.concatString(String.valueOf(ResourceType.Patient),
                Constants.FORWARD_SLASH,
                patientReference));
        Bundle bundleEncounter = restApiUtil.getBatchRequest(requestUrl);
        bundleEncounter.getEntry().forEach(entry -> {
            Encounter encounter = (Encounter) entry.getResource();
            encounter.setStatus(Encounter.EncounterStatus.FINISHED);
            String id = fhirUtils.getIdFromHistoryUrl(encounter.getId());
            String url = StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                    Constants.FORWARD_SLASH, id);
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, id);
            fhirUtils.setBundle(url,
                    fullUrl,
                    Bundle.HTTPVerb.PUT,
                    encounter,
                    bundle,
                    provenanceDTO);
        });
    }

    /**
     * Set Encounter using AssessmentDTO
     *
     * @param encounter        FHIR Encounter object
     * @param encounterDetails encounterDetails
     * @param encounterType    encounter Type
     * @return Encounter Object
     */
    public Encounter setEncounter(Encounter encounter, EncounterDetailsDTO encounterDetails, String encounterType, boolean isEncounterCreate) {
        if (isEncounterCreate) {
            Period period = new Period();
            period.setStart(encounterDetails.getStartTime());
            period.setEnd(encounterDetails.getEndTime());
            encounter.setPeriod(period);
        } else {
            Period period = encounter.getPeriod();
            period.setEnd(encounterDetails.getEndTime());
        }
        if (Objects.nonNull(encounterType) && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue(encounterType);
        }
        if (encounterDetails.isPrescribed() && !encounterDetails.isDispensed() && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL).setValue(Constants.PRESCRIBED);
        }
        if (encounterDetails.isDispensed() && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL).setValue(Constants.PRESCRIPTION_DISPENSED);
        }

        if (encounterDetails.isLabTest() && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL).setValue(Constants.INVESTIGATED);
        }

        if (!Objects.isNull(encounterDetails.getType()) && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.TYPE_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.TYPE_SYSTEM_URL).setValue(encounterDetails.getType());
        }

        if (Objects.nonNull(encounterDetails.getFollowUpId()) && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.FOLLOW_UP_ID_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.FOLLOW_UP_ID_SYSTEM_URL).setValue(String.valueOf(encounterDetails.getFollowUpId()));
        }
        if (encounterDetails.getVisitNumber() != Constants.ZERO) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL).setValue(
                    String.valueOf(encounterDetails.getVisitNumber()));
        }

        if (Objects.nonNull(encounterDetails.getVillageId()) && encounter.getIdentifier().stream().noneMatch(
                identifier -> identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                    .setValue(encounterDetails.getVillageId());
        }

        encounter.setServiceProvider(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                Constants.FORWARD_SLASH, encounterDetails.getProvenance().getOrganizationId())));
        encounter.setStatus(Encounter.EncounterStatus.INPROGRESS);
        if (!Objects.isNull(encounterDetails.getPatientReference())) {
            encounter.setSubject(new Reference(StringUtil.concatString(encounterDetails.getPatientReference())));
        } else {
            encounter.setSubject(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Group),
                    Constants.FORWARD_SLASH,
                    encounterDetails.getHouseholdId())));
        }

        //Add assessment Location
        if (!Objects.isNull(encounterDetails.getLatitude()) && !Objects.isNull(encounterDetails.getLongitude())) {
            Encounter.EncounterLocationComponent component = new Encounter.EncounterLocationComponent();
            Location location = new Location();
            location.getPosition().setLatitude(encounterDetails.getLatitude());
            location.getPosition().setLongitude(encounterDetails.getLongitude());
            component.setLocation(new Reference(location));
            encounter.addLocation(component);
        }

        if (encounter.getParticipant().stream().noneMatch(participant -> participant.getIndividual().getReference().contains(String.valueOf(ResourceType.RelatedPerson)))) {
            Encounter.EncounterParticipantComponent encounterParticipantPerson = new Encounter.EncounterParticipantComponent();
            encounterParticipantPerson.setIndividual(new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                    Constants.FORWARD_SLASH, encounterDetails.getMemberId())));
            encounter.addParticipant(encounterParticipantPerson);
        }

        if (encounter.getParticipant().stream().noneMatch(participant -> participant.getIndividual().getReference().contains(String.valueOf(ResourceType.Practitioner)))) {
            Encounter.EncounterParticipantComponent encounterParticipantComponent = new Encounter.EncounterParticipantComponent();
            encounterParticipantComponent.setIndividual(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Practitioner),
                    Constants.FORWARD_SLASH, encounterDetails.getProvenance().getUserId())));
            encounter.addParticipant(encounterParticipantComponent);
        }

        //Set Diagnosis
        if (!Objects.isNull(encounterDetails.getDiagnosisType())) {
            setDiagnosis(encounter, encounterDetails.getPatientReference(), encounterDetails.getDiagnosisType());
        }
        return encounter;
    }

    /**
     * Set Diagnosis values to the encounter
     *
     * @param encounter        encounter object
     * @param patientReference patient Reference
     * @param type             encounterType
     */
    public void setDiagnosis(Encounter encounter, String patientReference, String type) {
        if (!Objects.isNull(patientReference) && !patientReference.contains(Constants.FHIR_BASE_URL)) {
            Bundle bundle = restApiUtil.getBatchRequest(
                    String.format(Constants.CONDITION_QUERY, fhirUtils.getIdFromResourceUrl(patientReference), type));
            List<Encounter.DiagnosisComponent> diagnosisList = new ArrayList<>();
            bundle.getEntry().forEach(condition -> {
                Encounter.DiagnosisComponent component = new Encounter.DiagnosisComponent();
                component.setCondition(new Reference(
                        StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH,
                                condition.getResource().getIdPart())));
                diagnosisList.add(component);
            });
            if (!diagnosisList.isEmpty()) {
                encounter.setDiagnosis(diagnosisList);
            }
        }
    }

    /**
     * Map Condition o diagnosis
     *
     * @param condition              Condition object
     * @param isMedicalReviewSummary util conversion
     * @param type                   type of the review
     * @return DiseaseDTO object
     */
    public DiagnosisDTO.DiseaseDTO mapToDiagnosis(Condition condition, boolean isMedicalReviewSummary, String type) {
        DiagnosisDTO.DiseaseDTO disease = new DiagnosisDTO.DiseaseDTO();
        if (Boolean.TRUE.equals(isMedicalReviewSummary)) {
            disease.setDiseaseCategory(Objects.isNull(fhirUtils.getText(condition.getCategory().getFirst().getText())) ?
                    condition.getCategory().getFirst().getText() : fhirUtils.getText(condition.getCategory().getFirst().getText()));
            disease.setDiseaseCondition((Objects.isNull(fhirUtils.getText(condition.getCode().getText())) ?
                    condition.getCode().getText() : fhirUtils.getText(condition.getCode().getText())));
        } else {
            disease.setDiseaseCategory(condition.getCategory().getFirst().getText());
            disease.setDiseaseCondition(condition.getCode().getText());
        }
        disease.setType(type);
        return disease;
    }

    /**
     * <p>
     * This method extracts the provisional diagnosis details from a given Bundle.
     * </p>
     *
     * @param bundle the FHIR Bundle containing Condition resources
     * @return a List of Strings containing the text of each provisional diagnosis
     */
    public List<String> getProvisionalDiagnosisDetails(Bundle bundle) {
        List<String> provisionalDiagnosis = new ArrayList<>();
        for (Bundle.BundleEntryComponent resource : bundle.getEntry()) {
            Condition condition = (Condition) resource.getResource();
            List<CodeableConcept> categories = condition.getCategory();
            categories.forEach(category -> provisionalDiagnosis.add(category.getText())
            );
        }
        return provisionalDiagnosis;
    }

    /**
     * Map diagnosis details for a patient.
     * <p>
     * This method accepts a {@link Bundle} identifying the patient and returns detailed information
     * about the patient's diagnosis.
     * </p>
     *
     * @param bundle The {@link Bundle} identifying the patient.
     * @return A {@link ConfirmDiagnosisDTO} containing detailed diagnosis information.
     */
    public ConfirmDiagnosisDTO mapToDiagnosis(Bundle bundle) {
        ConfirmDiagnosisDTO confirmDiagnosisDTO = new ConfirmDiagnosisDTO();
        List<Map<String, String>> diagnosis = new ArrayList<>();
        AtomicReference<String> notes = new AtomicReference<>();
        List<String> mentalHealthLevels = new ArrayList<>();
        for (Bundle.BundleEntryComponent resource : bundle.getEntry()) {
            Condition condition = (Condition) resource.getResource();
            List<CodeableConcept> categories = condition.getCategory();
            condition.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_DIAGNOSIS_OTHER_IDENTIFIER_URL)) {
                    notes.set((Objects.nonNull(condition.getNote()) &&
                            !condition.getNote().isEmpty())
                            ? condition.getNote().getFirst().getText() : null);
                }
            });
            categories.forEach(category -> {
                Map<String, String> categoryMap = new HashMap<>();
                String value = category.getText();
                if (Objects.nonNull(fhirUtils.getCodeDetails().get(value))) {
                    categoryMap.put(Constants.NAME, fhirUtils.getCodeDetails().get(value).getText());
                    categoryMap.put(Constants.DISPLAY_VALUE, fhirUtils.getText(value));
                } else {
                    String name = value.replaceAll(Constants.WORD_SPACE_REGEX, "$1 $2").toLowerCase();
                    name = name.substring(Constants.ZERO, Constants.ONE).toUpperCase() + name.substring(Constants.ONE);
                    categoryMap.put(Constants.NAME, name);
                }
                categoryMap.put(Constants.VALUE, value);
                if (FhirConstants.ANXIETY.equals(category.getText())) {
                    mentalHealthLevels.add(Constants.GAD7);
                }
                if (FhirConstants.DEPRESSION.equals(category.getText())) {
                    mentalHealthLevels.add(Constants.PHQ9);
                }
                diagnosis.add(categoryMap);
            });
        }
        confirmDiagnosisDTO.setDiagnosis(diagnosis);
        confirmDiagnosisDTO.setIsConfirmDiagnosis(Boolean.TRUE);
        confirmDiagnosisDTO.setDiagnosisNotes(notes.get());
        confirmDiagnosisDTO.setMentalHealthLevels(mentalHealthLevels);
        return confirmDiagnosisDTO;
    }

    /**
     * Set Observation Resource using Reference values
     *
     * @param encounterDetailsDTO encounterDetails
     * @param name                Question Name
     * @param identifier          unique value
     * @return Observation Object
     */
    public Observation setObservation(EncounterDetailsDTO encounterDetailsDTO, String name,
                                      String identifier) {
        Observation observation = new Observation();
        if (!Objects.isNull(identifier)) {
            observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue(identifier);
        }

        observation.setStatus(Observation.ObservationStatus.PRELIMINARY);
        if (!Objects.isNull(encounterDetailsDTO.getId())) {
            observation.setEncounter(new Reference(encounterDetailsDTO.getId()));
        }
        observation.getEffectiveDateTimeType().setValue(encounterDetailsDTO.getProvenance().getModifiedDate());
        if (!Objects.isNull(encounterDetailsDTO.getPatientReference())) {
            observation.setSubject(new Reference(encounterDetailsDTO.getPatientReference()));
        }
        observation.addPerformer(new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                Constants.FORWARD_SLASH, encounterDetailsDTO.getMemberId())));
        observation.setCode(fhirUtils.setCodes(name));
        return observation;
    }

    /**
     * Set Answer To Observation
     *
     * @param observation Observation
     * @param valueString String value
     * @param answer      boolean value
     * @param date        date value
     */
    public void setValuesToObservation(Observation observation, String valueString, Boolean answer, Date date) {
        if (!Objects.isNull(answer)) {
            observation.getValueCodeableConcept().addCoding()
                    .setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                    .setCode(Boolean.TRUE.equals(answer) ? Constants.YES_CODE : Constants.NO_CODE)
                    .setDisplay(Boolean.TRUE.equals(answer) ? Constants.YES : Constants.NO);
            observation.getValueCodeableConcept().setText(answer.equals(Boolean.TRUE) ? Constants.YES : Constants.NO);
        } else if (!Objects.isNull(valueString)) {
            observation.getValueStringType().setValue(valueString);
        } else if (!Objects.isNull(date)) {
            observation.getValueDateTimeType().setValue(date);
        }
    }

    /**
     * Creates notes mapping.
     *
     * @param title Notes Title
     * @param notes Notes Value
     * @return Observation
     */
    public Observation createNotes(String title, String notes, EncounterDetailsDTO encounterDetailsDTO) {
        Observation observation = setObservation(encounterDetailsDTO, title, title);
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(notes);
        observation.setValue(codeableConcept);
        return observation;
    }

    /**
     * Set Quantity to the observation
     *
     * @param observation FHIR Observation object
     * @param quantity    Quantity Object
     * @param question    single quantity value
     */
    public void setQuantityToObservation(Observation observation, Quantity quantity,
                                         String question) {
        Observation.ObservationComponentComponent assessmentComponent = new Observation.ObservationComponentComponent();
        if (!Objects.isNull(quantity)) {
            assessmentComponent.setValue(quantity);
        }
        assessmentComponent.getCode().setText(question);
        assessmentComponent.setCode(fhirUtils.setCodes(question));
        observation.addComponent(assessmentComponent);
    }

    /**
     * Set Quantity to the observation
     *
     * @param observation FHIR Observation object
     * @param quantity    Quantity Object
     * @param question    single quantity value
     */
    public void setQuantityToObservation(Observation observation, double quantity,
                                         String question) {
        if (!Objects.isNull(quantity)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.setValue(new Quantity().setValue(quantity));
            observation.addComponent(observationComponent);
        }
    }

    /**
     * Assessment to setMedicationDispense
     *
     * @param encounterDetailsDTO Assessment Details
     * @param medicationName      Medication Name
     * @param status              Coding name
     * @return FHIR MedicationRequest Object
     */
    public MedicationDispense setMedicationDispense(EncounterDetailsDTO encounterDetailsDTO, String medicationName, String status, String encounterId) {
        MedicationDispense medicationDispense = new MedicationDispense();
        medicationDispense.getMedicationCodeableConcept().setText(medicationName);
        medicationDispense.getMedicationCodeableConcept().setCoding(fhirUtils.setCodes(medicationName).getCoding());
        medicationDispense.setStatus(MedicationDispense.MedicationDispenseStatus.valueOf(status));
        if (!Objects.isNull(encounterDetailsDTO.getPatientReference())) {
            medicationDispense.setSubject(new Reference(encounterDetailsDTO.getPatientReference()));
        }
        medicationDispense.setPerformer(List.of(new MedicationDispense.MedicationDispensePerformerComponent(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                        Constants.FORWARD_SLASH, encounterDetailsDTO.getMemberId())))));
        medicationDispense.setContext(new Reference(encounterId));
        return medicationDispense;
    }

    /**
     * Assessment to setMedicationRequest
     *
     * @param assessmentDTO  Assessment Details
     * @param medicationName Medication Name
     * @param coding         Coding name
     * @return FHIR MedicationRequest Object
     */
    public MedicationRequest setMedicationRequest(AssessmentDTO assessmentDTO, String medicationName,
                                                  List<Coding> coding, String status) {
        MedicationRequest medicationRequest = new MedicationRequest();
        medicationRequest.getMedicationCodeableConcept().setText(medicationName);
        medicationRequest.getMedicationCodeableConcept().setCoding(coding);
        medicationRequest.setStatus(MedicationRequest.MedicationRequestStatus.valueOf(status));
        if (assessmentDTO.getEncounter().isReferred()) {
            medicationRequest.setSubject(new Reference(assessmentDTO.getEncounter().getPatientReference()));
        }
        medicationRequest.setPerformer(new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                Constants.FORWARD_SLASH, assessmentDTO.getEncounter().getMemberId())));
        medicationRequest.setEncounter(new Reference(assessmentDTO.getId()));
        return medicationRequest;
    }

    /**
     * Create Observation Component BOOLEAN and add.
     *
     * @param value      Boolean value
     * @param question   Quest value
     * @param components Observation Component
     */
    public void createObservationComponent(Boolean value,
                                           String question,
                                           List<ObservationComponentComponent> components) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.getValueCodeableConcept().addCoding()
                    .setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                    .setCode(Boolean.TRUE.equals(value) ? Constants.YES_CODE : Constants.NO_CODE)
                    .setDisplay(Boolean.TRUE.equals(value) ? Constants.YES : Constants.NO);
            observationComponent.getValueCodeableConcept().setText(value.equals(Boolean.TRUE) ? Constants.YES : Constants.NO);
            components.add(observationComponent);
        }
    }

    /**
     * Create Observation Component BOOLEAN and add.
     *
     * @param value    Boolean value
     * @param question Quest value
     * @return observation component
     */
    public ObservationComponentComponent createObservationComponent(Boolean value, String question) {
        ObservationComponentComponent component = null;
        if (Objects.nonNull(value)) {
            component = createObservationComponent(question);
            component.getValueCodeableConcept().addCoding().setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                    .setCode(Boolean.TRUE.equals(value) ? Constants.YES_CODE : Constants.NO_CODE)
                    .setDisplay(Boolean.TRUE.equals(value) ? Constants.YES : Constants.NO);
            component.getValueCodeableConcept().setText(value.equals(Boolean.TRUE) ? Constants.YES : Constants.NO);
        }
        return component;
    }

    /**
     * Create Observation Component Date and add.
     *
     * @param value      Date value
     * @param question   Question Value
     * @param components Observation Component
     */
    public void createObservationComponent(Date value, String question, List<ObservationComponentComponent> components) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.setValue(new DateTimeType(value));
            components.add(observationComponent);
        }
    }

    /**
     * Create Observation Component String and add.
     *
     * @param value      String value
     * @param question   Question value
     * @param components Observation Component
     */
    public void createObservationComponent(String value,
                                           String question,
                                           List<ObservationComponentComponent> components) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.setValue(fhirUtils.createCodeableConcept(value));
            components.add(observationComponent);
        }
    }

    /**
     * Create Observation Component String and add.
     *
     * @param value      String value
     * @param question   Question value
     * @param components Observation Component
     */
    public void createObservationComponent(Double value,
                                           String question,
                                           List<ObservationComponentComponent> components, String unit) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.setValue(new Quantity(value).setUnit(unit));
            components.add(observationComponent);
        }
    }

    /**
     * Creates an observation component and adds it to the provided list if the value is not null.
     *
     * @param value      The integer value for the observation component.
     * @param question   The question associated with the observation component.
     * @param components The list of observation components to which the new component will be added.
     */
    public void createObservationComponent(Integer value,
                                           String question,
                                           List<ObservationComponentComponent> components) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.setValue(new Quantity().setValue(value));
            observationComponent.setCode(fhirUtils.setCodes(question));
            components.add(observationComponent);
        }
    }

    /**
     * Create Observation Component
     *
     * @param question Question value
     * @return FHIR ObservationComponentComponent
     */
    public ObservationComponentComponent createObservationComponent(String question) {
        ObservationComponentComponent observationComponent = new ObservationComponentComponent();
        observationComponent.setCode(fhirUtils.setCodes(question));
        return observationComponent;
    }

    /**
     * Map Assessment to DiagnosticReport
     *
     * @param name          Report Name
     * @param assessmentDTO Assessment Details
     * @param result        Result value
     * @param reference     Observation reference
     * @return FHIR DiagnosticReport Object
     */
    public DiagnosticReport mapDiagnosticReport(String name,
                                                AssessmentDTO assessmentDTO,
                                                String result,
                                                String reference) {
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setEncounter(new Reference(assessmentDTO.getId()));
        diagnosticReport.setCode(fhirUtils.setCodes(name));
        diagnosticReport.setConclusion(result);
        if (assessmentDTO.getEncounter().isReferred()) {
            diagnosticReport.setSubject(new Reference(assessmentDTO.getEncounter().getPatientReference()));
        }
        diagnosticReport.setStatus(Constants.NOT_APPLICABLE.equals(result) ? DiagnosticReport.DiagnosticReportStatus.REGISTERED
                : DiagnosticReport.DiagnosticReportStatus.FINAL);
        diagnosticReport.addResult(new Reference(reference));
        return diagnosticReport;
    }

    /**
     * Set Service request to ReferralTicketDTO
     *
     * @param serviceRequest    FHIR serviceRequest object
     * @param referralTicketDTO ticket dto
     * @param resourceMap       other supporting resources
     * @return ReferralTicketDTO Ticket Details
     */
    public ReferralTicketDTO setReferralTicket(ServiceRequest serviceRequest, ReferralTicketDTO referralTicketDTO,
                                               Map<String, Resource> resourceMap, Map<String, Provenance> provenanceMap) {
        String id = fhirUtils.getIdFromHistoryUrl(serviceRequest.getId());
        referralTicketDTO.setId(id);
        referralTicketDTO.setReferredReason(serviceRequest.getPatientInstruction());
        referralTicketDTO.setReferredDate(serviceRequest.getAuthoredOn());
        serviceRequest.getIdentifier().forEach(identifier -> {
            if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)) {
                referralTicketDTO.setPatientStatus(changePatientStatusToDisplayFormat(identifier.getValue()));
            }
        });
        Practitioner practitioner = userService.getUserById(
                provenanceMap.get(serviceRequest.getIdPart()).getAgent().getFirst().getWho().getReference());
        referralTicketDTO.setReferredBy(practitioner.getName().getFirst().getText());
        practitioner.getTelecom().forEach(teleCom -> {
            if (Constants.PHONE_FHIR.equals(String.valueOf(teleCom.getSystem()))) {
                referralTicketDTO.setPhoneNumber(teleCom.getValue());
            }
        });
        serviceRequest.getPerformer().forEach(resource -> {
            if (ResourceType.Organization.equals(resourceMap.get(resource.getReference()).getResourceType())) {
                //set Referred Site Name
                Organization organization = (Organization) resourceMap.get(resource.getReference());
                referralTicketDTO.setReferredTo(organization.getName());
            }
        });
        return referralTicketDTO;
    }

    /**
     * Set Signs into observation
     *
     * @param signs               List of signs
     * @param encounterDetailsDTO Reference
     * @param identifier          unique value
     * @param note                sign note
     * @return Observation Object
     */
    public Observation createSignsObservation(List<String> signs, EncounterDetailsDTO encounterDetailsDTO,
                                              String identifier, String note) {
        if (Objects.isNull(signs) || signs.isEmpty()) {
            return null;
        }
        Observation observation = new Observation();
        observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue(identifier);
        observation.setStatus(Observation.ObservationStatus.PRELIMINARY);
        observation.setEncounter(new Reference(encounterDetailsDTO.getId()));
        if (!Objects.isNull(note)) {
            observation.addNote(new Annotation().setText(note));
        }
        observation.setCode(fhirUtils.setCodes(identifier));
        if (!Objects.isNull(encounterDetailsDTO.getPatientReference())) {
            observation.setSubject(new Reference(encounterDetailsDTO.getPatientReference()));
        }
        if (!Objects.isNull(encounterDetailsDTO.getMemberId())) {
            observation.addPerformer(new Reference(
                    StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                            encounterDetailsDTO.getMemberId())));
        }
        signs.forEach(sign -> createObservationComponent(Boolean.TRUE, sign, observation.getComponent()));
        return observation;
    }

    /**
     * Creates an Observation for the APGAR score at one minute.
     *
     * @param apgarScoreDTO       List of APGARScoreDTO objects containing the APGAR score components.
     * @param encounterDetailsDTO Reference to the encounter.
     * @param identifier          Identifier for the observation.
     * @return An Observation object representing the APGAR score at one minute, or null if the apgarComponents list is empty.
     */
    public Observation createApgarObservationComponent(APGARScoreDTO apgarScoreDTO,
                                                       EncounterDetailsDTO encounterDetailsDTO,
                                                       String identifier) {
        Observation observation = setObservation(encounterDetailsDTO, identifier, identifier);
        observation.setValue(new Quantity(apgarScoreDTO.getOneMinuteTotalScore()));
        createObservationComponent(apgarScoreDTO.getActivity(), Constants.ACTIVITY,
                observation.getComponent());
        createObservationComponent(apgarScoreDTO.getPulse(), Constants.PULSE, observation.getComponent());
        createObservationComponent(apgarScoreDTO.getGrimace(), Constants.GRIMACE,
                observation.getComponent());
        createObservationComponent(apgarScoreDTO.getAppearance(), Constants.APPEARANCE,
                observation.getComponent());
        createObservationComponent(apgarScoreDTO.getRespiration(), Constants.RESPIRATION,
                observation.getComponent());
        return observation;
    }

    /**
     * create Referral Ticket for a patient
     *
     * @param referralDetailsDTO Ticket Details
     * @return ServiceRequest Object
     */
    public ServiceRequest createReferralTicket(ReferralDetailsDTO referralDetailsDTO) {
        boolean isRecoveredPatient =
                referralDetailsDTO.getPatientStatus().toLowerCase().contains(Constants.RECOVERED.toLowerCase());
        ServiceRequest serviceRequest = new ServiceRequest();
        serviceRequest.addIdentifier()
                .setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                .setValue(isRecoveredPatient ? Constants.RECOVERED : referralDetailsDTO.getPatientStatus());
        serviceRequest.addIdentifier()
                .setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL)
                .setValue(Objects.isNull(referralDetailsDTO.getCurrentPatientStatus()) ?
                        referralDetailsDTO.getPatientStatus() : referralDetailsDTO.getCurrentPatientStatus());
        serviceRequest.addIdentifier().setSystem(FhirIdentifierConstants.TICKET_CATEGORY_SYSTEM_URL)
                .setValue(referralDetailsDTO.getCategory());
        serviceRequest.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL)
                .setValue(referralDetailsDTO.getEncounterType());
        serviceRequest.setAuthoredOn(referralDetailsDTO.getProvenance().getModifiedDate());
        serviceRequest.setPatientInstruction(referralDetailsDTO.getReferredReason());
        if (!Objects.isNull(referralDetailsDTO.getEncounterId())) {
            serviceRequest.setEncounter(new Reference(referralDetailsDTO.getEncounterId()));
        }
        if (!Objects.isNull(referralDetailsDTO.getNextVisitDate())) {
            serviceRequest.setOccurrence(new DateTimeType(referralDetailsDTO.getNextVisitDate()));
        }

        //Add current organization
        serviceRequest.setRequester(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                Constants.FORWARD_SLASH, referralDetailsDTO.getProvenance().getOrganizationId())));
        serviceRequest.addPerformer(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Practitioner), Constants.FORWARD_SLASH,
                        !Objects.isNull(referralDetailsDTO.getReferredClinicianId()) ?
                                referralDetailsDTO.getReferredClinicianId() :
                                referralDetailsDTO.getProvenance().getUserId())));
        serviceRequest.addPerformer(new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson)
                , Constants.FORWARD_SLASH, referralDetailsDTO.getMemberId())));
        serviceRequest.setIntent(ServiceRequest.ServiceRequestIntent.ORDER);
        serviceRequest.setPriority(ServiceRequest.ServiceRequestPriority.URGENT);
        if (referralDetailsDTO.isReferred()) {
            serviceRequest.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
            // Add referred organization
            serviceRequest.addPerformer(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                    Constants.FORWARD_SLASH, referralDetailsDTO.getReferredSiteId())));
            serviceRequest.setRequisition(new Identifier().setSystem(FhirIdentifierConstants.TICKET_TYPE_SYSTEM_URL).setValue(Constants.MEDICAL_REVIEW));
        } else {
            serviceRequest.setStatus(
                    isRecoveredPatient ?
                            ServiceRequest.ServiceRequestStatus.COMPLETED : ServiceRequest.ServiceRequestStatus.ONHOLD);
            serviceRequest.addPerformer(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                    Constants.FORWARD_SLASH, referralDetailsDTO.getProvenance().getOrganizationId())));
            serviceRequest.setRequisition(new Identifier().setSystem(FhirIdentifierConstants.TICKET_TYPE_SYSTEM_URL).setValue(referralDetailsDTO.getType()));
        }
        if (!Objects.isNull(referralDetailsDTO.getPatientReference())) {
            serviceRequest.setSubject(new Reference(referralDetailsDTO.getPatientReference()));
        }

        if (Boolean.TRUE.equals(referralDetailsDTO.isAutoReferral())) {
            serviceRequest.addNote(new Annotation().setText(Constants.AUTO_REFERRAL_TICKET)
                    .setTime(referralDetailsDTO.getProvenance().getModifiedDate()));
        }
        return serviceRequest;
    }

    /**
     * Saves observations with a list of string values.
     * This method constructs observation resources for each string value in the list
     * and sets them as components of the main observation.
     *
     * @param values The list of string values to be saved as observations.
     */
    public void saveObservationWithList(List<String> values) {
        Observation observation = new Observation();
        observation.setStatus(Observation.ObservationStatus.FINAL);
        // Add components for each string value
        for (String value : values) {
            Observation.ObservationComponentComponent component = new Observation.ObservationComponentComponent();
            component.setCode(new CodeableConcept().setText("Value"));
            component.setValue(new StringType(value));
            observation.addComponent(component);
        }
    }

    /*
     * Create Observation Component String and add.
     *
     * @param value      String value
     * @param question   Question value
     * @param components Observation Component
     */
    public void createObservationComponent(Long value, String unit,
                                           String question,
                                           List<ObservationComponentComponent> components) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            Quantity quantity = new Quantity(null, value, "http://unitsofmeasure.org", null, unit);
            observationComponent.setValue(quantity);
            components.add(observationComponent);
        }
    }

    /*
     * Create Observation Component String and add.
     *
     * @param value      String value
     * @param question   Question value
     * @param components Observation Component
     */
    public void createObservationComponent(Double value, String unit,
                                           String question,
                                           List<ObservationComponentComponent> components) {
        if (!Objects.isNull(value)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            Quantity quantity = new Quantity(null, value, "http://unitsofmeasure.org", null, unit);
            observationComponent.setValue(quantity);
            components.add(observationComponent);
        }
    }

    /**
     * Assessment to set medication request
     *
     * @param prescriptionRequest Assessment Details
     * @return FHIR MedicationRequest Object
     */
    public MedicationRequest mapMedicationRequest(PrescriptionDTO prescriptionRequest, MedicationRequest medicationRequest) {
        if (medicationRequest.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.MEDICATION_ID_SYSTEM_URL))) {
            medicationRequest.addIdentifier().setSystem(FhirIdentifierConstants.MEDICATION_ID_SYSTEM_URL)
                    .setValue(prescriptionRequest.getMedicationId());
            medicationRequest.getMedicationReference().setDisplay(prescriptionRequest.getMedicationName());
        }
        updateIdentifier(medicationRequest, Constants.PRESCRIBED);
        medicationRequest.setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE);
        medicationRequest.setAuthoredOn(prescriptionRequest.getPrescribedSince());
        Dosage dosage = new Dosage();
        Timing timing = new Timing();
        timing.getRepeat().setFrequency(prescriptionRequest.getFrequency());
        timing.getRepeat().setPeriod(prescriptionRequest.getPrescribedDays());
        dosage.setTiming(timing);
        dosage.setText(prescriptionRequest.getFrequencyName());
        setDosageDetails(dosage, prescriptionRequest);
        medicationRequest.addDosageInstruction(dosage);
        List<Dosage> dosages = new ArrayList<>();
        dosages.add(dosage);
        medicationRequest.setDosageInstruction(dosages);
        medicationRequest.getDispenseRequest().getValidityPeriod().setStart(prescriptionRequest.getPrescribedSince());
        medicationRequest.getDispenseRequest().getValidityPeriod().setEnd(DateUtil.addDate(DateUtil.subtractDates(DateUtil.getCurrentDay(),
                Constants.ONE), Math.toIntExact(prescriptionRequest.getPrescribedDays())));
        medicationRequest.setIntent(MedicationRequest.MedicationRequestIntent.PROPOSAL);
        CodeableConcept medicationCodeableConcept = new CodeableConcept();
        medicationCodeableConcept.setText(prescriptionRequest.getMedicationName());
        if (Objects.nonNull(prescriptionRequest.getCodeDetails())) {
            medicationCodeableConcept.addCoding()
                    .setSystem(prescriptionRequest.getCodeDetails().getUrl())
                    .setCode(prescriptionRequest.getCodeDetails().getCode())
                    .setDisplay(prescriptionRequest.getMedicationName());
        }
        medicationRequest.setMedication(medicationCodeableConcept);
        return medicationRequest;
    }

    /**
     * <p>Sets the medication dispense details using given details</p>
     *
     * @param prescriptionRequest prescription details
     * @param medicationRequest   FHIR medication request entity
     * @param encounterDetails    The prescription encounter details
     * @return FHIR MedicationDispense entity contains dispensed details
     */
    public MedicationDispense mapMedicationDispense(PrescriptionDTO prescriptionRequest,
                                                    MedicationRequest medicationRequest,
                                                    EncounterDetailsDTO encounterDetails) {
        MedicationDispense medicationDispense = new MedicationDispense();
        Optional<Identifier> identifierOpt = medicationRequest.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.MEDICATION_ID_SYSTEM_URL
                        .equals(identifier.getSystem()))
                .findFirst();
        if (identifierOpt.isPresent()) {
            medicationDispense.addIdentifier(identifierOpt.get());
        }
        medicationDispense.setWhenHandedOver(new Date());
        MedicationDispense.MedicationDispensePerformerComponent
                organizationComponent = new MedicationDispense.MedicationDispensePerformerComponent();
        organizationComponent.setActor(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                encounterDetails.getProvenance().getOrganizationId())));
        medicationDispense.setPerformer(List.of(organizationComponent));
        MedicationDispense.MedicationDispenseSubstitutionComponent
                performerComponent = new MedicationDispense.MedicationDispenseSubstitutionComponent();
        performerComponent.setResponsibleParty(
                List.of(new Reference(String.format(FhirConstants.PRACTITIONER_ID,
                        encounterDetails.getProvenance().getUserId()))));
        medicationDispense.setSubstitution(performerComponent);
        setMedicationDispenseIdentitifer(medicationDispense, medicationRequest, prescriptionRequest);
        medicationDispense.setStatus(MedicationDispense.MedicationDispenseStatus.COMPLETED);
        medicationDispense.setDosageInstruction(medicationRequest.getDosageInstruction());
        medicationDispense.setMedication(medicationRequest.getMedication());
        medicationDispense.setAuthorizingPrescription(List.of(new Reference(
                String.format(FhirConstants.MEDICATION_REQUEST_ID, medicationRequest.getIdPart()))));
        medicationDispense.setSubject(medicationRequest.getSubject());
        medicationDispense.setContext(new Reference(String.format(FhirConstants.ENCOUNTER_ID,
                encounterDetails.getPatientVisitId())));
        medicationDispense.setNote(List.of(new Annotation().setText(prescriptionRequest.getReason())));
        return medicationDispense;
    }

    /**
     * Assessment to set medication request
     *
     * @param prescriptionRequest Assessment Details
     * @return FHIR MedicationRequest Object
     */
    public MedicationRequest updateDispenseRequest(PrescriptionDTO prescriptionRequest,
                                                   MedicationRequest medicationRequest) {
        int prescripedDays =
                medicationRequest.getDosageInstructionFirstRep().getTiming().getRepeat().getPeriod().intValue();
        int filledDays = Constants.ZERO;
        Duration filledDuration = null;

        if (Objects.nonNull(medicationRequest.getDispenseRequest().getInitialFill())) {
            filledDuration = medicationRequest.getDispenseRequest().getInitialFill().getDuration();
        } else {
            MedicationRequest.MedicationRequestDispenseRequestInitialFillComponent
                    filledComponent = new MedicationRequest.MedicationRequestDispenseRequestInitialFillComponent();
            filledComponent.setDuration(new Duration());
            medicationRequest.getDispenseRequest().setInitialFill(filledComponent);
        }
        if (Objects.nonNull(filledDuration) && Objects.nonNull(filledDuration.getValue())) {
            filledDays = filledDuration.getValue().intValue();
        }
        filledDays = filledDays + prescriptionRequest.getPrescriptionFilledDays();
        if (Objects.nonNull(medicationRequest.getDispenseRequest()) &&
                prescripedDays >= filledDays) {
            Duration duration = new Duration();
            duration.setValue(prescriptionRequest.getPrescriptionFilledDays());
            medicationRequest.getDispenseRequest().setExpectedSupplyDuration(duration);
            medicationRequest.getDispenseRequest().getInitialFill().getDuration().
                    setValue(filledDays);
        } else {
            throw new DataNotAcceptableException(5001);
        }
        if (filledDays == prescripedDays) {
            medicationRequest.setStatus(MedicationRequest.MedicationRequestStatus.COMPLETED);
        }
        updateIdentifier(medicationRequest, Constants.PRESCRIPTION_DISPENSED);
        return medicationRequest;
    }

    private void updateIdentifier(MedicationRequest medicationRequest, String status) {
        Optional<Identifier> identifierOpt = medicationRequest.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL.equals(identifier.getSystem()))
                .findFirst();
        identifierOpt.ifPresentOrElse(
                identifier -> identifier.setValue(status),
                () -> {
                    Identifier newIdentifier = new Identifier();
                    newIdentifier.setSystem(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL);
                    newIdentifier.setValue(status);
                    medicationRequest.addIdentifier(newIdentifier);
                }
        );
    }

    /**
     * <p>
     * Sets FHIR dosage details based on given prescription details
     * </p>
     *
     * @param dosage              The Fhir Dosage entity.
     * @param prescriptionRequest The PrescriptionDTO entity.
     */
    private void setDosageDetails(Dosage dosage, PrescriptionDTO prescriptionRequest) {
        if (Objects.nonNull(prescriptionRequest.getInstructionNote())) {
            dosage.setPatientInstruction(prescriptionRequest.getInstructionNote());
        }
        if (Objects.nonNull(prescriptionRequest.getDosageFrequencyName())) {
            dosage.setAdditionalInstruction(List.of(new CodeableConcept()
                    .setText(prescriptionRequest.getDosageFrequencyName())));
        }
        Dosage.DosageDoseAndRateComponent dosageDoseAndRateComponent
                = new Dosage.DosageDoseAndRateComponent();
        if (Objects.nonNull(prescriptionRequest.getDosageUnitName()) &&
                Objects.nonNull(prescriptionRequest.getDosageUnitValue())) {
            Quantity quantity = new Quantity();
            quantity.setValue(Double.valueOf(prescriptionRequest.getDosageUnitValue()));
            quantity.setUnit(prescriptionRequest.getDosageUnitName());
            dosageDoseAndRateComponent.setDose(quantity);
        }
        if (Objects.nonNull(prescriptionRequest.getDosageFormName())) {
            dosageDoseAndRateComponent.setType(new CodeableConcept()
                    .setText(prescriptionRequest.getDosageFormName()));
        }
        if (!dosageDoseAndRateComponent.isEmpty()) {
            dosage.setDoseAndRate(List.of(dosageDoseAndRateComponent));
        }
    }

    /**
     * Create VITAL Observation for patient
     *
     * @param observationDto Observation Details
     * @param bundle         Bundle Value
     */
    public Boolean createValueObservation(ObservationDTO observationDto, Bundle bundle) {
        // Create a new bundle for transaction
        Bundle.HTTPVerb verb = Bundle.HTTPVerb.PUT;
        Observation observation;
        // Generate a unique identifier for the observation
        String id = fhirUtils.getUniqueId();
        String url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, id);

        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, url);
        List<String> types = new ArrayList<>();
        types.add(observationDto.getType());
        Bundle basicDetails = getPatientBasicDetails(observationDto.getEncounter().getMemberId(),
                types);
        if (basicDetails.getTotal() > Constants.ZERO) {
            observation = (Observation) basicDetails.getEntry().getFirst().getResource();
            id = fhirUtils.getIdFromHistoryUrl(observation.getId());
            url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH, id);
            if (Constants.BIRTH_HISTORY.equalsIgnoreCase(observationDto.getType())) {
                for (Identifier identifier : observation.getIdentifier()) {
                    if (identifier.getSystem().equals(FhirIdentifierConstants.TYPE_SYSTEM_URL)) {
                        if (identifier.getValue().equals(Constants.MEDICAL_REVIEW)) {
                            return Boolean.FALSE;
                        } else if (observationDto.getEncounter().getType().equalsIgnoreCase(Constants.MEDICAL_REVIEW) &&
                                identifier.getValue().equals(Constants.ASSESSMENT)) {
                            identifier.setValue(Constants.MEDICAL_REVIEW);
                        }
                    }
                }
            }
        } else {
            verb = Bundle.HTTPVerb.POST;
            observation = setObservation(observationDto.getEncounter(), observationDto.getType(),
                    Constants.PATIENT_VITAL_INFORMATION);
            observation.addIdentifier().setSystem(FhirIdentifierConstants.VITALS_TYPE_SYSTEM_URL).setValue(observationDto.getType());
            observation.addIdentifier().setSystem(FhirIdentifierConstants.TYPE_SYSTEM_URL).setValue(observationDto.getEncounter().getType());
        }
        setValuesToObservation(observationDto, observation);
        fhirUtils.setBundle(url, fullUrl, verb, observation, bundle);
        return Boolean.TRUE;
    }

    /**
     * Update Vital Observation status to finished
     *
     * @param types         type of the assessment
     * @param bundle        Bundle object
     * @param memberId      Member value
     * @param provenanceDTO Object
     */
    public void updateVitalObservationsStatus(List<String> types, Bundle bundle, String memberId,
                                              ProvenanceDTO provenanceDTO) {
        Bundle observationValues = getPatientBasicDetails(memberId, types);
        observationValues.getEntry().forEach(entry -> {
            Observation observation = (Observation) entry.getResource();
            observation.setStatus(Observation.ObservationStatus.FINAL);
            String id = observation.getIdPart();
            String url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH, id);
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, id);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.PUT, observation, bundle, provenanceDTO);
        });
    }

    /**
     * Set Values to the observation
     *
     * @param observationDto Observation Details
     * @param observation    observation
     */
    private void setValuesToObservation(ObservationDTO observationDto, Observation observation) {
        if (Constants.WEIGHT.equals(observationDto.getType()) || Constants.BIRTHWEIGHT.equals(observationDto.getType())) {
            observation.setValue(new Quantity(observationDto.getWeight()));
        } else if (Constants.BP.equals(observationDto.getType())) {
            setBpValues(observationDto, observation);
        } else if (Constants.OBSERVATION_DATE_FIELDS.contains(observationDto.getType())) {
            observation.getValueDateTimeType().setValue(observationDto.getDateValue());
        } else if (Constants.OBSERVATION_INTEGER_FIELDS.contains(observationDto.getType())) {
            observation.getValueIntegerType().setValue(observationDto.getNumberValue());
        } else if (Constants.OBSERVATION_STRING_FIELDS.contains(observationDto.getType())) {
            observation.getValueStringType().setValue(observationDto.getStringValue());
        } else if (Constants.OBSERVATION_BOOLEAN_FIELDS.contains(observationDto.getType())) {
            observation.getValueBooleanType().setValue(observationDto.getBooleanValue());
        }
        if (Constants.HEIGHT.equals(observationDto.getType())) {
            observation.setValue(new Quantity(observationDto.getHeight()));
        }
        if (Constants.BIRTH_HISTORY.equals(observationDto.getType())) {
            setBirthHistory(observationDto, observation);
        }
    }

    /**
     * Get Patient Basic Details
     *
     * @param memberId memberId
     * @param types    category
     * @return Observation
     */
    public Bundle getPatientBasicDetails(String memberId, List<String> types) {
        String url = String.format(Constants.PATIENT_BASIC_DETAILS_PARAMS,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL, Constants.PATIENT_VITAL_INFORMATION, String.join(Constants.COMMA, types),
                StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, memberId));
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Get Patient Basic Details
     *
     * @param patientReference patientReference
     * @param types            category
     * @return Observation
     */
    public Bundle getPatientBasicDetails(List<String> types, String patientReference) {
        String url = String.format(Constants.PATIENT_BASIC_DETAILS_PARAMS_USE_PATIENT,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL, Constants.PATIENT_VITAL_INFORMATION, String.join(Constants.COMMA, types),
                StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, patientReference));
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Set Bp values in Observation
     *
     * @param observationDTO ObservationDetails DTO
     * @param observation    observation
     */
    private void setBpValues(ObservationDTO observationDTO, Observation observation) {
        List<Observation.ObservationComponentComponent> components = new ArrayList<>();
        createObservationComponent(observationDTO.getSystolic(), Constants.SYSTOLIC, components, Constants.MMHG);
        createObservationComponent(observationDTO.getDiastolic(), Constants.DIASTOLIC, components, Constants.MMHG);
        createObservationComponent(observationDTO.getPulse(), Constants.PULSE, components, Constants.BPM);
        observation.setComponent(components);
    }

    /**
     * Set Bp values in Observation
     *
     * @param observationDTO ObservationDetails DTO
     * @param observation    observation
     */
    private void setBirthHistory(ObservationDTO observationDTO, Observation observation) {
        List<Observation.ObservationComponentComponent> components = new ArrayList<>();
        BirthHistoryDTO birthHistoryDTO = observationDTO.getBirthHistoryDTO();
        createObservationComponent(birthHistoryDTO.getBirthWeight(), Constants.BIRTHWEIGHT, components, Constants.KG);
        createObservationComponent(birthHistoryDTO.getBirthWeightCategory(), Constants.BIRTHWEIGHT_CATEGORY,
                components);
        createObservationComponent(birthHistoryDTO.getGestationalAge(), Constants.GESTATIONAL_AGE, components);
        createObservationComponent(birthHistoryDTO.getGestationalAgeCategory(), Constants.GESTATIONAL_AGE_CATEGORY,
                components);
        createObservationComponent(birthHistoryDTO.getHaveBreathingProblem(), Constants.HAVE_BREATHING_PROBLEMS,
                components);
        observation.setComponent(components);
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     * @param type                vital type
     * @param value               vital value
     * @param patient             patientId
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO, String type,
                                       int value, String patient) {
        if (Constants.ZERO < value) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(patient);
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(type);
            observationDTO.setNumberValue(value);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     * @param type                vital type
     * @param value               vital value
     * @param patient             patientId
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO, String type,
                                       Boolean value, String patient) {
        if (!Objects.isNull(value)) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(patient);
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(type);
            observationDTO.setBooleanValue(value);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     * @param type                vital type
     * @param value               vital value
     * @param patient             patientId
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO, String type,
                                       String value, String patient) {
        if (!Objects.isNull(value)) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(patient);
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(type);
            observationDTO.setStringValue(value);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     * @param type                vital type
     * @param value               vital value
     * @param patient             patientId
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO, String type,
                                       BirthHistoryDTO value, String patient) {
        if (!Objects.isNull(value)) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(patient);
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(type);
            observationDTO.setBirthHistoryDTO(value);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     * @param type                vital type
     * @param value               vital value
     * @param patient             patientId
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO, String type,
                                       Double value, String patient) {
        if (!Objects.isNull(value)) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(patient);
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(type);
            observationDTO.setWeight(value);
            observationDTO.setHeight(value);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO,
                                       Double sys, Double dis, Double pulse) {
        if (!Objects.isNull(sys) && !Objects.isNull(dis)) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(encounterDetailsDTO.getPatientReference());
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(Constants.BP);
            observationDTO.setSystolic(sys);
            observationDTO.setDiastolic(dis);
            observationDTO.setPulse(pulse);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Vital Observation For patient
     *
     * @param bundle              Bundle
     * @param encounterDetailsDTO encounterDetails
     * @param type                vital type
     * @param value               vital value
     * @param patient             patientId
     */
    public void createVitalObservation(Bundle bundle, EncounterDetailsDTO encounterDetailsDTO, String type,
                                       Date value, String patient) {
        if (!Objects.isNull(value)) {
            ObservationDTO observationDTO = new ObservationDTO();
            observationDTO.setPatientReference(patient);
            observationDTO.setEncounter(encounterDetailsDTO);
            observationDTO.setType(type);
            observationDTO.setDateValue(value);
            createValueObservation(observationDTO, bundle);
        }
    }

    /**
     * Create Observation Component String and add.
     *
     * @param question   Question value
     * @param components Observation Component
     */
    public void createObservationComponent(String question,
                                           List<ObservationComponentComponent> components) {
        if (!Objects.isNull(question)) {
            ObservationComponentComponent observationComponent = createObservationComponent(question);
            observationComponent.setCode(fhirUtils.createCodeableConcept(question));
            components.add(observationComponent);
        }
    }

    /**
     * Create Note Observation Component String and add.
     *
     * @param encounterDetailsDTO   details containing encounter
     * @param identifier            The identifier of the observation-type
     * @param code                  this code is to be added on observation
     * @param note                  the note to be added in the Observation Component
     * @param isCounselorAssessment this boolean is to differentiate between the clinical note type
     * @param noteObservationId     this will be used if isCounselorAssessment is true
     * @return {@link Observation} an observation entity has been constructed with the note added in it.
     */
    public Observation createNoteObservation(EncounterDetailsDTO encounterDetailsDTO, ProvenanceDTO provenanceDTO,
                                             String identifier, String code, String note, boolean isCounselorAssessment, Long noteObservationId) {
        Observation observation = new Observation();
        observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue(identifier);
        observation.setStatus(Observation.ObservationStatus.FINAL);
        observation.setIssued(new Date());
        observation.setEncounter(new Reference(encounterDetailsDTO.getId()));
        if (StringUtils.isNotBlank(note)) {
            observation.addNote(new Annotation().setText(note));
        }
        observation.setCode(fhirUtils.setCodes(code));
        if (isCounselorAssessment) {
            observation.setStatus(Observation.ObservationStatus.PRELIMINARY);
            if (Objects.nonNull(noteObservationId)) {
                observation.setDerivedFrom(List.of(new Reference(ResourceType.Observation.name().concat(Constants.FORWARD_SLASH)
                        .concat(String.valueOf(noteObservationId)))));
            }
        }
        if (!Objects.isNull(encounterDetailsDTO.getPatientReference())) {
            observation.setSubject(new Reference(encounterDetailsDTO.getPatientReference()));
        }
        if (!Objects.isNull(encounterDetailsDTO.getMemberId())) {
            // adding related person in performer if any.
            observation.addPerformer(new Reference(encounterDetailsDTO.getMemberId()));
        }
        if (!Objects.isNull(provenanceDTO) && StringUtil.isNotBlank(provenanceDTO.getUserId())) {
            observation.addPerformer(new Reference(ResourceType.Practitioner.name().concat(Constants.FORWARD_SLASH).concat(provenanceDTO.getUserId())));
        }
        return observation;
    }

    /**
     * <p>
     * Set medication dispense identifier details using given medication request
     * and prescription request
     * </p>
     *
     * @param medicationDispense FHIR medication dispense entity
     * @param medicationRequest  FHIR medication request entity
     * @param prescriptionDTO    PrescriptionDTO entity
     */
    public void setMedicationDispenseIdentitifer(MedicationDispense medicationDispense,
                                                 MedicationRequest medicationRequest,
                                                 PrescriptionDTO prescriptionDTO) {
        int prescripedDays = medicationRequest.getDosageInstructionFirstRep()
                .getTiming().getRepeat().getPeriod().intValue();
        int dispensedDays = Constants.ZERO;
        Duration dispensedDuration = null;
        if (Objects.nonNull(medicationRequest.getDispenseRequest().getInitialFill())) {
            dispensedDuration = medicationRequest.getDispenseRequest().getInitialFill()
                    .getDuration();
        }
        if (Objects.nonNull(dispensedDuration) && Objects.nonNull(dispensedDuration.getValue())) {
            dispensedDays = dispensedDuration.getValue().intValue();
        }
        medicationDispense.addIdentifier().setSystem(FhirIdentifierConstants.PRESCRIBED_ID_URL)
                .setValue(medicationRequest.getIdPart());
        medicationDispense.addIdentifier().setSystem(
                        FhirIdentifierConstants.PRESCRIBED_DAYS_URL)
                .setValue(String.valueOf(prescripedDays));
        medicationDispense.addIdentifier().setSystem(
                        FhirIdentifierConstants.PRESCRIPTION_FILLED_DAYS_URL)
                .setValue(String.valueOf(prescriptionDTO.getPrescriptionFilledDays()));
        medicationDispense.addIdentifier().setSystem(
                        FhirIdentifierConstants.PRESCRIPTION_ALL_READY_FILLED_DAYS_URL)
                .setValue(String.valueOf(dispensedDays));
    }
}

