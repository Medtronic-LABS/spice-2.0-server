package com.mdtlabs.coreplatform.fhirmapper.prescription.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * Implementation of the MedicationRequestService implementation.
 * This class provides methods to handle medical request-related operations.
 *
 * @author Yogeshwaran Mohan created on Apr 17, 2024
 */
@Service
public class PrescriptionRequestServiceImpl implements PrescriptionRequestService {

    private final PatientService patientService;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final FhirMapper fhirMapper;

    private final CommonConverter commonConverter;

    private final PatientConverter patientConverter;

    private final AdminServiceApiInterface adminServiceApiInterface;

    private final PatientVisitService patientVisitService;

    public PrescriptionRequestServiceImpl(PatientService patientService, FhirUtils fhirUtils, RestApiUtil restApiUtil,
                                          FhirAssessmentMapper fhirAssessmentMapper, FhirMapper fhirMapper, CommonConverter commonConverter,
                                          PatientConverter patientConverter, AdminServiceApiInterface adminServiceApiInterface,
                                          PatientVisitService patientVisitService) {
        this.patientService = patientService;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirMapper = fhirMapper;
        this.commonConverter = commonConverter;
        this.patientConverter = patientConverter;
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.patientVisitService = patientVisitService;
    }

    /**
     * Adds list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param prescriptionToSave The list of prescription review DTO containing the data for adding medication-request reference.
     * @param patientReference   patient_res_id from fhir
     * @param encounterDetails   encounter details
     * @param bundle             bundle object
     */

    public void createMedicationRequest(List<PrescriptionDTO> prescriptionToSave, String patientReference,
                                        EncounterDetailsDTO encounterDetails, Bundle bundle) {
        String signatureRef = fhirAssessmentMapper.createObservation(encounterDetails, Constants.SIGNATURE,
                Constants.PRESCRIPTION, encounterDetails.getSignature(), bundle);
        prescriptionToSave.forEach(prescriptionRequest -> {
            MedicationRequest medicationRequest = new MedicationRequest();
            medicationRequest.setSubject(new Reference(patientReference));
            String uuid = fhirUtils.getUniqueId();
            String url = StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH,
                    Constants.FHIR_BASE_URL, uuid);
            medicationRequest.setEncounter(new Reference(encounterDetails.getId()));
            medicationRequest = fhirAssessmentMapper.mapMedicationRequest(prescriptionRequest, medicationRequest);
            medicationRequest.setSupportingInformation(List.of(new Reference(signatureRef)));
            fhirUtils.setBundle(url, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                    Bundle.HTTPVerb.POST, medicationRequest, bundle, encounterDetails.getProvenance());
        });
    }

    /**
     * Adds list of prescriptionDTO in medicationRequest resource
     * This method constructs a resource based on prescription
     *
     * @param prescriptionToSave The list of prescription review DTO containing the data for
     *                           adding medication-request reference.
     * @param encounterDetails   encounter details
     * @param bundle             bundle object
     */

    public void createNcdMedicationRequest(List<PrescriptionDTO> prescriptionToSave,
                                           EncounterDetailsDTO encounterDetails, Bundle bundle) {
        String signatureRef = fhirAssessmentMapper.createObservation(encounterDetails, Constants.SIGNATURE,
                Constants.PRESCRIPTION, encounterDetails.getSignature(), bundle);
        prescriptionToSave.forEach(prescriptionRequest -> {
            MedicationRequest medicationRequest = new MedicationRequest();
            medicationRequest.setSubject(new Reference(encounterDetails.getPatientReference()));
            medicationRequest.setRequester(new Reference(String.format(FhirConstants.RELATED_PERSON_ID,
                    encounterDetails.getMemberId())));
            medicationRequest.setPerformer(new Reference(String.format(FhirConstants.PRACTITIONER_ID,
                    encounterDetails.getProvenance().getUserId())));
            medicationRequest.setReported(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    encounterDetails.getProvenance().getOrganizationId())));
            String uuid = fhirUtils.getUniqueId();
            String url = StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH,
                    Constants.FHIR_BASE_URL, uuid);
            medicationRequest.setEncounter(new Reference(String.format(FhirConstants.ENCOUNTER_ID, encounterDetails.getPatientVisitId())));
            medicationRequest = fhirAssessmentMapper.mapMedicationRequest(prescriptionRequest, medicationRequest);
            medicationRequest.setSupportingInformation(List.of(new Reference(signatureRef)));
            fhirUtils.setBundle(url, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                    Bundle.HTTPVerb.POST, medicationRequest, bundle, encounterDetails.getProvenance());
        });
    }

    /**
     * returns list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param requestDTO The request review DTO containing the data for adding medication-request reference.
     * @return List<PrescriptionRequestDTO> which is the result of the operation.
     */
    @Override
    public List<PrescriptionDTO> getPrescriptions(RequestDTO requestDTO) {
        Bundle prescriptionBundle = getPrescriptionBundle(requestDTO);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionBundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                prescriptionDTOList.add(prescriptionDTO);
            }
        });
        return prescriptionDTOList;
    }

    /**
     * Create or Updates a medicationRequest resource
     * This method constructs resource based on prescription
     *
     * @param prescriptionRequestDTO The prescription review DTO containing the data for adding medication-request reference.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    @Override
    public Map<String, String> createOrUpdateMedicationRequest(PrescriptionRequestDTO prescriptionRequestDTO) {
        List<PrescriptionDTO> prescriptionToSave = new ArrayList<>();
        Map<String, PrescriptionDTO> prescriptionToUpdate = new HashMap<>();
        List<PrescriptionDTO> prescriptions = Objects.nonNull(prescriptionRequestDTO.getPrescriptions()) ?
                prescriptionRequestDTO.getPrescriptions() : new ArrayList<>();
        if (prescriptions.isEmpty()) {
            throw new DataNotAcceptableException(1011);
        }
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        patientService.setPatientReferenceInEncounterDetails(prescriptionRequestDTO.getEncounter(), bundle);
        prescriptionRequestDTO.getEncounter().setSignature(prescriptionRequestDTO.getSignature());
        prescriptions.forEach(prescriptionDTO -> {
            if (Objects.isNull(prescriptionDTO.getPrescriptionId())) {
                prescriptionToSave.add(prescriptionDTO);
            } else {
                prescriptionToUpdate.put(prescriptionDTO.getPrescriptionId(), prescriptionDTO);
            }
        });
        createEncounter(prescriptionRequestDTO.getEncounter(), bundle);
        if (!prescriptionToSave.isEmpty()) {
            createMedicationRequest(prescriptionToSave, prescriptionRequestDTO.getEncounter().getPatientReference(), prescriptionRequestDTO.getEncounter(), bundle);
        }
        if (!prescriptionToUpdate.isEmpty()) {
            updateMedicationRequest(prescriptionToUpdate, prescriptionRequestDTO.getEncounter().getPatientReference(), prescriptionRequestDTO.getEncounter(), bundle);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, String> response = new HashMap<>();
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER))
                && !fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).isEmpty()
                ? fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).getFirst() :
                fhirUtils.getIdFromReference(prescriptionRequestDTO.getEncounter().getId()));
        response.put(Constants.KEY_PATIENT_REFERENCE, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                !fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() :
                fhirUtils.getIdFromReference(prescriptionRequestDTO.getEncounter().getPatientReference()));
        return response;
    }


    /**
     * Creates an Encounter.
     *
     * @param encounterDetails Encounter Details dto
     * @param bundle           bundle object
     */
    private void createEncounter(EncounterDetailsDTO encounterDetails, Bundle bundle) {
        encounterDetails.setPrescribed(Boolean.TRUE);
        if (StringUtils.isEmpty(encounterDetails.getId())) {
            encounterDetails.setStartTime(new Date());
            encounterDetails.setEndTime(new Date());
            encounterDetails.setId(fhirAssessmentMapper.createEncounter(encounterDetails, bundle, null, encounterDetails.getPatientVisitId()));
        } else {
            encounterDetails.setStartTime(null);
            encounterDetails.setEndTime(new Date());
            fhirAssessmentMapper.updateEncounter(encounterDetails, bundle, null, encounterDetails.getPatientVisitId());
            encounterDetails.setId(StringUtil.concatString(Constants.FHIR_RESOURCE_ENCOUNTER, Constants.FORWARD_SLASH, encounterDetails.getId()));
        }
    }

    /**
     * Adds list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param prescriptionToUpdate The list of prescription review DTO containing the data for adding medication-request reference.
     * @param patientReference     patient_res_id from fhir
     * @param encounterDetails     encounter details
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */

    private void updateMedicationRequest(Map<String, PrescriptionDTO> prescriptionToUpdate, String patientReference, EncounterDetailsDTO encounterDetails, Bundle bundle) {
        Bundle bundleRequest = getMedicationRequestById(prescriptionToUpdate.keySet());
        bundleRequest.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescription = prescriptionToUpdate.get(medicationRequest.getIdPart());
                medicationRequest.setSubject(new Reference(patientReference));
                medicationRequest.setEncounter(new Reference(encounterDetails.getId()));
                medicationRequest = fhirAssessmentMapper.mapMedicationRequest(prescription, medicationRequest);
                fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH, medicationRequest.getIdPart()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, medicationRequest.getIdPart()),
                        Bundle.HTTPVerb.PUT, medicationRequest, bundle, encounterDetails.getProvenance());
            }
        });
    }

    /**
     * Adds list of prescriptionDTO in medicationRequest resource
     * This method constructs a resource based on prescription
     *
     * @param prescriptionToUpdate The list of prescription review DTO containing the data for adding medication-request reference.
     * @param encounterDetails     encounter details
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    private void updateNcdMedicationRequest(Map<String, PrescriptionDTO> prescriptionToUpdate, EncounterDetailsDTO encounterDetails, Bundle bundle) {
        Bundle bundleRequest = getMedicationRequestById(prescriptionToUpdate.keySet());
        bundleRequest.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescription = prescriptionToUpdate.get(medicationRequest.getIdPart());
                medicationRequest.setSubject(new Reference(encounterDetails.getPatientReference()));
                medicationRequest.setRequester(new Reference(String.format(FhirConstants.RELATED_PERSON_ID,
                        encounterDetails.getMemberId())));
                medicationRequest.setPerformer(new Reference(String.format(FhirConstants.PRACTITIONER_ID,
                        encounterDetails.getProvenance().getUserId())));
                medicationRequest.setReported(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                        encounterDetails.getProvenance().getOrganizationId())));
                medicationRequest.setEncounter(new Reference(String.format(FhirConstants.ENCOUNTER_ID, encounterDetails.getPatientVisitId())));
                medicationRequest = fhirAssessmentMapper.mapMedicationRequest(prescription, medicationRequest);
                setInitialDispenseRequest(medicationRequest);
                fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH, medicationRequest.getIdPart()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, medicationRequest.getIdPart()),
                        Bundle.HTTPVerb.PUT, medicationRequest, bundle, encounterDetails.getProvenance());
            }
        });
    }

    /**
     * <p>
     * Sets initial dispense of Medication request while updating prescription
     * </p>
     *
     * @param medicationRequest The Fhir MedicationRequest entity
     */
    private void setInitialDispenseRequest(MedicationRequest medicationRequest) {
        if (Objects.nonNull(medicationRequest.getDispenseRequest())) {
            Duration duration = new Duration();
            duration.setValue(Constants.ZERO);
            medicationRequest.getDispenseRequest().setExpectedSupplyDuration(duration);
            medicationRequest.getDispenseRequest().getInitialFill().getDuration().
                    setValue(Constants.ZERO);
        }
    }

    /**
     * returns bundle of  medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param requestDTO The prescription review DTO containing the data for adding medication-request reference.
     * @return Bundle which is the result of the operation.
     */
    private Bundle getPrescriptionBundle(RequestDTO requestDTO) {
        String url = String.format(Constants.PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference(),
                Boolean.FALSE.equals(requestDTO.getIsActive()) ? Constants.COMPLETED_SMALLER_CASE : Constants.ACTIVE);
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * returns bundle of  medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param requestDTO The prescription review DTO containing the data for adding medication-request reference.
     * @return Bundle which is the result of the operation.
     */
    private Bundle getNcdPrescriptionBundle(RequestDTO requestDTO) {
        String url = null;

        if (Boolean.TRUE.equals(requestDTO.getIsActive())) {
            url = String.format(Constants.ACTIVE_PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference());
        } else {
            url = String.format(Constants.PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference(),
                    MedicationRequest.MedicationRequestStatus.CANCELLED.toString().toLowerCase());
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Get Patient by Identifier without offset
     *
     * @param prescriptionResIds
     * @return Bundle
     */
    private Bundle getMedicationRequestById(Set<String> prescriptionResIds) {
        String url = String.format(Constants.GET_PRESCRIPTION_PARAMS, String.join(Constants.COMMA, prescriptionResIds));
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Get encounter history of patient.
     *
     * @param request
     * @return List<Map < String, Object>>
     */
    private List<Map<String, Object>> getEncounterHistory(RequestDTO request) {
        List<Map<String, Object>> encounterList = new ArrayList<>();
        String url = String.format(Constants.GET_PRESCRIPTION_ENCOUNTER_QUERY, request.getPatientReference(),
                StringUtil.concatString(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL, Constants.VERTICAL_BAR, Constants.PRESCRIBED));
        Bundle bundle = restApiUtil.getBatchRequest(url);
        bundle.getEntry().stream().forEach(resource -> {
            Encounter encounter = (Encounter) resource.getResource();
            encounterList.add(Map.of(Constants.ID, encounter.getIdPart(), Constants.DATE, encounter.getPeriod().getStart()));
        });
        return encounterList;
    }

    /**
     * <p>
     * Get encounter history of patient.
     * </p>
     *
     * @param request
     * @return List<Map < String, Object>>
     */
    private List<Map<String, Object>> getNcdEncounterHistory(RequestDTO request) {
        List<Map<String, Object>> encounterList = new ArrayList<>();
        String url = String.format(Constants.GET_PRESCRIPTION_VISIT_ENCOUNTER_QUERY, request.getPatientReference(),
                StringUtil.concatString(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL, Constants.VERTICAL_BAR,
                        Constants.PRESCRIBED));
        Bundle bundle = restApiUtil.getBatchRequest(url);
        bundle.getEntry().stream().forEach(resource -> {
            Encounter encounter = (Encounter) resource.getResource();
            if (checkVisitEncounterOrNot(encounter)) {
                encounterList.add(Map.of(Constants.ID, encounter.getIdPart(),
                        Constants.DATE, encounter.getPeriod().getStart()));
            }
        });
        return encounterList;
    }

    /**
     * <p> Checks whether the given encounter is
     * medical review visit encounter or not using identifier
     * </p>
     *
     * @param encounter FHIR Encounter entity
     * @return true if encounter is medical review visit encounter
     */
    private boolean checkVisitEncounterOrNot(Encounter encounter) {
        String[] status = {Constants.EMPTY_STRING};
        encounter.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL.equals(identifier.getSystem()))
                .map(Identifier::getValue)
                .findFirst()
                .ifPresent(value -> status[Constants.ZERO] = value);
        return status[Constants.ZERO].equals(Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE);
    }

    /**
     * {@inheritDoc}
     */
    public PrescriptionHistoryDTO getPrescribedDetails(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference())) {
            throw new SpiceValidation(2003);
        }
        if (Objects.nonNull(request.getEncounterId())) {
            return getPrescriptionHistoryByEncounter(request);
        } else {
            List<Map<String, Object>> encounters = getEncounterHistory(request);
            if (encounters.isEmpty()) {
                return new PrescriptionHistoryDTO();
            } else {
                String encounterId = (String) encounters.get(0).get(Constants.ID);
                request.setEncounterId(encounterId);
                PrescriptionHistoryDTO prescriptionHistory = getPrescriptionHistoryByEncounter(request);
                prescriptionHistory.setHistory(encounters);
                return prescriptionHistory;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public PrescriptionHistoryDTO getNcdPrescribedDetails(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference())) {
            throw new SpiceValidation(2003);
        }
        if (Objects.nonNull(request.getPatientVisitId())) {
            return getPrescriptionHistoryByVisitId(request);
        } else {
            List<Map<String, Object>> encounters = getNcdEncounterHistory(request);
            if (encounters.isEmpty()) {
                return new PrescriptionHistoryDTO();
            } else {
                String encounterId = (String) encounters.get(0).get(Constants.ID);
                request.setPatientVisitId(encounterId);
                PrescriptionHistoryDTO prescriptionHistory = getPrescriptionHistoryByVisitId(request);
                prescriptionHistory.setHistory(encounters);
                return prescriptionHistory;
            }
        }
    }

    /**
     * Get prescription history by encounter
     *
     * @param requestDTO
     * @return PrescriptionHistoryDTO
     */
    private PrescriptionHistoryDTO getPrescriptionHistoryByEncounter(RequestDTO requestDTO) {
        PrescriptionHistoryDTO prescriptionHistory = new PrescriptionHistoryDTO();
        String encounterUrl = String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, requestDTO.getEncounterId());
        Bundle encounterBundle = restApiUtil.getBatchRequest(encounterUrl);
        encounterBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof Encounter encounter) {
                prescriptionHistory.setEncounterId(encounter.getIdPart());
                prescriptionHistory.setDateOfReview(encounter.getPeriod().getStart());
                prescriptionHistory.setPatientReference(encounter.getSubject().getReferenceElement().getIdPart());
            }
        });
        String prescriptionUrl = String.format(Constants.PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference(),
                String.join(Constants.COMMA, List.of(Constants.COMPLETED_SMALLER_CASE, Constants.ACTIVE)));
        Bundle prescriptionBundle = restApiUtil.getBatchRequest(prescriptionUrl);
        List<String> medicationIds = prescriptionBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(MedicationRequest.class::isInstance)
                .map(Resource::getIdPart)
                .toList();
        prescriptionHistory.setPrescriptions(getPrescriptionHistoryByMedicationIds(medicationIds, requestDTO.getEncounterId()));
        return prescriptionHistory;
    }

    /**
     * Get prescription history by medication ids
     *
     * @param medicationIds
     * @param encounterId
     * @return List of PrescriptionDTO
     */
    private List<PrescriptionDTO> getPrescriptionHistoryByMedicationIds(List<String> medicationIds, String encounterId) {
        List<PrescriptionDTO> prescriptions = new ArrayList<>();
        for (String medicationId : medicationIds) {
            String prescriptionHistoryUrl = String.format(Constants.MEDICATION_REQUEST_HISTORY, medicationId);
            Bundle prescriptionHistoryBundle = restApiUtil.getBatchRequest(prescriptionHistoryUrl);
            MedicationRequest latestMedication = (MedicationRequest) prescriptionHistoryBundle.getEntry().stream()
                    .map(Bundle.BundleEntryComponent::getResource)
                    .filter(MedicationRequest.class::isInstance)
                    .filter(medicationRequest -> StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                            Constants.FORWARD_SLASH, encounterId).equals(((MedicationRequest) medicationRequest)
                            .getEncounter().getReference())).findFirst().orElse(null);
            if (Objects.nonNull(latestMedication)) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(latestMedication);
                prescriptionDTO.setPrescriptionId(latestMedication.getIdPart());
                prescriptions.add(prescriptionDTO);
            }
        }
        return prescriptions;
    }

    /**
     * <p>
     * Gets prescription history using prescription ids
     * </p>
     *
     * @param prescriptionIds it contains list of prescription ids
     * @param encounterId     it contains the encounter id
     * @return The List of PrescriptionDTO entity
     */
    private List<PrescriptionDTO> getPrescriptionHistoryByPrescriptionIds(List<String> prescriptionIds,
                                                                          String encounterId) {
        List<MedicationRequest> medicationRequests = new ArrayList<>();
        List<PrescriptionDTO> prescriptions = new ArrayList<>();
        for (String prescriptionId : prescriptionIds) {
            String prescriptionHistoryUrl = String.format(Constants.MEDICATION_REQUEST_HISTORY, prescriptionId);
            Bundle prescriptionHistoryBundle = restApiUtil.getBatchRequest(prescriptionHistoryUrl);
            filterByEncounterIds(prescriptionHistoryBundle, encounterId, medicationRequests);
        }
        medicationRequests.sort(Comparator.comparing(medicationRequest
                -> medicationRequest.getMeta().getLastUpdated()));
        medicationRequests.forEach(medicationRequest -> {
            PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
            prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
            prescriptions.add(prescriptionDTO);
        });
        return prescriptions;
    }

    /**
     * <p>
     * Set and Filter Fhir Medication Request using encounter ids
     * </p>
     *
     * @param prescriptionHistoryBundle Fhir medication request history bundle
     * @param visitId                   it contains list of encounter ids
     * @param medicationRequests        it contains list of Medication requests
     */
    private void filterByEncounterIds(Bundle prescriptionHistoryBundle, String visitId,
                                      List<MedicationRequest> medicationRequests) {
        MedicationRequest prescription = prescriptionHistoryBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(MedicationRequest.class::isInstance)
                .map(MedicationRequest.class::cast)
                .filter(medicationRequest -> {
                    String encounterReference = medicationRequest.getEncounter().getReference();
                    if (checkIsPrescribed(medicationRequest) &&
                            encounterReference != null && encounterReference.contains(Constants.FORWARD_SLASH)) {
                        String encounterId = encounterReference.split(Constants.FORWARD_SLASH)[Constants.ONE];
                        return visitId.equals(encounterId);
                    }
                    return false;
                }).findFirst().orElse(null);
        if (Objects.nonNull(prescription)) {
            medicationRequests.add(prescription);
        }
    }

    /**
     * {@inheritDoc}
     */
    public List<PrescriptionDTO> getPrescriptionsByEncounter(String encounterId, String patientReference) {
        String prescriptionUrl = String.format(Constants.PRESCRIPTION_LIST_PARAMS, patientReference,
                String.join(Constants.COMMA, List.of(Constants.COMPLETED_SMALLER_CASE, Constants.ACTIVE)));
        Bundle prescriptionBundle = restApiUtil.getBatchRequest(prescriptionUrl);
        List<String> medicationIds = prescriptionBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(MedicationRequest.class::isInstance)
                .map(Resource::getIdPart)
                .toList();
        return getPrescriptionHistoryByMedicationIds(medicationIds, encounterId);
    }

    /**
     * {@inheritDoc}
     */
    public List<PrescriptionDTO> getPrescriptionHistory(RequestDTO request) {
        String url = String.format(Constants.MEDICATION_REQUEST_HISTORY, request.getPrescriptionId());
        Bundle prescriptionBundle = restApiUtil.getBatchRequest(url);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionBundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                prescriptionDTOList.add(prescriptionDTO);
            }
        });
        return prescriptionDTOList;
    }

    /**
     * {@inheritDoc}
     */
    public List<PrescriptionDTO> getNcdPrescriptionHistory(RequestDTO request) {
        String url = String.format(Constants.MEDICATION_REQUEST_HISTORY, request.getPrescriptionId());
        Bundle prescriptionBundle = restApiUtil.getBatchRequest(url);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionBundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof MedicationRequest medicationRequest && checkIsPrescribed(medicationRequest) &&
                    !medicationRequest.getStatus().equals(MedicationRequest.MedicationRequestStatus.CANCELLED)) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                prescriptionDTOList.add(prescriptionDTO);
            }
        });
        return prescriptionDTOList;
    }

    /**
     * <p> Checks whether the given medication request is
     * prescribed or not based on identifier
     * </p>
     *
     * @param medicationRequest FHIR MedicationRequest entity
     * @return true if medication request is prescribed
     */
    private boolean checkIsPrescribed(MedicationRequest medicationRequest) {
        String[] status = {Constants.EMPTY_STRING};
        medicationRequest.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL.equals(identifier.getSystem()))
                .map(Identifier::getValue)
                .findFirst()
                .ifPresent(value -> status[Constants.ZERO] = value);
        return status[Constants.ZERO].equals(Constants.PRESCRIBED);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removePrescription(PrescriptionRequestDTO prescriptionRequestDTO,
                                   MedicationRequest.MedicationRequestStatus status) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Bundle medicationBundle = getMedicationRequestById(Set.of(prescriptionRequestDTO.getPrescriptionId()));
        medicationBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof MedicationRequest medicationRequest) {
                medicationRequest.setStatus(status);
                Annotation annotation = new Annotation();
                annotation.setText(prescriptionRequestDTO.getDiscontinuedReason());
                annotation.setTime(new Date());
                medicationRequest.addNote(annotation);
                fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH, medicationRequest.getIdPart()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, medicationRequest.getIdPart()),
                        Bundle.HTTPVerb.PUT, medicationRequest, bundle, prescriptionRequestDTO.getProvenance());
                restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, String> createOrUpdateNcdPrescription(PrescriptionRequestDTO prescriptionRequestDTO) {
        List<PrescriptionDTO> prescriptionToSave = new ArrayList<>();
        Map<String, PrescriptionDTO> prescriptionToUpdate = new HashMap<>();
        List<PrescriptionDTO> prescriptions = Objects.nonNull(prescriptionRequestDTO.getPrescriptions()) ?
                prescriptionRequestDTO.getPrescriptions() : new ArrayList<>();
        if (prescriptions.isEmpty()) {
            throw new DataNotAcceptableException(1011);
        }
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        setPatientReferenceInEncounter(prescriptionRequestDTO, bundle);
        prescriptionRequestDTO.getEncounter().setSignature(prescriptionRequestDTO.getSignature());
        prescriptions.forEach(prescriptionDTO -> {
            if (prescriptionDTO.getPrescribedDays() < Constants.ZERO) {
                throw new DataNotAcceptableException(5002);
            }
            if (Objects.isNull(prescriptionDTO.getPrescribedSince())) {
                prescriptionDTO.setPrescribedSince(new Date());
            }
            if (Objects.isNull(prescriptionDTO.getPrescriptionId())) {
                prescriptionToSave.add(prescriptionDTO);
            } else {
                prescriptionToUpdate.put(prescriptionDTO.getPrescriptionId(), prescriptionDTO);
            }
        });
        String visitId = prescriptionRequestDTO.getEncounter().getPatientVisitId();
        String patientReference = prescriptionRequestDTO.getEncounter().
                getPatientReference().split(Constants.FORWARD_SLASH)[Constants.ONE];
        if (!prescriptionToSave.isEmpty()) {
            createNcdMedicationRequest(prescriptionToSave, prescriptionRequestDTO.getEncounter(), bundle);
        }
        if (!prescriptionToUpdate.isEmpty()) {
            updateNcdMedicationRequest(prescriptionToUpdate, prescriptionRequestDTO.getEncounter(), bundle);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Bundle transactionBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Encounter encounter = patientVisitService.updatePatientVisitStatus(visitId, true, false, false, patientReference);
        fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH, visitId),
                StringUtil.concatString(Constants.FHIR_BASE_URL, visitId),
                Bundle.HTTPVerb.PUT, encounter, transactionBundle, prescriptionRequestDTO.getProvenance());
        ResponseEntity<FhirResponseDTO> responseData = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transactionBundle));
        if (Objects.isNull(responseData.getBody())) {
            throw new Validation(1006);
        }
        Map<String, String> response = new HashMap<>();
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER))
                && !fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).isEmpty()
                ? fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).getFirst() :
                fhirUtils.getIdFromReference(prescriptionRequestDTO.getEncounter().getId()));
        response.put(Constants.KEY_PATIENT_REFERENCE, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                !fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() :
                fhirUtils.getIdFromReference(prescriptionRequestDTO.getEncounter().getPatientReference()));
        return response;
    }

    /**
     * <p> Set patient reference in medication request bundle</p>
     *
     * @param prescriptionRequestDTO The request review DTO containing
     *                               the data for adding medication-request reference.
     * @param bundle                 The FHIR bundle entity
     */
    private void setPatientReferenceInEncounter(PrescriptionRequestDTO prescriptionRequestDTO, Bundle bundle) {
        String url = String.format(Constants.GET_MEMBER_ID, prescriptionRequestDTO.getEncounter().getMemberId());
        Bundle resultBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) resultBundle.getEntry().getFirst().getResource();
        Patient patient = null;
        if (Objects.nonNull(relatedPerson.getPatient()) && Objects.nonNull(relatedPerson.getPatient().getReference())) {
            prescriptionRequestDTO.getEncounter().setPatientReference(
                    String.format(FhirConstants.PATIENT_ID,
                            relatedPerson.getPatient().getReference().split(Constants.FORWARD_SLASH)[Constants.ONE]));
        } else if (Objects.isNull(prescriptionRequestDTO.getEncounter().getPatientReference())) {
            ScreeningLogRequestDTO screeningLogRequestDTO = commonConverter.setPatientDetails(relatedPerson,
                    prescriptionRequestDTO.getEncounter().getProvenance().getOrganizationId());
            patient = patientConverter.createPatient(patient, screeningLogRequestDTO.getBioData(),
                    screeningLogRequestDTO.getBioMetrics(), relatedPerson.getBirthDate());
            patient.addIdentifier()
                    .setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                    .setValue(Constants.MEDICAL_REVIEW.toLowerCase());
            patient.setManagingOrganization(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    prescriptionRequestDTO.getEncounter().getProvenance().getOrganizationId())));
            prescriptionRequestDTO.getEncounter().setPatientReference(FhirConstants.PATIENT_IDENTIFIER_URL);
            commonConverter.setPatientLinkComponentReference(patient, relatedPerson);
            String uuid = fhirUtils.getUniqueId();
            String id = StringUtil.concatString(Constants.PATIENT,
                    Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
            relatedPerson.setPatient(new Reference(id));
            setPatientReferenceInPatientVisit(prescriptionRequestDTO.getEncounter().getPatientVisitId(),
                    id, bundle, prescriptionRequestDTO.getEncounter().getProvenance());
            fhirUtils.setBundle(id, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                    Bundle.HTTPVerb.POST, patient, bundle, prescriptionRequestDTO.getEncounter().getProvenance());
            fhirUtils.setBundle(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, relatedPerson.getIdPart()),
                    Bundle.HTTPVerb.PUT, relatedPerson, bundle, prescriptionRequestDTO.getEncounter().getProvenance());
            prescriptionRequestDTO.getEncounter().setPatientReference(id);
        } else {
            prescriptionRequestDTO.getEncounter().setPatientReference(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                    Constants.FORWARD_SLASH,
                    prescriptionRequestDTO.getEncounter().getPatientReference()));
        }
    }

    /**
     * <p> Set patient reference in patient visit encounter</p>
     *
     * @param patientVisitId The visit id of the patient
     * @param patientId      The id of the patient
     * @param bundle         The FHIR bundle entity
     * @param provenanceDTO  The ProvenanceDTO entity
     */
    private void setPatientReferenceInPatientVisit(String patientVisitId, String patientId,
                                                   Bundle bundle, ProvenanceDTO provenanceDTO) {
        String url = String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, patientVisitId);
        Bundle resultBundle = restApiUtil.getBatchRequest(url);
        Encounter encounter = (Encounter) resultBundle.getEntry().getFirst().getResource();
        encounter.setSubject(new Reference(patientId));
        fhirUtils.setBundle(String.format(FhirConstants.ENCOUNTER_ID, encounter.getIdPart()),
                StringUtil.concatString(Constants.FHIR_BASE_URL, encounter.getIdPart()),
                Bundle.HTTPVerb.PUT, encounter, bundle, provenanceDTO);
    }

    /**
     * returns list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param requestDTO The request review DTO containing the data for adding medication-request reference.
     * @return List<PrescriptionRequestDTO> which is the result of the operation.
     */
    @Override
    public List<PrescriptionDTO> getNcdPrescriptions(RequestDTO requestDTO) {
        Bundle prescriptionBundle = getNcdPrescriptionBundle(requestDTO);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionBundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                prescriptionDTOList.add(prescriptionDTO);
            }
        });
        setMedicationDetails(prescriptionDTOList);
        return prescriptionDTOList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PrescriptionDTO> listDispensePrescription(RequestDTO requestDTO) {
        requestDTO.setIsActive(Boolean.TRUE);
        Bundle prescriptionBundle = getPrescriptionBundle(requestDTO);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionBundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                prescriptionDTOList.add(prescriptionDTO);
            }
        });
        setMedicationDetails(prescriptionDTOList);
        return prescriptionDTOList;
    }

    /**
     * Create or Updates a medicationRequest resource
     * This method constructs resource based on prescription
     *
     * @param prescriptionRequestDTO The prescription review DTO containing the data for adding medication-request reference.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    @Override
    public Map<String, String> updateDispensePrescription(PrescriptionRequestDTO prescriptionRequestDTO) {
        Map<String, PrescriptionDTO> prescriptionToDispense = new HashMap<>();
        List<PrescriptionDTO> prescriptions = Objects.nonNull(prescriptionRequestDTO.getPrescriptions()) ?
                prescriptionRequestDTO.getPrescriptions() : new ArrayList<>();
        if (prescriptions.isEmpty()) {
            throw new DataNotAcceptableException(1011);
        }
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        patientService.setPatientReferenceInEncounterDetails(prescriptionRequestDTO.getEncounter(), bundle);
        prescriptionRequestDTO.getEncounter().setDispensed(Boolean.TRUE);
        prescriptions.forEach(prescriptionDTO -> {
            if (Objects.nonNull(prescriptionDTO.getPrescriptionId())) {
                prescriptionToDispense.put(prescriptionDTO.getPrescriptionId(), prescriptionDTO);
            }
        });
        if (!prescriptionToDispense.isEmpty()) {
            updateDispenseRequest(prescriptionToDispense, prescriptionRequestDTO.getEncounter(), bundle);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, String> response = new HashMap<>();
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER))
                && !fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).isEmpty()
                ? fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).getFirst() :
                fhirUtils.getIdFromReference(prescriptionRequestDTO.getEncounter().getId()));
        response.put(Constants.KEY_PATIENT_REFERENCE, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                !fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() :
                fhirUtils.getIdFromReference(prescriptionRequestDTO.getEncounter().getPatientReference()));
        return response;
    }

    /**
     * Adds list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param prescriptionToDispense The list of prescription review DTO containing the data for adding medication-request reference.
     * @param encounterDetails       encounter details
     */
    private void updateDispenseRequest(Map<String, PrescriptionDTO> prescriptionToDispense,
                                       EncounterDetailsDTO encounterDetails, Bundle bundle) {
        Bundle bundleRequest = getMedicationRequestById(prescriptionToDispense.keySet());
        bundleRequest.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescription = prescriptionToDispense.get(medicationRequest.getIdPart());
                medicationRequest = fhirAssessmentMapper.updateDispenseRequest(prescription, medicationRequest);
                medicationRequest.getDispenseRequest().setPerformer(new Reference(String.format(
                        FhirConstants.ORGANIZATION_ID, encounterDetails.getProvenance().getOrganizationId())));
                medicationRequest.setRecorder(new Reference(String.format(FhirConstants.PRACTITIONER_ID,
                        encounterDetails.getProvenance().getUserId())));
                if (Objects.nonNull(encounterDetails.getPatientVisitId())) {
                    medicationRequest.setEncounter(new Reference(String.format(FhirConstants.ENCOUNTER_ID,
                            encounterDetails.getPatientVisitId())));
                }
                MedicationDispense medicationDispense = fhirAssessmentMapper.mapMedicationDispense(prescription, medicationRequest, encounterDetails);
                fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH, medicationRequest.getIdPart()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, medicationRequest.getIdPart()),
                        Bundle.HTTPVerb.PUT, medicationRequest, bundle, encounterDetails.getProvenance());
                String uuid = fhirUtils.getUniqueId();
                String url = StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_DISPENSE, Constants.FORWARD_SLASH,
                        Constants.FHIR_BASE_URL, uuid);
                fhirUtils.setBundle(url, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                        Bundle.HTTPVerb.POST, medicationDispense, bundle, encounterDetails.getProvenance());
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    public List<PrescriptionDTO> getDispenseHistory(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference()) ||
                Objects.isNull(request.getPatientVisitId())) {
            throw new SpiceValidation(2003);
        }
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        String dispenseHistoryUrl = String.format(Constants.PRESCRIPTION_DISPENSE_HISTORY,
                ResourceType.Encounter.name().concat(Constants.FORWARD_SLASH).concat(request.getPatientVisitId()));
        Bundle dispenseHistoryBundle = restApiUtil.getBatchRequest(dispenseHistoryUrl);
        dispenseHistoryBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof MedicationDispense medicationDispense) {
                prescriptionDTOList.add(fhirMapper.mapDispensePrescriptionDTO(medicationDispense));
            }
        });
        setMedicationDetails(prescriptionDTOList);
        return prescriptionDTOList;
    }

    /**
     * <p>
     * Gets prescription history of particular patient using patient visit id
     * and patient id.
     * </p>
     *
     * @param request it contains patient visit id and patient id
     * @return The PrescriptionHistoryDTO entity
     */
    private PrescriptionHistoryDTO getPrescriptionHistoryByVisitId(RequestDTO request) {
        PrescriptionHistoryDTO prescriptionHistoryDTO = new PrescriptionHistoryDTO();
        String encounterId = null;
        if (Objects.isNull(request.getPatientReference()) ||
                Objects.isNull(request.getPatientVisitId())) {
            throw new SpiceValidation(2003);
        }
        String prescribedEncountersUrl = String.format(Constants.PRESCRIPTION_DISPENSE_ENCOUNTER_LIST_QUERY,
                request.getPatientVisitId(), request.getPatientReference(),
                FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL
                        + Constants.VERTICAL_BAR + Constants.PRESCRIBED);
        Bundle encounterBundle = restApiUtil.getBatchRequest(prescribedEncountersUrl);
        if (!CollectionUtils.isEmpty(encounterBundle.getEntry())) {
            encounterId = encounterBundle.getEntry().getFirst().getResource().getIdPart();
        }
        if (Objects.isNull(encounterId)) {
            return prescriptionHistoryDTO;
        }
        String prescribedHistoryUrl = String.format(Constants.GET_PRESCRIPTION_BY_PATIENT_AND_ENCOUNTER,
                request.getPatientReference());
        Bundle prescribeHistoryBundle = restApiUtil.getBatchRequest(prescribedHistoryUrl);
        List<String> medicationIds = prescribeHistoryBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(MedicationRequest.class::isInstance)
                .map(Resource::getIdPart)
                .toList();
        List<PrescriptionDTO> prescriptionDTOList = getPrescriptionHistoryByPrescriptionIds(medicationIds, encounterId);
        setMedicationDetails(prescriptionDTOList);
        prescriptionHistoryDTO.setPrescriptions(prescriptionDTOList);
        prescriptionHistoryDTO.setPatientReference(request.getPatientReference());
        prescriptionHistoryDTO.setEncounterId(request.getPatientVisitId());
        return prescriptionHistoryDTO;
    }

    /**
     * <p>
     * This method used to set medication details to appropriate prescriptionDTO entity
     * </p>
     *
     * @param prescriptionDTOList The list of prescriptionDTO entity
     */
    public void setMedicationDetails(List<PrescriptionDTO> prescriptionDTOList) {
        Map<Long, MedicationDTO> map = this.getMedicationDetails(prescriptionDTOList);
        for (PrescriptionDTO prescriptionDTO : prescriptionDTOList) {
            MedicationDTO medicationDTO = map.get(Long.valueOf(prescriptionDTO.getMedicationId()));
            prescriptionDTO.setClassificationName(medicationDTO.getClassificationName());
            prescriptionDTO.setBrandName(medicationDTO.getBrandName());
        }
    }

    /**
     * <p>
     * This method used to get medication details for provided prescriptionDTO list
     * </p>
     *
     * @param prescriptionDTOList The list of prescriptionDTO entity
     */
    public Map<Long, MedicationDTO> getMedicationDetails(List<PrescriptionDTO> prescriptionDTOList) {
        List<Long> medicationIds = prescriptionDTOList.stream()
                .map(prescriptionDTO -> Long.valueOf(prescriptionDTO.getMedicationId()))
                .toList();
        List<MedicationDTO> medicationDTOList = adminServiceApiInterface
                .getAllMedicationByIds(CommonUtil.getAuthToken(), CommonUtil.getClient(), medicationIds);
        return medicationDTOList.stream()
                .collect(Collectors.toMap(
                        MedicationDTO::getId,
                        medicationDTO -> medicationDTO
                ));
    }

    /**
     * {@inheritDoc}
     */
    public Integer getPrescriptionCount(RequestDTO request) {
        request.setIsActive(Boolean.TRUE);
        Bundle bundle = getNcdPrescriptionBundle(request);
        int count = Constants.ZERO;
        for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
            if (component.getResource() instanceof MedicationRequest medicationRequest && medicationRequest.hasDispenseRequest() &&
                    medicationRequest.getDispenseRequest().hasValidityPeriod()) {
                Date endDate = medicationRequest.getDispenseRequest().getValidityPeriod().getEnd();
                if (endDate != null && (endDate.before(new Date()) || endDate.equals(new Date()))) {
                    count++;
                }
            }
        }
        return count;
    }

    /**
     * <p>
     * Get patients active prescriptions using given memberId
     * </p>
     *
     * @param memberId member id of the patient
     * @return Bundle which is the result of the operation.
     */
    private Bundle getPrescriptionBundleByMemberId(String memberId) {
        String url = String.format(Constants.ACTIVE_PRESCRIPTION_LIST_USING_RELATED_PERSON,
                FhirConstants.RELATED_PERSON + Constants.FORWARD_SLASH + memberId);
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * {@inheritDoc}
     */
    public List<PrescriptionDTO> getPrescriptionsByMemberId(String memberId) {
        Bundle prescriptionBundle = getPrescriptionBundleByMemberId(memberId);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionBundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof MedicationRequest medicationRequest) {
                PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                prescriptionDTOList.add(prescriptionDTO);
            }
        });
        setMedicationDetails(prescriptionDTOList);
        return prescriptionDTOList;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, List<PrescriptionDTO>> getPrescriptionHistoryByPrescriptions(List<String> prescriptionIds) {
        Map<String, List<PrescriptionDTO>> prescriptionHistoryMap = new HashMap<>();
        Map<String, List<MedicationRequest>> medicationRequestHistories = new HashMap<>();
        Bundle requestBundle = new Bundle();
        requestBundle.setType(Bundle.BundleType.BATCH);

        prescriptionIds.forEach(prescriptionId -> {
            Bundle.BundleEntryRequestComponent requestComponent = new Bundle.BundleEntryRequestComponent()
                    .setMethod(Bundle.HTTPVerb.GET)
                    .setUrl(String.format(Constants.MEDICATION_REQUEST_HISTORY, prescriptionId));
            Bundle.BundleEntryComponent entry = new Bundle.BundleEntryComponent()
                    .setRequest(requestComponent);
            requestBundle.addEntry(entry);
        });
        Bundle historyBundle = restApiUtil.getBundle(requestBundle);
        for (Bundle.BundleEntryComponent component : historyBundle.getEntry()) {
            if (component.getResource() instanceof Bundle bundle) {
                for (Bundle.BundleEntryComponent entryComponent : bundle.getEntry()) {
                    if (entryComponent.getResource() instanceof MedicationRequest medicationRequest
                            && checkIsPrescribed(medicationRequest)) {
                        String medicationRequestId = medicationRequest.getIdPart();

                        if (medicationRequestHistories.containsKey(medicationRequestId)) {
                            medicationRequestHistories.get(medicationRequestId).add(medicationRequest);
                        } else {
                            medicationRequestHistories
                                    .computeIfAbsent(medicationRequest.getIdPart(), k -> new ArrayList<>())
                                    .add(medicationRequest);
                        }
                    }
                }
            }
        }
        medicationRequestHistories.values().forEach(historyEntries ->
                historyEntries.sort(Comparator.comparing(entry ->
                        entry.getMeta().getLastUpdated(), Comparator.reverseOrder()))
        );
        for (Map.Entry<String, List<MedicationRequest>> entry : medicationRequestHistories.entrySet()) {
            prescriptionHistoryMap.put(entry.getKey(), convertToPrescriptionDTO(entry.getValue()));
        }
        return prescriptionHistoryMap;
    }

    /**
     * <p>
     * Converts Medication request to prescriptionDTO
     * </p>
     *
     * @param medicationRequestList list of FHIR MedicationRequest entity's
     * @return list of converted PrescriptionDTO
     */
    private List<PrescriptionDTO> convertToPrescriptionDTO(List<MedicationRequest> medicationRequestList) {
        return medicationRequestList.stream()
                .map(fhirMapper::mapPrescriptionDTO)
                .toList();
    }
}
