package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.ICriterion;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PhysicalExamDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.GeneralMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.HistoryService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.IccmMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyANCService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyPNCService;
import com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.NcdMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

/**
 * <p>
 * Service class to perform operation on History
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 09, 2024
 */
@Service
public class HistoryServiceImpl implements HistoryService {

    private final FhirUtils fhirUtils;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final GeneralMedicalReviewService generalMedicalReviewService;

    private final IccmMedicalReviewService iccmMedicalReviewService;

    private final MedicalReviewPregnancyANCService medicalReviewPregnancyANCService;

    private final MedicalReviewPregnancyPNCService medicalReviewPregnancyPNCService;

    private final LabourServiceImpl labourService;

    private final PatientService patientService;

    private final NcdMedicalReviewService ncdMedicalReviewService;

    private final RestApiUtil restApiUtil;

    public HistoryServiceImpl(FhirUtils fhirUtils, GeneralMedicalReviewService generalMedicalReviewService,
                              IccmMedicalReviewService iccmMedicalReviewService, MedicalReviewPregnancyANCService medicalReviewPregnancyANCService,
                              MedicalReviewPregnancyPNCService medicalReviewPregnancyPNCService, LabourServiceImpl labourService,
                              PatientService patientService, NcdMedicalReviewService ncdMedicalReviewService, RestApiUtil restApiUtil) {
        this.fhirUtils = fhirUtils;
        this.generalMedicalReviewService = generalMedicalReviewService;
        this.iccmMedicalReviewService = iccmMedicalReviewService;
        this.medicalReviewPregnancyANCService = medicalReviewPregnancyANCService;
        this.medicalReviewPregnancyPNCService = medicalReviewPregnancyPNCService;
        this.labourService = labourService;
        this.patientService = patientService;
        this.ncdMedicalReviewService = ncdMedicalReviewService;
        this.restApiUtil = restApiUtil;
    }

    /**
     * {@inheritDoc}
     */
    public MedicalReviewHistoryDTO getHistory(MedicalReviewRequestDTO requestDTO) {
        return getHistory(requestDTO, Boolean.FALSE);
    }

    /**
     * {@inheritDoc}
     */
    public MedicalReviewHistoryDTO getPncHistory(MedicalReviewRequestDTO requestDTO) {
        return getHistory(requestDTO, Boolean.TRUE);
    }

    /**
     * Get the history of medical reviews based on the provided request.
     * It first validates the request, then retrieves a Bundle of medical reviews,
     * and finally converts the Bundle to a MedicalReviewHistoryDTO.
     *
     * @param requestDTO   The request data transfer object containing the details for the search operation.
     * @param isPncHistory A boolean flag indicating whether the history is for PNC or not.
     * @return A MedicalReviewHistoryDTO object containing the history of medical reviews.
     */
    private MedicalReviewHistoryDTO getHistory(MedicalReviewRequestDTO requestDTO, boolean isPncHistory) {
        MedicalReviewHistoryDTO medicalReviewHistoryDTO = validateRequest(requestDTO, isPncHistory);
        if (!Objects.isNull(medicalReviewHistoryDTO)) {
            Bundle bundle = getMedicalReviewBundle(requestDTO, isPncHistory);
            bundleToHistoryDTO(medicalReviewHistoryDTO, bundle, requestDTO, isPncHistory);
        }
        return medicalReviewHistoryDTO;
    }

    /**
     * Validate the provided request
     * whether both the patient reference and encounter id in the request are null,
     *
     * @param requestDTO The request data transfer object to be validated.
     */
    private MedicalReviewHistoryDTO validateRequest(MedicalReviewRequestDTO requestDTO, boolean isPncHistory) {
        if (Objects.isNull(requestDTO.getPatientReference()) && Objects.isNull(requestDTO.getEncounterId())) {
            return null;
        }
        if (isPncHistory) {
            Bundle bundle = patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference());
            Patient patient = (Patient) bundle.getEntry().getFirst().getResource();
            if (Constants.SIXTY <= DateUtil.daysSincePast(patient.getBirthDate())) {
                return null;
            }
        }
        return new MedicalReviewHistoryDTO();
    }

    /**
     * Convert a Bundle of medical reviews to a MedicalReviewHistoryDTO.
     *
     * @param medicalReviewHistory The MedicalReviewHistoryDTO where the history will be set.
     * @param bundle               The Bundle of medical reviews to be converted.
     * @param requestDTO           The request data transfer object containing additional details for setting the child id.
     * @param isPncHistory         A boolean flag indicating whether the history is for PNC or not.
     * @return The MedicalReviewHistoryDTO with the history set.
     */
    private MedicalReviewHistoryDTO bundleToHistoryDTO(MedicalReviewHistoryDTO medicalReviewHistory, Bundle bundle
            , MedicalReviewRequestDTO requestDTO, boolean isPncHistory) {
        if (Objects.nonNull(bundle) && !bundle.getEntry().isEmpty()) {
            List<Map<String, Object>> histories = new ArrayList<>();
            setMedicalReviewHistoryId(requestDTO, medicalReviewHistory);
            boolean isFirst = true;
            for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                if (ResourceType.Encounter.equals(entry.getResource().getResourceType())) {
                    if (Bundle.SearchEntryMode.INCLUDE.equals(entry.getSearch().getMode())) {
                        requestDTO.setChildId(entry.getResource().getIdPart());
                        continue;
                    }
                    Encounter encounter = (Encounter) entry.getResource();
                    histories.add(getHistoryMap(encounter));
                    if (isFirst) {
                        setMedicalReviewHistoryDetails(medicalReviewHistory, encounter, requestDTO, isPncHistory);
                    }
                    isFirst = false;
                } else if (ResourceType.ServiceRequest.equals(entry.getResource().getResourceType()) || Objects.nonNull(medicalReviewHistory.getNextVisitDate())) {
                    ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
                    medicalReviewHistory.setNextVisitDate(getNextVisitDateFromServiceRequest(serviceRequest));
                }
            }
            medicalReviewHistory.setHistory(histories);
        }
        return medicalReviewHistory;
    }

    /**
     * This method is used to get a Bundle of medical reviews based on the provided request.
     * It uses the HAPI FHIR client to perform a search operation on the Encounter resource.
     * The search operation is configured based on the details provided in the request.
     *
     * @param requestDTO The request data transfer object containing the details for the search operation.
     *                   It may contain a patient ID and/or an encounter ID.
     * @return A Bundle of medical reviews. If no matching medical reviews are found, this method returns null.
     */
    private Bundle getMedicalReviewBundle(MedicalReviewRequestDTO requestDTO, boolean isPncHistory) {
        IGenericClient client = fhirUtils.getClient(fhirServerUrl, CommonUtil.getClient(), CommonUtil.getAuthToken());
        if (Objects.nonNull(requestDTO.getPatientReference()) && Objects.isNull(requestDTO.getEncounterId())) {
            ICriterion criterion = isPncHistory ?
                    Encounter.IDENTIFIER.exactly()
                            .systemAndValues(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                                    Constants.PNC_CHILD_MEDICAL_REVIEW) :
                    Encounter.IDENTIFIER.exactly().systemAndValues(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                            Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW,
                            Constants.ICCM_UNDER_2M,
                            Constants.ICCM_ABOVE_2M_5Y,
                            Constants.PREGNANCY_ANC_MEDICAL_REVIEW,
                            Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW,
                            Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW,
                            Constants.PNC_CHILD_MEDICAL_REVIEW,
                            Constants.PNC_MOTHER_MEDICAL_REVIEW);
            return client.search()
                    .forResource(Encounter.class)
                    .where(Encounter.PATIENT.hasId(requestDTO.getPatientReference()))
                    .where(criterion)
                    .revInclude(ServiceRequest.INCLUDE_ENCOUNTER)
                    .sort()
                    .descending(Encounter.DATE)
                    .returnBundle(Bundle.class)
                    .execute();
        } else if (Objects.nonNull(requestDTO.getEncounterId())) {
            return getEncounterDetails(requestDTO.getEncounterId());
        }
        return null;
    }

    /**
     * Sets the patientId or encounterId in the MedicalReviewHistoryDTO based on the provided request.
     *
     * @param requestDTO           The request data transfer object containing the patientId and/or encounterId.
     * @param medicalReviewHistory The MedicalReviewHistoryDTO where the patientId or encounterId will be set.
     */
    private void setMedicalReviewHistoryId(MedicalReviewRequestDTO requestDTO,
                                           MedicalReviewHistoryDTO medicalReviewHistory) {
        if (Objects.nonNull(requestDTO.getPatientReference()) && Objects.isNull(requestDTO.getEncounterId())) {
            medicalReviewHistory.setPatientReference(requestDTO.getPatientReference());
        } else if (Objects.nonNull(requestDTO.getEncounterId())) {
            medicalReviewHistory.setId(requestDTO.getEncounterId());
        }
    }

    /**
     * Creates a map containing the date of review, encounter id and type from the provided Encounter.
     *
     * @param encounter The Encounter object from which the details will be extracted.
     * @return A map containing the date of review, encounter id and type.
     */
    private Map<String, Object> getHistoryMap(Encounter encounter) {
        Map<String, Object> history = new HashMap<>();
        history.put(Constants.DATE, encounter.getPeriod().getEnd());
        history.put(Constants.ID, encounter.getIdPart());
        encounter.getIdentifier().forEach(identifier -> {
            if (FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL.equals(identifier.getSystem())) {
                history.put(Constants.TYPE, identifier.getValue());
            }
        });
        return history;
    }

    /**
     * Sets the details in the MedicalReviewHistoryDTO based on the provided Encounter and request.
     *
     * @param medicalReviewHistory The MedicalReviewHistoryDTO where the details will be set.
     * @param encounter            The Encounter object from which the details will be extracted.
     * @param requestDTO           The request data transfer object containing additional details for setting the type.
     */
    private void setMedicalReviewHistoryDetails(MedicalReviewHistoryDTO medicalReviewHistory,
                                                Encounter encounter,
                                                MedicalReviewRequestDTO requestDTO,
                                                boolean isPncHistory) {
        medicalReviewHistory.setDateOfReview(encounter.getPeriod().getEnd());
        medicalReviewHistory.setPatientReference(fhirUtils.getIdFromReference(encounter.getSubject().getReference()));
        medicalReviewHistory.setId(encounter.getIdPart());
        encounter.getIdentifier().forEach(identifier -> {
            setMedicalReviewHistoryTypeDetails(medicalReviewHistory, identifier, encounter, requestDTO, isPncHistory);
            if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && !Objects.isNull(identifier.getValue())) {
                medicalReviewHistory.setPatientStatus(Constants.PATIENT_STATUS_DISPLAY_NAMES.get(identifier.getValue().toLowerCase()));
            }
        });
    }

    /**
     * Sets the details in the MedicalReviewHistoryDTO based on the provided Encounter and request.
     *
     * @param medicalReviewHistory The MedicalReviewHistoryDTO where the details will be set.
     * @param encounter            The Encounter object from which the details will be extracted.
     * @param requestDTO           The request data transfer object containing additional details for setting the type.
     */
    private void setMedicalReviewHistoryTypeDetails(MedicalReviewHistoryDTO medicalReviewHistory,
                                                    Identifier identifier,
                                                    Encounter encounter,
                                                    MedicalReviewRequestDTO requestDTO,
                                                    boolean isPncHistory) {
        switch (identifier.getValue()) {
            case Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW:
                GeneralMedicalReviewSummaryDetailsDTO generalMedicalReviewSummaryDetailsDTO = generalMedicalReviewService.getGeneralMedicalReviewDetails(
                        encounter.getId(), requestDTO.getPatientReference());
                generalMedicalReviewSummaryDetailsDTO.setId(encounter.getIdPart());
                medicalReviewHistory.setReviewDetails(generalMedicalReviewSummaryDetailsDTO);
                medicalReviewHistory.setType(identifier.getValue());
                break;
            case Constants.ICCM_UNDER_2M:
            case Constants.ICCM_ABOVE_2M_5Y:
                requestDTO.setAssessmentName(identifier.getValue());
                requestDTO.setPatientId(fhirUtils.getIdFromResourceUrl(encounter.getSubject().getReference()));
                requestDTO.setEncounterId(medicalReviewHistory.getId());
                IccmResponseDTO iccmMedicalReviewDetails = iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(
                        requestDTO);
                medicalReviewHistory.setReviewDetails(iccmMedicalReviewDetails);
                medicalReviewHistory.setType(identifier.getValue());
                break;
            case Constants.PREGNANCY_ANC_MEDICAL_REVIEW:
                MedicalReviewPregnancySummaryDetailsDTO medicalReviewPregnancySummaryDetailsDTO =
                        medicalReviewPregnancyANCService.getPregnancyMedicalReviewDetails(encounter.getId(), requestDTO.getPatientReference());
                medicalReviewPregnancySummaryDetailsDTO.setId(encounter.getIdPart());
                medicalReviewHistory.setReviewDetails(medicalReviewPregnancySummaryDetailsDTO);
                medicalReviewHistory.setVisitNumber(String.valueOf(medicalReviewPregnancySummaryDetailsDTO.getVisitNumber()));
                medicalReviewHistory.setType(identifier.getValue());
                break;
            case Constants.PNC_CHILD_MEDICAL_REVIEW:
            case Constants.PNC_MOTHER_MEDICAL_REVIEW:
                setPncMedicalReviewDetails(medicalReviewHistory, identifier, encounter, requestDTO, isPncHistory);
                break;
            case Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW:
                RequestDTO tempRequest = new RequestDTO();
                tempRequest.setMotherId(Objects.nonNull(requestDTO.getEncounterId()) ? requestDTO.getEncounterId() :
                        medicalReviewHistory.getId());
                tempRequest.setPatientReference(requestDTO.getPatientReference());
                MotherDTO motherDTO = labourService.getLabourMotherAndNeonateDetails(tempRequest).getMotherDTO();
                motherDTO.setPatientStatus(Constants.POST_NATAL);
                medicalReviewHistory.setReviewDetails(motherDTO);
                medicalReviewHistory.setType(identifier.getValue());
                break;
            case Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW:
                RequestDTO tempRequestDTO = new RequestDTO();
                tempRequestDTO.setNeonateId(Objects.nonNull(requestDTO.getEncounterId()) ? requestDTO.getEncounterId() :
                        medicalReviewHistory.getId());
                MotherNeonateDTO motherNeonateDTO = labourService.getLabourMotherAndNeonateDetails(tempRequestDTO);
                motherNeonateDTO.getNeonateDTO().setPatientStatus(Constants.NEONATE);
                medicalReviewHistory.setReviewDetails(motherNeonateDTO.getNeonateDTO());
                medicalReviewHistory.setNextVisitDate(
                        getNextVisitDateFromServiceRequest(
                                getServiceRequestByEncounterId(motherNeonateDTO.getMotherDTO().getEncounter().getId())));
                medicalReviewHistory.setType(identifier.getValue());
                break;
            default:
                break;
        }
    }

    /**
     * Sets the type details in the MedicalReviewHistoryDTO based on the provided identifier, Encounter and request.
     *
     * @param medicalReviewHistory The MedicalReviewHistoryDTO where the type details will be set.
     * @param identifier           The Identifier object from which the type will be extracted.
     * @param encounter            The Encounter object from which additional details will be extracted.
     * @param requestDTO           The request data transfer object containing additional details for setting the type.
     */
    private void setPncMedicalReviewDetails(MedicalReviewHistoryDTO medicalReviewHistory,
                                            Identifier identifier,
                                            Encounter encounter,
                                            MedicalReviewRequestDTO requestDTO,
                                            boolean isPncHistory) {
        PncMedicalReviewDTO pncMedicalReviewDTO = getPncMedicalReview(requestDTO,
                medicalReviewHistory,
                encounter,
                identifier);
        if (isPncHistory) {
            medicalReviewHistory.setReviewDetails(pncMedicalReviewDTO);
        } else {
            if (Constants.PNC_MOTHER_MEDICAL_REVIEW.equals(identifier.getValue())) {
                medicalReviewHistory.setReviewDetails(pncMedicalReviewDTO.getPncMother());
            } else {
                medicalReviewHistory.setReviewDetails(pncMedicalReviewDTO.getPncChild());
            }
        }
        ServiceRequest serviceRequest = getServiceRequestByEncounterId(pncMedicalReviewDTO.getPncMother().getId());
        medicalReviewHistory.setNextVisitDate(Objects.nonNull(serviceRequest) ? serviceRequest.getOccurrenceDateTimeType().getValue() : null);
        medicalReviewHistory.setType(identifier.getValue());
    }

    /**
     * Retrieves the PNC Medical Review details based on the provided request.
     *
     * @param requestDTO           The request data transfer object containing the details for the search operation.
     * @param medicalReviewHistory The MedicalReviewHistoryDTO where the history will be set.
     * @param encounter            The Encounter object from which the details will be extracted.
     * @param identifier           The Identifier object from which the type will be extracted.
     * @return The PncMedicalReviewDTO containing the PNC Medical Review details.
     */
    private PncMedicalReviewDTO getPncMedicalReview(MedicalReviewRequestDTO requestDTO,
                                                    MedicalReviewHistoryDTO medicalReviewHistory,
                                                    Encounter encounter,
                                                    Identifier identifier) {
        if (Constants.PNC_CHILD_MEDICAL_REVIEW.equals(identifier.getValue())) {
            requestDTO.setChildId(medicalReviewHistory.getId());
            requestDTO.setMotherId(fhirUtils.getIdFromReference(encounter.getPartOf().getReference()));
        } else {
            requestDTO.setMotherId(medicalReviewHistory.getId());
            if (Objects.isNull(requestDTO.getChildId())) {
                Encounter childEncounter = getChildEncounterByMotherEncounterId(requestDTO.getMotherId());
                if (!Objects.isNull(childEncounter)) {
                    requestDTO.setChildId(childEncounter.getIdPart());
                }
            }
        }
        return medicalReviewPregnancyPNCService.getPNCMedicalReviewDetails(requestDTO.getMotherId(),
                requestDTO.getChildId(), null);
    }

    /**
     * Retrieves the child Encounter based on the provided mother encounter id.
     *
     * @param motherEncounterId The id of the mother Encounter.
     * @return The child Encounter.
     */
    private Encounter getChildEncounterByMotherEncounterId(String motherEncounterId) {
        IGenericClient client = fhirUtils.getClient(fhirServerUrl, CommonUtil.getClient(), CommonUtil.getAuthToken());
        return (Encounter) client.search()
                .forResource(Encounter.class)
                .where(Encounter.PART_OF.hasId(motherEncounterId))
                .returnBundle(Bundle.class)
                .execute()
                .getEntryFirstRep()
                .getResource();
    }

    /**
     * Retrieves the details of an Encounter from the FHIR server based on the provided id.
     *
     * @param id The id of the Encounter to retrieve.
     * @return A Bundle containing the details of the Encounter. If no matching Encounter is found, this method returns an empty Bundle.
     */
    private Bundle getEncounterDetails(String id) {
        IGenericClient client = fhirUtils.getClient(fhirServerUrl, CommonUtil.getClient(), CommonUtil.getAuthToken());
        return client.search()
                .forResource(Encounter.class)
                .where(Encounter.RES_ID.exactly().identifier(id))
                .revInclude(ServiceRequest.INCLUDE_ENCOUNTER)
                .revInclude(Encounter.INCLUDE_PART_OF)
                .returnBundle(Bundle.class).execute();
    }

    /**
     * Retrieves the ServiceRequest based on the provided encounter id.
     *
     * @param encounterId The id of the Encounter.
     * @return The ServiceRequest.
     */
    private ServiceRequest getServiceRequestByEncounterId(String encounterId) {
        IGenericClient client = fhirUtils.getClient(fhirServerUrl, CommonUtil.getClient(), CommonUtil.getAuthToken());
        return (ServiceRequest) client.search()
                .forResource(ServiceRequest.class)
                .where(ServiceRequest.ENCOUNTER.hasId(encounterId))
                .returnBundle(Bundle.class)
                .execute()
                .getEntryFirstRep()
                .getResource();
    }

    /**
     * Retrieves the next visit date from the provided ServiceRequest.
     *
     * @param serviceRequest The ServiceRequest from which the next visit date will be extracted.
     * @return The next visit date.
     */
    private Date getNextVisitDateFromServiceRequest(ServiceRequest serviceRequest) {
        return Objects.nonNull(serviceRequest) &&
                Objects.nonNull(serviceRequest.getOccurrence()) &&
                Objects.nonNull(serviceRequest.getOccurrence().dateTimeValue()) ?
                serviceRequest.getOccurrence().dateTimeValue().getValue() : null;
    }

    /**
     * {@inheritDoc}
     */
    public NCDMedicalReviewHistoryDTO getNCDMedicalReviewHistory(MedicalReviewRequestDTO requestDTO) {
        fhirUtils.initiateCodesMap();
        NCDMedicalReviewHistoryDTO response = new NCDMedicalReviewHistoryDTO();
        List<Map<String, Object>> patientVisits = new ArrayList<>();

        if (Objects.nonNull(requestDTO.getPatientReference()) && Objects.isNull(requestDTO.getPatientVisitId())) {
            Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.GET_LATEST_ENCOUNTER_BY_PERIOD, requestDTO.getPatientReference(), Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE, Constants.NCD_MEDICAL_REVIEWED));
            if (!bundle.getEntry().isEmpty()) {
                bundle.getEntry().forEach(resource -> {
                    if (resource.getResource() instanceof Encounter encounter) {
                        patientVisits.add(Map.of(Constants.ID, encounter.getIdPart(), Constants.DATE, encounter.getPeriod().getStart()));
                    }
                    Bundle.BundleEntryComponent encounterComponent = bundle.getEntry().getLast();
                    if (encounterComponent.getResource() instanceof Encounter encounter) {
                        requestDTO.setPatientVisitId(encounter.getIdPart());
                        response.setPatientVisitId(encounter.getIdPart());
                    }
                });
            } else {
                return null;
            }
        }

        if (Objects.nonNull(requestDTO.getPatientVisitId())) {
            response.setPatientVisitId(requestDTO.getPatientVisitId());
        }
        response.setHistory(patientVisits);
        List<String> encounterIds = ncdMedicalReviewService.getEncounterIdsByVisit(requestDTO.getPatientVisitId());
        getMedicalReviewResponse(response, encounterIds);
        return response;
    }

    /**
     * The function `getMedicalReviewResponse` retrieves observation responses for a list of encounter IDs
     * using a batch request.
     *
     * @param response     It is used to map the fields in the NcdMedicalReviewResponse DTO
     * @param encounterIds The encounter ids for which the data to be retrieved is given.
     */
    private void getMedicalReviewResponse(NCDMedicalReviewHistoryDTO response, List<String> encounterIds) {
        Bundle batchRequest = restApiUtil.getBatchRequest(String.format(Constants.OBSERVATION_REV_INCLUDE_ENCOUNTER_IDS, String.join(Constants.COMMA, encounterIds)));
        List<Resource> resources = new ArrayList<>();

        if (!batchRequest.getEntry().isEmpty()) {
            batchRequest.getEntry().forEach(resource ->
                    resources.add(resource.getResource())
            );
        }
        resources.sort(Comparator.comparing(resource -> resource.getMeta().getLastUpdated()));
        resources.stream().forEach(resource ->
                getObservationResponse(response, resource)
        );
    }

    /**
     * The function retrieves a list of component values from a given
     * Observation object.
     *
     * @param observation It contains components, each of which has a code and
     *                    text.
     * @return A list of component values extracted from the given Observation object is being returned.
     */
    private List<String> getComponentValuesFromObservation(Observation observation) {
        return observation.getComponent()
                .stream()
                .map(component -> Objects.isNull(fhirUtils.getText(component.getCode().getText()))
                        ? component.getCode().getText() : fhirUtils.getText(component.getCode().getText()))
                .toList();
    }

    /**
     * {@inheritDoc}
     */
    public NCDMedicalReviewHistoryDTO getNCDMedicalReviewSummaryHistory(MedicalReviewRequestDTO requestDTO) {
        fhirUtils.initiateCodesMap();
        NCDMedicalReviewHistoryDTO response = new NCDMedicalReviewHistoryDTO();
        List<Map<String, Object>> patientVisits = new ArrayList<>();
        Map<String, Object> encounterMap = new HashMap<>();
        Map<String, Object> visitMap = new HashMap<>();
        List<String> encounterIds = new ArrayList<>();

        if (Objects.nonNull(requestDTO.getPatientReference()) && Objects.isNull(requestDTO.getPatientVisitId())) {
            Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.GET_LATEST_ENCOUNTER_BY_IDENTIFIER, requestDTO.getPatientReference(), Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE));
            if (!bundle.getEntry().isEmpty()) {
                bundle.getEntry().forEach(resource -> {
                    if (resource.getResource() instanceof Encounter encounter) {
                        encounterMap.put(encounter.getIdPart(), encounter.getPeriod().getStart());
                    }
                });
            }
            Bundle bundleEncounters = restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_PART_OF, String.join(Constants.COMMA, encounterMap.keySet())));
            if (!bundleEncounters.getEntry().isEmpty()) {
                List<String> finalEncounterIds = encounterIds;
                bundleEncounters.getEntry().forEach(resource -> {
                            String latestEncounterId = Constants.EMPTY_STRING;
                            if (resource.getResource() instanceof Encounter) {
                                Bundle.BundleEntryComponent encounterComponent = bundle.getEntry().getFirst();
                                Encounter encounter = (Encounter) encounterComponent.getResource();
                                latestEncounterId = encounter.getIdPart();
                                requestDTO.setPatientVisitId(latestEncounterId);
                            }

                            if (resource.getResource() instanceof Encounter partOfEncounter) {
                                String encounterReference = fhirUtils.getIdFromReference(partOfEncounter.getPartOf().getReference());
                                visitMap.put(Constants.ID, encounterReference);
                                visitMap.put(Constants.DATE, encounterMap.get(encounterReference));
                                if (latestEncounterId.equals(encounterReference)) {
                                    finalEncounterIds.add(partOfEncounter.getIdPart());
                                }

                            }
                        }
                );
            }
        }
        if (encounterIds.isEmpty()) {
            encounterIds = ncdMedicalReviewService.getEncounterIdsByVisit(requestDTO.getPatientVisitId());
        }

        if (Objects.nonNull(requestDTO.getPatientVisitId())) {
            response.setPatientVisitId(requestDTO.getPatientVisitId());
        }
        patientVisits.add(visitMap);
        response.setHistory(patientVisits);
        getMedicalReviewHistoryResponse(response, encounterIds);

        return response;
    }


    /**
     * This function processes a batch request for medical review
     * history data, extracting observations, prescriptions, and investigations from the resources
     * obtained.
     *
     * @param response     It is used to store the medical review history
     *                     response data, including prescriptions and investigations related to the provided encounter IDs.
     * @param encounterIds The encounterIds are used to retrieve medical review history information
     *                     related to those specific encounters.
     */
    private void getMedicalReviewHistoryResponse(NCDMedicalReviewHistoryDTO response, List<String> encounterIds) {
        if (!encounterIds.isEmpty()) {
            Bundle batchRequest = restApiUtil.getBatchRequest(String.format(Constants.REV_INCLUDE_ENCOUNTER_IDS, String.join(Constants.COMMA, encounterIds),
                    Constants.OBSERVATION, Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT));
            if (!batchRequest.getEntry().isEmpty()) {
                batchRequest.getEntry().forEach(resource -> {
                    getObservationResponse(response, resource.getResource());

                    if (resource.getResource() instanceof MedicationRequest medicationRequest && medicationRequest.hasMedicationCodeableConcept()) {
                        response.getMedicalReview().getPrescriptions().add(medicationRequest.getMedicationCodeableConcept().getText());
                    }

                    if (resource.getResource() instanceof DiagnosticReport diagnosticReport) {
                        response.getMedicalReview().getInvestigations().add(diagnosticReport.getCode().getText());
                    }
                });
            }
        }
    }

    /**
     * The observations from a FHIR resource and populates
     * different fields in a medical review response object based on the observation code.
     *
     * @param response It is used to map the fields in the NcdMedicalReviewResponse DTO
     * @param resource It represents a single entry in a Bundle resource, which is a
     *                 collection of resources being sent or received in a transaction.
     */
    private void getObservationResponse(NCDMedicalReviewHistoryDTO response, Resource resource) {
        if (resource instanceof Observation observation) {
            if (Constants.OBSERVATION_PHYSICAL_EXAMINATION.equals(observation.getCode().getText())) {
                AtomicReference<String> notes = new AtomicReference<>();
                if (observation.hasNote()) {
                    observation.getNote().forEach(note ->
                            notes.set(String.valueOf(note.getText()))
                    );
                }
                List<PhysicalExamDTO> physicalExamDTOS = new ArrayList<>();
                PhysicalExamDTO physicalExamDTO = new PhysicalExamDTO();
                physicalExamDTO.setPhysicalExaminations(getComponentValuesFromObservation(observation));
                physicalExamDTO.setPhysicalExaminationsNote(Objects.nonNull(notes.get()) ? String.valueOf(notes.get()) : null);
                physicalExamDTOS.add(physicalExamDTO);
                response.getMedicalReview().getPhysicalExams().addAll(physicalExamDTOS);
            }
            if (Constants.OBSERVATION_COMPLAINTS.equals(observation.getCode().getText())) {
                response.getMedicalReview().getComplaints().addAll(getComponentValuesFromObservation(observation));
            }
            if (Constants.OBSERVATION_CLINICAL_NOTES.equals(observation.getCode().getText())) {
                response.getMedicalReview().getNotes().add(observation.getValueCodeableConcept().getText());
            }
        }
    }
}