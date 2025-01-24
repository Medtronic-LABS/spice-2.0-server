package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.CqlApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyAncDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyANCService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * Implementation of the MedicalReviewService interface.
 * This class provides methods to handle medical review-related operations.
 *
 * @author Nanthinee sugumar created on Mar 27, 2024
 */
@Service
public class MedicalReviewPregnancyANCServiceImpl implements MedicalReviewPregnancyANCService {

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final FhirUtils fhirUtils;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final RestApiUtil restApiUtil;

    private final PatientService patientService;

    private final PrescriptionRequestService prescriptionService;

    @Value("${app.smart-anc}")
    private Boolean isSmartAnc;

    private final CqlApiInterface cqlApiInterface;

    private final InvestigationService investigationService;

    public MedicalReviewPregnancyANCServiceImpl(FhirAssessmentMapper fhirAssessmentMapper, FhirUtils fhirUtils, RestApiUtil restApiUtil,
                                                PatientService patientService, PrescriptionRequestService prescriptionService,
                                                CqlApiInterface cqlApiInterface, InvestigationService investigationService) {
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.patientService = patientService;
        this.prescriptionService = prescriptionService;
        this.cqlApiInterface = cqlApiInterface;
        this.investigationService = investigationService;
    }

    /**
     * Creates a new value observation in the FHIR database.
     * This method constructs an observation resource with the provided observation DTO
     * and saves it to the FHIR server using a bundle transaction.
     *
     * @param observationDto The observation DTO containing details of the observation to be created.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    @Override
    public ObservationDTO createValueObservation(ObservationDTO observationDto) {
        fhirUtils.initiateCodesMap();
        // Create a new bundle for transaction
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (Objects.isNull(observationDto.getPatientReference())) {
            Map<String, String> patientRefDetails = patientService.createPatient(observationDto.getEncounter().getPatientId(),
                    bundle, observationDto.getEncounter().getProvenance());
            observationDto.getEncounter().setMemberId(patientRefDetails.get(Constants.MEMBER_ID));
            observationDto.getEncounter().setPatientReference(patientRefDetails.get(String.valueOf(ResourceType.Patient)));
        } else {
            observationDto.getEncounter().setPatientReference(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                    Constants.FORWARD_SLASH,
                    observationDto.getPatientReference()));
        }

        fhirAssessmentMapper.createValueObservation(observationDto, bundle);
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        observationDto.setPatientReference(!Objects.isNull(response.get(String.valueOf(ResourceType.Patient)))
                && !response.get(String.valueOf(ResourceType.Patient)).isEmpty()
                ? response.get(String.valueOf(ResourceType.Patient)).getFirst()
                : fhirUtils.getIdFromResourceUrl(observationDto.getEncounter().getPatientReference()));
        return observationDto;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Double> getPatientWeight(RequestDTO requestDTO) {
        Map<String, Double> responseMap = new HashMap<>();
        List<String> types = new ArrayList<>();
        types.add(requestDTO.getType());
        Bundle basicDetails = fhirAssessmentMapper.getPatientBasicDetails(requestDTO.getMemberId(),
                types);
        if (basicDetails.getTotal() > Constants.ZERO) {
            Observation observation = (Observation) basicDetails.getEntry().getFirst().getResource();
            responseMap.put(observation.getCode().getText(), observation.getValueQuantity().getValue().doubleValue());
        }
        return responseMap;
    }

    /**
     * Get Height of the Patient from vitals
     *
     * @param patient member Details
     * @return height value
     */
    private Double getHeight(String patient) {
        List<String> types = new ArrayList<>();
        types.add(Constants.HEIGHT);
        Double height = null;
        Bundle basicDetails = fhirAssessmentMapper.getPatientBasicDetails(types, patient);
        if (basicDetails.getTotal() > Constants.ZERO) {
            Observation observation = (Observation) basicDetails.getEntry().getFirst().getResource();
            height = observation.getValueQuantity().getValue().doubleValue();
        }
        return height;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Double> getPatientBp(RequestDTO requestDTO) {
        Map<String, Double> responseMap = new HashMap<>();
        List<String> types = new ArrayList<>();
        types.add(requestDTO.getType());
        Bundle basicDetails = fhirAssessmentMapper.getPatientBasicDetails(requestDTO.getMemberId(),
                types);
        if (basicDetails.getTotal() > Constants.ZERO) {
            Observation observation = (Observation) basicDetails.getEntry().getFirst().getResource();
            observation.getComponent().forEach(component ->
                responseMap.put(component.getCode().getText(), component.getValueQuantity().getValue().doubleValue()));
        }
        return responseMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createMedicalReview(MedicalReviewPregnancyDTO medicalReviewDTO) {
        fhirUtils.initiateCodesMap();
        medicalReviewDTO.getEncounter().setType(Constants.MEDICAL_REVIEW);
        medicalReviewDTO.getEncounter().setDiagnosisType(Constants.PREGNANCY_ANC_MEDICAL_REVIEW);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        patientService.setPatientReferenceInEncounterDetails(medicalReviewDTO.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(medicalReviewDTO.getId(),
                medicalReviewDTO.getEncounter(), Constants.PREGNANCY_ANC_MEDICAL_REVIEW, null, bundle);
        medicalReviewDTO.setId(encounterId);
        String urlSign = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL);
        Observation observation = fhirAssessmentMapper.setObservation(medicalReviewDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MEDICAL_REVIEW, Constants.PREGNANCY_ANC_MEDICAL_REVIEW);
        Observation observationNotes = fhirAssessmentMapper.createNotes(Constants.CLINICAL_NOTES,
                medicalReviewDTO.getClinicalNotes(), medicalReviewDTO.getEncounter());
        fhirAssessmentMapper.addObservationToBundle(observationNotes, bundle,
                medicalReviewDTO.getEncounter().getProvenance());
        observation.setComponent(createComponentsForPregnancySummary(observation, medicalReviewDTO.getPregnancyDetails()));
        fhirAssessmentMapper.createVitalObservation(bundle, medicalReviewDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MEDICAL_REVIEW, medicalReviewDTO.getEncounter().getVisitNumber(),
                medicalReviewDTO.getEncounter().getPatientReference());
        if (!Objects.isNull(medicalReviewDTO.getPregnancyDetails().getWeight())) {
            fhirAssessmentMapper.createVitalObservation(bundle, medicalReviewDTO.getEncounter(), Constants.WEIGHT,
                    medicalReviewDTO.getPregnancyDetails().getWeight(), medicalReviewDTO.getEncounter().getPatientReference());
        }

        if (medicalReviewDTO.getEncounter().getVisitNumber() == Constants.ONE) {
            PregnancyInfo pregnancyInfo = getPatientVitals(medicalReviewDTO.getEncounter().getMemberId());
            if (Objects.nonNull(pregnancyInfo.getPncVisitNo()) ||
                    Objects.nonNull(pregnancyInfo.getPncMotherMedicalReviewVisitNo())) {
                RequestDTO requestDTO = new RequestDTO();
                requestDTO.setMemberId(medicalReviewDTO.getEncounter().getMemberId());
                requestDTO.setEncounterId(encounterId);
                requestDTO.setProvenance(medicalReviewDTO.getEncounter().getProvenance());
                requestDTO.setClosedEncounterType(Constants.PREGNANCY_ANC_MEDICAL_REVIEW);
                requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_REASON);
                requestDTO.setAncStarted(Boolean.TRUE);
                patientService.closePncDetails(bundle, requestDTO);
            }
        }

        if (!Objects.isNull(medicalReviewDTO.getPregnancyDetails().getHeight())) {
            fhirAssessmentMapper.createVitalObservation(bundle, medicalReviewDTO.getEncounter(),
                    Constants.HEIGHT, medicalReviewDTO.getPregnancyDetails().getHeight(),
                    medicalReviewDTO.getEncounter().getPatientReference());
        }

        if (!Objects.isNull(medicalReviewDTO.getPregnancyDetails().getDiastolic()) ||
                !Objects.isNull(medicalReviewDTO.getPregnancyDetails().getSystolic()) ||
                !Objects.isNull(medicalReviewDTO.getPregnancyDetails().getPulse())) {
            fhirAssessmentMapper.createVitalObservation(bundle, medicalReviewDTO.getEncounter(),
                    medicalReviewDTO.getPregnancyDetails().getSystolic(),
                    medicalReviewDTO.getPregnancyDetails().getDiastolic(),
                    medicalReviewDTO.getPregnancyDetails().getPulse());
        }

        if (!Objects.isNull(medicalReviewDTO.getPregnancyDetails().getLastMenstrualPeriod())) {
            fhirAssessmentMapper.createVitalObservation(bundle, medicalReviewDTO.getEncounter(),
                    Constants.LAST_MENSTRUAL_PERIOD,
                    medicalReviewDTO.getPregnancyDetails().getLastMenstrualPeriod(),
                    medicalReviewDTO.getEncounter().getPatientReference());
        }

        Observation presentingComplaints = fhirAssessmentMapper.createSignsObservation(medicalReviewDTO.getPresentingComplaints(), medicalReviewDTO.getEncounter(),
                Constants.PRESENTING_COMPLAINTS, medicalReviewDTO.getPresentingComplaintsNotes());
        if (!Objects.isNull(presentingComplaints)) {
            fhirAssessmentMapper.addObservationToBundle(presentingComplaints, bundle,
                    medicalReviewDTO.getEncounter().getProvenance());
        }

        Observation obstetricExamination = fhirAssessmentMapper.createSignsObservation(medicalReviewDTO.getObstetricExaminations(),
                medicalReviewDTO.getEncounter(), Constants.OBSTETRIC_EXAMINATION, medicalReviewDTO.getObstetricExaminationNotes());
        if (!Objects.isNull(obstetricExamination)) {
            fhirAssessmentMapper.addObservationToBundle(obstetricExamination, bundle,
                    medicalReviewDTO.getEncounter().getProvenance());
        }

        if (null != medicalReviewDTO.getPregnancyHistory()) {
            Observation pregnancyHistory = fhirAssessmentMapper.createSignsObservation(medicalReviewDTO.getPregnancyHistory(),
                    medicalReviewDTO.getEncounter(),
                    Constants.PREGNANCY_HISTORY, medicalReviewDTO.getPregnancyHistoryNotes());
            if (!Objects.isNull(pregnancyHistory)) {
                String pregnancyHistoryRef = fhirAssessmentMapper.addObservationToBundle(pregnancyHistory, bundle, medicalReviewDTO.getEncounter().getProvenance());
                observation.addHasMember(new Reference(pregnancyHistoryRef));
            }
        }
        fhirAssessmentMapper.createObservationComponent(medicalReviewDTO.getFundalHeight(),
                Constants.FUNDAL_HEIGHT,
                observation.getComponent(), Constants.CMS);

        fhirAssessmentMapper.createObservationComponent(medicalReviewDTO.getFetalHeartRate(),
                Constants.FETAL_HEART_RATE,
                observation.getComponent(), Constants.BPM);
        fhirAssessmentMapper.createObservationComponent(medicalReviewDTO.isDeliveryKit(),
                Constants.DOES_MOTHER_HAVE_A_DELIVERY_KIT,
                observation.getComponent());
        patientService.updatePatientStatus(bundle,
                Boolean.TRUE,
                medicalReviewDTO.getEncounter().getProvenance(),
                medicalReviewDTO.getEncounter().getPatientId(), Boolean.TRUE);
        fhirUtils.setBundle(urlSign, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, medicalReviewDTO.getEncounter().getProvenance());
        medicalReviewDTO.getEncounter().setPatientReference(
                fhirUtils.getIdFromResourceUrl(medicalReviewDTO.getEncounter().getPatientReference()));
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, String> responseMap = new HashMap<>();
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        responseMap.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(response.get(String.valueOf(ResourceType.Encounter))) &&
                !response.get(String.valueOf(ResourceType.Encounter)).isEmpty() ?
                response.get(String.valueOf(ResourceType.Encounter)).getFirst() :
                fhirUtils.getIdFromReference(medicalReviewDTO.getId()));
        responseMap.put(Constants.KEY_PATIENT_REFERENCE,
                !Objects.isNull(response.get(String.valueOf(ResourceType.Patient))) &&
                        !response.get(String.valueOf(ResourceType.Patient)).isEmpty() ?
                        response.get(String.valueOf(ResourceType.Patient)).getFirst() :
                        fhirUtils.getIdFromReference(medicalReviewDTO.getEncounter().getPatientReference()));
        if (Boolean.TRUE.equals(isSmartAnc) && Objects.nonNull(medicalReviewDTO.getPregnancyDetails()) &&
                Objects.nonNull(medicalReviewDTO.getPregnancyDetails().getLastMenstrualPeriod()) &&
                Objects.nonNull(medicalReviewDTO.getPregnancyDetails().getEstimatedDeliveryDate()) &&
                medicalReviewDTO.getVisitNumber() == Constants.ONE) {
            cqlApiInterface.evaluateByEncounterId(responseMap.get(Constants.KEY_ENCOUNTER_ID),
                    CommonUtil.getAuthToken(),
                    CommonUtil.getClient());
        }
        return responseMap;
    }

    /**
     * Creates a list of components to observation for ANC.
     *
     * @param pregnancyDetails Anc Details
     * @return List<ObservationComponentComponent>
     */
    private List<Observation.ObservationComponentComponent> createComponentsForPregnancySummary(Observation observation, PregnancyAncDetailsDTO pregnancyDetails) {
        if (null != pregnancyDetails) {
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getLastMenstrualPeriod(),
                    Constants.LAST_MENSTRUAL_PERIOD,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getBmi(),
                    Constants.BMI,
                    observation.getComponent(), Constants.KGM2);
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getEstimatedDeliveryDate(),
                    Constants.ESTIMATED_DELIVERY_DATE,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getNoOfFetus(),
                    Constants.NO_OF_FETUS,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getGravida(),
                    Constants.GRAVIDA,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getParity(),
                    Constants.PARITY,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getPatientBloodGroup(),
                    Constants.PATIENT_BLOOD_GROUP,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getGestationalAge(),
                    Constants.GESTATIONAL_AGE,
                    observation.getComponent());
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getHeight(),
                    Constants.HEIGHT,
                    observation.getComponent(), Constants.CM);
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getPulse(),
                    Constants.PULSE,
                    observation.getComponent(), Constants.BPM);
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getSystolic(),
                    Constants.SYSTOLIC,
                    observation.getComponent(), Constants.MMHG);
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getDiastolic(),
                    Constants.DIASTOLIC,
                    observation.getComponent(), Constants.MMHG);
            fhirAssessmentMapper.createObservationComponent(pregnancyDetails.getWeight(),
                    Constants.WEIGHT,
                    observation.getComponent(), Constants.KG);
        }
        return observation.getComponent();
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public MedicalReviewPregnancySummaryDetailsDTO getPregnancyMedicalReviewDetails(String id, String patientReference) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = fhirAssessmentMapper.getEncounterDetails(id, Boolean.TRUE);
        List<DiagnosisDTO.DiseaseDTO> diseases = new ArrayList<>();
        MedicalReviewPregnancySummaryDetailsDTO medicalReviewPregnancySummaryDetailsDTO = new MedicalReviewPregnancySummaryDetailsDTO();
        medicalReviewPregnancySummaryDetailsDTO.setId(id);
        bundle.getEntry().forEach(entry -> {
            ResourceType resourceType = entry.getResource().getResourceType();
            if (ResourceType.Encounter.equals(resourceType)) {
                Encounter encounter = (Encounter) entry.getResource();
                encounter.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())  && !Objects.isNull(identifier.getValue())) {
                        medicalReviewPregnancySummaryDetailsDTO.setPatientStatus(Constants.PATIENT_STATUS_DISPLAY_NAMES.get(identifier.getValue().toLowerCase()));
                    } else if (FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL.equals(identifier.getSystem())) {
                        medicalReviewPregnancySummaryDetailsDTO.setVisitNumber(Integer.parseInt(identifier.getValue()));
                    }
                });
            } else if (ResourceType.Observation.equals(resourceType)) {
                Observation observation = (Observation) entry.getResource();
                observation.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem())) {

                        if (Constants.PRESENTING_COMPLAINTS.equals(identifier.getValue())) {
                            medicalReviewPregnancySummaryDetailsDTO.setPresentingComplaints(
                                    getSignsFromObservation(observation));
                            medicalReviewPregnancySummaryDetailsDTO.setPresentingComplaintsNotes(
                                    !(observation.getNote().isEmpty()) ? observation.getNote().getFirst().getText() :
                                            null);
                        }

                        if (Constants.OBSTETRIC_EXAMINATION.equals(identifier.getValue())) {
                            medicalReviewPregnancySummaryDetailsDTO.setObstetricExaminations(
                                    getSignsFromObservation(observation));
                            medicalReviewPregnancySummaryDetailsDTO.setObstetricExaminationNotes(
                                    !(observation.getNote().isEmpty()) ? observation.getNote().getFirst().getText() :
                                            null);
                        }

                        if (Constants.CLINICAL_NOTES.equals(observation.getCode().getText())) {
                            medicalReviewPregnancySummaryDetailsDTO.setClinicalNotes(
                                    observation.getValueCodeableConcept().getText());
                        }

                        if (Constants.PREGNANCY_ANC_MEDICAL_REVIEW.equals(identifier.getValue())) {
                            observation.getComponent().forEach(component -> {
                                if (Constants.BMI.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setBmi(
                                            component.getValueQuantity().getValue().doubleValue());
                                } else if (Constants.FUNDAL_HEIGHT.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setFundalHeight(
                                            component.getValueQuantity().getValue().doubleValue());
                                } else if (Constants.FETAL_HEART_RATE.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setFetalHeartRate(
                                            component.getValueQuantity().getValue().doubleValue());
                                } else if (Constants.WEIGHT.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setWeight(
                                            component.getValueQuantity().getValue().doubleValue());
                                } else if (Constants.SYSTOLIC.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setSystolic(
                                            component.getValueQuantity().getValue().doubleValue());
                                } else if (Constants.DIASTOLIC.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setDiastolic(
                                            component.getValueQuantity().getValue().doubleValue());
                                } else if (Constants.PULSE.equals(component.getCode().getText())) {
                                    medicalReviewPregnancySummaryDetailsDTO.setPulse(
                                            component.getValueQuantity().getValue().doubleValue());
                                }
                            });
                        }
                    }
                });
            } else if (ResourceType.Condition.equals(resourceType)) {
                diseases.add(fhirAssessmentMapper.mapToDiagnosis((Condition) entry.getResource(), Boolean.TRUE,
                        Constants.ANC));
            } else if (ResourceType.Patient.equals(resourceType)) {
                Patient patient = (Patient) entry.getResource();
                medicalReviewPregnancySummaryDetailsDTO.setSummaryStatus(fhirAssessmentMapper.getSummaryStatus(patient));
            }
        });
        medicalReviewPregnancySummaryDetailsDTO.setHeight(getHeight(patientReference));
        medicalReviewPregnancySummaryDetailsDTO.setPrescriptions(prescriptionService.getPrescriptionsByEncounter(id, patientReference));
        medicalReviewPregnancySummaryDetailsDTO.setInvestigations(investigationService.getInvestigationsByEncounter(id, patientReference));
        medicalReviewPregnancySummaryDetailsDTO.setDiagnosis(diseases);
        return medicalReviewPregnancySummaryDetailsDTO;
    }

    /**
     * Get Signs from Observation
     *
     * @param observation Observation Object
     * @return List of signs
     */
    private List<String> getSignsFromObservation(Observation observation) {
        return observation.getComponent()
                .stream()
                .map(component -> fhirUtils.getText(component.getCode().getText()))
                .toList();
    }


    /**
     * {@inheritDoc}
     */
    public MedicalReviewPregnancySummaryDetailsDTO getLatestEncounter(RequestDTO request) {
        String url = String.format(Constants.GET_LATEST_ENCOUNTER, FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                Constants.PREGNANCY_ANC_MEDICAL_REVIEW, request.getPatientReference());
        Bundle bundle = restApiUtil.getBatchRequest(url);
        MedicalReviewPregnancySummaryDetailsDTO pregnancySummary = null;
        if (!Objects.isNull(bundle) && !bundle.getEntry().isEmpty()) {
            Encounter encounter = (Encounter) bundle.getEntry().getFirst().getResource();
            pregnancySummary = getPregnancyMedicalReviewDetails(encounter.getIdPart(), request.getPatientReference());
            pregnancySummary.setDateOfReview(encounter.getPeriod().getStart());
        }
        return pregnancySummary;
    }


    /**
     * Get patient vitals information from member
     *
     * @param memberId - member Id
     * @return Pregnancy Info map
     */
    private PregnancyInfo getPatientVitals(String memberId) {
        RequestDTO request = new RequestDTO();
        request.setMemberId(memberId);
        return patientService.getPatientVitals(request);
    }

}

