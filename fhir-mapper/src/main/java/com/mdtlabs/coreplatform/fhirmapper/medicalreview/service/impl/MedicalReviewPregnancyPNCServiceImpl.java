package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.SpiceServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabourDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncChildMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMotherMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RelationshipAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyPNCService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * Implementation of the MedicalReviewService interface.
 * This class provides methods to handle medical review-related operations.
 *
 * @author Nandhakumar created on Mar 27, 2024
 */
@Service
public class MedicalReviewPregnancyPNCServiceImpl implements MedicalReviewPregnancyPNCService {

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final RelationshipAlgorithm relationshipAlgoritham;

    private final SpiceServiceApiInterface spiceServiceApiInterface;

    private final HouseholdService householdService;

    private final FhirUtils fhirUtils;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final RestApiUtil restApiUtil;

    private final PatientService patientService;

    private final PrescriptionRequestService prescriptionService;

    private final InvestigationService investigationService;

    private final LabourServiceImpl labourService;

    public MedicalReviewPregnancyPNCServiceImpl(FhirAssessmentMapper fhirAssessmentMapper, RelationshipAlgorithm relationshipAlgoritham,
                                                SpiceServiceApiInterface spiceServiceApiInterface, HouseholdService householdService,
                                                FhirUtils fhirUtils, RestApiUtil restApiUtil, PatientService patientService, PrescriptionRequestService prescriptionService,
                                                InvestigationService investigationService, LabourServiceImpl labourService) {
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.relationshipAlgoritham = relationshipAlgoritham;
        this.spiceServiceApiInterface = spiceServiceApiInterface;
        this.householdService = householdService;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.patientService = patientService;
        this.prescriptionService = prescriptionService;
        this.investigationService = investigationService;
        this.labourService = labourService;
    }

    /**
     * Get Signs from Observation
     *
     * @param observation Observation Object
     * @return List of signs
     */
    private List<String> getSignsFromObservation(Observation observation) {
        return observation.getComponent().stream().map(component -> fhirUtils.getText(component.getCode().getText()))
                .toList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> savePncMedicalReview(PncMedicalReviewDTO pncMedicalReviewDTO) {
        fhirUtils.initiateCodesMap();
        Map<String, String> response = createPncMotherReview(pncMedicalReviewDTO.getPncMother(), pncMedicalReviewDTO.getPncChild());
        if (Objects.nonNull(pncMedicalReviewDTO.getPncChild())) {
            createPncChildReview(pncMedicalReviewDTO.getPncChild(),
                    pncMedicalReviewDTO.getPncMother().getId(), response);
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdMemberDTO createChild(HouseholdMemberDTO householdMemberDTO) {
        householdMemberDTO.setPatientId(null);
        householdMemberDTO.setHouseholdHeadRelationship(
                relationshipAlgoritham.getChildRelationshipFromMother(householdMemberDTO.getHouseholdHeadRelationship()));
        if (Objects.nonNull(householdMemberDTO.getHouseholdId())) {
            HouseholdDTO householdDTO = householdService.getHouseholdById(householdMemberDTO.getHouseholdId());
            householdDTO.setProvenance(householdMemberDTO.getProvenance());
            int relatedPersonCount = householdService.getRelatedPersonCountByHouseholdId(householdDTO.getId());
            if (householdDTO.getNoOfPeople() == relatedPersonCount) {
                householdDTO.setNoOfPeople(householdDTO.getNoOfPeople() + Constants.ONE);
                householdService.updateHousehold(householdDTO);
            }
        }
        householdMemberDTO = householdService.createHouseholdMember(householdMemberDTO);

        if (Objects.isNull(householdMemberDTO.getHouseholdId())) {
            spiceServiceApiInterface.createHouseholdMemberLink(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdMemberDTO);
        }
        return householdMemberDTO;
    }

    /**
     * Create Pnc MedicalReview for Mother
     *
     * @param pncMedicalReviewDTO pnc Medical Review Details
     */
    private Map<String, String> createPncMotherReview(PncMotherMedicalReviewDTO pncMedicalReviewDTO,  PncChildMedicalReviewDTO pncChildMedicalReviewDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        pncMedicalReviewDTO.getEncounter().setType(Constants.MEDICAL_REVIEW);
        pncMedicalReviewDTO.getEncounter().setDiagnosisType(Constants.PNC_MOTHER_MEDICAL_REVIEW);
        patientService.setPatientReferenceInEncounterDetails(pncMedicalReviewDTO.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(pncMedicalReviewDTO.getId(),
                pncMedicalReviewDTO.getEncounter(), Constants.PNC_MOTHER_MEDICAL_REVIEW, null, bundle);
        String urlSign = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL);
        fhirAssessmentMapper.createVitalObservation(bundle, pncMedicalReviewDTO.getEncounter(),
                Constants.PNC_MOTHER_MEDICAL_REVIEW, pncMedicalReviewDTO.getEncounter().getVisitNumber(),
                pncMedicalReviewDTO.getEncounter().getPatientReference());
        Observation observation = fhirAssessmentMapper.setObservation(pncMedicalReviewDTO.getEncounter(),
                Constants.MOTHER_ALIVE, Constants.PNC_MOTHER_MEDICAL_REVIEW);
        fhirAssessmentMapper.setValuesToObservation(observation, null, pncMedicalReviewDTO.getIsMotherAlive(), null);

        if (!Objects.isNull(pncMedicalReviewDTO.getNeonateOutcome())) {
            fhirAssessmentMapper.createVitalObservation(bundle, pncMedicalReviewDTO.getEncounter(), Constants.NEONATE_OUTCOME,
                    pncMedicalReviewDTO.getNeonateOutcome(), pncMedicalReviewDTO.getEncounter().getPatientReference());
        }

        if (Objects.nonNull(pncMedicalReviewDTO.getLabourDTO())) {
            observation.addHasMember(new Reference(createLabourMedicalReview(pncMedicalReviewDTO, bundle)));
        }
        if (!Objects.isNull(pncMedicalReviewDTO.getClinicalNotes())) {
            Observation observationNotes = fhirAssessmentMapper.createNotes(Constants.CLINICAL_NOTES,
                    pncMedicalReviewDTO.getClinicalNotes(), pncMedicalReviewDTO.getEncounter());
            fhirAssessmentMapper.addObservationToBundle(observationNotes, bundle,
                    pncMedicalReviewDTO.getEncounter().getProvenance());
        }

        if (Objects.nonNull(pncChildMedicalReviewDTO) &&  Boolean.FALSE.equals(pncChildMedicalReviewDTO.getIsChildAlive())) {
            fhirAssessmentMapper.createVitalObservation(bundle, pncMedicalReviewDTO.getEncounter(), Constants.NEONATE_DEATH_RECORDED_BY_PHU,
                    Boolean.TRUE, pncMedicalReviewDTO.getEncounter().getPatientReference());
        }

        if (Boolean.FALSE.equals(pncMedicalReviewDTO.getIsMotherAlive())) {
            patientService.handlePatientDeath(Constants.PNC_MOTHER_MEDICAL_REVIEW, bundle, pncMedicalReviewDTO.getEncounter().getMemberId(),
                    encounterId, pncMedicalReviewDTO.getEncounter().getProvenance());
        } else if (pncMedicalReviewDTO.getEncounter().getVisitNumber() == Constants.ONE) {
            PregnancyInfo pregnancyInfo = getPatientVitals(pncMedicalReviewDTO.getEncounter().getMemberId());
            if ((Objects.nonNull(pregnancyInfo.getAncVisitNo()) ||
                    Objects.nonNull(pregnancyInfo.getAncMedicalReviewVisitNo()))) {
                RequestDTO requestDTO = new RequestDTO();
                requestDTO.setMemberId(pncMedicalReviewDTO.getEncounter().getMemberId());
                requestDTO.setEncounterId(encounterId);
                requestDTO.setProvenance(pncMedicalReviewDTO.getEncounter().getProvenance());
                requestDTO.setClosedEncounterType(Constants.PNC_MOTHER_MEDICAL_REVIEW);
                requestDTO.setClosedReason(Constants.AUTO_CLOSE_ANC_REASON);
                patientService.closeAncDetails(bundle, requestDTO);
            }

        }

        Observation presentingComplains = fhirAssessmentMapper.createSignsObservation(
                pncMedicalReviewDTO.getPresentingComplaints(), pncMedicalReviewDTO.getEncounter(),
                Constants.PRESENTING_COMPLAINTS,
                pncMedicalReviewDTO.getPresentingComplaintsNotes());
        if (!Objects.isNull(presentingComplains)) {
            fhirAssessmentMapper.addObservationToBundle(presentingComplains, bundle,
                    pncMedicalReviewDTO.getEncounter().getProvenance());
        }
        Observation systemicExamination = fhirAssessmentMapper.createSignsObservation(
                pncMedicalReviewDTO.getSystemicExaminations(), pncMedicalReviewDTO.getEncounter(),
                Constants.SYSTEMIC_EXAMINATIONS,
                pncMedicalReviewDTO.getSystemicExaminationsNotes());
        if (!Objects.isNull(systemicExamination)) {
            fhirAssessmentMapper.addObservationToBundle(systemicExamination, bundle,
                    pncMedicalReviewDTO.getEncounter().getProvenance());
        }
        if (!Objects.isNull(pncMedicalReviewDTO.getBreastCondition()) ||
                !Objects.isNull(pncMedicalReviewDTO.getInvolutionsOfTheUterus())) {
            createSystemicExaminationQuestion(pncMedicalReviewDTO, bundle);
        }
        patientService.updatePatientStatus(bundle,
                Boolean.FALSE,
                pncMedicalReviewDTO.getEncounter().getProvenance(),
                pncMedicalReviewDTO.getEncounter().getPatientId(),
                Boolean.FALSE.equals(pncMedicalReviewDTO.getIsMotherAlive()) ? Boolean.FALSE : null);

        fhirUtils.setBundle(urlSign, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, pncMedicalReviewDTO.getEncounter().getProvenance());
        pncMedicalReviewDTO.getEncounter().setPatientReference(
                fhirUtils.getIdFromResourceUrl(pncMedicalReviewDTO.getEncounter().getPatientReference()));
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        Map<String, String> responseMap = new HashMap<>();
        responseMap.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(response.get(String.valueOf(ResourceType.Encounter))) &&
                !response.get(String.valueOf(ResourceType.Encounter)).isEmpty() ?
                response.get(String.valueOf(ResourceType.Encounter)).getFirst() :
                fhirUtils.getIdFromReference(pncMedicalReviewDTO.getId()));
        responseMap.put(Constants.KEY_PATIENT_REFERENCE,
                !Objects.isNull(response.get(String.valueOf(ResourceType.Patient))) &&
                        !response.get(String.valueOf(ResourceType.Patient)).isEmpty() ?
                        response.get(String.valueOf(ResourceType.Patient)).getFirst() :
                        fhirUtils.getIdFromReference(pncMedicalReviewDTO.getEncounter().getPatientReference()));
        pncMedicalReviewDTO.setId(responseMap.get(Constants.KEY_ENCOUNTER_ID));
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setProvenance(pncMedicalReviewDTO.getEncounter().getProvenance());
        requestDTO.setGender(Constants.FEMALE);
        requestDTO.setMemberId(pncMedicalReviewDTO.getEncounter().getMemberId());
        requestDTO.setIsPregnant(Boolean.FALSE);
        requestDTO.setPatientId(pncMedicalReviewDTO.getEncounter().getPatientId());
        requestDTO.setType(pncMedicalReviewDTO.getEncounter().getType());
        patientService.getPatientStatus(requestDTO);
        return responseMap;
    }

    /**
     * Creates a medical review for a labour.
     *
     * @param motherDTO The LabourDTO object containing information about the labour.
     */
    public String createLabourMedicalReview(PncMotherMedicalReviewDTO motherDTO, Bundle bundle) {
        LabourDTO labourDTO = motherDTO.getLabourDTO();
        Observation observation = fhirAssessmentMapper.setObservation(motherDTO.getEncounter(),
                Constants.LABOUR_DETAILS, Constants.LABOUR_DETAILS);
        fhirAssessmentMapper.createObservationComponent(labourDTO.getDateAndTimeOfDelivery(),
                Constants.DATE_AND_TIME_OF_DELIVERY, observation.getComponent());
        fhirAssessmentMapper.createVitalObservation(bundle, motherDTO.getEncounter(), Constants.PNC_VISIT_NUMBER,
                Constants.ONE, motherDTO.getEncounter().getPatientReference());
        fhirAssessmentMapper.createVitalObservation(bundle, motherDTO.getEncounter(), Constants.PNC_CREATED_DATE,
                motherDTO.getEncounter().getProvenance().getModifiedDate(), motherDTO.getEncounter().getPatientReference());
        if (!Objects.isNull(labourDTO.getDateAndTimeOfDelivery())) {
            fhirAssessmentMapper.createVitalObservation(bundle, motherDTO.getEncounter(), Constants.DATE_OF_DELIVERY,
                    labourDTO.getDateAndTimeOfDelivery(), motherDTO.getEncounter().getPatientReference());
        }
        if (!Objects.isNull(labourDTO.getNoOfNeoNates())) {
            fhirAssessmentMapper.createVitalObservation(bundle, motherDTO.getEncounter(), Constants.NO_OF_NEONATES,
                    labourDTO.getNoOfNeoNates(), motherDTO.getEncounter().getPatientReference());
        }
        if (!Objects.isNull(labourDTO.getNeonatePatientId())) {
            fhirAssessmentMapper.createVitalObservation(bundle, motherDTO.getEncounter(), Constants.NEONATE_PATIENT_ID,
                    labourDTO.getNeonatePatientId(), motherDTO.getEncounter().getPatientReference());
        }
        if (!Objects.isNull(labourDTO.getDeliveryByOther())) {
            fhirAssessmentMapper.createObservationComponent(labourDTO.getDeliveryByOther(), Constants.DELIVERY_BY_OTHER,
                    observation.getComponent());
        }
        fhirAssessmentMapper.createObservationComponent(labourDTO.getDateAndTimeOfLabourOnset(),
                Constants.DATE_AND_TIME_OF_LABOUR_ONSET, observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(labourDTO.getDeliveryType(), Constants.DELIVERY_TYPE,
                observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(labourDTO.getDeliveryBy(), Constants.DELIVERY_BY,
                observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(labourDTO.getDeliveryAt(), Constants.DELIVERY_AT,
                observation.getComponent());
        if (!Objects.isNull(labourDTO.getDeliveryAtOther())) {
            fhirAssessmentMapper.createObservationComponent(labourDTO.getDeliveryAtOther(), Constants.DELIVERY_AT_OTHER,
                    observation.getComponent());
        }
        fhirAssessmentMapper.createObservationComponent(labourDTO.getDeliveryStatus(),
                Constants.DELIVERY_STATUS, observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(motherDTO.getNeonateOutcome(),
                Constants.NEONATE_OUTCOME, observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(labourDTO.getNoOfNeoNates(),
                Constants.NO_OF_NEONATES, observation.getComponent());
        return fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                motherDTO.getEncounter().getProvenance());
    }

    /**
     * Create Systemic Examination Other question
     *
     * @param pncMedicalReviewDTO Review Details
     * @param bundle              Bundle object
     */
    private void createSystemicExaminationQuestion(PncMotherMedicalReviewDTO pncMedicalReviewDTO, Bundle bundle) {
        Observation observationPncSystem = fhirAssessmentMapper.setObservation(pncMedicalReviewDTO.getEncounter(),
                Constants.PNC_MOTHER_SYSTEMIC_EXAMINATION, Constants.PNC_MOTHER_SYSTEMIC_EXAMINATION);
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getBreastCondition(),
                Constants.BREAST_CONDITION, observationPncSystem.getComponent());
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getBreastConditionNotes(),
                Constants.BREAST_CONDITION_NOTES, observationPncSystem.getComponent());
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getInvolutionsOfTheUterus(),
                Constants.INVOLUTIONS_OF_THE_UTERUS, observationPncSystem.getComponent());
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getInvolutionsOfTheUterusNotes(),
                Constants.INVOLUTIONS_OF_THE_UTERUS_NOTES, observationPncSystem.getComponent());
        fhirAssessmentMapper.addObservationToBundle(observationPncSystem, bundle,
                pncMedicalReviewDTO.getEncounter().getProvenance());
    }

    /**
     * Create Pnc MedicalReview for Child
     *
     * @param pncMedicalReviewDTO pnc Medical Review Details
     */
    private void createPncChildReview(PncChildMedicalReviewDTO pncMedicalReviewDTO, String motherEncounterId, Map<String, String> responseMap) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        pncMedicalReviewDTO.getEncounter().setType(Constants.MEDICAL_REVIEW);
        //Create Patient
        pncMedicalReviewDTO.setVisitNumber(pncMedicalReviewDTO.getEncounter().getVisitNumber());
        patientService.setPatientReferenceInEncounterDetails(pncMedicalReviewDTO.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(pncMedicalReviewDTO.getId(),
                pncMedicalReviewDTO.getEncounter(), Constants.PNC_CHILD_MEDICAL_REVIEW, motherEncounterId, bundle);
        pncMedicalReviewDTO.setId(encounterId);
        String urlSign = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL);
        Observation observation = fhirAssessmentMapper.setObservation(pncMedicalReviewDTO.getEncounter(),
                Constants.CHILD_ALIVE, Constants.PNC_CHILD_MEDICAL_REVIEW);
        fhirAssessmentMapper.setValuesToObservation(observation, null, pncMedicalReviewDTO.getIsChildAlive(), null);

        if (!Objects.isNull(pncMedicalReviewDTO.getClinicalNotes())) {
            Observation observationNotes = fhirAssessmentMapper.createNotes(Constants.CLINICAL_NOTES,
                    pncMedicalReviewDTO.getClinicalNotes(), pncMedicalReviewDTO.getEncounter());
            fhirAssessmentMapper.addObservationToBundle(observationNotes, bundle,
                    pncMedicalReviewDTO.getEncounter().getProvenance());
        }

        fhirAssessmentMapper.createVitalObservation(bundle, pncMedicalReviewDTO.getEncounter(),
                Constants.PNC_CHILD_MEDICAL_REVIEW, pncMedicalReviewDTO.getEncounter().getVisitNumber(),
                pncMedicalReviewDTO.getEncounter().getPatientReference());
        Observation presentingComplains = fhirAssessmentMapper.createSignsObservation(
                pncMedicalReviewDTO.getPresentingComplaints(), pncMedicalReviewDTO.getEncounter(),
                Constants.PRESENTING_COMPLAINTS,
                pncMedicalReviewDTO.getPresentingComplaintsNotes());
        if (!Objects.isNull(presentingComplains)) {
            fhirAssessmentMapper.addObservationToBundle(presentingComplains, bundle,
                    pncMedicalReviewDTO.getEncounter().getProvenance());
        }

        Observation systemicExamination = fhirAssessmentMapper.createSignsObservation(
                pncMedicalReviewDTO.getPhysicalExaminations(), pncMedicalReviewDTO.getEncounter(),
                Constants.PHYSICAL_EXAMINATIONS, pncMedicalReviewDTO.getPhysicalExaminationNotes());
        if (!Objects.isNull(systemicExamination)) {
            fhirAssessmentMapper.addObservationToBundle(systemicExamination, bundle,
                    pncMedicalReviewDTO.getEncounter().getProvenance());
        }
        if (!Objects.isNull(pncMedicalReviewDTO.getBreastFeeding()) ||
                !Objects.isNull(pncMedicalReviewDTO.getCongenitalDetect()) ||
                !Objects.isNull(pncMedicalReviewDTO.getCordExamination())) {
            createPhysicalExaminationQuestion(pncMedicalReviewDTO, bundle);
        }
        if (Boolean.FALSE.equals(pncMedicalReviewDTO.getIsChildAlive())) {
            patientService.updatePatientStatus(bundle,
                    null,
                    pncMedicalReviewDTO.getEncounter().getProvenance(),
                    pncMedicalReviewDTO.getEncounter().getPatientId(),
                    Boolean.FALSE);
            patientService.handlePatientDeath(Constants.PNC_CHILD_MEDICAL_REVIEW, bundle, pncMedicalReviewDTO.getEncounter().getMemberId(),
                    encounterId, pncMedicalReviewDTO.getEncounter().getProvenance());
        }
        fhirUtils.setBundle(urlSign, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, pncMedicalReviewDTO.getEncounter().getProvenance());
        pncMedicalReviewDTO.getEncounter().setPatientReference(
                fhirUtils.getIdFromResourceUrl(pncMedicalReviewDTO.getEncounter().getPatientReference()));
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        responseMap.put(Constants.KEY_CHILD_ENCOUNTER_ID, !Objects.isNull(response.get(String.valueOf(ResourceType.Encounter))) &&
                !response.get(String.valueOf(ResourceType.Encounter)).isEmpty() ?
                response.get(String.valueOf(ResourceType.Encounter)).getFirst() :
                fhirUtils.getIdFromReference(pncMedicalReviewDTO.getId()));
        responseMap.put(Constants.KEY_CHILD_PATIENT_REFERENCE,
                !Objects.isNull(response.get(String.valueOf(ResourceType.Patient))) &&
                        !response.get(String.valueOf(ResourceType.Patient)).isEmpty() ?
                        response.get(String.valueOf(ResourceType.Patient)).getFirst() :
                        fhirUtils.getIdFromReference(pncMedicalReviewDTO.getEncounter().getPatientReference()));

    }


    /**
     * Create Physical Examination Other question
     *
     * @param pncMedicalReviewDTO Review Details
     * @param bundle              Bundle object
     */
    private void createPhysicalExaminationQuestion(PncChildMedicalReviewDTO pncMedicalReviewDTO, Bundle bundle) {
        Observation observationPncSystem = fhirAssessmentMapper.setObservation(pncMedicalReviewDTO.getEncounter(),
                Constants.PNC_CHILD_PHYSICAL_EXAMINATIONS, Constants.PNC_CHILD_PHYSICAL_EXAMINATIONS);
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getBreastFeeding(),
                Constants.BREAST_FEEDING, observationPncSystem.getComponent());
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getExclusiveBreastFeeding(),
                Constants.EXCLUSIVE_BREAST_FEEDING, observationPncSystem.getComponent());
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getCongenitalDetect(),
                Constants.CONGENITAL_DETECT, observationPncSystem.getComponent());
        fhirAssessmentMapper.createObservationComponent(pncMedicalReviewDTO.getCordExamination(),
                Constants.CORD_EXAMINATION, observationPncSystem.getComponent());
        fhirAssessmentMapper.addObservationToBundle(observationPncSystem, bundle,
                pncMedicalReviewDTO.getEncounter().getProvenance());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PncMedicalReviewDTO getPNCMedicalReviewDetails(String motherId, String childId, String motherPatientReference) {
        fhirUtils.initiateCodesMap();
        Bundle bundleMother = fhirAssessmentMapper.getEncounterDetails(motherId, Boolean.TRUE);
        Bundle bundleChild = null;
        if (!Objects.isNull(childId)) {
            bundleChild = fhirAssessmentMapper.getEncounterDetails(childId, Boolean.TRUE);
        }
        PncMedicalReviewDTO pncMedicalReviewDTO = new PncMedicalReviewDTO();
        PncMotherMedicalReviewDTO pncMotherMedicalReviewDTO = new PncMotherMedicalReviewDTO();
        PncChildMedicalReviewDTO pncChildMedicalReviewDTO = new PncChildMedicalReviewDTO();
        setPncMotherReviewDetails(bundleMother, pncMotherMedicalReviewDTO, motherId, motherPatientReference);
        if (!Objects.isNull(bundleChild)) {
            setPncChildReviewDetails(bundleChild, pncChildMedicalReviewDTO);
            pncMedicalReviewDTO.setPncChild(pncChildMedicalReviewDTO);
        }
        pncMedicalReviewDTO.setPncMother(pncMotherMedicalReviewDTO);
        return pncMedicalReviewDTO;
    }

    /**
     * set Pnc Child Details
     *
     * @param bundle                    Bundle Object
     * @param pncMotherMedicalReviewDTO Details DTO
     */
    private void setPncMotherReviewDetails(Bundle bundle, PncMotherMedicalReviewDTO pncMotherMedicalReviewDTO,
                                           String motherId, String patientReference) {
        List<DiagnosisDTO.DiseaseDTO> diseases = new ArrayList<>();
        bundle.getEntry().forEach(entry -> {
            ResourceType resourceType = entry.getResource().getResourceType();
            if (ResourceType.Encounter.equals(resourceType)) {
                Encounter encounter = (Encounter) entry.getResource();
                pncMotherMedicalReviewDTO.setId(encounter.getIdPart());
                pncMotherMedicalReviewDTO.setPatientReference(encounter
                        .getSubject().getReference().replaceAll(
                                Constants.FHIR_RESOURCE_PATIENT.concat(Constants.FORWARD_SLASH), Constants.EMPTY_STRING));
                encounter.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())  && !Objects.isNull(identifier.getValue())) {
                        pncMotherMedicalReviewDTO.setPatientStatus(Constants.PATIENT_STATUS_DISPLAY_NAMES.get(identifier.getValue().toLowerCase()));
                    } else if (FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL.equals(identifier.getSystem())) {
                        pncMotherMedicalReviewDTO.setVisitNumber(Integer.parseInt(identifier.getValue()));
                    }
                });
            } else if (ResourceType.Observation.equals(resourceType)) {
                Observation observation = (Observation) entry.getResource();
                observation.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem())) {
                        if (Constants.PRESENTING_COMPLAINTS.equals(identifier.getValue())) {
                            pncMotherMedicalReviewDTO.setPresentingComplaints(
                                    getSignsFromObservation(observation));
                            pncMotherMedicalReviewDTO.setPresentingComplaintsNotes(
                                    !(observation.getNote().isEmpty()) ? observation.getNote().getFirst().getText() :
                                            null);
                        } else if (Constants.SYSTEMIC_EXAMINATIONS.equals(identifier.getValue())) {
                            pncMotherMedicalReviewDTO.setSystemicExaminations(
                                    getSignsFromObservation(observation));
                            pncMotherMedicalReviewDTO.setSystemicExaminationsNotes(
                                    !(observation.getNote().isEmpty()) ? observation.getNote().getFirst().getText() :
                                            null);
                        } else if (Constants.PNC_MOTHER_MEDICAL_REVIEW.equals(identifier.getValue())) {
                            pncMotherMedicalReviewDTO.setIsMotherAlive(
                                    Constants.YES.equalsIgnoreCase(observation.getValueCodeableConcept().getText()));
                        } else if (Constants.CLINICAL_NOTES.equals(identifier.getValue())) {
                            pncMotherMedicalReviewDTO.setClinicalNotes(observation.getValueCodeableConcept().getText());
                        }
                        setPncMotherSystemicOtherValues(observation, identifier, pncMotherMedicalReviewDTO);
                    }
                });
            } else if (ResourceType.Condition.equals(resourceType)) {
                diseases.add(fhirAssessmentMapper.mapToDiagnosis((Condition) entry.getResource(), Boolean.TRUE,
                        Constants.PNC_MOTHER_MEDICAL_REVIEW));
            } else if (ResourceType.Patient.equals(entry.getResource().getResourceType())) {
                Patient patient = (Patient) entry.getResource();
                pncMotherMedicalReviewDTO.setSummaryStatus(fhirAssessmentMapper.getSummaryStatus(patient));
            }
        });
        pncMotherMedicalReviewDTO.setDiagnosis(diseases);
        if (!Objects.isNull(patientReference)) {
            pncMotherMedicalReviewDTO.setPrescriptions(
                    prescriptionService.getPrescriptionsByEncounter(motherId, patientReference));
            pncMotherMedicalReviewDTO.setInvestigations(
                    investigationService.getInvestigationsByEncounter(motherId, patientReference));
        }
    }

    /**
     * set Pnc Child Details
     *
     * @param bundle                   Bundle Object
     * @param pncChildMedicalReviewDTO Details DTO
     */
    private void setPncChildReviewDetails(Bundle bundle, PncChildMedicalReviewDTO pncChildMedicalReviewDTO) {
        bundle.getEntry().forEach(entry -> {
            String resourceType = String.valueOf(entry.getResource().getResourceType());
            if (String.valueOf(ResourceType.Encounter).equals(resourceType)) {
                Encounter encounter = (Encounter) entry.getResource();
                pncChildMedicalReviewDTO.setId(encounter.getIdPart());
                pncChildMedicalReviewDTO.setPatientReference(encounter.getSubject().getReference()
                        .replaceAll(Constants.FHIR_RESOURCE_PATIENT.concat(Constants.FORWARD_SLASH), Constants.EMPTY_STRING));
                encounter.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())) {
                        pncChildMedicalReviewDTO.setPatientStatus(identifier.getValue());
                    } else if (FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL.equals(identifier.getSystem())) {
                        pncChildMedicalReviewDTO.setVisitNumber(Integer.parseInt(identifier.getValue()));
                    }
                });
            }
            if (String.valueOf(ResourceType.Observation).equals(resourceType)) {
                Observation observation = (Observation) entry.getResource();
                observation.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem())) {
                        if (Constants.PRESENTING_COMPLAINTS.equals(identifier.getValue())) {
                            pncChildMedicalReviewDTO.setPresentingComplaints(
                                    getSignsFromObservation(observation));
                            pncChildMedicalReviewDTO.setPresentingComplaintsNotes(
                                    !observation.getNote().isEmpty() ? observation.getNote().getFirst().getText() :
                                            null);
                        } else if (Constants.PHYSICAL_EXAMINATIONS.equals(identifier.getValue())) {
                            pncChildMedicalReviewDTO.setPhysicalExaminations(
                                    getSignsFromObservation(observation));
                            pncChildMedicalReviewDTO.setPhysicalExaminationNotes(
                                    !observation.getNote().isEmpty() ? observation.getNote().getFirst().getText() :
                                            null);
                        } else if (Constants.PNC_CHILD_MEDICAL_REVIEW.equals(identifier.getValue())) {
                            pncChildMedicalReviewDTO.setIsChildAlive(Constants.YES.equalsIgnoreCase(
                                    observation.getValueCodeableConcept().getText()));
                        } else if (Constants.CLINICAL_NOTES.equals(identifier.getValue())) {
                            pncChildMedicalReviewDTO.setClinicalNotes(
                                    observation.getValueCodeableConcept().getText());
                        }
                        setPncChildPhysicalOtherValues(observation, identifier, pncChildMedicalReviewDTO);
                    }
                });
            }
        });
    }

    /**
     * Set PncMother Details from Observation
     *
     * @param observation               Observation Values
     * @param identifier                identifier
     * @param pncMotherMedicalReviewDTO ChildDto
     */
    private void setPncMotherSystemicOtherValues(Observation observation, Identifier identifier,
                                                 PncMotherMedicalReviewDTO pncMotherMedicalReviewDTO) {
        if (Constants.PNC_MOTHER_SYSTEMIC_EXAMINATION.equals(identifier.getValue())) {
            observation.getComponent().forEach(component -> {
                if (Constants.BREAST_CONDITION.equals(component.getCode().getText())) {
                    pncMotherMedicalReviewDTO.setBreastCondition(component.getValueCodeableConcept().getText());
                } else if (Constants.BREAST_CONDITION_NOTES.equals(component.getCode().getText())) {
                    pncMotherMedicalReviewDTO.setBreastConditionNotes(component.getValueCodeableConcept().getText());
                } else if (Constants.INVOLUTIONS_OF_THE_UTERUS.equals(component.getCode().getText())) {
                    pncMotherMedicalReviewDTO.setInvolutionsOfTheUterus(component.getValueCodeableConcept().getText());
                } else if (Constants.INVOLUTIONS_OF_THE_UTERUS_NOTES.equals(component.getCode().getText())) {
                    pncMotherMedicalReviewDTO.setInvolutionsOfTheUterusNotes(
                            component.getValueCodeableConcept().getText());
                }
            });
        }
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

    /**
     * Set PncChild Details from Observation
     *
     * @param observation              Observation Values
     * @param identifier               identifier
     * @param pncChildMedicalReviewDTO ChildDto
     */
    private void setPncChildPhysicalOtherValues(Observation observation, Identifier identifier,
                                                PncChildMedicalReviewDTO pncChildMedicalReviewDTO) {
        if (Constants.PNC_CHILD_PHYSICAL_EXAMINATIONS.equals(identifier.getValue())) {
            observation.getComponent().forEach(component -> {
                if (Constants.CORD_EXAMINATION.equals(component.getCode().getText())) {
                    pncChildMedicalReviewDTO.setCordExamination(component.getValueCodeableConcept().getText());
                } else if (Constants.CONGENITAL_DETECT.equals(component.getCode().getText())) {
                    pncChildMedicalReviewDTO.setCongenitalDetect(component.getValueCodeableConcept().getText());
                } else if (Constants.BREAST_FEEDING.equals(component.getCode().getText())) {
                    pncChildMedicalReviewDTO.setBreastFeeding(
                            Constants.YES.equalsIgnoreCase(component.getValueCodeableConcept().getText()));
                } else if (Constants.EXCLUSIVE_BREAST_FEEDING.equals(component.getCode().getText())) {
                    pncChildMedicalReviewDTO.setExclusiveBreastFeeding(
                            Constants.YES.equalsIgnoreCase(component.getValueCodeableConcept().getText()));
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createPncChild(HouseholdMemberDTO householdMemberDTO) {
        if (!Objects.isNull(householdMemberDTO.getMotherPatientId()) && !Objects.isNull(householdMemberDTO.getMemberId())) {
            Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
            householdMemberDTO = labourService.createChild(householdMemberDTO.getHouseholdId(),
                    householdMemberDTO);
            encounterDetailsDTO.setMemberId(householdMemberDTO.getMemberId());
            encounterDetailsDTO.setProvenance(householdMemberDTO.getProvenance());
            encounterDetailsDTO.setType(Constants.MEDICAL_REVIEW);
            fhirAssessmentMapper.createVitalObservation(bundle, encounterDetailsDTO, Constants.NEONATE_PATIENT_ID,
                    householdMemberDTO.getPatientId(), householdMemberDTO.getMotherPatientId());
            fhirAssessmentMapper.createVitalObservation(bundle, encounterDetailsDTO, Constants.DATE_OF_DELIVERY,
                    householdMemberDTO.getDateOfBirth(),null);
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        } else {
            throw new SpiceValidation(1009);
        }
    }
}