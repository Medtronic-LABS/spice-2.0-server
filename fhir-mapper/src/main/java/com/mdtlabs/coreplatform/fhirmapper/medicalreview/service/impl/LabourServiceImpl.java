package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.SpiceServiceApiInterface;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.IdType;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.APGARScoreDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabourDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RelationshipAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.LabourService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * Implementation of the {@code LabourService} interface.
 * Provides functionality related to labor and delivery.
 *
 * @author Nanthinee sugumar created on April 25, 2024
 */
@Service
public class LabourServiceImpl implements LabourService {

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final SpiceServiceApiInterface spiceServiceApiInterface;

    private final FhirUtils fhirUtils;

    private final HouseholdService householdService;

    private final RelationshipAlgorithm relationshipAlgoritham;

    private final RestApiUtil restApiUtil;

    private final PatientService patientService;

    private final PrescriptionRequestService prescriptionRequestService;

    private final InvestigationService investigationService;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    @Autowired
    public LabourServiceImpl(FhirAssessmentMapper fhirAssessmentMapper, SpiceServiceApiInterface spiceServiceApiInterface,
                             FhirUtils fhirUtils, HouseholdService householdService, RelationshipAlgorithm relationshipAlgoritham,
                             RestApiUtil restApiUtil, PatientService patientService, PrescriptionRequestService prescriptionRequestService,
                             InvestigationService investigationService) {
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.spiceServiceApiInterface = spiceServiceApiInterface;
        this.fhirUtils = fhirUtils;
        this.householdService = householdService;
        this.relationshipAlgoritham = relationshipAlgoritham;
        this.restApiUtil = restApiUtil;
        this.patientService = patientService;
        this.prescriptionRequestService = prescriptionRequestService;
        this.investigationService = investigationService;
    }

    /**
     * Creates a medical review for a labour.
     *
     * @param motherDTO The LabourDTO object containing information about the labour.
     */
    public String createLabourMedicalReview(MotherDTO motherDTO, Bundle bundle) {
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
        fhirAssessmentMapper.createObservationComponent(labourDTO.getNoOfNeoNates(),
                Constants.NO_OF_NEONATES, observation.getComponent());
        patientService.updatePatientStatus(bundle, Boolean.FALSE, motherDTO.getEncounter().getProvenance(),
                motherDTO.getEncounter().getPatientId(), null);
        return fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                motherDTO.getEncounter().getProvenance());
    }

    /**
     * Creates a medical review for a mother.
     *
     * @param motherDTO The MotherDTO object containing information about the mother.
     */
    public void createMotherMedicalReview(MotherDTO motherDTO, Map<String, String> response) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        motherDTO.getEncounter().setType(Constants.MEDICAL_REVIEW);
        patientService.setPatientReferenceInEncounterDetails(motherDTO.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(motherDTO.getId(),
                motherDTO.getEncounter(), Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW, null, bundle);
        motherDTO.setId(encounterId);
        String urlSign = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL);
        Observation observation = fhirAssessmentMapper.setObservation(motherDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW, Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW);
        Observation signs = fhirAssessmentMapper.createSignsObservation(motherDTO.getSigns(), motherDTO.getEncounter(),
                Constants.SIGNS, null);
        if (!Objects.isNull(signs)) {
            String signsRef = fhirAssessmentMapper.addObservationToBundle(signs, bundle,
                    motherDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(signsRef));
        }
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(motherDTO.getEncounter().getMemberId());
        requestDTO.setEncounterId(encounterId);
        requestDTO.setProvenance(motherDTO.getEncounter().getProvenance());
        requestDTO.setClosedEncounterType(Constants.PNC_MOTHER_MEDICAL_REVIEW);
        requestDTO.setClosedReason(Constants.AUTO_CLOSE_LABOUR_REASON);
        requestDTO.setAncStarted(Boolean.TRUE);
        patientService.closeAncDetails(bundle, requestDTO);
        patientService.closePncDetails(bundle, requestDTO);

        if (!Objects.isNull(motherDTO.getNeonateOutcome())) {
            fhirAssessmentMapper.createVitalObservation(bundle, motherDTO.getEncounter(), Constants.NEONATE_OUTCOME,
                    motherDTO.getNeonateOutcome(), motherDTO.getEncounter().getPatientReference());
        }

        observation.addHasMember(new Reference(createLabourMedicalReview(motherDTO, bundle)));
        fhirAssessmentMapper.createObservationComponent(motherDTO.getGeneralConditions(),
                Constants.GENERAL_CONDITION_OF_MOTHER, observation.getComponent());
        fhirAssessmentMapper.setQuantityToObservation(observation, new Quantity(motherDTO.getTtDoseTaken()),
                Constants.TT_DOSE);
        fhirAssessmentMapper.createObservationComponent(motherDTO.getStateOfPerineum(),
                Constants.STATE_OF_THE_PERINEUM, observation.getComponent());
        Observation riskFactors = fhirAssessmentMapper.createSignsObservation(motherDTO.getRiskFactors(),
                motherDTO.getEncounter(), Constants.RISK_FACTORS, null);
        if (!Objects.isNull(riskFactors)) {
            String riskFactorsRef = fhirAssessmentMapper.addObservationToBundle(riskFactors, bundle,
                    motherDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(riskFactorsRef));
        }
        Observation status = fhirAssessmentMapper.createSignsObservation(motherDTO.getStatus(),
                motherDTO.getEncounter(), Constants.STATUS, null);
        if (!Objects.isNull(status)) {
            String statusRef = fhirAssessmentMapper.addObservationToBundle(status, bundle,
                    motherDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(statusRef));
        }
        fhirAssessmentMapper.createObservationComponent(motherDTO.getTear(), Constants.TEAR,
                observation.getComponent());
        fhirUtils.setBundle(urlSign, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, motherDTO.getEncounter().getProvenance());
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));

        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, List<String>> responseIds = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put("motherId",
                !Objects.isNull(responseIds.get(String.valueOf(ResourceType.Encounter))) &&
                        !responseIds.get(String.valueOf(ResourceType.Encounter)).isEmpty() ?
                        responseIds.get(String.valueOf(ResourceType.Encounter)).getFirst() :
                        fhirUtils.getIdFromReference(motherDTO.getId()));
        response.put(Constants.KEY_PATIENT_REFERENCE,
                !Objects.isNull(responseIds.get(String.valueOf(ResourceType.Patient))) &&
                        !responseIds.get(String.valueOf(ResourceType.Patient)).isEmpty() ?
                        responseIds.get(String.valueOf(ResourceType.Patient)).getFirst() :
                        fhirUtils.getIdFromResourceUrl(motherDTO.getEncounter().getPatientReference()));
    }

    /**
     * Creates a medical review for a neonate.
     *
     * @param neonateDTO The NeonateDTO object containing information about the neonate.
     */
    public void createNeonateMedicalReview(NeonateDTO neonateDTO, Map<String, String> response) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        neonateDTO.getEncounter().setType(Constants.MEDICAL_REVIEW);
        patientService.setPatientReferenceInEncounterDetails(neonateDTO.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(neonateDTO.getId(),
                neonateDTO.getEncounter(), Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW, null, bundle);
        neonateDTO.setId(encounterId);
        String urlSign = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL);
        Observation observation = fhirAssessmentMapper.setObservation(neonateDTO.getEncounter(),
                Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW, Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW);

        Observation signs = fhirAssessmentMapper.createSignsObservation(neonateDTO.getSigns(),
                neonateDTO.getEncounter(), Constants.SIGNS, null);
        if (!Objects.isNull(signs)) {
            String signsRef = fhirAssessmentMapper.addObservationToBundle(signs, bundle,
                    neonateDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(signsRef));
        }
        fhirAssessmentMapper.createObservationComponent(neonateDTO.getNeonateOutcome(),
                Constants.NEONATE_OUTCOME, observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(neonateDTO.getBirthWeight(), Constants.BIRTHWEIGHT,
                observation.getComponent(), Constants.KG);
        fhirAssessmentMapper.createVitalObservation(bundle, neonateDTO.getEncounter(), Constants.WEIGHT,
                neonateDTO.getBirthWeight(), neonateDTO.getEncounter().getPatientReference());
        fhirAssessmentMapper.createBirthHistory(neonateDTO.getSigns(), neonateDTO.getBirthWeight(),
                neonateDTO.getGestationalAge(), neonateDTO.getEncounter(), bundle);

        fhirAssessmentMapper.createObservationComponent(neonateDTO.getGender().toLowerCase(), Constants.GENDER,
                observation.getComponent());
        fhirAssessmentMapper.createObservationComponent(neonateDTO.getGestationalAge(),
                Constants.GESTATIONAL_AGE, observation.getComponent());

        if (!Objects.isNull(neonateDTO.getApgarScoreOneMinuteDTO())) {
            Observation apgarScoreOneMinute = fhirAssessmentMapper.createApgarObservationComponent(
                    neonateDTO.getApgarScoreOneMinuteDTO(), neonateDTO.getEncounter(),
                    Constants.APGAR_SCORE_ONE_MINUTE);
            apgarScoreOneMinute.setValue(new Quantity(neonateDTO.getApgarScoreOneMinuteDTO().getOneMinuteTotalScore()));
            String apgarScoreRef = fhirAssessmentMapper.addObservationToBundle(apgarScoreOneMinute, bundle,
                    neonateDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(apgarScoreRef));
        }

        if (!Objects.isNull(neonateDTO.getApgarScoreFiveMinuteDTO())) {
            Observation apgarScoreFiveMinute = fhirAssessmentMapper.createApgarObservationComponent(
                    neonateDTO.getApgarScoreFiveMinuteDTO(), neonateDTO.getEncounter(),
                    Constants.APGAR_SCORE_FIVE_MINUTE);
            apgarScoreFiveMinute.setValue(
                    new Quantity(neonateDTO.getApgarScoreFiveMinuteDTO().getFiveMinuteTotalScore()));
            String apgarScoreRef = fhirAssessmentMapper.addObservationToBundle(apgarScoreFiveMinute, bundle,
                    neonateDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(apgarScoreRef));
        }

        if (!Objects.isNull(neonateDTO.getApgarScoreTenMinuteDTO())) {
            Observation apgarScoreTenMinute = fhirAssessmentMapper.createApgarObservationComponent(
                    neonateDTO.getApgarScoreTenMinuteDTO(), neonateDTO.getEncounter(),
                    Constants.APGAR_SCORE_TEN_MINUTE);
            apgarScoreTenMinute.setValue(new Quantity(neonateDTO.getApgarScoreTenMinuteDTO().getTenMinuteTotalScore()));
            String apgarScoreRef = fhirAssessmentMapper.addObservationToBundle(apgarScoreTenMinute, bundle,
                    neonateDTO.getEncounter().getProvenance());
            observation.addHasMember(new Reference(apgarScoreRef));
        }

        fhirAssessmentMapper.createObservationComponent(neonateDTO.getStateOfBaby(), Constants.STATE_OF_BABY,
                observation.getComponent());
        fhirUtils.setBundle(urlSign, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, neonateDTO.getEncounter().getProvenance());
        neonateDTO.getEncounter()
                .setPatientReference(fhirUtils.getIdFromResourceUrl(neonateDTO.getEncounter().getPatientReference()));

        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));

        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, List<String>> responseIds = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put("neonateId",
                !Objects.isNull(responseIds.get(String.valueOf(ResourceType.Encounter))) &&
                        !responseIds.get(String.valueOf(ResourceType.Encounter)).isEmpty() ?
                        responseIds.get(String.valueOf(ResourceType.Encounter)).getFirst() :
                        fhirUtils.getIdFromReference(neonateDTO.getId()));
        response.put(Constants.KEY_CHILD_PATIENT_REFERENCE,
                !Objects.isNull(responseIds.get(String.valueOf(ResourceType.Patient))) &&
                        !responseIds.get(String.valueOf(ResourceType.Patient)).isEmpty() ?
                        responseIds.get(String.valueOf(ResourceType.Patient)).getFirst() :
                        fhirUtils.getIdFromResourceUrl(neonateDTO.getEncounter().getPatientReference()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createLabourMotherAndNeonateMedicalReview(MotherNeonateDTO motherNeonateDTO) {
        fhirUtils.initiateCodesMap();
        Map<String, String> responseMap = new HashMap<>();
        if (!Objects.isNull(motherNeonateDTO.getChild())) {
            HouseholdMemberDTO householdMemberDTO = createChild(motherNeonateDTO.getMotherDTO().getEncounter().getHouseholdId(), motherNeonateDTO.getChild());
            motherNeonateDTO.getNeonateDTO().getEncounter().setPatientId(householdMemberDTO.getPatientId());
            motherNeonateDTO.getNeonateDTO().getEncounter().setMemberId(householdMemberDTO.getId());
            motherNeonateDTO.getMotherDTO().getLabourDTO().setNeonatePatientId(householdMemberDTO.getPatientId());
            if (Objects.isNull(householdMemberDTO.getHouseholdId())) {
                spiceServiceApiInterface.createHouseholdMemberLink(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdMemberDTO);
            }
        }
        createMotherMedicalReview(motherNeonateDTO.getMotherDTO(), responseMap);
        if (!Objects.isNull(motherNeonateDTO.getChild())) {
            createNeonateMedicalReview(motherNeonateDTO.getNeonateDTO(), responseMap);
        }
        return responseMap;
    }

    /**
     * Create Child from Mother Delivery Details
     *
     * @param householdId Mother household Id
     * @param householdMemberDTO Neonate Details
     * @return HouseHoldMember
     */
    public HouseholdMemberDTO createChild(String householdId, HouseholdMemberDTO householdMemberDTO) {
        householdMemberDTO.setPatientId(null);
        householdMemberDTO.setHouseholdHeadRelationship(
                relationshipAlgoritham.getChildRelationshipFromMother(householdMemberDTO.getHouseholdHeadRelationship()));
        if (Objects.nonNull(householdId)) {
            householdMemberDTO.setHouseholdId(householdId);
            HouseholdDTO householdDTO = householdService.getHouseholdById(householdMemberDTO.getHouseholdId());
            householdDTO.setProvenance(householdMemberDTO.getProvenance());
            int relatedPersonCount = householdService.getRelatedPersonCountByHouseholdId(householdDTO.getId());
            if (householdDTO.getNoOfPeople() == relatedPersonCount) {
                householdDTO.setNoOfPeople(householdDTO.getNoOfPeople() + Constants.ONE);
                householdService.updateHousehold(householdDTO);
            }
        }
        householdMemberDTO = householdService.createHouseholdMember(householdMemberDTO);
        return householdMemberDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MotherNeonateDTO getLabourMotherAndNeonateDetails(RequestDTO requestDTO) {
        fhirUtils.initiateCodesMap();
        Bundle bundleMother = getEncounterDetailsEverything(requestDTO.getMotherId());
        MotherNeonateDTO motherNeonateDTO = new MotherNeonateDTO();
        MotherDTO motherDTO = new MotherDTO();
        setMotherDetails(bundleMother, motherDTO);
        if (Objects.nonNull(requestDTO.getNeonateId())) {
            Bundle bundleNeonate = getEncounterDetailsEverything(requestDTO.getNeonateId());
            NeonateDTO neonateDTO = new NeonateDTO();
            setNeonateDetails(bundleNeonate, neonateDTO);
            motherNeonateDTO.setNeonateDTO(neonateDTO);
        }
        motherDTO.setPrescriptions(prescriptionRequestService.getPrescriptionsByEncounter(requestDTO.getMotherId(),
                requestDTO.getPatientReference()));
        motherDTO.setInvestigations(investigationService.getInvestigationsByEncounter(requestDTO.getMotherId(),
                requestDTO.getPatientReference()));
        motherNeonateDTO.setMotherDTO(motherDTO);
        return motherNeonateDTO;
    }

    /**
     * Sets the mother details from a FHIR Bundle object to a MotherDTO object.
     *
     * @param bundle    The FHIR Bundle object containing mother details.
     * @param motherDTO The MotherDTO object to which mother details will be set.
     */
    private void setMotherDetails(Bundle bundle, MotherDTO motherDTO) {
        motherDTO.setEncounter(new EncounterDetailsDTO());
        motherDTO.getEncounter().setProvenance(new ProvenanceDTO());
        motherDTO.setLabourDTO(new LabourDTO());
        bundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof Observation observation) {
                setMotherDetailsFromObservation(observation, motherDTO);
            } else if (entry.getResource() instanceof Encounter encounter) {
                setEncounterDetailsFromEncounter(encounter, motherDTO.getEncounter());
            } else if (entry.getResource() instanceof Provenance provenance) {
                setProvenanceDetailsFromProvenance(provenance, motherDTO.getEncounter().getProvenance());
            } else if (entry.getResource() instanceof RelatedPerson relatedPerson) {
                setEncounterDetailsFromRelatedPerson(relatedPerson, motherDTO.getEncounter());
            }
        });
    }

    /**
     * <p>
     * Sets the mother details from a FHIR Observation object to a MotherDTO object.
     * </p>
     *
     * @param observation The Observation object containing mother details.
     * @param motherDTO   The MotherDTO object to which mother details will be set.
     */
    private void setMotherDetailsFromObservation(Observation observation, MotherDTO motherDTO) {
        observation.getIdentifier().forEach(identifier -> {
            if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                    (Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW.equals(identifier.getValue()))) {
                setMotherDetailsFromObservationComponent(observation, motherDTO);
            } else if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                    (Constants.STATUS.equals(identifier.getValue()))) {
                motherDTO.setStatus(getSignsFromObservation(observation));
            } else if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                    (Constants.RISK_FACTORS.equals(identifier.getValue()))) {
                motherDTO.setRiskFactors(getSignsFromObservation(observation));
            } else if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                    (Constants.SIGNS.equals(identifier.getValue()))) {
                motherDTO.setSigns(getSignsFromObservation(observation));
            } else if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem()) &&
                    (Constants.LABOUR_DETAILS.equals(identifier.getValue()))) {
                setLabourDetails(observation, motherDTO.getLabourDTO());
            }
        });
    }

    /**
     * <p>
     * This method extracts the signs from the given Observation object.
     * The signs are obtained from the 'code' field of the 'component' of the Observation.
     * </p>
     *
     * @param observation The Observation object containing the signs.
     * @return A list of signs extracted from the Observation object.
     */
    private List<String> getSignsFromObservation(Observation observation) {
        return observation.getComponent().stream().map(component -> fhirUtils.getText(component.getCode().getText()))
                .toList();
    }

    /**
     * <p>
     * Sets the mother details from a FHIR Observation object to a MotherDTO object.
     * </p>
     *
     * @param observation The Observation object containing mother details.
     * @param motherDTO   The MotherDTO object to which mother details will be set.
     */
    private void setMotherDetailsFromObservationComponent(Observation observation, MotherDTO motherDTO) {
        observation.getComponent().forEach(component -> {
            if (Constants.TEAR.equals(component.getCode().getText())) {
                motherDTO.setTear(component.getValueCodeableConcept().getText());
            } else if (Constants.STATE_OF_THE_PERINEUM.equals(component.getCode().getText())) {
                motherDTO.setStateOfPerineum(component.getValueCodeableConcept().getText());
            } else if (Constants.GENERAL_CONDITION_OF_MOTHER.equals(component.getCode().getText())) {
                motherDTO.setGeneralConditions(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.TT_DOSE.equals(component.getCode().getText())) {
                motherDTO.setTtDoseTaken(component.getValueQuantity().getValue().intValue());
            }
        });
    }

    /**
     * <p>
     * Sets the labour details from a FHIR Observation object to a LabourDTO object.
     * </p>
     *
     * @param observation The Observation object containing labour details.
     * @param labourDTO   The LabourDTO object to which labour details will be set.
     */
    private void setLabourDetails(Observation observation, LabourDTO labourDTO) {
        observation.getComponent().forEach(component -> {
            if (Constants.DATE_AND_TIME_OF_DELIVERY.equals(component.getCode().getText())) {
                labourDTO.setDateAndTimeOfDelivery(component.getValueDateTimeType().getValue());
            } else if (Constants.DATE_AND_TIME_OF_LABOUR_ONSET.equals(component.getCode().getText())) {
                labourDTO.setDateAndTimeOfLabourOnset(component.getValueDateTimeType().getValue());
            } else if (Constants.DELIVERY_TYPE.equals(component.getCode().getText())) {
                labourDTO.setDeliveryType(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.DELIVERY_BY.equals(component.getCode().getText())) {
                labourDTO.setDeliveryBy(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.DELIVERY_AT.equals(component.getCode().getText())) {
                labourDTO.setDeliveryAt(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.DELIVERY_AT_OTHER.equals(component.getCode().getText())) {
                labourDTO.setDeliveryAtOther(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.DELIVERY_STATUS.equals(component.getCode().getText())) {
                labourDTO.setDeliveryStatus(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.NO_OF_NEONATES.equals(component.getCode().getText())) {
                labourDTO.setNoOfNeoNates(component.getValueQuantity().getValue().intValue());
            } else if (Constants.DELIVERY_BY_OTHER.equals(component.getCode().getText())) {
                labourDTO.setDeliveryByOther(component.getValueCodeableConcept().getText());
            }
        });
        labourDTO.setDeliveryBy(Constants.OTHERS_SPECIFY.equals(labourDTO.getDeliveryBy()) ?
                labourDTO.getDeliveryBy().replace(Constants.SPECIFY, labourDTO.getDeliveryByOther())
                : labourDTO.getDeliveryBy());
        labourDTO.setDeliveryAt(Constants.OTHER.equals(labourDTO.getDeliveryAt()) ?
                labourDTO.getDeliveryAt().concat(" (").concat(labourDTO.getDeliveryAtOther()).concat(")")
                : labourDTO.getDeliveryAt());
    }

    /**
     * <p>
     * Sets the encounter details from an FHIR Encounter object to an EncounterDetailsDTO object.
     * </p>
     *
     * @param encounter           The Encounter object containing mother details.
     * @param encounterDetailsDTO The EncounterDetailsDTO object to which mother details will be set.
     */
    private void setEncounterDetailsFromEncounter(Encounter encounter, EncounterDetailsDTO encounterDetailsDTO) {
        encounterDetailsDTO.setId(fhirUtils.getIdFromHistoryUrl(encounter.getId()));
        if (Objects.nonNull(encounter.getSubject()) && encounter.getSubject().hasReference()) {
            if (encounter.getSubject().getReference().contains(ResourceType.Group.toString())) {
                encounterDetailsDTO.setHouseholdId(fhirUtils.getIdFromReference(encounter.getSubject().getReference()));
            } else {
                encounterDetailsDTO.setPatientReference(
                        fhirUtils.getIdFromReference(encounter.getSubject().getReference()));
            }
        }
        encounter.getParticipant().forEach(encounterParticipantComponent -> {
            if (encounterParticipantComponent.getIndividual().getReference()
                    .contains(ResourceType.RelatedPerson.toString())) {
                encounterDetailsDTO.setMemberId(
                        fhirUtils.getIdFromReference(encounterParticipantComponent.getIndividual().getReference()));
            } else if (encounterParticipantComponent.getIndividual().getReference()
                    .contains(ResourceType.Practitioner.toString())) {
                encounterDetailsDTO.getProvenance().setUserId(
                        fhirUtils.getIdFromReference(encounterParticipantComponent.getIndividual().getReference()));
            }
        });
        encounterDetailsDTO.setStartTime(encounter.getPeriod().getStart());
        encounterDetailsDTO.setEndTime(encounter.getPeriod().getEnd());
        encounterDetailsDTO.getProvenance()
                .setOrganizationId(fhirUtils.getIdFromReference(encounter.getServiceProvider().getReference()));
        if (encounter.hasIdentifier()) {
            encounter.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL.equals(identifier.getSystem()) &&
                        Objects.nonNull(identifier.getValue())) {
                    encounterDetailsDTO.setVisitNumber(Integer.parseInt(identifier.getValue()));
                }
            });
        }
        setEncounterDetailsFromLocation(encounter.getLocation(), encounterDetailsDTO);
    }

    /**
     * <p>
     * Sets the latitude and longitude from an FHIR Location object to an EncounterDetailsDTO object.
     * </p>
     *
     * @param locationComponents  The Location Components object containing latitude and longitude details.
     * @param encounterDetailsDTO The EncounterDetailsDTO object to which latitude and longitude details will be set.
     */
    private void setEncounterDetailsFromLocation(List<Encounter.EncounterLocationComponent> locationComponents,
                                                 EncounterDetailsDTO encounterDetailsDTO) {
        locationComponents.forEach(encounterLocationComponent -> {
            if (Objects.nonNull(encounterLocationComponent.getLocation())) {
                Location location = (Location) encounterLocationComponent.getLocation().getResource();
                encounterDetailsDTO.setLatitude(location.getPosition().getLatitude().doubleValue());
                encounterDetailsDTO.setLongitude(location.getPosition().getLongitude().doubleValue());
            }
        });
    }

    /**
     * <p>
     * Sets the provenance details from an FHIR Provenance object to a ProvenanceDTO object.
     * </p>
     *
     * @param provenance    The Provenance object containing provenance details.
     * @param provenanceDTO The ProvenanceDTO object to which provenance details will be set.
     */
    private void setProvenanceDetailsFromProvenance(Provenance provenance, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(provenance)) {
            provenanceDTO.setModifiedDate(provenance.getRecorded());
        }
    }

    /**
     * <p>
     * This method sets the encounter details from an FHIR RelatedPerson object to an EncounterDetailsDTO object.
     * </p>
     *
     * @param relatedPerson       The RelatedPerson object containing patient and household details.
     * @param encounterDetailsDTO The EncounterDetailsDTO object to which patient and household details will be set.
     */
    private void setEncounterDetailsFromRelatedPerson(RelatedPerson relatedPerson,
                                                      EncounterDetailsDTO encounterDetailsDTO) {
        if (relatedPerson.hasIdentifier()) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL.equals(identifier.getSystem())) {
                    encounterDetailsDTO.setPatientId(identifier.getValue());
                } else if (FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL.equals(identifier.getSystem())) {
                    encounterDetailsDTO.setHouseholdId(identifier.getValue());
                }
            });
        }
    }

    /**
     * Sets the neonate details from a FHIR Bundle object to a NeonateDTO object.
     *
     * @param bundle     The FHIR Bundle object containing neonate details.
     * @param neonateDTO The NeonateDTO object to which neonate details will be set.
     */
    public void setNeonateDetails(Bundle bundle, NeonateDTO neonateDTO) {
        neonateDTO.setEncounter(new EncounterDetailsDTO());
        neonateDTO.getEncounter().setProvenance(new ProvenanceDTO());
        bundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof Observation observation) {
                setNeonateDetailsFromObservation(observation, neonateDTO);
                observation.getComponent().stream().forEach(component -> {
                    String key = component.getCode().getText();
                    if ((Constants.GENDER).equals(key)) {
                        neonateDTO.setGender(component.getValueCodeableConcept().getText());
                    }
                    if (Constants.NEONATE_OUTCOME.equals(key)) {
                        neonateDTO.setNeonateOutcome(fhirUtils.getText(component.getValueCodeableConcept().getText()));
                    }
                });
            } else if (entry.getResource() instanceof Encounter encounter) {
                setEncounterDetailsFromEncounter(encounter, neonateDTO.getEncounter());
            } else if (entry.getResource() instanceof Provenance provenance) {
                setProvenanceDetailsFromProvenance(provenance, neonateDTO.getEncounter().getProvenance());
            } else if (entry.getResource() instanceof RelatedPerson relatedPerson) {
                setEncounterDetailsFromRelatedPerson(relatedPerson, neonateDTO.getEncounter());
            }
        });
        if (Objects.nonNull(neonateDTO.getApgarScoreOneMinuteDTO())
                && Objects.nonNull(neonateDTO.getApgarScoreFiveMinuteDTO())
                && Objects.nonNull(neonateDTO.getApgarScoreTenMinuteDTO())) {
            neonateDTO.setTotal(neonateDTO.getApgarScoreOneMinuteDTO().getOneMinuteTotalScore() +
                    neonateDTO.getApgarScoreFiveMinuteDTO().getFiveMinuteTotalScore() +
                    neonateDTO.getApgarScoreTenMinuteDTO().getTenMinuteTotalScore());
        }
    }

    /**
     * <p>
     * Sets the neonate details from a FHIR Observation object to a NeonateDTO object.
     * </p>
     *
     * @param observation The Observation object containing neonate details.
     * @param neonateDTO  The NeonateDTO object to which neonate details will be set.
     */
    private void setNeonateDetailsFromObservation(Observation observation, NeonateDTO neonateDTO) {
        observation.getIdentifier().forEach(identifier -> {
            if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem())) {
                if (Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW.equals(identifier.getValue())) {
                    setNeonateDetailsFromObservationComponent(observation, neonateDTO);
                } else if (Constants.SIGNS.equals(identifier.getValue())) {
                    neonateDTO.setSigns(getSignsFromObservation(observation));
                } else if (Constants.APGAR_SCORE_ONE_MINUTE.equals(identifier.getValue())) {
                    neonateDTO.setApgarScoreOneMinuteDTO(getAPGARScoreFromObservation(observation, Constants.ONE));
                } else if (Constants.APGAR_SCORE_FIVE_MINUTE.equals(identifier.getValue())) {
                    neonateDTO.setApgarScoreFiveMinuteDTO(getAPGARScoreFromObservation(observation, Constants.FIVE));
                } else if (Constants.APGAR_SCORE_TEN_MINUTE.equals(identifier.getValue())) {
                    neonateDTO.setApgarScoreTenMinuteDTO(getAPGARScoreFromObservation(observation, Constants.TEN));
                }
            }
        });
    }

    /**
     * <p>
     * This method is used to set the neonate details from a FHIR Observation object to a NeonateDTO object.
     * </p>
     *
     * @param observation The Observation object containing neonate details.
     * @param neonateDTO  The NeonateDTO object to which neonate details will be set.
     */
    private void setNeonateDetailsFromObservationComponent(Observation observation, NeonateDTO neonateDTO) {
        observation.getComponent().forEach(component -> {
            if (Constants.NEONATE_OUTCOME.equals(component.getCode().getText())) {
                neonateDTO.setNeonateOutcome(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.GENDER.equals(component.getCode().getText())) {
                neonateDTO.setGender(fhirUtils.getText(component.getValueCodeableConcept().getText()));
            } else if (Constants.BIRTHWEIGHT.equals(component.getCode().getText())) {
                neonateDTO.setBirthWeight(component.getValueQuantity().getValue().doubleValue());
            } else if (Constants.STATE_OF_BABY.equals(component.getCode().getText())) {
                neonateDTO.setStateOfBaby(component.getValueCodeableConcept().getText());
            } else if (Constants.GESTATIONAL_AGE.equals(component.getCode().getText())) {
                neonateDTO.setGestationalAge(component.getValueQuantity().getValue().intValue());
            }
        });
    }

    /**
     * <p>
     * This method is used to get the APGAR Score details from a FHIR Observation object to an APGARScoreDTO object.
     * </p>
     *
     * @param observation The Observation object containing APGAR Score details.
     * @param min         The min for which the APGAR Score is being set (1, 5, or 10 minutes).
     * @return The APGAR Score details.
     */
    private APGARScoreDTO getAPGARScoreFromObservation(Observation observation, Integer min) {
        APGARScoreDTO apgarScoreDTO = new APGARScoreDTO();
        observation.getComponent().forEach(component -> {
            if (Constants.ACTIVITY.equals(component.getCode().getText())) {
                apgarScoreDTO.setActivity(component.getValueQuantity().getValue().intValue());
            } else if (Constants.PULSE.equals(component.getCode().getText())) {
                apgarScoreDTO.setPulse(component.getValueQuantity().getValue().intValue());
            } else if (Constants.GRIMACE.equals(component.getCode().getText())) {
                apgarScoreDTO.setGrimace(component.getValueQuantity().getValue().intValue());
            } else if (Constants.APPEARANCE.equals(component.getCode().getText())) {
                apgarScoreDTO.setAppearance(component.getValueQuantity().getValue().intValue());
            } else if (Constants.RESPIRATION.equals(component.getCode().getText())) {
                apgarScoreDTO.setRespiration(component.getValueQuantity().getValue().intValue());
            }
        });
        if (Objects.nonNull(observation.getValueQuantity())) {
            switch (min) {
                case Constants.ONE:
                    apgarScoreDTO.setOneMinuteTotalScore(observation.getValueQuantity().getValue().intValue());
                    break;
                case Constants.FIVE:
                    apgarScoreDTO.setFiveMinuteTotalScore(observation.getValueQuantity().getValue().intValue());
                    break;
                case Constants.TEN:
                    apgarScoreDTO.setTenMinuteTotalScore(observation.getValueQuantity().getValue().intValue());
                    break;
                default:
                    break;
            }
        }
        return apgarScoreDTO;
    }

    /**
     * Get Birth-History Details
     *
     * @param memberId encounter id
     * @return BirthHistoryDTO Details
     */
    @Override
    public BirthHistoryDTO getBirthHistory(String memberId) {
        BirthHistoryDTO birthHistoryDTO = new BirthHistoryDTO();
        List<String> types = new ArrayList<>();
        types.add(Constants.BIRTH_HISTORY);
        Bundle basicDetails = fhirAssessmentMapper.getPatientBasicDetails(memberId, types);
        if (basicDetails.getTotal() > Constants.ZERO) {
            Observation observation = (Observation) basicDetails.getEntry().getFirst().getResource();
            observation.getComponent().forEach(component -> {
                if (Constants.BIRTHWEIGHT.equals(component.getCode().getText())) {
                    birthHistoryDTO.setBirthWeight(component.getValueQuantity().getValue().doubleValue());
                } else if (Constants.GESTATIONAL_AGE.equals(component.getCode().getText())) {
                    birthHistoryDTO.setGestationalAge(component.getValueQuantity().getValue().intValue());
                } else if (Constants.HAVE_BREATHING_PROBLEMS.equals(component.getCode().getText())) {
                    birthHistoryDTO.setHaveBreathingProblem(
                            Constants.YES.equalsIgnoreCase(component.getValueCodeableConcept().getText()));
                } else if (Constants.BIRTHWEIGHT_CATEGORY.equals(component.getCode().getText())) {
                    birthHistoryDTO.setBirthWeightCategory(component.getValueCodeableConcept().getText());
                } else if (Constants.GESTATIONAL_AGE_CATEGORY.equals(component.getCode().getText())) {
                    birthHistoryDTO.setGestationalAgeCategory(component.getValueCodeableConcept().getText());
                }
            });
        }
        return birthHistoryDTO;
    }

    /**
     * <p>
     * This method retrieves all the details related to a specific encounter, including all the associated observations.
     * </p>
     *
     * @param id The unique identifier of the encounter.
     * @return A Bundle object containing all the details related to the encounter.
     */
    private Bundle getEncounterDetailsEverything(String id) {
        IGenericClient client = fhirUtils.getClient(fhirServerUrl, CommonUtil.getClient(), CommonUtil.getAuthToken());
        return client.operation()
                .onInstance(new IdType(Constants.FHIR_RESOURCE_ENCOUNTER, id))
                .named(Constants.EVERYTHING)
                .withNoParameters(Parameters.class)
                .returnResourceType(Bundle.class)
                .execute();
    }

}