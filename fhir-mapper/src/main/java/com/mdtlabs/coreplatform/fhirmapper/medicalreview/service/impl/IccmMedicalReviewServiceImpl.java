package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Condition.ConditionStageComponent;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ClinicalSummaryAndSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.AnaemiaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.BreastfeedingProblemDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.CoughDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.DiarrhoeaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.EarProblemDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.FeverDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.GeneralDangerSigns;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.HIVInfectionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.HivRdtTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.JaundiceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.NonBreastfeedingProblemDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto.VerySevereDiseaseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.IccmMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * <p>
 * This class is a service class to perform operation on iccm medical review
 * operations.
 * </p>
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Service
public class IccmMedicalReviewServiceImpl implements IccmMedicalReviewService {

    private final FhirUtils fhirUtils;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final PatientService patientService;

    private final HouseholdService householdService;

    private final RestApiUtil restApiUtil;

    private final PrescriptionRequestService prescriptionService;

    private final InvestigationService investigationService;

    @Autowired
    public IccmMedicalReviewServiceImpl(FhirUtils fhirUtils, FhirAssessmentMapper fhirAssessmentMapper, PatientService patientService,
                                        HouseholdService householdService, RestApiUtil restApiUtil, PrescriptionRequestService prescriptionService,
                                        InvestigationService investigationService) {
        this.fhirUtils = fhirUtils;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.patientService = patientService;
        this.householdService = householdService;
        this.restApiUtil = restApiUtil;
        this.prescriptionService = prescriptionService;
        this.investigationService = investigationService;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, String> createMedicalReviewForUnder5years(UnderFiveIccmDTO request) {
        fhirUtils.initiateCodesMap();
        request.getEncounter().setType(Constants.MEDICAL_REVIEW);
        request.getEncounter().setDiagnosisType(
                request.getAssessmentName().equals(Constants.ICCM_UNDER_2M) ? Constants.ICCM_UNDER_2M :
                        Constants.ICCM_ABOVE_2M_5Y);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        patientService.setPatientReferenceInEncounterDetails(request.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(request.getId(),
                request.getEncounter(), request.getAssessmentName(), null, bundle);
        request.setId(encounterId);
        checkAndCloseRmnchDetails(request, bundle);
        createAssessment(request, bundle);
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, String> response = new HashMap<>();
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(fhirResponse.get(String.valueOf(ResourceType.Encounter)))
                && !fhirResponse.get(String.valueOf(ResourceType.Encounter)).isEmpty()
                ? fhirResponse.get(String.valueOf(ResourceType.Encounter)).getFirst()
                : fhirUtils.getIdFromReference(request.getId()));

        response.put(Constants.KEY_PATIENT_REFERENCE, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                !fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() :
                fhirUtils.getIdFromReference(request.getEncounter().getPatientReference()));
        return response;
    }

    /**
     * Creates assessment for under5.
     *
     * @param assessment assessment Details
     * @param bundle     Bundle Object
     */
    private void createAssessment(UnderFiveIccmDTO assessment, Bundle bundle) {
        if (!Objects.isNull(assessment.getClinicalSummaryAndSigns())) {
            createClinicalSummary(assessment, bundle);
        }
        if (!Objects.isNull(assessment.getExamination())) {
            createExamination(assessment, bundle);
        }
        if (!Objects.isNull(assessment.getPresentingComplaints())) {
            Observation observation = createNotes(Constants.PRESENTING_COMPLAINTS,
                    assessment.getPresentingComplaints(), assessment);
            addObservationToBundle(observation, bundle, assessment);
        }
        if (!Objects.isNull(assessment.getClinicalNotes())) {
            Observation observation = createNotes(Constants.CLINICAL_NOTES,
                    assessment.getClinicalNotes(), assessment);
            addObservationToBundle(observation, bundle, assessment);
        }
        if (!Objects.isNull(assessment.getSystemicExamination())) {
            Observation observation = createNotes(Constants.SYSTEMIC_EXAMINATIONS,
                    assessment.getSystemicExaminationNotes(), assessment);
            for (String sign : assessment.getSystemicExamination()) {
                observation.getComponent().add(fhirAssessmentMapper.createObservationComponent(sign));
            }
            addObservationToBundle(observation, bundle, assessment);
        }
    }

    /**
     * Adds a observation to the bundle.
     *
     * @param observation Observation Deatils
     * @param assessment  assessment Details
     * @param bundle      Bundle Object
     * @return String
     */
    private String addObservationToBundle(Observation observation, Bundle bundle, UnderFiveIccmDTO assessment) {
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        observation.setEncounter(new Reference(assessment.getId()));
        observation.setSubject(new Reference(assessment.getEncounter().getPatientReference()));
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle, assessment.getEncounter().getProvenance());
        return url;
    }

    /**
     * Creates a Examination.
     *
     * @param assessment assessment Details
     * @param bundle     Bundle Object
     */
    private void createExamination(UnderFiveIccmDTO assessment, Bundle bundle) {
        UnderFiveIccmDTO.Examination examination = assessment.getExamination();

        if (!Objects.isNull(examination.getJaundice())) {
            addObservationToBundle(createJaundice(examination.getJaundice(), assessment), bundle, assessment);
        }
        if (!Objects.isNull(examination.getBreastfeedingProblem())) {
            Observation breastfeeding = createBreastfeedingProblem(examination.getBreastfeedingProblem(), assessment);
            String url = addObservationToBundle(breastfeeding, bundle, assessment);
            if (Boolean.TRUE.equals(examination.getBreastfeedingProblem().getUnderweight())) {
                createCondition(Constants.UNDER_WEIGHT, url, assessment, bundle);
            }
            if (Boolean.TRUE.equals(examination.getBreastfeedingProblem().getMouthUlcersOrThrush())) {
                createCondition(Constants.MOUTH_ULCER_OR_THRUSH, url, assessment, bundle);
            }
        }
        if (!Objects.isNull(examination.getDiarrhoea())) {
            String url = addObservationToBundle(createDiarrhea(examination.getDiarrhoea(), assessment), bundle,
                    assessment);
            if (!Objects.isNull(examination.getDiarrhoea().getSigns())) {
                addObservationToBundle(createSigns(examination.getDiarrhoea().getSigns(),
                        Constants.OBSERVATION_DIARRHOEA_SIGNS, url, assessment), bundle, assessment);
            }
        }
        if (!Objects.isNull(examination.getNonBreastfeedingProblem())) {
            addObservationToBundle(createNonBreastfeedingProblem(examination.getNonBreastfeedingProblem(), assessment),
                    bundle, assessment);
        }
        if (!Objects.isNull(examination.getVerySevereDisease())) {
            addObservationToBundle(createVerySevereDisease(examination.getVerySevereDisease(), assessment), bundle,
                    assessment);
        }

        if (!Objects.isNull(examination.getHivInfection())) {
            addObservationToBundle(createHivInfection(examination.getHivInfection(), assessment), bundle, assessment);
        }

        if (!Objects.isNull(examination.getGeneralDangerSigns())) {
            addObservationToBundle(createGeneralDangerSigns(examination.getGeneralDangerSigns(), assessment), bundle,
                    assessment);
        }

        if (!Objects.isNull(examination.getCough())) {
            addObservationToBundle(createCough(examination.getCough(), assessment), bundle, assessment);
        }
        if (!Objects.isNull(examination.getFever())) {
            String url = addObservationToBundle(createFever(examination.getFever(), assessment), bundle, assessment);
            if (!Objects.isNull(examination.getFever().getSigns())) {
                addObservationToBundle(
                        createSigns(examination.getFever().getSigns(), Constants.OBSERVATION_FEVER_SIGNS,
                                url, assessment), bundle, assessment);
            }
        }
        if (!Objects.isNull(examination.getEarProblem())) {
            addObservationToBundle(createEarProblem(examination.getEarProblem(), assessment), bundle, assessment);
        }
        if (!Objects.isNull(examination.getAnaemia())) {
            String url = addObservationToBundle(createAnaemia(examination.getAnaemia(), assessment), bundle,
                    assessment);
            if (!Objects.isNull(examination.getAnaemia().getSigns())) {
                addObservationToBundle(
                        createSigns(examination.getAnaemia().getSigns(), Constants.OBSERVATION_ANAEMIA_SIGNS,
                                url, assessment), bundle, assessment);
            }
        }
        if (!Objects.isNull(examination.getHivRDT())) {
            addObservationToBundle(createHIVRDT(examination.getHivRDT(), assessment), bundle, assessment);
        }
    }

    /**
     * Mapping for hiv infection observation.
     *
     * @param hivInfection HIV Details
     * @return Observation Object
     */
    private Observation createHivInfection(HIVInfectionDTO hivInfection, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_HIV_INFECTION, Constants.OBSERVATION_HIV_INFECTION);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(hivInfection.getHasPositiveVirologicalTestForInfant(),
                Constants.POSITIVE_VIROLOGICAL_TEST_INFANT, componentComponents);
        fhirAssessmentMapper.createObservationComponent(hivInfection.getIsMotherPostiveAndChildNegative(),
                Constants.IS_MOTHER_POSITIVE_AND_CHILD_NEGATIVE, componentComponents);
        fhirAssessmentMapper.createObservationComponent(hivInfection.getHasPositiveAntibodyTestForInfant(),
                Constants.POSITIVE_ANTIBODY_FOR_INFANT, componentComponents);
        fhirAssessmentMapper.createObservationComponent(hivInfection.getIsMotherPostiveAndInfantNotTested(),
                Constants.IS_MOTHER_POSITIVE_AND_INFANT_NOT_TESTED, componentComponents);
        fhirAssessmentMapper.createObservationComponent(hivInfection.getHasNegativeForMotherAndChild(),
                Constants.NEGATIVE_MOTHER_AND_CHILD, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for jaundice observation.
     *
     * @param jaundice jaundice Details
     * @return Observation Object
     */
    private Observation createJaundice(JaundiceDTO jaundice, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_JAUNDICE, Constants.OBSERVATION_JAUNDICE);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(jaundice.getYellowSkinLessThan24hrs(),
                Constants.YELLOW_SKIN_OR_FACE_LESS_THAN_24_HRS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(jaundice.getYellowPalmsAndSoles(),
                Constants.YELLOW_PALMS_AND_SOLES, componentComponents);
        fhirAssessmentMapper.createObservationComponent(jaundice.getJaundiceAppearing(),
                Constants.JAUNDICE_APPEARING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(jaundice.getNoJaundice(), Constants.NO_JAUNDICE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(jaundice.getSolesNotYellow(),
                Constants.SOLES_NOT_YELLOW, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for verySevereDisease observation.
     *
     * @param verySevereDisease verySevereDisease Details
     * @return Observation object
     */
    private Observation createVerySevereDisease(VerySevereDiseaseDTO verySevereDisease, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_VERY_RARE_DISEASE, Constants.OBSERVATION_VERY_RARE_DISEASE);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getStoppedFeeding(),
                Constants.STOPED_FEEDING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getConvulsions(),
                Constants.CONVULSIONS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getLowBodyTemperature(),
                Constants.LOW_BODY_TEMPERATURE, componentComponents);
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getMovementOnStimulation(),
                Constants.MOVE_ONLY_WHEN_STIMULATED_OR_NO_MOVEMENT_WHEN_STIMULATED, componentComponents);
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getSkinPustules(),
                Constants.SKIN_PUSTULES, componentComponents);
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getUmbilicusRedOrDrainingPus(),
                Constants.UNBILICUS_RED_OR_DRAINING_PUS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(verySevereDisease.getSevereChestIndrawing(),
                Constants.SEVERE_CHEST_INDRAWING, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for diarrhoea observation.
     *
     * @param diarrhoea diarrhoea Details
     * @return Observation object
     */
    private Observation createDiarrhea(DiarrhoeaDTO diarrhoea, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_DIARRHOEA, Constants.OBSERVATION_DIARRHOEA);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getBloodInStool(),
                Constants.BLOOD_IN_STOOL, componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getMovementOnStimulation(),
                Constants.MOVES_WHEN_STIMULATED, componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getNoMovementOnStimulation(),
                Constants.NO_MOVEMENT_ON_STIMILATION, componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getSkinPinch(), Constants.SKIN_PINCH,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getSunkenEyes(), Constants.SUNKEN_EYES,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getTimePeriod(), Constants.TIME_PERIOD,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getRestlessOrIrritable(),
                Constants.RESTLESS_OR_IRRITABLE, componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getHasDiarrhoea(), Constants.HAS_DIARRHOEA,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(diarrhoea.getBloodyDiarrhoea(),
                Constants.BLOODY_DIARRHOEA, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for breastfeedingProblem observation.
     *
     * @param breastfeedingProblem breastfeedingProblem Details
     * @return Observation object
     */
    private Observation createBreastfeedingProblem(BreastfeedingProblemDTO breastfeedingProblem,
                                                   UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_BREAST_FEEDING_PROBLEM,
                Constants.OBSERVATION_BREAST_FEEDING_PROBLEM);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getAnyBreastfeedingDifficulty(),
                Constants.ANY_BREAST_FEEDING_DIFFICULTY, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getLessThan8BreastfeedIn24hrs(),
                Constants.LESS_THAN_8_BREASTFEEDS_IN_24_HRS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getSwitchingBreastFrequently(),
                Constants.SWITCHING_BREAST_FREQUENTLY, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getNotIncreasingBFInIllness(),
                Constants.NOT_INCREASING_BF_DURING_ILLNESS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getReceivesOtherFoodsOrDrinks(),
                Constants.RECEIVES_OTHER_FOOD_OR_DRINKS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getPositioning(),
                Constants.POSITIONING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getAttachment(),
                Constants.ATTACHMENT, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getSuckling(),
                Constants.SUCKLING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getNoFeedingProblem(),
                Constants.NO_FEEDING_PROBLEM, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getUnderweight(),
                Constants.UNDER_WEIGHT, componentComponents);
        fhirAssessmentMapper.createObservationComponent(breastfeedingProblem.getMouthUlcersOrThrush(),
                Constants.MOUTH_ULCER_OR_THRUSH, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Create condition for diagnosis.
     *
     * @param diagnosis     diagnosis Details
     * @param assessmentUrl Assessment Url
     * @param assessment    Assessment Details
     * @param bundle        Bundle Object
     */
    private void createCondition(String diagnosis, String assessmentUrl, UnderFiveIccmDTO assessment, Bundle bundle) {
        Condition condition = new Condition();
        condition.setSubject(new Reference(assessment.getEncounter().getPatientReference()));
        condition.setEncounter(new Reference(assessment.getId()));
        condition.getRecordedDateElement().setValue(new Date());
        condition.setRecorder(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Practitioner), Constants.FORWARD_SLASH, assessment.getEncounter().getProvenance().getUserId()))); // user
        ConditionStageComponent conditionStageComponent = new ConditionStageComponent();
        conditionStageComponent.setAssessment(List.of(new Reference(assessmentUrl)));
        condition.setStage(List.of(conditionStageComponent));
        condition.setCode(fhirUtils.createCodeableConcept(diagnosis));
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, condition, bundle, assessment.getEncounter().getProvenance());
    }

    /**
     * Mapping for breastfeedingProblem observation.
     *
     * @param nonBreastfeedingProblem nonBreastfeedingProblem Object
     * @return Observation Object
     */
    private Observation createNonBreastfeedingProblem(NonBreastfeedingProblemDTO nonBreastfeedingProblem,
                                                      UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_NON_BREAST_FEEDING_PROBLEM,
                Constants.OBSERVATION_NON_BREAST_FEEDING_PROBLEM);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getIncorrectlyPreparedMilk(),
                Constants.INCORRECTLY_PREPARED_MILK, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getInsufficientReplacementFeeds(),
                Constants.INSUFFICENT_REPLACEMENT_FEEDS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getInappropriateReplacementFeeds(),
                Constants.INAPPROPRIATE_REPLACEMENT_FEEDS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getBottleFeeding(),
                Constants.BOTTLE_FEEDING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getFeedFormHIVPositiveMother(),
                Constants.HIV_MOTHER_MIXING_FEEDS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getLowWeightForAge(),
                Constants.LOW_WEIGHT_FOR_AGE, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getThrush(),
                Constants.THRUSH, componentComponents);
        fhirAssessmentMapper.createObservationComponent(nonBreastfeedingProblem.getUseOfFeedingBottle(),
                Constants.USE_OF_FEEDING_BOTTLE, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Creates notes mapping.
     *
     * @param title Notes Title
     * @param notes Notes Value
     * @return Observation
     */
    private Observation createNotes(String title, String notes, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(), title, title);
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(notes);
        observation.setValue(codeableConcept);
        return observation;
    }

    /**
     * Creates a clinical
     *
     * @param assessment Assessment Details
     * @param bundle     Bundle Object
     */
    private void createClinicalSummary(UnderFiveIccmDTO assessment, Bundle bundle) {
        ClinicalSummaryAndSignsDTO clinicalSummaryAndSigns = assessment.getClinicalSummaryAndSigns();
        if (!Objects.isNull(clinicalSummaryAndSigns.getHeight()) || !Objects.isNull(clinicalSummaryAndSigns.getWhz())) {
            Observation heightObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                    Constants.HEIGHT, Constants.HEIGHT);
            if (!Objects.isNull(clinicalSummaryAndSigns.getHeight())) {
                Quantity heightQuantity = new Quantity(null, clinicalSummaryAndSigns.getHeight(), null, null,
                        clinicalSummaryAndSigns.getHeightUnit());
                heightObservation.setValue(heightQuantity);
            }

            if (!Objects.isNull(clinicalSummaryAndSigns.getWhz())) {
                ObservationComponentComponent heightObservationComponent = fhirAssessmentMapper.createObservationComponent(
                        Constants.WHZ);
                heightObservationComponent.setValue(new Quantity(clinicalSummaryAndSigns.getWhz()));
                heightObservation.getComponent().add(heightObservationComponent);
            }
            heightObservation.setEncounter(new Reference(assessment.getId()));
            addObservationToBundle(heightObservation, bundle, assessment);
        }

        if (!Objects.isNull(clinicalSummaryAndSigns.getWeight()) || !Objects.isNull(clinicalSummaryAndSigns.getWaz())) {
            Observation weightObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                    Constants.WEIGHT, Constants.WEIGHT);
            if (!Objects.isNull(clinicalSummaryAndSigns.getWeight())) {
                Quantity weightQuantity = new Quantity(null, clinicalSummaryAndSigns.getWeight(), null, null,
                        clinicalSummaryAndSigns.getWeightUnit());
                weightObservation.setValue(weightQuantity);
            }

            if (!Objects.isNull(clinicalSummaryAndSigns.getWaz())) {
                ObservationComponentComponent weightObservationComponent = fhirAssessmentMapper.createObservationComponent(
                        Constants.WAZ);
                weightObservationComponent.setValue(new Quantity(clinicalSummaryAndSigns.getWaz()));
                weightObservation.getComponent().add(weightObservationComponent);
            }
            addObservationToBundle(weightObservation, bundle, assessment);
        }

        if (!Objects.isNull(clinicalSummaryAndSigns.getTemperature())) {
            Observation temperatureObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                    Constants.TEMPERATURE, Constants.TEMPERATURE);
            Quantity temperatureQuantity = new Quantity(null, clinicalSummaryAndSigns.getTemperature(), null, null, clinicalSummaryAndSigns.getTemperatureUnit());
            temperatureObservation.setValue(temperatureQuantity);
            addObservationToBundle(temperatureObservation, bundle, assessment);
        }
        Observation signsObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_CLINICAL_SUMMARY, Constants.OBSERVATION_CLINICAL_SUMMARY);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        createMedicationDispense(bundle, Constants.ALBENDAZOLE, assessment.getEncounter(),
                Boolean.TRUE.equals(clinicalSummaryAndSigns.getAlbendazole()) ? Constants.COMPLETED :
                        Constants.UNKNOWN);
        fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getAlbendazole(), Constants.ALBENDAZOLE, componentComponents);
        fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getVitAForMother(), Constants.VIT_A_FOR_MOTHER, componentComponents);
        fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getImmunisationStatus(), Constants.IMMUNISATION_STATUS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getBreastFeeding(), Constants.BREAST_FEEDING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getExclusiveBreastFeeding(), Constants.EXCLUSIVELY_BREASTFEEDING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getMuacStatus(), Constants.MUAC, componentComponents);
        if (!Objects.isNull(clinicalSummaryAndSigns.getMuacInCentimeter())) {
            fhirAssessmentMapper.createObservationComponent(clinicalSummaryAndSigns.getMuacInCentimeter(), Constants.MUAC_CM, componentComponents, Constants.CM);
        }
        if (!Objects.isNull(clinicalSummaryAndSigns.getRespirationRate())) {
            for (Double rate : clinicalSummaryAndSigns.getRespirationRate()) {
                ObservationComponentComponent observationComponent = fhirAssessmentMapper.createObservationComponent(Constants.RESPIRATION_RATE);
                Quantity respirationQuantity = new Quantity(null, rate, null, null, Constants.MIN);
                observationComponent.setValue(respirationQuantity);
                componentComponents.add(observationComponent);
            }
        }
        signsObservation.setComponent(componentComponents);
        addObservationToBundle(signsObservation, bundle, assessment);
    }

    /**
     * Create Medication Dispense for the patient
     *
     * @param bundle              Bundle Object
     * @param name                Medication Name
     * @param encounterDetailsDTO Assessment Details
     * @return String medicationDispense reference
     */
    private String createMedicationDispense(Bundle bundle, String name, EncounterDetailsDTO encounterDetailsDTO,
                                            String status) {
        String uuidMedication = fhirUtils.getUniqueId();
        MedicationDispense medicationDispense = fhirAssessmentMapper.setMedicationDispense(encounterDetailsDTO, name,
                status, encounterDetailsDTO.getId());
        String medicationUrl = StringUtil.concatString(String.valueOf(ResourceType.MedicationDispense),
                Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuidMedication);
        String fullMedicationUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuidMedication);
        fhirUtils.setBundle(medicationUrl, fullMedicationUrl, Bundle.HTTPVerb.POST, medicationDispense, bundle,
                encounterDetailsDTO.getProvenance());
        return medicationUrl;
    }

    /**
     * {@inheritDoc}
     */
    public IccmResponseDTO getMedicalReviewSummaryByEncounter(MedicalReviewRequestDTO request,
                                                              List<String> examinationNames, String diagnosisType) {
        fhirUtils.initiateCodesMap();
        IccmResponseDTO response = new IccmResponseDTO();
        List<DiagnosisDTO.DiseaseDTO> diseases = new ArrayList<>();
        if (!Objects.isNull(request.getEncounterId())) {
            response.setPrescriptions(prescriptionService.getPrescriptionsByEncounter(request.getEncounterId(),
                    request.getPatientReference()));
            response.setInvestigations(investigationService.getInvestigationsByEncounter(request.getEncounterId(),
                    request.getPatientReference()));
            Bundle bundle = fhirAssessmentMapper.getEncounterDetails(request.getEncounterId(), Boolean.TRUE);
            if (!Objects.isNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
                bundle.getEntry().forEach(resource -> {
                    if (ResourceType.Observation.equals(resource.getResource().getResourceType())) {
                        Observation observation = (Observation) resource.getResource();
                        String keyName = observation.getCode().getText();
                        if (keyName.equals(Constants.PRESENTING_COMPLAINTS)) {
                            response.setPresentingComplaints(observation.getValueCodeableConcept().getText());
                        } else if (keyName.equals(Constants.CLINICAL_NOTES)) {
                            response.setClinicalNotes(observation.getValueCodeableConcept().getText());
                        } else if (keyName.equals(Constants.SYSTEMIC_EXAMINATIONS)) {
                            response.setSystemicExamination(observation.getComponent().stream()
                                    .map(component -> fhirUtils.getText(component.getCode().getText()))
                                    .toList());
                            response.setSystemicExaminationNotes(observation.getValueCodeableConcept().getText());
                        } else if (examinationNames.contains(keyName)) {
                            if (Objects.isNull(response.getExamination())) {
                                response.setExamination(new HashMap<>());
                            }
                            if (!response.getExamination().containsKey(keyName)) {
                                response.getExamination().put(keyName, new ArrayList<>());
                            }
                            response.getExamination().get(keyName)
                                    .addAll(setObservationComponent(observation.getComponent()));
                        } else if (Constants.UNDER_5_SIGNS.containsKey(keyName)) {
                            String signKeyName = Constants.UNDER_5_SIGNS.get(keyName);
                            if (!response.getExamination().containsKey(signKeyName)) {
                                response.getExamination().put(signKeyName, new ArrayList<>());
                            }
                            List<String> signs = observation.getComponent().stream()
                                    .map(component -> fhirUtils.getText(component.getCode().getText())).toList();
                            IccmResponseDTO.ExaminationQA examinationQA = new IccmResponseDTO.ExaminationQA();
                            examinationQA.setTitle(Constants.SIGNS_DISPLAY_NAME);
                            examinationQA.setValue(signs);
                            response.getExamination().get(signKeyName).add(examinationQA);
                        }
                    } else if (ResourceType.Condition.equals(resource.getResource().getResourceType())) {
                        diseases.add(
                                fhirAssessmentMapper.mapToDiagnosis((Condition) resource.getResource(), Boolean.TRUE,
                                        diagnosisType));
                    } else if (ResourceType.Encounter.equals(resource.getResource().getResourceType())) {
                        Encounter encounter = (Encounter) resource.getResource();
                        encounter.getIdentifier().forEach(identifier -> {
                            if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && !Objects.isNull(identifier.getValue())) {
                                response.setPatientStatus(Constants.PATIENT_STATUS_DISPLAY_NAMES.get(identifier.getValue().toLowerCase()));
                            }
                        });
                    } else if (ResourceType.Patient.equals(resource.getResource().getResourceType())) {
                        Patient patient = (Patient) resource.getResource();
                        response.setSummaryStatus(fhirAssessmentMapper.getSummaryStatus(patient));
                    }
                });
            }
        }
        response.setDiagnosis(diseases);
        return response;
    }

    /**
     * Sets a observation Components from observation resource.
     *
     * @param componentComponents Observation Components
     * @return List<Map < String, Object>>
     */
    private List<IccmResponseDTO.ExaminationQA> setObservationComponent(List<ObservationComponentComponent> componentComponents) {
        List<IccmResponseDTO.ExaminationQA> componentResponse = new ArrayList<>();
        componentComponents.forEach(component -> {
            Object value = null;
            String keyName = component.getCode().getText();
            IccmResponseDTO.ExaminationQA examinationQA = new IccmResponseDTO.ExaminationQA();
            if (component.getValue() instanceof CodeableConcept) {
                value = component.getValueCodeableConcept().getText();
                if (Constants.YES.equals(value) || Constants.NO.equals(value)) {
                    value = Constants.YES.equals(value);
                }
            } else if (component.getValue() instanceof Quantity) {
                Quantity quantity = component.getValueQuantity();
                value = quantity.getValueElement().asStringValue();
            } else if (component.getValue() instanceof StringType) {
                value = component.getValueStringType().getValue();
            }
            examinationQA.setTitle(!Objects.isNull(fhirUtils.getCodeDetails().get(keyName)) ?
                    fhirUtils.getCodeDetails().get(keyName).getText() : null);
            examinationQA.setValue(value);
            componentResponse.add(examinationQA);
        });

        return componentResponse;
    }

    /**
     * Mapping for general danger signs observation.
     *
     * @param generalDangerSigns DangerSigns List
     * @return Observation Object
     */
    private Observation createGeneralDangerSigns(GeneralDangerSigns generalDangerSigns, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_GENERAL_DANGER_SIGNS, Constants.OBSERVATION_GENERAL_DANGER_SIGNS);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(generalDangerSigns.getConvulsingNow(),
                Constants.CONVULSING_NOW, componentComponents);
        fhirAssessmentMapper.createObservationComponent(generalDangerSigns.getUnableToDrinkOrBreastfeed(),
                Constants.UNABLE_TO_DRINK_OR_BREASTFEED, componentComponents);
        fhirAssessmentMapper.createObservationComponent(generalDangerSigns.getVomitingEverything(),
                Constants.VOMITING_EVERYTHING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(generalDangerSigns.getHistoryOfConvulsion(),
                Constants.HISTORY_OF_CONVULSION, componentComponents);
        fhirAssessmentMapper.createObservationComponent(generalDangerSigns.getLethargicOrUnconscious(),
                Constants.LETHARGIC_OR_UNCONSCIOUS, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for cough observation.
     *
     * @param cough cough Details
     * @return Observation object
     */
    private Observation createCough(CoughDTO cough, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_COUGH, Constants.OBSERVATION_COUGH);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(cough.getCoughOrDIfficultBreathing(),
                Constants.COUGH_OR_DIFFICULT_BREATHING, componentComponents);
        fhirAssessmentMapper.createObservationComponent(cough.getNoOfDays(), Constants.DAYS,
                Constants.NO_OF_DAYS_OF_COUGH,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(cough.getChestIndrawing(), Constants.CHEST_INDRAWING,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(cough.getStridor(), Constants.STRIDOR,
                componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for fever observation.
     *
     * @param fever Fever Details
     * @return Observation object
     */
    private Observation createFever(FeverDTO fever, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_FEVER, Constants.OBSERVATION_FEVER);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(fever.getHasFever(), Constants.FEVER,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(fever.getIsMotherHasFever(),
                Constants.IS_MOTHER_HAS_FEVER, componentComponents);
        fhirAssessmentMapper.createObservationComponent(fever.getNoOfDays(), Constants.DAYS,
                Constants.NO_OF_DAYS, componentComponents);
        fhirAssessmentMapper.createObservationComponent(fever.getMicroscopyResult(),
                Constants.MICROSCOPY_RESULT, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for ear problem observation.
     *
     * @param earProblem earProblem Details
     * @return Observation Object
     */
    private Observation createEarProblem(EarProblemDTO earProblem, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_EAR_PROBLEM, Constants.OBSERVATION_EAR_PROBLEM);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(earProblem.getEarDischarge(),
                Constants.EAR_DISCHARGE, componentComponents);
        fhirAssessmentMapper.createObservationComponent(earProblem.getHasEarPain(), Constants.HAS_EAR_PAIN,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(earProblem.getNoOfDays(), Constants.DAYS,
                Constants.NO_OF_DAYS, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping for anaemia observation.
     *
     * @param anaemia anaemia Details
     * @return Observation Object
     */
    private Observation createAnaemia(AnaemiaDTO anaemia, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_ANAEMIA, Constants.OBSERVATION_ANAEMIA);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(anaemia.getAppetiteTest(), Constants.APPETITE_TEST,
                componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * Mapping signs.
     *
     * @param signs           List of signs
     * @param observationName Observation Name
     * @param refUrl          reference Url
     * @return Observation Object
     */
    private Observation createSigns(List<String> signs, String observationName, String refUrl,
                                    UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                observationName, observationName);
        observation.addHasMember(new Reference(refUrl));
        signs.forEach(sign -> fhirAssessmentMapper.createObservationComponent(Boolean.TRUE, sign, observation.getComponent()));
        return observation;
    }

    /**
     * Creates hiv rdt test observation.
     *
     * @param hivrdt hivrdt Details
     * @return observation
     */
    private Observation createHIVRDT(HivRdtTestDTO hivrdt, UnderFiveIccmDTO assessment) {
        Observation observation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.OBSERVATION_HIV_RDT, Constants.OBSERVATION_HIV_RDT);
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();
        fhirAssessmentMapper.createObservationComponent(hivrdt.getMother(), Constants.MOTHER, componentComponents);
        fhirAssessmentMapper.createObservationComponent(hivrdt.getChild(), Constants.CHILD, componentComponents);
        observation.setComponent(componentComponents);
        return observation;
    }

    /**
     * {@inheritDoc}
     */
    public IccmResponseDTO getMedicalReviewDetailsForUnderFive(MedicalReviewRequestDTO request) {
        IccmResponseDTO response = null;
        if (Constants.ICCM_ABOVE_2M_5Y.equals(request.getAssessmentName())) {
            response = getMedicalReviewSummaryByEncounter(request, Constants.UNDER_5_YEARS_EXAMINATION,
                    Constants.ICCM_ABOVE_2M_5Y);
            response.setExaminationDisplayNames(Constants.UNDER_5_YEARS_EXAMINATION_DISPLAY_NAMES);
        } else if (Constants.ICCM_UNDER_2M.equals(request.getAssessmentName())) {
            response = getMedicalReviewSummaryByEncounter(request, Constants.UNDER_2_MONTHS_EXAMINATION,
                    Constants.ICCM_UNDER_2M);
            response.setExaminationDisplayNames(Constants.UNDER_2_MONTHS_EXAMINATION_DISPLAY_NAMES_MAP);
        }
        return response;
    }

    /**
     * Check and close RMNCH details
     *
     * @param underFiveIccmDTO Under five medical review Details
     * @param bundle           Bundle object
     */
    public void checkAndCloseRmnchDetails(UnderFiveIccmDTO underFiveIccmDTO, Bundle bundle) {
        HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberById(underFiveIccmDTO.getEncounter().getMemberId());
        PregnancyInfo pregnancyInfo = getPatientVitals(underFiveIccmDTO.getEncounter().getMemberId());
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setProvenance(underFiveIccmDTO.getEncounter().getProvenance());
        requestDTO.setMemberId(underFiveIccmDTO.getEncounter().getMemberId());
        requestDTO.setEncounterId(underFiveIccmDTO.getEncounter().getId());
        requestDTO.setClosedEncounterType(underFiveIccmDTO.getAssessmentName());
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.CHILD_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_CHILHOOD_REASON);
            patientService.closeChildhoodDetails(bundle, requestDTO);
        }
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_CHILD_REASON);
            patientService.closePncNeonateDetails(bundle, requestDTO);
        }
        Date pncDeliveryDate = Objects.nonNull(pregnancyInfo.getDateOfDelivery()) ? pregnancyInfo.getDateOfDelivery() :
                pregnancyInfo.getPncCreatedDate();
        if (Objects.nonNull(pncDeliveryDate) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(pncDeliveryDate)) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_MOTHER_REASON);
            patientService.closePncDetails(bundle, requestDTO);
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
}