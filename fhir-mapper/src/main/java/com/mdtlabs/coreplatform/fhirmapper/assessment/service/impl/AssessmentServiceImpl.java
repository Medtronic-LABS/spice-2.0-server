package com.mdtlabs.coreplatform.fhirmapper.assessment.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CarePlan;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.codesystems.LocationPhysicalType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.CqlApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.SpiceServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AncDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentTreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FeverDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ICCMDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.OtherSymptomsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncChildDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMotherDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncNeonatalDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanFrequencyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodGlucoseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodPressureConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.ComplianceConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.QuestionnaireResponseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.MentalHealthService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;

/**
 * <p>
 * This class is a service class to perform operation on Assessment
 * operations.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Feb 05, 2024
 */
@Service
public class AssessmentServiceImpl implements AssessmentService {

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    @Value("${app.smart-anc}")
    private Boolean isSmartAnc;

    private final FhirAssessmentMapper fhirAssessmentMapper;
    private final FhirUtils fhirUtils;
    private final RestApiUtil restApiUtil;
    private final MentalHealthService mentalHealthService;
    private final PatientService patientService;
    private final HouseholdService householdService;
    private final CqlApiInterface cqlApiInterface;
    private final QuestionnaireResponseConverter questionnaireResponseConverter;
    private final BloodPressureConverter bloodPressureConverter;
    private final BloodGlucoseConverter bloodGlucoseConverter;
    private final EncounterConverter encounterConverter;
    private final CommonConverter commonConverter;
    private final PatientConverter patientConverter;
    private final PregnancyConverter pregnancyConverter;
    private final SymptomConverter symptomConverter;
    private final SpiceConverter spiceConverter;
    private final ComplianceConverter complianceConverter;
    private final VitalSignsConverter vitalSignsConverter;
    private final PatientTreatmentPlanService patientTreatmentPlanService;
    private final SpiceServiceApiInterface spiceServiceApiInterface;

    public AssessmentServiceImpl(FhirAssessmentMapper fhirAssessmentMapper, FhirUtils fhirUtils, RestApiUtil restApiUtil, MentalHealthService mentalHealthService, PatientService patientService, HouseholdService householdService, BloodPressureConverter bloodPressureConverter, BloodGlucoseConverter bloodGlucoseConverter,
                                 EncounterConverter encounterConverter,
                                 CommonConverter commonConverter, VitalSignsConverter vitalSignsConverter,
                                 QuestionnaireResponseConverter questionnaireResponseConverter,
                                 PatientConverter patientConverter, PregnancyConverter pregnancyConverter,
                                 SymptomConverter symptomConverter, ComplianceConverter complianceConverter,
                                 PatientTreatmentPlanService patientTreatmentPlanService,
                                 SpiceServiceApiInterface spiceServiceApiInterface, SpiceConverter spiceConverter,
                                 CqlApiInterface cqlApiInterface) {
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.mentalHealthService = mentalHealthService;
        this.patientService = patientService;
        this.householdService = householdService;
        this.questionnaireResponseConverter = questionnaireResponseConverter;
        this.vitalSignsConverter = vitalSignsConverter;
        this.bloodPressureConverter = bloodPressureConverter;
        this.bloodGlucoseConverter = bloodGlucoseConverter;
        this.encounterConverter = encounterConverter;
        this.commonConverter = commonConverter;
        this.patientConverter = patientConverter;
        this.pregnancyConverter = pregnancyConverter;
        this.symptomConverter = symptomConverter;
        this.complianceConverter = complianceConverter;
        this.patientTreatmentPlanService = patientTreatmentPlanService;
        this.spiceServiceApiInterface = spiceServiceApiInterface;
        this.spiceConverter = spiceConverter;
        this.cqlApiInterface = cqlApiInterface;
    }


    @Override
    public AssessmentDTO createAssessment(AssessmentDTO assessmentDTO) {
        if (Constants.NON_COMMUNITY.equals(assessmentDTO.getAssessmentType())) {
            createNcdAssessment(assessmentDTO);
        } else {
            Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            fhirUtils.initiateCodesMap();
            //Create Patient Reference
            if (Boolean.TRUE.equals(assessmentDTO.getEncounter().isReferred())) {
                patientService.setPatientReferenceInEncounterDetails(assessmentDTO.getEncounter(), bundle);
            }
            assessmentDTO.getEncounter().setType(Constants.ASSESSMENT);
            assessmentDTO.getEncounter().setFollowUpId(
                    Objects.nonNull(assessmentDTO.getFollowUp()) ? assessmentDTO.getFollowUp().getId() : null);
            assessmentDTO.getEncounter().setVillageId(assessmentDTO.getVillageId());
            String encounterId = fhirAssessmentMapper.createEncounter(assessmentDTO.getEncounter(), bundle,
                    assessmentDTO.getAssessmentType(), null);
            assessmentDTO.setId(encounterId);
            assessmentDTO.getEncounter().setId(encounterId);
            createAssessment(assessmentDTO, bundle);
            ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                    restApiUtil.constructRequestEntity(bundle));
            if (Objects.isNull(responseEntity.getBody())) {
                throw new Validation(1006);
            }
            Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
            assessmentDTO.setId(response.get(String.valueOf(ResourceType.Encounter)).get(Constants.ZERO));
            if (Boolean.TRUE.equals(isSmartAnc) && Constants.ANC.equals(assessmentDTO.getAssessmentType())
                    && Objects.nonNull(assessmentDTO.getAssessmentDetails()) && Objects.nonNull(
                    assessmentDTO.getAssessmentDetails().getAnc()) && Objects.nonNull(
                    assessmentDTO.getAssessmentDetails().getAnc().getLastMenstrualPeriod()) && Objects.nonNull(
                    assessmentDTO.getAssessmentDetails().getAnc().getEstimatedDeliveryDate())
                    && assessmentDTO.getAssessmentDetails().getAnc().getVisitNo() == Constants.ONE) {
                try {
                    assessmentDTO.setAncContactDetails(new ObjectMapper().writeValueAsString(
                            cqlApiInterface.evaluateByEncounterId(assessmentDTO.getId(), CommonUtil.getAuthToken(),
                                    CommonUtil.getClient()).getBody()));
                } catch (JsonProcessingException e) {
                    throw new ServicesException(1005);
                }
            }
        }
        return assessmentDTO;
    }

    /**
     * {@inheritDoc}
     */
    public AssessmentDTO createNcdAssessment(AssessmentDTO assessmentDTO) {
        if (Objects.isNull(assessmentDTO.getMemberReference())) {
            throw new SpiceValidation(2004);
        }
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url = String.format(Constants.GET_MEMBER_ID, assessmentDTO.getMemberReference());
        Bundle relatedPersonBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();
        ProvenanceDTO provenanceDTO = assessmentDTO.getEncounter().getProvenance();
        provenanceDTO.setModifiedDate(assessmentDTO.getAssessmentTakenOn());
        fhirUtils.initiateCodesMap();
        Patient patient = null;
        if (Objects.isNull(assessmentDTO.getPatientReference()) || StringUtils.isEmpty(assessmentDTO.getPatientReference())) {
            patient = createPatient(assessmentDTO, patient, relatedPerson);
            relatedPerson.setPatient(new Reference(StringUtil.concatString(ResourceType.Patient.toString(),
                    Constants.FORWARD_SLASH, FhirConstants.PATIENT_IDENTIFIER_URL)));
        } else {
            patient = restApiUtil.getPatientById(
                    StringUtil.concatString(fhirServerUrl, FhirConstants.PATIENT, Constants.FORWARD_SLASH,
                            assessmentDTO.getPatientReference()));
        }

        if (Objects.nonNull(patient.getIdentifier())) {
            patient.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    assessmentDTO.setVillageId(identifier.getValue());
                }
            });
        } else if (Objects.isNull(assessmentDTO.getVillageId()) && Objects.nonNull(relatedPerson.getIdentifier())) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    assessmentDTO.setVillageId(identifier.getValue());
                }
            });
        }
        updatePatientStatus(relatedPerson, patient);
        if (!assessmentDTO.isOldRecord()) {
            if (Objects.nonNull(assessmentDTO.getRiskLevel())) {
                commonConverter.updateRiskLevel(Constants.HIGH.equalsIgnoreCase(assessmentDTO.getRiskLevel()) ? Constants.HIGH : Constants.NA,
                        patient);
                commonConverter.updateRiskLevel(Constants.HIGH.equalsIgnoreCase(assessmentDTO.getRiskLevel()) ? Constants.HIGH : Constants.NA,
                        relatedPerson);
            }
            createProvsionalDiagnosisCondition(bundle, assessmentDTO,
                    (Objects.nonNull(assessmentDTO.getPatientReference()) ? assessmentDTO.getPatientReference() : FhirConstants.PATIENT_IDENTIFIER_URL),
                    relatedPerson, provenanceDTO);
        }
        fhirUtils.setBundle(StringUtil.concatString(ResourceType.RelatedPerson.toString(), Constants.FORWARD_SLASH, relatedPerson.getIdPart()),
                Constants.EMPTY_SPACE,
                Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);

        Location location = createLocation(assessmentDTO);

        Encounter encounter = encounterConverter.createEncounter(patient, relatedPerson, location,
                Objects.isNull(assessmentDTO.getAssessmentLocation())
                        ? null
                        : assessmentDTO.getAssessmentLocation().get(Constants.ASSESSMENT_CATEGORY),
                assessmentDTO.getAssessmentTakenOn());
        encounter.setStatus(Encounter.EncounterStatus.INPROGRESS);
        encounter.setServiceProvider(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Organization), Constants.FORWARD_SLASH,
                        assessmentDTO.getAssessmentOrganizationId())));   //orgId
        if ((Objects.nonNull(assessmentDTO.getType()) && assessmentDTO.getType().equals(Constants.MEDICAL_REVIEW))) {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue(Constants.MEDICAL_REVIEW);
        } else {
            encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue(Constants.ASSESSMENT);
            assessmentDTO.setType(Constants.ASSESSMENT);
        }

        Observation bpLogObservation = createBpLogObservation(assessmentDTO, encounter);
        Observation glucoseLogObservation = createGlucoseLogObservation(assessmentDTO, encounter);
        Observation heightObservation = createHeightObservation(assessmentDTO, encounter);
        Observation weightObservation = createWeightObservation(assessmentDTO, encounter);
        Observation bmiObservation = createBmiObservation(assessmentDTO, encounter);
        Observation pregnancyObservation = createPregnancyObservation(assessmentDTO, encounter);
        Observation suicideScreener = createSuicideObservation(assessmentDTO, encounter);
        Observation substanceAbuse = createSubstanceAbuseObservation(assessmentDTO, encounter);
        Observation symptomObservation = createSymptomObservation(assessmentDTO, encounter);
        Observation complianceObservation = createComplianceObservation(assessmentDTO, encounter);
        Observation temperatureObservation = createTemperatureObservation(assessmentDTO, encounter);
        Map<String, QuestionnaireResponse> questionnaireResponses = mentalHealthService.createQuestionnaireResponse(assessmentDTO,
                patient, relatedPerson, encounter, bundle, provenanceDTO);
        Observation mentalHealthObservation = createMentalHealthObservation(assessmentDTO, relatedPerson, questionnaireResponses, encounter);
        Observation redRiskObservation = createRedRiskObservation(assessmentDTO, relatedPerson);

        commonConverter.setPatientDetailsInBundle(bundle, patient,
                FhirConstants.PATIENT_IDENTIFIER_URL, provenanceDTO);
        commonConverter.setEncounterDetailsInBundle(bundle, encounter,
                FhirConstants.ENCOUNTER_IDENTIFIER_URL, provenanceDTO);
        setObservationDetails(bundle, bpLogObservation, FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, glucoseLogObservation, FhirConstants.BLOOD_SUGAR_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, heightObservation, FhirConstants.HEIGHT_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, weightObservation, FhirConstants.WEIGHT_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, bmiObservation, FhirConstants.BMI_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, suicideScreener, FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, substanceAbuse, FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, pregnancyObservation, FhirConstants.PREGNANCY_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, symptomObservation, FhirConstants.SYMPTOM_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, complianceObservation, FhirConstants.COMPLIANCE_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setObservationDetails(bundle, temperatureObservation, FhirConstants.TEMPERATURE_IDENTIFIER_URL,
                assessmentDTO, patient, relatedPerson, provenanceDTO);
        setPatientRiskDetailsInBundle(redRiskObservation, mentalHealthObservation, patient,
                null, provenanceDTO, bundle);
        commonConverter.setLocationDetailsInBundle(bundle, location,
                FhirConstants.LOCATION_IDENTIFIER_URL, provenanceDTO);
        if (!assessmentDTO.isOldRecord()) {
            Observation vitalSignsObservation = createVitalSignsObservation(bundle, relatedPerson,
                    provenanceDTO, assessmentDTO);
            commonConverter.setObservationReference(vitalSignsObservation, patient, null);
            commonConverter.setObservationDetailsInBundle(bundle, vitalSignsObservation,
                    FhirConstants.VITAL_SIGNS_IDENTIFIER_URL, provenanceDTO);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        setResourceIds(assessmentDTO, responseEntity, relatedPerson, patient);
        if (!assessmentDTO.isOldRecord()) {
            createProvisionalTreatmentPlan(assessmentDTO);
        }
        return assessmentDTO;
    }

    /**
     * <p>
     * Creates a FHIR Condition resource based on the provided assessment data, patient, and related person.
     * </p>
     *
     * @param bundle          The FHIR Bundle containing Condition resources
     * @param assessmentDTO   The data transfer object containing assessment details.
     * @param patientReference  The patient associated with the condition.
     * @param relatedPerson   The related person asserting the condition.
     */
    private void createProvsionalDiagnosisCondition(Bundle bundle, AssessmentDTO assessmentDTO, String patientReference,
                                                    RelatedPerson relatedPerson, ProvenanceDTO provenanceDTO) {
        Condition condition = new Condition();
        boolean isConditionExists = Boolean.FALSE;
        String url = String.format(Constants.PROVISIONAL_DIAGNOSIS_QUERY, relatedPerson.getIdPart());
        Bundle conditionBundle = restApiUtil.getBatchRequest(url);
        if (!Objects.isNull(conditionBundle.getEntry()) && !conditionBundle.getEntry().isEmpty()) {
            isConditionExists = Boolean.TRUE;
            for (Bundle.BundleEntryComponent resource : conditionBundle.getEntry()) {
                condition = (Condition) resource.getResource();
            }
        } else {
            condition.setVerificationStatus(fhirUtils.setCodes(Constants.PROVISIONAL));
            condition.setClinicalStatus(fhirUtils.setCodes(FhirConstants.ACTIVE));
            condition.setRecordedDate(new Date());
            condition.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, patientReference));
            condition.setAsserter(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, relatedPerson.getIdPart()));
        }
        List<CodeableConcept> codeableConcepts = new ArrayList<>();
        if (Objects.nonNull(assessmentDTO.getProvisionalDiagnosis()) && !assessmentDTO.getProvisionalDiagnosis().isEmpty()) {
            assessmentDTO.getProvisionalDiagnosis().forEach(diagnosis ->
                codeableConcepts.add(fhirUtils.createCodeableConcept(diagnosis))
            );
        }
        condition.setCategory(codeableConcepts);
        commonConverter.setConditionInBundle(bundle, condition, FhirConstants.CONDITION_IDENTIFIER_URL, isConditionExists ? Boolean.TRUE : Boolean.FALSE,
                provenanceDTO);
    }

    /**
     * <p>
     * Updates the status of the patient.
     * </p>
     *
     * @param relatedPerson The RelatedPerson object whose status needs to be updated.
     * @param patient The Patient object whose status needs to be updated.
     */
    private void updatePatientStatus(RelatedPerson relatedPerson, Patient patient) {
        if (Objects.nonNull(relatedPerson)) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && FhirConstants.SCREENED.equals(
                        identifier.getValue())) {
                    identifier.setValue(FhirConstants.ASSESSED);
                }
            });
        }
        if (Objects.nonNull(patient)) {
            patient.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && FhirConstants.SCREENED.equals(
                        identifier.getValue())) {
                    identifier.setValue(FhirConstants.ASSESSED);
                }
            });
        }
    }

    /**
     * <p>
     * Sets patient mental health and red risk details in bundle
     * </p>
     *
     * @param redRiskObservation          The FHIR Observation entity
     * @param mentalHealthObservation     The FHIR Observation entity
     * @param patient                     The FHIR Patient entity.
     * @param relatedPerson               The FHIR RelatedPerson entity.
     * @param provenanceDTO               The ProvenanceDTO entity to store performers information
     * @param bundle                      The FHIR bundle entity it contains patient vital observations
     */
    private void setPatientRiskDetailsInBundle(Observation redRiskObservation,
                                               Observation mentalHealthObservation,
                                               Patient patient, RelatedPerson relatedPerson,
                                               ProvenanceDTO provenanceDTO, Bundle bundle) {
        if (Objects.nonNull(mentalHealthObservation)) {
            commonConverter.setObservationReference(mentalHealthObservation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, mentalHealthObservation,
                    FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL, provenanceDTO);
        }
        if (Objects.nonNull(redRiskObservation)) {
            commonConverter.setObservationReference(redRiskObservation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, redRiskObservation,
                    FhirConstants.RED_RISK_OBSERVATION_IDENTIFIER_URL, provenanceDTO);
        }
    }

    /**
     * Process and create FHIR vital signs observation based on given
     * bundle details
     *
     * @param bundle                 The FHIR bundle entity it contains patient vital observations.
     * @param relatedPerson          The FHIR RelatedPerson entity.
     * @param provenanceDTO          The ProvenanceDTO entity to store performers information
     * @return Converted FHIR Observation entity.
     */
    private Observation createVitalSignsObservation(Bundle bundle, RelatedPerson relatedPerson,
                                                    ProvenanceDTO provenanceDTO, AssessmentDTO assessmentDTO) {
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(relatedPerson.getIdPart());
        vitalSignsDTO.setProvenanceDTO(provenanceDTO);
        vitalSignsDTO.setBpObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL));
        if (Objects.nonNull(assessmentDTO.getGlucoseLog()) &&
                Objects.nonNull(assessmentDTO.getGlucoseLog().getGlucoseValue())) {
            vitalSignsDTO.setBgObservation(commonConverter
                    .getObservationFromBundleByIdentifier(bundle, FhirConstants.BLOOD_SUGAR_IDENTIFIER_URL));
        }
        vitalSignsDTO.setTemperatureObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.TEMPERATURE_IDENTIFIER_URL));
        vitalSignsDTO.setHeightObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.HEIGHT_IDENTIFIER_URL));
        vitalSignsDTO.setWeightObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.WEIGHT_IDENTIFIER_URL));
        vitalSignsDTO.setBmiObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.BMI_IDENTIFIER_URL));
        vitalSignsDTO.setSubstanceAbuseObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL));
        vitalSignsDTO.setSuicideObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL));
        vitalSignsDTO.setPregnancyObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.PREGNANCY_IDENTIFIER_URL));
        vitalSignsDTO.setMentalHealthObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL));
        vitalSignsDTO.setRedRiskObservation(commonConverter
                .getObservationFromBundleByIdentifier(bundle, FhirConstants.RED_RISK_OBSERVATION_IDENTIFIER_URL));
        Observation observation = vitalSignsConverter
                .createOrUpdateVitalSigns(vitalSignsDTO, bundle);
        commonConverter.setObservationText(observation, FhirConstants.VITAL_SIGNS);
        return observation;
    }

    /**
     * Set the resource id of the patient, encounter and related person into the assessmentDto.
     *
     * @param assessmentDTO assessmentDTO resource for set the resource id of the patient, encounter and related person
     * @param responseEntity responseEntity for fetch the resource id of the patient, encounter
     * @param relatedPerson relatedPerson to fetch the id.
     * @param patient patient to fetch the id.
     */
    private void setResourceIds(AssessmentDTO assessmentDTO, ResponseEntity<FhirResponseDTO> responseEntity,
                                RelatedPerson relatedPerson, Patient patient) {
        if (Objects.nonNull(responseEntity) && Objects.nonNull(responseEntity.getBody())) {
            Map<String, List<String>> fhirIds = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
            List<String> patientIds = fhirIds.get(FhirConstants.PATIENT);
            if (Objects.nonNull(patientIds) && !patientIds.isEmpty()) {
                assessmentDTO.setPatientId(patientIds.getFirst());
                assessmentDTO.setPatientReference(patientIds.getFirst());
            }
            if (Objects.isNull(assessmentDTO.getPatientReference())) {
                assessmentDTO.setPatientId(patient.getIdPart());
                assessmentDTO.setPatientReference(patient.getIdPart());
            }
            List<String> encounterIds = fhirIds.get(FhirConstants.ENCOUNTER);
            if (Objects.nonNull(encounterIds) && !encounterIds.isEmpty()) {
                assessmentDTO.getEncounter().setId(encounterIds.getFirst());
            }
        }
        if (Objects.nonNull(relatedPerson)) {
            assessmentDTO.setMemberReference(relatedPerson.getIdPart());
        }
    }

    /**
     * <p>
     * Create patient red risk observation based on given
     * assessment details
     * </p>
     *
     * @param assessmentDTO            The patient assessment details
     * @param relatedPerson            The Fhir RelatedPerson entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createRedRiskObservation(AssessmentDTO assessmentDTO,
                                                 RelatedPerson relatedPerson) {
        Observation redRiskObservation = null;
        if (Objects.nonNull(assessmentDTO.getRiskLevel()) && !assessmentDTO.isOldRecord()) {
            redRiskObservation = commonConverter.createRiskLevelObservation(assessmentDTO.getRiskLevel(),
                    assessmentDTO.getRiskMessage(), relatedPerson.getIdPart());
        }
        return redRiskObservation;
    }

    /**
     * <p>
     * Create patient mental health observation based on given
     * assessment details
     * </p>
     *
     * @param assessmentDTO            The patient assessment details
     * @param relatedPerson            The Fhir RelatedPerson entity
     * @param questionnaireResponses   The Map of Fhir QuestionnaireResponse entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createMentalHealthObservation(AssessmentDTO assessmentDTO,
                                                      RelatedPerson relatedPerson,
                                                      Map<String, QuestionnaireResponse> questionnaireResponses, Encounter encounter) {
        MentalHealthObservationDTO mentalHealthObservationDTO = new MentalHealthObservationDTO();
        Map<String, String> mentalHealthRiskDetails = new HashMap<>();
        Observation observation = null;

        if (Objects.nonNull(assessmentDTO.getPhq4())) {
            mentalHealthRiskDetails
                    .put(FhirConstants.PHQ4_RISK_LEVEL, assessmentDTO.getPhq4().getRiskLevel());
            mentalHealthRiskDetails.put(FhirConstants.PHQ4_SCORE,
                    String.valueOf(assessmentDTO.getPhq4().getScore()));
            if (!Objects.isNull(assessmentDTO.getPhq4().getMentalHealthDetails())) {
                for (MentalHealthDetailsDTO mentalHealthDetails : assessmentDTO.getPhq4().getMentalHealthDetails()) {
                    if (Constants.ONE == mentalHealthDetails.getDisplayOrder()
                            || Constants.TWO == mentalHealthDetails.getDisplayOrder()) {
                        assessmentDTO.getPhq4().setFirstScore(assessmentDTO.getPhq4().getFirstScore() + mentalHealthDetails.getScore());
                    } else if (Constants.THREE == mentalHealthDetails.getDisplayOrder()
                            || Constants.FOUR == mentalHealthDetails.getDisplayOrder()) {
                        assessmentDTO.getPhq4().setSecondScore(assessmentDTO.getPhq4().getSecondScore() + mentalHealthDetails.getScore());
                    }
                }
                mentalHealthRiskDetails.put(FhirConstants.PHQ4_FIRST_SCORE,
                        String.valueOf(assessmentDTO.getPhq4().getFirstScore()));
                mentalHealthRiskDetails.put(FhirConstants.PHQ4_SECOND_SCORE,
                        String.valueOf(assessmentDTO.getPhq4().getSecondScore()));
            }
        }
        if (Objects.nonNull(assessmentDTO.getPhq9())) {
            mentalHealthRiskDetails
                    .put(FhirConstants.PHQ9_RISK_LEVEL, assessmentDTO.getPhq9().getRiskLevel());
            mentalHealthRiskDetails.put(FhirConstants.PHQ9_SCORE,
                    String.valueOf(assessmentDTO.getPhq9().getScore()));
        }
        if (Objects.nonNull(assessmentDTO.getGad7())) {
            mentalHealthRiskDetails
                    .put(FhirConstants.GAD7_RISK_LEVEL, assessmentDTO.getGad7().getRiskLevel());
            mentalHealthRiskDetails.put(FhirConstants.GAD7_SCORE,
                    String.valueOf(assessmentDTO.getGad7().getScore()));
        }
        if (!mentalHealthRiskDetails.isEmpty()) {
            mentalHealthObservationDTO.setMentalRiskDetails(mentalHealthRiskDetails);
            mentalHealthObservationDTO.setRelatedPersonId(relatedPerson.getIdPart());
            mentalHealthObservationDTO.setQuestionnaireResponses(questionnaireResponses);
            observation = questionnaireResponseConverter.processMentalHealthDetails(mentalHealthObservationDTO, encounter, null);
        }
        return  observation;
    }

    /**
     * Create a new symptom observation based on the assessment details
     *
     * @param assessmentDTO The assessment details for the given patient
     * @param encounter     The FHIR Encounter entity
     * @return {@link Observation} Converted FHIR Observation entity.
     */
    private Observation createSymptomObservation(AssessmentDTO assessmentDTO, Encounter encounter) {
        Observation observation =  null;
        if (Objects.nonNull(assessmentDTO.getNcdSymptoms())) {
            observation = symptomConverter.createSymptomObservation(assessmentDTO.getNcdSymptoms(), assessmentDTO.getAssessmentTakenOn());
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.SYMPTOM);
        }
        return  observation;
    }

    /**
     * Create a new compliance observation based on the assessment details
     *
     * @param assessmentDTO The assessment details for the given patient
     * @param encounter     The FHIR Encounter entity
     * @return {@link Observation} Converted FHIR Observation entity.
     */
    private Observation createComplianceObservation(AssessmentDTO assessmentDTO, Encounter encounter) {
        Observation observation =  null;
        if (Objects.nonNull(assessmentDTO.getCompliance())) {
            observation = complianceConverter
                    .createComplianceObservation(assessmentDTO.getCompliance(), assessmentDTO.getAssessmentTakenOn());
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.COMPLIANCE);
        }
        return  observation;
    }

    /**
     * Create a new pregnancy observation based on the assessment details
     *
     * @param assessmentDTO The assessment details for the given patient
     * @param encounter     The FHIR Encounter entity
     * @return {@link Observation} Converted FHIR Observation entity.
     */
    private Observation createPregnancyObservation(AssessmentDTO assessmentDTO,
                                                   Encounter encounter) {
        Observation observation =  null;
        PregnancyDetailsDTO pregnancyDetailsDTO = assessmentDTO.getPregnancyAnc();
        if (Objects.nonNull(pregnancyDetailsDTO) && (Boolean.TRUE.equals(pregnancyDetailsDTO.getIsPregnant()))) {
            ProvenanceDTO provenanceDTO = assessmentDTO.getEncounter().getProvenance();
            assessmentDTO.getPregnancyAnc().setProvenance(provenanceDTO);
            String url = String.format(Constants.FETCH_LATEST_OBSERVATION_QUERY, FhirConstants.PREGNANCY.toLowerCase(),
                    Observation.ObservationStatus.PRELIMINARY.name().toLowerCase(), assessmentDTO.getMemberReference());
            Bundle pregnancyBundle = restApiUtil.getBatchRequest(url);
            if (pregnancyBundle.getEntry().isEmpty()) {
                observation = pregnancyConverter.createPregnancyObservation(pregnancyDetailsDTO);
            } else {
                Observation pregnancyObservation = (Observation) pregnancyBundle.getEntry().getFirst().getResource();
                observation = pregnancyConverter.updatePregnancyObservation(pregnancyObservation, pregnancyDetailsDTO);
            }
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
        }
        return  observation;
    }

    /**
     * Process and create FHIR Location entity based on given
     * screening log details
     *
     * @param assessmentDTO    The assessment details
     *
     * @return Converted FHIR Location entity.
     */
    private Location createLocation(AssessmentDTO assessmentDTO) {
        Location location = null;
        if (!Objects.isNull(assessmentDTO.getAssessmentLocation()) && !assessmentDTO.getAssessmentLocation().isEmpty()) {
            location = new Location();
            location.setStatus(Location.LocationStatus.ACTIVE);
            if (assessmentDTO.getAssessmentLocation().get(Constants.ASSESSMENT_TYPE).equals(Constants.DOOR_TO_DOOR)) {
                CodeableConcept codeableConcept = new CodeableConcept();
                Coding coding = new Coding(LocationPhysicalType.HO.getSystem(), LocationPhysicalType.HO.toCode(),
                        LocationPhysicalType.HO.getDefinition());
                codeableConcept.addCoding(coding);
                location.setPhysicalType(codeableConcept);
            } else if (assessmentDTO.getAssessmentLocation().get(Constants.ASSESSMENT_TYPE).equals(Constants.CAMP)) {
                CodeableConcept codeableConcept = new CodeableConcept();
                Coding coding = new Coding(LocationPhysicalType.WA.getSystem(), LocationPhysicalType.WA.toCode(),
                        LocationPhysicalType.WA.getDefinition());
                codeableConcept.addCoding(coding);
                location.setPhysicalType(codeableConcept);
            }
            location.setManagingOrganization(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID, assessmentDTO.getEncounter().getProvenance().getOrganizationId())));
        }
        return location;
    }

    /**
     * Process and create FHIR Patient entity based on given
     * screening log details
     *
     * @param assessmentDTO    The screening log details
     * @param patient          The FHIR Patient entity
     * @param relatedPerson    The FHIR Related Person entity
     *
     * @return {@link Observation} Converted FHIR Observation entity.
     */
    private Patient createPatient(AssessmentDTO assessmentDTO, Patient patient, RelatedPerson relatedPerson) {
        spiceConverter.setPatientBioDetails(relatedPerson, assessmentDTO);
        patient = patientConverter.createPatient(patient, assessmentDTO.getBioData(),
                assessmentDTO.getBioMetrics(), assessmentDTO.getBioMetrics().getDateOfBirth());
        patient.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                .setValue(FhirConstants.ASSESSED);
        patient.setManagingOrganization(new
                Reference(String.format(FhirConstants.ORGANIZATION_ID,
                assessmentDTO.getEncounter().getProvenance().getOrganizationId())));
        patient.setLink(List.of(new Patient.PatientLinkComponent().setOther(
                new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                        Constants.FORWARD_SLASH, assessmentDTO.getMemberReference())))));
        return patient;
    }

    /**
     * {@inheritDoc}
     */
    public void setObservationDetails(Bundle bundle,
                                       Observation observation,
                                       String identifierUrl, AssessmentDTO assessmentDTO, Patient patient,
                                       RelatedPerson relatedPerson, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(observation)) {
            commonConverter.setObservationReference(observation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, observation, identifierUrl, provenanceDTO);
            if (!observation.hasPerformer() || (observation.hasPerformer() && !observation.getPerformer().stream().map(Reference::getReference).toList().contains(StringUtil.concatString(FhirConstants.ORGANIZATION, Constants.FORWARD_SLASH, assessmentDTO.getEncounter().getProvenance().getOrganizationId())))) {
                observation.addPerformer(new
                        Reference(String.format(FhirConstants.ORGANIZATION_ID, assessmentDTO.getEncounter().getProvenance().getOrganizationId())));
            }
        }
    }

    /**
     * Process and create FHIR bp log observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createBpLogObservation(AssessmentDTO assessmentDTO,
                                               Encounter encounter) {
        Map<String, String> cvdRiskDetails = new HashMap<>();
        Observation observation = null;

        if (Objects.nonNull(assessmentDTO.getBpLog())) {
            if (Objects.isNull(assessmentDTO.getBpLog().getType())) {
                assessmentDTO.getBpLog().setType(Constants.ASSESSMENT_TYPE_ASSESSMENT);
            }
            if (Objects.nonNull(assessmentDTO.getCvdRiskScore())) {
                cvdRiskDetails.put(FhirConstants.CVD_RISK_SCORE,
                        String.valueOf(assessmentDTO.getCvdRiskScore()));
            }
            if (Objects.nonNull(assessmentDTO.getCvdRiskLevel())) {
                cvdRiskDetails.put(FhirConstants.CVD_RISK_LEVEL,
                        assessmentDTO.getCvdRiskLevel());
            }
            if (Objects.nonNull(assessmentDTO.getCvdRiskScoreDisplay())) {
                cvdRiskDetails.put(FhirConstants.CVD_RISK_SCORE_DISPLAY,
                        assessmentDTO.getCvdRiskScoreDisplay());
            }

            observation = bloodPressureConverter.createBloodPressureObservation(assessmentDTO.getBpLog(),
                    assessmentDTO.getAssessmentTakenOn(), cvdRiskDetails);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.BLOOD_PRESSURE);
        }
        return observation;
    }

    /**
     * Process and create FHIR glucose log observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createGlucoseLogObservation(AssessmentDTO assessmentDTO,
                                                    Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getGlucoseLog())) {
            observation = bloodGlucoseConverter
                    .createBloodGlucoseObservation(assessmentDTO.getGlucoseLog());
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.BLOOD_GLUCOSE);
        }
        return  observation;
    }

    /**
     * Process and create FHIR height observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createHeightObservation(AssessmentDTO assessmentDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getBioMetrics())
                && Objects.nonNull(assessmentDTO.getBioMetrics().getHeight())) {
            observation = commonConverter.createBasicObservation(assessmentDTO.getBioMetrics(),
                    assessmentDTO.getAssessmentTakenOn(), FhirConstants.HEIGHT);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.HEIGHT);
        }
        return  observation;
    }

    /**
     * Process and create FHIR weight observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createWeightObservation(AssessmentDTO assessmentDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getBioMetrics())
                && Objects.nonNull(assessmentDTO.getBioMetrics().getWeight())) {
            observation = commonConverter.createBasicObservation(assessmentDTO.getBioMetrics(),
                    assessmentDTO.getAssessmentTakenOn(), FhirConstants.WEIGHT);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.WEIGHT);
        }
        return  observation;
    }

    /**
     * Process and create FHIR BMI observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createBmiObservation(AssessmentDTO assessmentDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getBioMetrics()) && Objects.nonNull(assessmentDTO.getBioMetrics().getBmi())) {
            observation = commonConverter.createBasicObservation(assessmentDTO.getBioMetrics(),
                    assessmentDTO.getAssessmentTakenOn(), FhirConstants.BMI);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.BMI);
        }
        return  observation;
    }

    /**
     * Process and create FHIR Temperature observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    private Observation createTemperatureObservation(AssessmentDTO assessmentDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getTemperature())) {
            observation = commonConverter.createBasicObservation(assessmentDTO.getTemperature(),
                    assessmentDTO.getAssessmentTakenOn(), MetaCodeConstants.TEMPERATURE_KEY,
                    FhirConstants.CELSIUS_CODE, FhirConstants.TEMPERATURE, Constants.OBSERVATION_TEMPERATURE);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.TEMPERATURE);
        }
        return  observation;
    }

    /**
     * {@inheritDoc}
     */
    public Observation createSubstanceAbuseObservation(AssessmentDTO assessmentDTO,
                                                       Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getSubstanceAbuse()) && !assessmentDTO.getSubstanceAbuse().isEmpty()) {
            if (Objects.nonNull(assessmentDTO.getSubstanceAbuse().get(Constants.OBSERVATION_ID))) {
                Bundle observationBundle = restApiUtil.getBatchRequest(String.format(Constants.OBSERVATION_QUERY_WITH_ID, assessmentDTO.getSubstanceAbuse().get(Constants.OBSERVATION_ID)));
                if (Objects.nonNull(observationBundle.getEntry()) && !observationBundle.getEntry().isEmpty()) {
                    for (Bundle.BundleEntryComponent entry : observationBundle.getEntry()) {
                        if (entry.getResource() instanceof Observation response) {
                            observation = response;
                            observation.setEffective(new DateTimeType(assessmentDTO.getAssessmentTakenOn()));
                            observation.setComponent(new ArrayList<>());
                            assessmentDTO.getSubstanceAbuse().remove(Constants.OBSERVATION_ID);
                            for (Map.Entry<String, String> data : assessmentDTO.getSubstanceAbuse().entrySet()) {
                                commonConverter.setSuicideOrSubstanceComponent(observation, data);
                                observation.setValue(new StringType(String.valueOf(assessmentDTO.getCageAid())));
                            }
                        }
                    }
                }
            } else {
                observation = commonConverter.createSubstanceAbuseObservation(assessmentDTO.getSubstanceAbuse(),
                        assessmentDTO.getCageAid(), assessmentDTO.getAssessmentTakenOn());
                commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
                commonConverter.setObservationText(observation, MetaCodeConstants.SUBSTANCE_ABUSE);
            }
        }
        return observation;
    }

    /**
     * {@inheritDoc}
     */
    public Observation createSuicideObservation(AssessmentDTO assessmentDTO,
                                                 Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(assessmentDTO.getSuicideScreener()) && !assessmentDTO.getSuicideScreener().isEmpty()) {
            if (Objects.nonNull(assessmentDTO.getSuicideScreener().get(Constants.OBSERVATION_ID))) {
                Bundle observationBundle = restApiUtil.getBatchRequest(String.format(Constants.OBSERVATION_QUERY_WITH_ID, assessmentDTO.getSuicideScreener().get(Constants.OBSERVATION_ID)));
                if (Objects.nonNull(observationBundle.getEntry()) && !observationBundle.getEntry().isEmpty()) {
                    for (Bundle.BundleEntryComponent entry : observationBundle.getEntry()) {
                        if (entry.getResource() instanceof Observation response) {
                            observation = response;
                            observation.setEffective(new DateTimeType(assessmentDTO.getAssessmentTakenOn()));
                            observation.setComponent(new ArrayList<>());
                            assessmentDTO.getSuicideScreener().remove(Constants.OBSERVATION_ID);
                            for (Map.Entry<String, String> data : assessmentDTO.getSuicideScreener().entrySet()) {
                                    commonConverter.setSuicideOrSubstanceComponent(observation, data);
                                }
                            }
                        }
                }
            } else {
                observation = commonConverter.createSuicideScreenerObservation(assessmentDTO.getSuicideScreener(),
                        assessmentDTO.getAssessmentTakenOn());
                commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
                commonConverter.setObservationText(observation, MetaCodeConstants.SUICIDAL_SCREENER);
            }
        }
        return  observation;
    }

    /**
     * Create Assessment in FHIR Encounter
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle Object
     */
    private void createAssessment(AssessmentDTO assessmentDTO, Bundle bundle) {
        switch (assessmentDTO.getAssessmentType()) {
            case Constants.ICCM:
                if (Objects.nonNull(assessmentDTO.getAssessmentDetails().getIccm())) {
                    checkAndCloseRmnchDetails(assessmentDTO, bundle);
                    createIccmAssessment(assessmentDTO, bundle);
                    createSummary(assessmentDTO, bundle);
                    assessmentDTO.setReferralTicketType(Constants.ICCM);
                    createReferralTicketForICCM(assessmentDTO, bundle);
                }
                break;
            case Constants.OTHER_SYMPTOMS:
                if (Objects.nonNull(assessmentDTO.getAssessmentDetails().getOtherSymptoms())) {
                    checkAndCloseRmnchDetails(assessmentDTO, bundle);
                    createOtherSymptoms(assessmentDTO, bundle);
                    createSummary(assessmentDTO, bundle);
                    assessmentDTO.setReferralTicketType(Constants.ICCM);
                    createReferralTicketForICCM(assessmentDTO, bundle);
                }
                break;
            case Constants.ANC:
                if (Objects.nonNull(assessmentDTO.getAssessmentDetails().getAnc())) {
                    assessmentDTO.setReferralTicketType(Constants.RMNCH);
                    assessmentDTO.setReferralTicketVisitType(Constants.RMNCH_VISIT);
                    updatePregnancyDetails(assessmentDTO, bundle);
                    createANCObservation(assessmentDTO, bundle);
                    createSummary(assessmentDTO, bundle);
                    createReferralTicketForRMNCH(assessmentDTO, bundle, Boolean.TRUE);
                }
                break;
            case Constants.PNC_MOTHER:
                if (Objects.nonNull(assessmentDTO.getAssessmentDetails().getPncMother())) {
                    createPNCMotherObservation(assessmentDTO, bundle);
                    patientService.updatePatientStatus(bundle, Boolean.FALSE,
                            assessmentDTO.getEncounter().getProvenance(), assessmentDTO.getEncounter().getPatientId(), null);
                    fhirAssessmentMapper.createVitalObservation(bundle, assessmentDTO.getEncounter(),
                            Constants.PNC_VISIT_NUMBER, assessmentDTO.getEncounter().getVisitNumber(),
                            assessmentDTO.getEncounter().getPatientReference());
                    assessmentDTO.setReferralTicketType(Constants.RMNCH);
                    assessmentDTO.setReferralTicketVisitType(Constants.RMNCH_VISIT);
                    createSummary(assessmentDTO, bundle);
                    createReferralTicketForRMNCH(assessmentDTO, bundle, Boolean.FALSE);
                }
                break;
            case Constants.PNC_NEONATE:
                PncNeonatalDTO pncNeonatalDTO = assessmentDTO.getAssessmentDetails().getPncNeonatal();
                if (Objects.nonNull(pncNeonatalDTO)) {
                    createPNCNeonatalObservation(assessmentDTO, bundle);
                    assessmentDTO.setReferralTicketType(Constants.RMNCH);
                    assessmentDTO.setReferralTicketVisitType(Constants.RMNCH_VISIT);
                    if (assessmentDTO.getEncounter().getVisitNumber() == Constants.ONE) {
                        fhirAssessmentMapper.createBirthHistory(pncNeonatalDTO.getPncNeonatalSigns(), null,
                                pncNeonatalDTO.getGestationalAge(), assessmentDTO.getEncounter(), bundle);
                    }
                    if (Boolean.TRUE.equals(pncNeonatalDTO.getDeathOfNewborn())) {
                        patientService.updatePatientStatus(bundle, null,
                                assessmentDTO.getEncounter().getProvenance(), assessmentDTO.getEncounter().getPatientId(), Boolean.FALSE);
                    }
                    createSummary(assessmentDTO, bundle);
                    createReferralTicketForRMNCH(assessmentDTO, bundle, null);
                }
                break;
            case Constants.CHILDHOOD_VISIT:
                if (Objects.nonNull(assessmentDTO.getAssessmentDetails().getPncChild())) {
                    HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberById(
                            assessmentDTO.getEncounter().getMemberId());
                    if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) && Constants.PNC_VISIT_CLOSE_DAYS <
                            DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
                        RequestDTO requestDTO = new RequestDTO();
                        requestDTO.setMemberId(assessmentDTO.getEncounter().getMemberId());
                        requestDTO.setEncounterId(assessmentDTO.getEncounter().getId());
                        requestDTO.setProvenance(assessmentDTO.getEncounter().getProvenance());
                        requestDTO.setClosedEncounterType(assessmentDTO.getAssessmentType());
                        requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_CHILD_REASON);
                        patientService.closePncNeonateDetails(bundle, requestDTO);
                    }
                    createPNCChildObservation(assessmentDTO, bundle);
                    boolean isChildDead = assessmentDTO.getAssessmentDetails().getPncChild().getDeathOfBaby();
                    if (isChildDead) {
                        patientService.updatePatientStatus(bundle, null,
                                assessmentDTO.getEncounter().getProvenance(), assessmentDTO.getEncounter().getPatientId(), Boolean.FALSE);
                    }
                    createSummary(assessmentDTO, bundle);
                    fhirAssessmentMapper.createVitalObservation(bundle, assessmentDTO.getEncounter(),
                            Constants.CHILDHOOD_VISIT_NUMBER, assessmentDTO.getEncounter().getVisitNumber(),
                            assessmentDTO.getEncounter().getPatientReference());
                    assessmentDTO.setReferralTicketType(Constants.CHILDHOOD_VISIT);
                    assessmentDTO.setReferralTicketVisitType(Constants.CHILD_VISIT);
                    createReferralTicketForRMNCH(assessmentDTO, bundle, null);
                }
                break;
            default: break;
        }
    }

    /**
     * update Patient pregnancy details
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle Object
     */
    private void updatePregnancyDetails(AssessmentDTO assessmentDTO, Bundle bundle) {
        boolean pregnancyStatus = Objects.isNull(assessmentDTO.getAssessmentDetails().getAnc().getMiscarriage()) ||
                !assessmentDTO.getAssessmentDetails().getAnc().getMiscarriage();
        boolean isMotherDead = Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getAnc().getDeathOfMother());
        patientService.updatePatientStatus(bundle, pregnancyStatus, assessmentDTO.getEncounter().getProvenance(),
                assessmentDTO.getEncounter().getPatientId(), isMotherDead ? Boolean.FALSE : null);
        if (Boolean.FALSE.equals(assessmentDTO.getAssessmentDetails().getAnc().getMiscarriage())) {
            fhirAssessmentMapper.createVitalObservation(bundle, assessmentDTO.getEncounter(),
                    Constants.ANC_VISIT_NUMBER, assessmentDTO.getEncounter().getVisitNumber(),
                    assessmentDTO.getEncounter().getPatientReference());
            if (!Objects.isNull(assessmentDTO.getAssessmentDetails().getAnc().getLastMenstrualPeriod())) {
                fhirAssessmentMapper.createVitalObservation(bundle, assessmentDTO.getEncounter(),
                        Constants.LAST_MENSTRUAL_PERIOD,
                        assessmentDTO.getAssessmentDetails().getAnc().getLastMenstrualPeriod(),
                        assessmentDTO.getEncounter().getPatientReference());
            }
            if (!Objects.isNull(assessmentDTO.getAssessmentDetails().getAnc().getEstimatedDeliveryDate())) {
                fhirAssessmentMapper.createVitalObservation(bundle, assessmentDTO.getEncounter(),
                        Constants.ESTIMATED_DELIVERY_DATE,
                        assessmentDTO.getAssessmentDetails().getAnc().getEstimatedDeliveryDate(),
                        assessmentDTO.getEncounter().getPatientReference());
            }
        }
    }

    /**
     * Create Observation For Other symptoms
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle Object
     */
    private void createOtherSymptoms(AssessmentDTO assessmentDTO, Bundle bundle) {
        OtherSymptomsDTO otherSymptomsDTO = assessmentDTO.getAssessmentDetails().getOtherSymptoms();
        createFever(assessmentDTO, otherSymptomsDTO.getFever(), bundle);
        //createObservation for otherSymptoms
        if (!Objects.isNull(otherSymptomsDTO.getSignsAndSymptoms())) {
            Observation observation = fhirAssessmentMapper.createSignsObservation(
                    otherSymptomsDTO.getSignsAndSymptoms().getSymptoms(),
                    assessmentDTO.getEncounter(), Constants.SIGNS,
                    otherSymptomsDTO.getSignsAndSymptoms().getOtherConcerningSymptoms());
            if (!Objects.isNull(observation)) {
                fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                        assessmentDTO.getEncounter().getProvenance());
            }
        }
    }

    /**
     * Create Service Request for the Referred Patient
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        FHIR bundle Object
     */
    private void createReferralTicketForICCM(AssessmentDTO assessmentDTO, Bundle bundle) {
        if (Constants.RECOVERED.equalsIgnoreCase(assessmentDTO.getPatientStatus())) {
            RequestDTO requestDTO = getRequestDto(assessmentDTO, Constants.MUAC_UPPER_CASE, Constants.MEDICAL_REVIEW,
                    List.of(Constants.ACTIVE), Constants.RECOVERED);
            requestDTO.setUpdatePriorityStatus(Boolean.TRUE);
            requestDTO.setClosedReason(Constants.RECOVERED);
            patientService.updateReferralTicketByMemberId(requestDTO, bundle);
            if (Objects.nonNull(assessmentDTO.getFollowUp()) &&
                    assessmentDTO.getFollowUp().getEncounterType().equals(assessmentDTO.getReferralTicketType())) {
                requestDTO = getRequestDto(assessmentDTO, assessmentDTO.getFollowUp().getReason(), Constants.ASSESSMENT,
                        null, Constants.RECOVERED);
                requestDTO.setClosedReason(Constants.RECOVERED);
                patientService.updateReferralTicketByMemberId(requestDTO, bundle);
                requestDTO = getRequestDto(assessmentDTO, assessmentDTO.getFollowUp().getReason(),
                        Constants.MEDICAL_REVIEW, List.of(Constants.ACTIVE), Constants.RECOVERED);
                requestDTO.setClosedReason(Constants.RECOVERED);
                requestDTO.setUpdatePriorityStatus(Boolean.TRUE);
                patientService.updateReferralTicketByMemberId(requestDTO, bundle);
            }
        } else if (Constants.ON_TREATMENT.equalsIgnoreCase(assessmentDTO.getPatientStatus())) {
            createReferralTicketForAssessment(assessmentDTO, bundle, null);
        } else if (Constants.REFERRED.equalsIgnoreCase(assessmentDTO.getPatientStatus())) {
            RequestDTO requestDTO = getRequestDto(assessmentDTO, assessmentDTO.getReferredReasons(),
                    Constants.ASSESSMENT, null, Constants.REFERRED);
            requestDTO.setCloseReferralTicket(Boolean.TRUE);
            requestDTO.setClosedReason(Constants.REFERRED);
            patientService.updateReferralTicketByMemberId(requestDTO, bundle);
            createReferralTicketForAssessment(assessmentDTO, bundle, null);
        }
    }

    /**
     * Create Service Request for the Referred Patient
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        FHIR bundle Object
     */
    private void createReferralTicketForRMNCH(AssessmentDTO assessmentDTO, Bundle bundle, Boolean isPregnant) {
        EncounterDetailsDTO encounter = assessmentDTO.getEncounter();
        String memberId = encounter.getMemberId();
        String encounterId = encounter.getId();
        ProvenanceDTO provenance = encounter.getProvenance();
        String assessmentType = assessmentDTO.getAssessmentType();
        int visitNumber = encounter.getVisitNumber();
        boolean isMotherDead = (Constants.ANC.equals(assessmentType) &&
                Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getAnc().getDeathOfMother()));
        boolean isPncChildDead = (Constants.CHILDHOOD_VISIT.equals(assessmentType) &&
                Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getPncChild().getDeathOfBaby()));
        boolean isPncNeonateChildDead = (Constants.PNC_NEONATE.equals(assessmentType) &&
                assessmentDTO.getAssessmentDetails().getPncNeonatal().getDeathOfNewborn());
        boolean miscarriage = (Constants.ANC.equals(assessmentType) &&
                Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getAnc().getMiscarriage()));
        if (isMotherDead || isPncChildDead || isPncNeonateChildDead) {
            patientService.handlePatientDeath(assessmentDTO.getAssessmentType(), bundle, memberId, encounterId, provenance);
        } else if (miscarriage) {
            handleMiscarriage(assessmentDTO, bundle, memberId, encounterId, provenance);
        } else if (visitNumber == Constants.ONE) {
            handleFirstVisit(bundle, memberId, encounterId, isPregnant, assessmentDTO);
        } else {
            handleSubsequentVisit(assessmentDTO, bundle, isPregnant, visitNumber, assessmentType);
        }
        if (Constants.REFERRED.equalsIgnoreCase(assessmentDTO.getPatientStatus()) && !miscarriage) {
            createReferralTicketForAssessment(assessmentDTO, bundle, isPregnant);
        }
    }

    /**
     * Close ANC details and create referral Ticket for referral
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle Object
     * @param memberId      MemberId
     * @param encounterId   EncounterId
     * @param provenance    provenanceDetails
     */
    private void handleMiscarriage(AssessmentDTO assessmentDTO, Bundle bundle, String memberId, String encounterId,
                                   ProvenanceDTO provenance) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(memberId);
        requestDTO.setEncounterId(encounterId);
        requestDTO.setProvenance(provenance);
        requestDTO.setClosedEncounterType(assessmentDTO.getAssessmentType());
        requestDTO.setClosedReason(Constants.MISCARRIAGE);
        patientService.closeAncDetails(bundle, requestDTO);
        assessmentDTO.setAssessmentType(Constants.OTHER_SYMPTOMS);
        assessmentDTO.setReferralTicketType(Constants.ICCM);
        createReferralTicketForICCM(assessmentDTO, bundle);
    }

    /**
     * Close Pnc visits in ANC first Visit and Close
     * Anc visits in PNC first visit
     *
     * @param bundle         Bundle Object
     * @param memberId       MemberId
     * @param encounterId    EncounterId
     * @param assessment     provenanceDetails
     */
    private void handleFirstVisit(Bundle bundle, String memberId, String encounterId, Boolean isPregnant,
                                  AssessmentDTO assessment) {
        PregnancyInfo pregnancyInfo = getPatientVitals(memberId);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(memberId);
        requestDTO.setEncounterId(encounterId);
        requestDTO.setProvenance(assessment.getEncounter().getProvenance());
        requestDTO.setClosedEncounterType(assessment.getAssessmentType());
        if (Constants.ANC.equals(assessment.getAssessmentType()) && (Objects.nonNull(pregnancyInfo.getPncVisitNo()) ||
                Objects.nonNull(pregnancyInfo.getPncMotherMedicalReviewVisitNo()))) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_REASON);
            requestDTO.setAncStarted(Boolean.TRUE);
            patientService.closePncDetails(bundle, requestDTO);
        }

        if (Constants.PNC_MOTHER.equals(assessment.getAssessmentType()) && (Objects.nonNull(pregnancyInfo.getAncVisitNo()) ||
                Objects.nonNull(pregnancyInfo.getAncMedicalReviewVisitNo()))) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_ANC_REASON);
            patientService.closeAncDetails(bundle, requestDTO);
        }
        ReferralDetailsDTO referralDetailsDTO = createReferralDetailsDTOForVisit(assessment, assessment.getEncounter().getVisitNumber(),
                assessment.getAssessmentType());
        patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE, Boolean.TRUE, isPregnant);
    }

    /**
     * Close Anc previous visit details and create
     * Referral Ticket
     *
     * @param assessmentType Assessment Type
     * @param bundle         Bundle Object
     * @param isPregnant     Pregnant Details
     * @param visitNumber    Visit count
     */
    private void handleSubsequentVisit(AssessmentDTO assessmentDTO, Bundle bundle, Boolean isPregnant, int visitNumber,
                                       String assessmentType) {
        if (visitNumber > Constants.ZERO) {
            boolean isReferred = Constants.REFERRED.equalsIgnoreCase(assessmentDTO.getPatientStatus());
            boolean isRecovered = Constants.RECOVERED.equals(assessmentDTO.getPatientStatus());
            String ticketStatus = isReferred ? Constants.REFERRED : Constants.RECOVERED;
            RequestDTO requestDTO = getRequestDto(assessmentDTO, null, Constants.ASSESSMENT, null, ticketStatus);
            requestDTO.setClosedReason(isReferred ? Constants.REFERRED : Constants.RMNCH_VISIT_COMPLETED);
            patientService.updateReferralTicketByMemberId(requestDTO, bundle);
            if (isRecovered) {
                requestDTO = getRequestDto(assessmentDTO, null, Constants.MEDICAL_REVIEW, List.of(Constants.ACTIVE),
                        Constants.RECOVERED);
                requestDTO.setClosedReason(Constants.RECOVERED);
                requestDTO.setUpdatePriorityStatus(Boolean.TRUE);
                patientService.updateReferralTicketByMemberId(requestDTO, bundle);
            }
            ReferralDetailsDTO referralDetailsDTO = createReferralDetailsDTOForVisit(assessmentDTO, visitNumber,
                    assessmentType);
            patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE, Boolean.TRUE, isPregnant);
        }
    }

    /**
     * Construct ReferralDetailsDTO for create new Ticket
     *
     * @param assessmentDTO  Assessment Details
     * @param visitNumber    VisitNumber
     * @param assessmentType AssessmentType
     * @return ReferralDetailsDTO
     */
    private ReferralDetailsDTO createReferralDetailsDTOForVisit(AssessmentDTO assessmentDTO, int visitNumber,
                                                                String assessmentType) {
        String visitReason = String.format(Constants.RMNCH_VISIT_REASON,
                Constants.RMNCH_VISIT_MAPPING.get(assessmentType), visitNumber);
        ReferralDetailsDTO referralDetailsDTO = getReferralDetailsDTO(assessmentDTO, visitReason, Constants.ASSESSMENT,
                assessmentDTO.getReferralTicketVisitType());
        referralDetailsDTO.setEncounterType(assessmentType);
        referralDetailsDTO.setReferred(Boolean.FALSE);
        referralDetailsDTO.setPatientStatus(Constants.ON_TREATMENT);
        referralDetailsDTO.setCurrentPatientStatus(Constants.ON_TREATMENT);
        return referralDetailsDTO;
    }

    /**
     * Update Previous EncounterDetails
     *
     * @param reason        Ticket reason
     * @param ticketType    Ticket Type
     */
    private RequestDTO getRequestDto(AssessmentDTO assessmentDTO, String reason, String ticketType,
                                     List<String> ticketStatuses, String patientStatus) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setCategory(
                Constants.ICCM.equals(assessmentDTO.getReferralTicketType()) ? assessmentDTO.getReferralTicketType() :
                        StringUtil.concatString(assessmentDTO.getReferralTicketType(), Constants.COMMA,
                                assessmentDTO.getReferralTicketVisitType()));
        requestDTO.setMemberId(assessmentDTO.getEncounter().getMemberId());
        requestDTO.setReason(reason);
        requestDTO.setPatientStatus(patientStatus);
        requestDTO.setClosedEncounterType(assessmentDTO.getAssessmentType());
        requestDTO.setTicketType(ticketType);
        requestDTO.setTicketStatuses(ticketStatuses);
        requestDTO.setProvenance(assessmentDTO.getEncounter().getProvenance());
        requestDTO.setEncounterId(assessmentDTO.getId());
        return requestDTO;
    }

    /**
     * Create referral Ticket for assessment
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle object
     */
    private void createReferralTicketForAssessment(AssessmentDTO assessmentDTO, Bundle bundle, Boolean pregnant) {
        ReferralDetailsDTO referralDetailsDTO = getReferralDetailsDTO(assessmentDTO, null,
                assessmentDTO.getEncounter().isReferred() ? Constants.MEDICAL_REVIEW : Constants.ASSESSMENT,
                assessmentDTO.getReferralTicketType());
        referralDetailsDTO.setEncounterType(assessmentDTO.getAssessmentType());
        patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE, Boolean.TRUE, pregnant);
    }

    /**
     * Set ReferralTicket Details Based on assessment Details
     *
     * @param assessmentDTO Assessment Details
     * @return ReferralDetailsDTO
     */
    private ReferralDetailsDTO getReferralDetailsDTO(AssessmentDTO assessmentDTO, String status, String referralType,
                                                     String category) {
        ReferralDetailsDTO referralDetailsDTO = new ReferralDetailsDTO();
        referralDetailsDTO.setPatientStatus(assessmentDTO.getPatientStatus());
        referralDetailsDTO.setProvenance(assessmentDTO.getEncounter().getProvenance());
        referralDetailsDTO.setReferredReason(Objects.isNull(status) ? assessmentDTO.getReferredReasons() : status);
        referralDetailsDTO.setPatientReference(assessmentDTO.getEncounter().getPatientReference());
        referralDetailsDTO.setReferred(assessmentDTO.getEncounter().isReferred());
        if (!Objects.isNull(assessmentDTO.getSummary())) {
            referralDetailsDTO.setReferredSiteId(assessmentDTO.getSummary().getReferredSiteId());
            referralDetailsDTO.setNextVisitDate(assessmentDTO.getSummary().getNextVisitDate());
        }
        referralDetailsDTO.setEncounterId(assessmentDTO.getId());
        referralDetailsDTO.setCurrentPatientStatus(assessmentDTO.getPatientStatus());
        if (Constants.PNC_MOTHER.equals(assessmentDTO.getAssessmentType())) {
            referralDetailsDTO.setDateOfDelivery(
                    assessmentDTO.getAssessmentDetails().getPncMother().getDateOfDelivery());
        }
        referralDetailsDTO.setEncounterId(assessmentDTO.getId());
        referralDetailsDTO.setCategory(category);
        referralDetailsDTO.setMemberId(assessmentDTO.getEncounter().getMemberId());
        referralDetailsDTO.setType(referralType);
        return referralDetailsDTO;
    }

    /**
     * Create ICCM assessment for a patient
     *
     * @param assessmentDTO assessment Object
     * @param bundle        Bundle Object
     */
    public void createIccmAssessment(AssessmentDTO assessmentDTO, Bundle bundle) {
        ICCMDTO iccmDto = assessmentDTO.getAssessmentDetails().getIccm();
        createGeneralSignAndNutritionalStatus(assessmentDTO, bundle);
        createDiarrhoea(assessmentDTO, bundle);
        createFever(assessmentDTO, iccmDto.getFever(), bundle);
        createCough(assessmentDTO, bundle);
    }

    /**
     * Create Summary with fields such as  Notes, isTakenToClinic etc., for Assessment
     *
     * @param assessmentDTO AssessmentDTO object
     * @param bundle        Bundle object
     */
    private void createSummary(AssessmentDTO assessmentDTO, Bundle bundle) {
        if (!Objects.isNull(assessmentDTO.getSummary())) {
            String uuid = fhirUtils.getUniqueId();
            Observation observationSummary = fhirAssessmentMapper.setObservation(assessmentDTO.getEncounter(),
                    Constants.SUMMARY, Constants.SUMMARY);
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getNotes(), Constants.NOTES,
                    observationSummary.getComponent());
            //set Summary Details
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getIsTakenToClinic(),
                    Constants.IS_TAKEN_TO_CLINIC, observationSummary.getComponent());
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getReferredSite(),
                    Constants.NEAREST_PHU_REFERRED_TO, observationSummary.getComponent());
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getNextVisitDate(),
                    Constants.NEXT_VISIT_DATE, observationSummary.getComponent());
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getMalnutritionCondition(),
                    Constants.MUAC, observationSummary.getComponent());
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getCoughCondition(),
                    Constants.COUGH, observationSummary.getComponent());
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getFeverCondition(),
                    Constants.FEVER, observationSummary.getComponent());
            fhirAssessmentMapper.createObservationComponent(assessmentDTO.getSummary().getDiarrheaCondition(),
                    Constants.DIARRHOEA, observationSummary.getComponent());
            String summaryUrl = StringUtil.concatString(String.valueOf(ResourceType.Observation),
                    Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
            String fullSummaryUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            fhirUtils.setBundle(summaryUrl, fullSummaryUrl, Bundle.HTTPVerb.POST, observationSummary, bundle,
                    assessmentDTO.getEncounter().getProvenance());
        }
    }

    /**
     * create GeneralSigns and nutritional status for Patient
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        FHIR bundle object
     */
    private void createGeneralSignAndNutritionalStatus(AssessmentDTO assessmentDTO, Bundle bundle) {
        ICCMDTO iccmDto = assessmentDTO.getAssessmentDetails().getIccm();

        //createObservation for generalDangerSigns
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.VOMITING, iccmDto.getGeneralDangerSigns().getIsVomiting(),
                Constants.GENERAL_DANGER_SIGNS, bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.BREASTFEED, iccmDto.getGeneralDangerSigns().getIsBreastfeed(),
                Constants.GENERAL_DANGER_SIGNS, bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.UNUSUAL_SLEEPY, iccmDto.getGeneralDangerSigns().getIsUnusualSleepy(),
                Constants.GENERAL_DANGER_SIGNS, bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.CONVULSION,
                iccmDto.getGeneralDangerSigns().getIsConvulsionPastFewDays(),
                Constants.GENERAL_DANGER_SIGNS,
                bundle);

        //createObservation for NutritionalStatus
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.OEDEMA_OF_BOTH_FEET,
                iccmDto.getNutritionalStatusDetails().getHasOedemaOfBothFeet(),
                Constants.NUTRITIONAL_STATUS,
                bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.MUAC_CODE, Constants.NUTRITIONAL_STATUS,
                iccmDto.getNutritionalStatusDetails().getMuacCode(),
                bundle);
    }

    /**
     * Create Observation For Diarrhoea
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle Object
     */
    public void createDiarrhoea(AssessmentDTO assessmentDTO, Bundle bundle) {
        ICCMDTO iccmDto = assessmentDTO.getAssessmentDetails().getIccm();
        String uuidDiarrhoea = fhirUtils.getUniqueId();
        String fullUrlDiarrhoea = StringUtil.concatString(Constants.FHIR_BASE_URL, uuidDiarrhoea);
        String urlDiarrhoea = StringUtil.concatString(String.valueOf(ResourceType.Observation),
                Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuidDiarrhoea);

        Observation observationDiarrhoea = fhirAssessmentMapper.setObservation(assessmentDTO.getEncounter(),
                Constants.DIARRHOEA, Constants.DIARRHOEA);
        if (Objects.nonNull(iccmDto.getDiarrhoea().getIsBloodyDiarrhoea())) {
            fhirAssessmentMapper.createObservationComponent(iccmDto.getDiarrhoea().getIsBloodyDiarrhoea(),
                    Constants.BLOODY_DIARRHOEA, observationDiarrhoea.getComponent());
        }
        if (Objects.nonNull(iccmDto.getDiarrhoea().getNumberOfDaysDiarrhoea())) {
            fhirAssessmentMapper.setQuantityToObservation(observationDiarrhoea,
                    new Quantity(iccmDto.getDiarrhoea().getNumberOfDaysDiarrhoea()),
                    Constants.NO_OF_DAYS_OF_DIARRHOEA
            );
        }
        if (Objects.nonNull(iccmDto.getDiarrhoea().getOrsDispensedStatus())) {
            String orsStatus = iccmDto.getDiarrhoea()
                    .getOrsDispensedStatus()
                    .equalsIgnoreCase(Constants.DISPENSED) ? Constants.COMPLETED : Constants.UNKNOWN;
            observationDiarrhoea.addPartOf(new Reference(createMedicationDispense(bundle, Constants.ORS, assessmentDTO,
                    orsStatus)));
        }
        if (Objects.nonNull(iccmDto.getDiarrhoea().getZincDispensedStatus())) {
            String zincStatus = iccmDto.getDiarrhoea()
                    .getZincDispensedStatus()
                    .equalsIgnoreCase(Constants.DISPENSED) ? Constants.COMPLETED : Constants.UNKNOWN;
            observationDiarrhoea.addPartOf(new Reference(createMedicationDispense(bundle, Constants.ZINC, assessmentDTO,
                    zincStatus)));
        }
        if (Objects.nonNull(iccmDto.getDiarrhoea().getJellyWaterDispensedStatus())) {
            String jellyWaterStatus = iccmDto.getDiarrhoea()
                    .getJellyWaterDispensedStatus()
                    .equalsIgnoreCase(Constants.DISPENSED) ? Constants.COMPLETED : Constants.UNKNOWN;
            observationDiarrhoea.addPartOf(new Reference(createMedicationDispense(bundle, Constants.JELLY_WATER, assessmentDTO,
                    jellyWaterStatus)));
        }
        if (Objects.nonNull(iccmDto.getDiarrhoea().getSssDispensedStatus())) {
            String sssStatus = iccmDto.getDiarrhoea()
                    .getSssDispensedStatus()
                    .equalsIgnoreCase(Constants.DISPENSED) ? Constants.COMPLETED : Constants.UNKNOWN;
            observationDiarrhoea.addPartOf(new Reference(createMedicationDispense(bundle, Constants.SSS, assessmentDTO,
                    sssStatus)));
        }
        if (!Objects.isNull(iccmDto.getDiarrhoea().getDiarrhoeaSigns())) {
            Observation observation = fhirAssessmentMapper.createSignsObservation(
                    iccmDto.getDiarrhoea().getDiarrhoeaSigns(),
                    assessmentDTO.getEncounter(), Constants.SIGNS,
                    iccmDto.getDiarrhoea().getOtherSigns());
            if (!Objects.isNull(observation)) {
                String signReference = fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                        assessmentDTO.getEncounter().getProvenance());
                observationDiarrhoea.addHasMember(new Reference(signReference));
            }
        }
        observationDiarrhoea.setValue(new CodeableConcept().setText(
                Boolean.TRUE.equals(iccmDto.getDiarrhoea().getHasDiarrhoea()) ? Constants.YES : Constants.NO));
        fhirUtils.setBundle(urlDiarrhoea, fullUrlDiarrhoea, Bundle.HTTPVerb.POST, observationDiarrhoea, bundle,
                assessmentDTO.getEncounter().getProvenance());
    }

    /**
     * Create Medication Dispense for the patient
     *
     * @param bundle        Bundle Object
     * @param name          Medication Name
     * @param assessmentDTO Assessment Details
     * @return String medicationDispense reference
     */
    private String createMedicationDispense(Bundle bundle, String name, AssessmentDTO assessmentDTO, String status) {
        String uuidMedication = fhirUtils.getUniqueId();
        MedicationDispense medicationDispense = fhirAssessmentMapper.setMedicationDispense(assessmentDTO.getEncounter(), name, status, assessmentDTO.getId());
        String medicationUrl = StringUtil.concatString(String.valueOf(ResourceType.MedicationDispense),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, uuidMedication);
        String fullMedicationUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuidMedication);
        fhirUtils.setBundle(medicationUrl, fullMedicationUrl, Bundle.HTTPVerb.POST, medicationDispense, bundle,
                assessmentDTO.getEncounter().getProvenance());
        return medicationUrl;
    }

    /**
     * create DiagnosticReport for patient
     *
     * @param bundle        Bundle Object
     * @param name          Diagnosis Name
     * @param assessmentDTO AssessmentDetails
     * @param result        Diagnosis Result
     * @param reference     Observation Reference
     */
    private void createDiagnosticReport(Bundle bundle,
                                        String name,
                                        AssessmentDTO assessmentDTO,
                                        String result,
                                        String reference) {
        String uuid = fhirUtils.getUniqueId();
        DiagnosticReport diagnosticReport = fhirAssessmentMapper.mapDiagnosticReport(name,
                assessmentDTO,
                result,
                reference);
        String diagnosticReportUrl = StringUtil.concatString(String.valueOf(ResourceType.DiagnosticReport),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL,
                uuid);
        String fullDiagnosticReportUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(diagnosticReportUrl,
                fullDiagnosticReportUrl,
                Bundle.HTTPVerb.POST,
                diagnosticReport,
                bundle,
                assessmentDTO.getEncounter().getProvenance());
    }


    /**
     * Create Observation For Fever
     *
     * @param assessmentDTO Assessment Details
     * @param feverDTO      Fever Details
     * @param bundle        Bundle Object
     */
    public void createFever(AssessmentDTO assessmentDTO, FeverDTO feverDTO, Bundle bundle) {
        String uuid = fhirUtils.getUniqueId();
        Observation observationFever = fhirAssessmentMapper.setObservation(assessmentDTO.getEncounter(),
                Constants.HAS_FEVER,
                Constants.FEVER);
        if (Objects.nonNull(feverDTO.getRdtTest())) {
            createDiagnosticReport(bundle, Constants.RDT_TEST, assessmentDTO, feverDTO.getRdtTest(),
                    StringUtil.concatString(String.valueOf(ResourceType.Observation),
                            Constants.FORWARD_SLASH,
                            Constants.FHIR_BASE_URL,
                            uuid));
        }
        if (Objects.nonNull(feverDTO.getNoOfDaysOfFever())) {
            fhirAssessmentMapper.setQuantityToObservation(observationFever,
                    new Quantity(feverDTO.getNoOfDaysOfFever()),
                    Constants.NO_OF_DAYS_OF_FEVER
            );
        }
        if (Objects.nonNull(feverDTO.getAct())) {
            String actStatus = feverDTO.getAct().equalsIgnoreCase(Constants.DISPENSED) ?
                    Constants.COMPLETED : Constants.UNKNOWN;
            observationFever.addPartOf(new Reference(createMedicationDispense(bundle,
                    Constants.ACT,
                    assessmentDTO,
                    actStatus)));
        }
        if (Objects.nonNull(feverDTO.getTemperature())) {
            Quantity quantityMap = new Quantity();
            quantityMap.setValue(feverDTO.getTemperature());
            quantityMap.setSystem(Constants.TEMPERATURE_SYSTEM);
            quantityMap.setUnit(Constants.TEMPERATURE_UNIT);
            quantityMap.setCode(Constants.TEMPERATURE_CODE);
            fhirAssessmentMapper.setQuantityToObservation(observationFever,
                    quantityMap,
                    Constants.TEMPERATURE
            );
        }
        observationFever.setValue(new CodeableConcept().setText(
                Boolean.TRUE.equals(feverDTO.getHasFever()) ? Constants.YES : Constants.NO));
        String feverUrl = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, uuid);
        String fullFeverUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(feverUrl, fullFeverUrl, Bundle.HTTPVerb.POST, observationFever, bundle,
                assessmentDTO.getEncounter().getProvenance());
    }


    /**
     * Create Observation For Cough
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle Object
     */
    public void createCough(AssessmentDTO assessmentDTO, Bundle bundle) {
        ICCMDTO iccmDto = assessmentDTO.getAssessmentDetails().getIccm();
        String uuid = fhirUtils.getUniqueId();
        Observation observationCough = fhirAssessmentMapper.setObservation(assessmentDTO.getEncounter(),
                Constants.COUGH_OR_DIFFICULT_BREATHING,
                Constants.COUGH);
        if (Objects.nonNull(iccmDto.getCough().getNoOfDaysOfCough())) {
            fhirAssessmentMapper.setQuantityToObservation(observationCough,
                    new Quantity(iccmDto.getCough().getNoOfDaysOfCough()),
                    Constants.NO_OF_DAYS_OF_COUGH
            );
        }
        if (Objects.nonNull(iccmDto.getCough().getBreathPerMinute())) {
            fhirAssessmentMapper.setQuantityToObservation(observationCough,
                    new Quantity(iccmDto.getCough().getBreathPerMinute()),
                    Constants.BREATH_PER_MINUTE
            );
        }
        if (Objects.nonNull(iccmDto.getCough().getAmoxicillin())) {
            String amoxicillinStatus = iccmDto.getCough()
                    .getAmoxicillin()
                    .equalsIgnoreCase(Constants.DISPENSED) ? Constants.COMPLETED : Constants.UNKNOWN;
            observationCough.addPartOf(new Reference(createMedicationDispense(bundle, Constants.AMOXICILLIN,
                    assessmentDTO, amoxicillinStatus)));
        }
        if (Objects.nonNull(iccmDto.getCough().getChestInDrawing())) {
            fhirAssessmentMapper.createObservationComponent(iccmDto.getCough().getChestInDrawing(),
                    Constants.CHEST_DRAWING,
                    observationCough.getComponent());
        }
        observationCough.setValue(new CodeableConcept().setText(
                Boolean.TRUE.equals(iccmDto.getCough().getHasCough()) ? Constants.YES : Constants.NO));
        String coughUrl = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, uuid);
        String fullCoughUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);

        fhirUtils.setBundle(coughUrl,
                fullCoughUrl,
                Bundle.HTTPVerb.POST,
                observationCough,
                bundle,
                assessmentDTO.getEncounter().getProvenance());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseEntity<FhirResponseDTO> createTB(AssessmentDTO assessmentDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String uuid = fhirUtils.getUniqueId();

        Encounter encounter = fhirAssessmentMapper.setEncounter(new Encounter(),
                assessmentDTO.getEncounter(),
                assessmentDTO.getAssessmentType(), Boolean.TRUE);
        String encounterReference = StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(encounterReference,
                StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                Bundle.HTTPVerb.POST,
                encounter,
                bundle,
                assessmentDTO.getEncounter().getProvenance());
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(), Constants.HAS_COUGH,
                assessmentDTO.getAssessmentDetails().getTb().isHasCough(),
                assessmentDTO.getAssessmentType(),
                bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.COUGH_LASTED_THAN_2_WEEKS,
                assessmentDTO.getAssessmentDetails().getTb().isCoughLastedThan2Weeks(),
                assessmentDTO.getAssessmentType(),
                bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(), Constants.HAS_FEVER,
                assessmentDTO.getAssessmentDetails().getTb().isHasFever(),
                assessmentDTO.getAssessmentType(),
                bundle);
        fhirAssessmentMapper.createObservation(assessmentDTO.getEncounter(),
                Constants.WEIGHT_LOSS,
                assessmentDTO.getAssessmentDetails().getTb().isWeightLoss(),
                assessmentDTO.getAssessmentType(),
                bundle);
        return restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * Creates ANC observation.
     *
     * @param assessment assessment Details
     * @param bundle     Bundle Object
     */
    private void createANCObservation(AssessmentDTO assessment, Bundle bundle) {
        AncDTO anc = assessment.getAssessmentDetails().getAnc();
        Observation ancObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.ANC, Constants.ANC);
        ancObservation.setComponent(createComponentsForAnc(anc));
        String fancidarTabletStatus = Boolean.TRUE.equals(anc.getTakesFancidarTablets()) ? Constants.COMPLETED : Constants.UNKNOWN;
        ancObservation.addPartOf(new Reference(createMedicationDispense(bundle,
                Constants.TAKES_FANCIDAR_TABLETS,
                assessment,
                fancidarTabletStatus)));
        String ironFloatTabletStatus = Boolean.TRUE.equals(anc.getTakesIronFloatTablets()) ? Constants.COMPLETED : Constants.UNKNOWN;
        ancObservation.addPartOf(new Reference(createMedicationDispense(bundle,
                Constants.TAKES_IRON_FLOAT_TABLETS,
                assessment,
                ironFloatTabletStatus)));
        if (!Objects.isNull(anc.getAncSigns()) && !anc.getAncSigns().isEmpty()) {
            Observation observation =
                    fhirAssessmentMapper.createSignsObservation(anc.getAncSigns(),
                            assessment.getEncounter(), Constants.SIGNS,
                            anc.getOtherSigns());
            if (!Objects.isNull(observation)) {
                String signReference = fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                        assessment.getEncounter().getProvenance());
                ancObservation.addHasMember(new Reference(signReference));
            }
        }
    }

    /**
     * Creates a list of components to observation for ANC.
     *
     * @param anc Anc Details
     * @return List<ObservationComponentComponent>
     */
    private List<ObservationComponentComponent> createComponentsForAnc(AncDTO anc) {
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();

        fhirAssessmentMapper.createObservationComponent(anc.getLastMenstrualPeriod(),
                Constants.LAST_MENSTRUAL_PERIOD,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getEstimatedDeliveryDate(),
                Constants.ESTIMATED_DELIVERY_DATE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getIsMalePartnerPresent(),
                Constants.IS_MALE_PARTNER_PRESENT,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getSleepsUnderBedNet(),
                Constants.SLEEPS_UNDER_BED_NET,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getEatsMoreThanBefore(),
                Constants.EATS_MORE_THAN_BEFORE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getTakesIronFloatTablets(),
                Constants.TAKES_IRON_FLOAT_TABLETS,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getPriorityPregnancy(),
                Constants.PRIORITY_PREGNANCY,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getMiscarriage(),
                Constants.MISCARRIAGE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getDeathOfMother(),
                Constants.DEATH_OF_MOTHER,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getPlaceOfDelivery(),
                Constants.PLACE_OF_DELIVERY,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getOtherPlaceOfDelivery(),
                Constants.OTHER_PLACE_OF_DELIVERY,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getEats4GroupIronVitARichFoods(),
                Constants.EATS_4GROUP_IRON_VITA_RICH_FOODS,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getGestationalAge(),
                Constants.GESTATIONAL_AGE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(anc.getBirthPlanMade(),
                Constants.BIRTH_PLAN_MADE,
                componentComponents);
        return componentComponents;
    }

    /**
     * Creates ANC observation.
     *
     * @param assessment Assessment Details
     * @param bundle     Bundle Object
     */
    private void createPNCMotherObservation(AssessmentDTO assessment, Bundle bundle) {
        PncMotherDTO pnc = assessment.getAssessmentDetails().getPncMother();
        if (assessment.getEncounter().getVisitNumber() == Constants.ONE) {
            fhirAssessmentMapper.createVitalObservation(bundle, assessment.getEncounter(),
                    Constants.NO_OF_NEONATES, Objects.isNull(pnc.getNoOfNeonates()) ? Constants.ZERO : pnc.getNoOfNeonates(),
                    assessment.getEncounter().getPatientReference());

            fhirAssessmentMapper.createVitalObservation(bundle, assessment.getEncounter(),
                    Constants.NEONATE_PATIENT_ID, pnc.getNeonatePatientId(),
                    assessment.getEncounter().getPatientReference());

            fhirAssessmentMapper.createVitalObservation(bundle, assessment.getEncounter(),
                    Constants.DATE_OF_DELIVERY, pnc.getDateOfDelivery(),
                    assessment.getEncounter().getPatientReference());

            fhirAssessmentMapper.createVitalObservation(bundle, assessment.getEncounter(),
                    Constants.PNC_CREATED_DATE, assessment.getEncounter().getProvenance().getModifiedDate(),
                    assessment.getEncounter().getPatientReference());
        }
        Observation pncObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.PNC, Constants.PNC);
        createMedicationDispense(bundle, Constants.CHLORHEXIDINE, assessment, Boolean.TRUE.equals(pnc.getChlorhexidine()) ? Constants.COMPLETED : Constants.UNKNOWN);
        pncObservation.setComponent(createComponentsForPncMother(pnc));
        if (!Objects.isNull(pnc.getPncMotherSigns()) && !pnc.getPncMotherSigns().isEmpty()) {
            Observation observation =
                    fhirAssessmentMapper.createSignsObservation(pnc.getPncMotherSigns(),
                            assessment.getEncounter(), Constants.SIGNS,
                            pnc.getOtherSigns());
            if (!Objects.isNull(observation)) {
                String signReference = fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                        assessment.getEncounter().getProvenance());
                pncObservation.addHasMember(new Reference(signReference));
            }
        }

    }

    /**
     * Creates a ANC observation.
     *
     * @param assessment Assessment Details
     * @param bundle     Bundle Object
     */
    private void createPNCNeonatalObservation(AssessmentDTO assessment, Bundle bundle) {
        PncNeonatalDTO pnc = assessment.getAssessmentDetails().getPncNeonatal();
        Observation pncObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.PNC_NEONATE, Constants.PNC_NEONATE);
        pncObservation.setComponent(createComponentsForPncNeonatal(pnc));
        if (!Objects.isNull(pnc.getPncNeonatalSigns()) && !pnc.getPncNeonatalSigns().isEmpty()) {
            Observation observation =
                    fhirAssessmentMapper.createSignsObservation(pnc.getPncNeonatalSigns(),
                            assessment.getEncounter(), Constants.SIGNS,
                            pnc.getOtherSigns());
            if (!Objects.isNull(observation)) {
                String signReference = fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                        assessment.getEncounter().getProvenance());
                pncObservation.addHasMember(new Reference(signReference));
            }
        }
    }

    /**
     * Creates a PNC child observation.
     *
     * @param assessment Assessment Details
     * @param bundle     Bundle Object
     */
    private void createPNCChildObservation(AssessmentDTO assessment, Bundle bundle) {
        PncChildDTO pnc = assessment.getAssessmentDetails().getPncChild();
        Observation pncObservation = fhirAssessmentMapper.setObservation(assessment.getEncounter(),
                Constants.CHILDHOOD, Constants.CHILDHOOD);
        pncObservation.setComponent(createComponentsForPncChild(pnc));
        if (!Objects.isNull(pnc.getPncChildSigns()) && !pnc.getPncChildSigns().isEmpty()) {
            Observation observation =
                    fhirAssessmentMapper.createSignsObservation(pnc.getPncChildSigns(),
                            assessment.getEncounter(), Constants.SIGNS,
                            pnc.getOtherSigns());
            if (!Objects.isNull(observation)) {
                String signReference = fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                        assessment.getEncounter().getProvenance());
                pncObservation.addHasMember(new Reference(signReference));
            }
        }
    }

    /**
     * Creates a list of components to observation for PNC Mother.
     *
     * @param pnc Anc Details
     * @return List<ObservationComponentComponent>
     */
    private List<ObservationComponentComponent> createComponentsForPncMother(PncMotherDTO pnc) {
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();

        fhirAssessmentMapper.createObservationComponent(pnc.getDateOfDelivery(),
                Constants.DATE_OF_DELIVERY,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getNoOfNeonates(),
                Constants.NO_OF_NEONATES,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getChlorhexidine(),
                Constants.CHLORHEXIDINE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getFatherPresent(),
                Constants.FATHER_PRESENT,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getExclusivelyBreastfeeding(),
                Constants.EXCLUSIVELY_BREASTFEEDING,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getSleepsUnderBedNet(),
                Constants.SLEEPS_UNDER_BED_NET,
                componentComponents);
        return componentComponents;
    }

    /**
     * Creates a list of components to observation for PNC Neonatal.
     *
     * @param pnc PNC Details
     * @return List<ObservationComponentComponent>
     */
    private List<ObservationComponentComponent> createComponentsForPncNeonatal(PncNeonatalDTO pnc) {
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();

        fhirAssessmentMapper.createObservationComponent(pnc.getDeathOfNewborn(),
                Constants.DEATH_OF_NEW_BORN,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getLowBirthWeight(),
                Constants.LOW_BIRTH_WEIGHT,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getNewbornReferredToSBCU(),
                Constants.NEWBORN_REFERRED_TO_SBCU,
                componentComponents);
        return componentComponents;
    }

    /**
     * Creates a list of components to observation for PNC Child.
     *
     * @param pnc PNC Details
     * @return List<ObservationComponentComponent>
     */
    private List<ObservationComponentComponent> createComponentsForPncChild(PncChildDTO pnc) {
        List<ObservationComponentComponent> componentComponents = new ArrayList<>();

        fhirAssessmentMapper.createObservationComponent(pnc.getExclusivelyBreastfeeding(),
                Constants.EXCLUSIVELY_BREASTFEEDING,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getFatherPresent(),
                Constants.FATHER_PRESENT,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getMuac(), Constants.MUAC, componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getDeathOfBaby(),
                Constants.DEATH_OF_BABY,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getFedFrom4FoodGroups(),
                Constants.FED_FROM_4FOOD_GROUPS,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getMotherOrPartnerUsingFamilyPlanning(),
                Constants.MOTHER_OR_PARTNER_USING_FAMILY_PLANNING,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getTakingMinimumMealsPerDay(),
                Constants.TAKING_MINIMUM_MEALS_PER_DAY,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getSleepsUnderBedNet(),
                Constants.SLEEPS_UNDER_BED_NET,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getPlannedVisitDate(),
                Constants.PLANNED_VISIT_DATE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getActualVisitDate(),
                Constants.ACTUAL_VISIT_DATE,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getPentaOpvGiven(),
                Constants.PENTA_OPV_GIVEN,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getMeasles1Given(),
                Constants.MEASLES_ONE_GIVEN,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getYellowFeverVacineGiven(),
                Constants.YELLOW_FEVER_VACCINE_GIVEN,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getMeasles2Given(),
                Constants.MEASLES_TWO_GIVEN,
                componentComponents);
        fhirAssessmentMapper.createObservationComponent(pnc.getPostReferralFollowUpDone(),
                Constants.POST_REFERRAL_FOLLOWUP_DONE,
                componentComponents);
        return componentComponents;
    }

    /**
     * Check and close RMNCH details
     *
     * @param assessmentDTO Assessment Details
     * @param bundle        Bundle object
     */
    public void checkAndCloseRmnchDetails(AssessmentDTO assessmentDTO, Bundle bundle) {
        HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberById(assessmentDTO.getEncounter().getMemberId());
        PregnancyInfo pregnancyInfo = getPatientVitals(assessmentDTO.getEncounter().getMemberId());
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setProvenance(assessmentDTO.getEncounter().getProvenance());
        requestDTO.setMemberId(assessmentDTO.getEncounter().getMemberId());
        requestDTO.setEncounterId(assessmentDTO.getEncounter().getId());
        requestDTO.setClosedEncounterType(assessmentDTO.getAssessmentType());
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

    /**
     * Creates Provisional treatment plan for a patient.
     *
     * @param assessmentDto
     * @return TreatmentPlanResponseDTO
     */
    public TreatmentPlanResponseDTO createProvisionalTreatmentPlan(AssessmentDTO assessmentDto) {
        CarePlan patientTreatmentPlan = patientTreatmentPlanService.getCarePlanForPatient(assessmentDto.getMemberReference());
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();

        List<String> details = new ArrayList<>();
        if (!Objects.isNull(patientTreatmentPlan)) {
            patientTreatmentPlan.getActivity().forEach(
                   activityComponent ->  details.add(activityComponent.getDetail().getCode().getText())
            );
        }
        TreatmentPlanResponseDTO response = new TreatmentPlanResponseDTO();
        if (!Objects.isNull(patientTreatmentPlan) && (details.contains(Constants.FREQUENCY_BP_CHECK) || details.contains(Constants.FREQUENCY_BG_CHECK))) {
            if (!Objects.isNull(assessmentDto.getBpLog()) && details.contains(Constants.FREQUENCY_BP_CHECK)) {
                Date nextBpDate = patientTreatmentPlanService.updateNextVisitDateForPatient(assessmentDto.getMemberReference(),assessmentDto.getPatientId(), Constants.FREQUENCY_BP_CHECK, patientTreatmentPlan, assessmentDto.getEncounter().getProvenance(), assessmentDto.getAssessmentTakenOn());
                treatmentPlanDTO.setNextBpAssessmentDate(nextBpDate);
            }
            if (!Objects.isNull(assessmentDto.getGlucoseLog())
                    && (!Objects.isNull(assessmentDto.getGlucoseLog().getGlucoseValue())
                    || !Objects.isNull(assessmentDto.getGlucoseLog().getHba1c()))
                    && details.contains(Constants.FREQUENCY_BG_CHECK)) {
                Date nextBgDate = patientTreatmentPlanService.updateNextVisitDateForPatient(assessmentDto.getMemberReference(),assessmentDto.getPatientId(), Constants.FREQUENCY_BG_CHECK, patientTreatmentPlan, assessmentDto.getEncounter().getProvenance(), assessmentDto.getAssessmentTakenOn());
                treatmentPlanDTO.setNextBgAssessmentDate(nextBgDate);
            }
        }

        if (Constants.ASSESSMENT.equals(assessmentDto.getType())
                && (Objects.isNull(patientTreatmentPlan) || Objects.equals(CarePlan.CarePlanStatus.DRAFT, patientTreatmentPlan.getStatus()))
                && ((StringUtils.isNotBlank(assessmentDto.getCvdRiskLevel()) && !new HashSet<>(details).containsAll(List.of(Constants.FREQUENCY_BP_CHECK, Constants.FREQUENCY_BG_CHECK, Constants.FREQUENCY_HBA1C_CHECK, Constants.FREQUENCY_MEDICAL_REVIEW)))
                || (Objects.nonNull(assessmentDto.getPregnancyAnc()) && !new HashSet<>(details).containsAll(List.of(Constants.FREQUENCY_MEDICAL_REVIEW, Constants.FREQUENCY_CHO_CHECK))))) {
            boolean isPregnancyAnc = !Objects.isNull(assessmentDto.getPregnancyAnc());
            treatmentPlanDTO.setBGDefaultFrequency((!Objects.isNull(assessmentDto.getGlucoseLog()) &&
                    !Objects.isNull(assessmentDto.getGlucoseLog().getGlucoseValue())));
            treatmentPlanDTO.setHba1c(!Objects.isNull(assessmentDto.getGlucoseLog()) && (!Objects.isNull(assessmentDto.getGlucoseLog().getGlucoseValue())
                    || !Objects.isNull(assessmentDto.getGlucoseLog().getHba1c())) ? Boolean.TRUE : Boolean.FALSE);
            treatmentPlanDTO.setPregnancyAnc(isPregnancyAnc);
            treatmentPlanDTO.setCvdRiskLevel(assessmentDto.getCvdRiskLevel());
            treatmentPlanDTO.setProvenance(assessmentDto.getEncounter().getProvenance());
            treatmentPlanDTO.setMemberReference(assessmentDto.getMemberReference());
            treatmentPlanDTO.setPatientReference(Objects.isNull(assessmentDto.getPatientReference()) ?
                    assessmentDto.getPatientId() : assessmentDto.getPatientReference());
            treatmentPlanDTO.setFrequencies(spiceServiceApiInterface.getFrequencies(CommonUtil.getAuthToken(), CommonUtil.getClient()));
            response = patientTreatmentPlanService.createProvisionalPlan(treatmentPlanDTO, null, Objects.isNull(patientTreatmentPlan) ? null : patientTreatmentPlan, details);
        }
        assessmentDto.setNextBgAssessmentDate(treatmentPlanDTO.getNextBgAssessmentDate());
        assessmentDto.setNextBpAssessmentDate(treatmentPlanDTO.getNextBpAssessmentDate());
        assessmentDto.setNextMedicalReviewDate(treatmentPlanDTO.getNextMedicalReviewDate());
        if (Objects.nonNull(response)) {
            assessmentDto.setTreatmentPlanResponse(convertToAssessmentTreatmentPlan(response));
        }
        return response;
    }

    /**
     * <p>
     * Convert treatment plan response to assessment treatment plan response
     * </p>
     *
     * @param treatmentPlanResponseDTO  TreatmentPlanResponseDTO entity contains bp, bg and hba1c frequencies
     *
     * @return AssessmentTreatmentPlanDTO entity
     */
    private AssessmentTreatmentPlanDTO convertToAssessmentTreatmentPlan(TreatmentPlanResponseDTO treatmentPlanResponseDTO) {
        AssessmentTreatmentPlanDTO assessmentTreatmentPlan = new AssessmentTreatmentPlanDTO();

        if (Objects.nonNull(treatmentPlanResponseDTO.getBpCheckFrequency())) {
            TreatmentPlanFrequencyDTO treatmentPlanFrequencyDTO = new TreatmentPlanFrequencyDTO();
            treatmentPlanFrequencyDTO.setLabel(Constants.BP_CHECK_FREQUENCY);
            treatmentPlanFrequencyDTO.setValue(treatmentPlanResponseDTO.getBpCheckFrequency());
            assessmentTreatmentPlan.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
        }
        if (Objects.nonNull(treatmentPlanResponseDTO.getBgCheckFrequency())) {
            TreatmentPlanFrequencyDTO treatmentPlanFrequencyDTO = new TreatmentPlanFrequencyDTO();
            treatmentPlanFrequencyDTO.setLabel(Constants.BG_CHECK_FREQUENCY);
            treatmentPlanFrequencyDTO.setValue(treatmentPlanResponseDTO.getBgCheckFrequency());
            assessmentTreatmentPlan.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
        }
        if (Objects.nonNull(treatmentPlanResponseDTO.getMedicalReviewFrequency())) {
            TreatmentPlanFrequencyDTO treatmentPlanFrequencyDTO = new TreatmentPlanFrequencyDTO();
            treatmentPlanFrequencyDTO.setLabel(Constants.MEDICAL_REVIEW_FREQUENCY);
            treatmentPlanFrequencyDTO.setValue(treatmentPlanResponseDTO.getMedicalReviewFrequency());
            assessmentTreatmentPlan.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
        }
        if (Objects.nonNull(treatmentPlanResponseDTO.getHba1cCheckFrequency())) {
            TreatmentPlanFrequencyDTO treatmentPlanFrequencyDTO = new TreatmentPlanFrequencyDTO();
            treatmentPlanFrequencyDTO.setLabel(Constants.HBA1C_CHECK_FREQUENCY);
            treatmentPlanFrequencyDTO.setValue(treatmentPlanResponseDTO.getHba1cCheckFrequency());
            assessmentTreatmentPlan.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
        }
        if (Objects.nonNull(treatmentPlanResponseDTO.getChoCheckFrequency())) {
            TreatmentPlanFrequencyDTO treatmentPlanFrequencyDTO = new TreatmentPlanFrequencyDTO();
            treatmentPlanFrequencyDTO.setLabel(Constants.CHO_CHECK_FREQUENCY);
            treatmentPlanFrequencyDTO.setValue(treatmentPlanResponseDTO.getChoCheckFrequency());
            assessmentTreatmentPlan.getTreatmentPlan().add(treatmentPlanFrequencyDTO);
        }
        if (Objects.nonNull(treatmentPlanResponseDTO.getCarePlanId())) {
            assessmentTreatmentPlan.setCarePlanId(treatmentPlanResponseDTO.getCarePlanId());
        }
        return assessmentTreatmentPlan;
    }

    /**
     * {@inheritDoc}
     */
    public BpLogDTO getLatestBpLog(AssessmentDTO assessmentDto) {
        String url = String.format(Constants.GET_LATEST_OBSERVATION, MetaCodeConstants.BLOOD_PRESSURE_KEY,
                StringUtil.concatString(ResourceType.RelatedPerson.toString(),
                        Constants.FORWARD_SLASH, assessmentDto.getMemberReference()));
        Bundle observationBundle = restApiUtil.getBatchRequest(url);
        BpLogDTO bpLogDTO = new BpLogDTO();
        observationBundle.getEntry().forEach(entry -> {
            Observation observation = (Observation) entry.getResource();
            bpLogDTO.setBpTakenOn(observation.getEffectiveDateTimeType().getValue());
        });
        return bpLogDTO;
    }

    /**
     * {@inheritDoc}
     */
    public GlucoseLogDTO getLatestGlucoseLog(AssessmentDTO assessmentDto) {
        String url = String.format(Constants.GET_LATEST_OBSERVATION, MetaCodeConstants.BLOOD_GLUCOSE_KEY,
                StringUtil.concatString(ResourceType.RelatedPerson.toString(),
                        Constants.FORWARD_SLASH, assessmentDto.getMemberReference()));
        Bundle observationBundle = restApiUtil.getBatchRequest(url);
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        observationBundle.getEntry().forEach(entry -> {
            Observation observation = (Observation) entry.getResource();
            glucoseLogDTO.setBgTakenOn(observation.getEffectiveDateTimeType().getValue());
            if (observation.hasValueQuantity() && Objects.nonNull(observation.getValueQuantity().getValue())) {
                glucoseLogDTO.setGlucoseValue(observation.getValueQuantity().getValue().doubleValue());
            }
        });
        return glucoseLogDTO;
    }
}