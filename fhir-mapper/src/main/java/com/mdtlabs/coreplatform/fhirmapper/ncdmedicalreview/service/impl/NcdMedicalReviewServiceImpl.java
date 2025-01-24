package com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.MedicationStatement.MedicationStatementStatus;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ContinuousMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.CurrentMedicationDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.InitialMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewMetaDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientStatusConverter;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.NcdMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * <p>
 * This class is a service class to perform operation on iccm medical review
 * operations.
 * </p>
 *
 * @author Karthick M created on Sep 01, 2024
 */
@Service
public class NcdMedicalReviewServiceImpl implements NcdMedicalReviewService {

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final PatientConverter patientConverter;

    private final PatientStatusConverter patientStatusConverter;

    private final EncounterConverter encounterConverter;

    private final PatientVisitService patientVisitService;

    private final PatientTreatmentPlanService patientTreatmentPlanService;

    private final PatientService patientService;

    private final PrescriptionRequestService prescriptionRequestService;

    private final FhirMapper fhirMapper;

    @Autowired
    public NcdMedicalReviewServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil, FhirAssessmentMapper fhirAssessmentMapper,
                                       PatientConverter patientConverter, PatientStatusConverter patientStatusConverter,
                                       EncounterConverter encounterConverter, PatientVisitService patientVisitService,
                                       PatientTreatmentPlanService patientTreatmentPlanService, PatientService patientService,
                                       PrescriptionRequestService prescriptionRequestService, FhirMapper fhirMapper) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.patientConverter = patientConverter;
        this.patientStatusConverter = patientStatusConverter;
        this.encounterConverter = encounterConverter;
        this.patientVisitService = patientVisitService;
        this.patientTreatmentPlanService = patientTreatmentPlanService;
        this.patientService = patientService;
        this.prescriptionRequestService = prescriptionRequestService;
        this.fhirMapper = fhirMapper;
    }

    private static final String GET_RELATED_PERSON_BY_ID_WITH_PATIENT = "RelatedPerson?_id=%s&_include=RelatedPerson:patient";
    private static final String GET_PATIENT_DIAGNOSIS = "Condition?identifier=%s|&subject=Patient/%s";
    private static final String GET_ENCOUNTER_DETAILS = "Encounter/%s/$everything?_count=9999999";
    private static final String GET_LIFESTYLE_DETAILS = "Observation?identifier=%s|%s&subject=Patient/%s";

    /**
     * {@inheritDoc}
     */
    public Map<String, String> createNcdMedicalReview(NCDMedicalReviewDTO request) {
        fhirUtils.initiateCodesMap();
        Bundle transactionBundle = new Bundle().setType(BundleType.TRANSACTION);
        Patient patient = null;
        RelatedPerson relatedPerson = null;
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_RELATED_PERSON_BY_ID_WITH_PATIENT,
                request.getMemberReference()));
        if (!bundle.isEmpty()) {
            for (BundleEntryComponent entry : bundle.getEntry()) {
                if (entry.getResource() instanceof Patient patientResource) {
                    patient = patientResource;
                }
                if (entry.getResource() instanceof RelatedPerson relatedPersonResource) {
                    relatedPerson = relatedPersonResource;
                }
            }
        } else {
            throw new SpiceValidation(1000);
        }
        String visitId = request.getEncounterReference();
        if (Objects.isNull(request.getPatientReference()) && Objects.isNull(patient)) {
            patient = createPatient(request, relatedPerson, transactionBundle);
        }
        createEncounter(request, transactionBundle);
        if (!Objects.isNull(request.getInitialMedicalReview())) {
            createInitialEncounter(request, transactionBundle, patient, relatedPerson);
        }
        if (!Objects.isNull(request.getContinuousMedicalReview())) {
            createContinuousMedicalReview(request, transactionBundle);
        }
        Encounter encounter = patientVisitService.updatePatientVisitStatus(visitId, false, true, false, request.getPatientReference());
        fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH, visitId),
                StringUtil.concatString(Constants.FHIR_BASE_URL, visitId),
                Bundle.HTTPVerb.PUT, encounter, transactionBundle, request.getProvenance());
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transactionBundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, String> response = new HashMap<>();
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        response.put(Constants.KEY_ENCOUNTER_REFERENCE, !Objects.isNull(fhirResponse.get(String.valueOf(ResourceType.Encounter)))
                && !fhirResponse.get(String.valueOf(ResourceType.Encounter)).isEmpty()
                ? fhirResponse.get(String.valueOf(ResourceType.Encounter)).getFirst()
                : fhirUtils.getIdFromReference(request.getEncounterReference()));

        response.put(Constants.KEY_PATIENT_REFERENCE, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                !fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() :
                fhirUtils.getIdFromReference(request.getPatientReference()));
        return response;

    }

    /**
     * <p>
     * Updates the status of the patient and related person to "MEDICAL_REVIEWED" if their current status is "SCREENED" or "ASSESSED".
     * </p>
     *
     * @param patient           The Patient object whose status needs to be updated.
     * @param relatedPerson     The RelatedPerson object whose status needs to be updated.
     * @param transactionBundle The Bundle object to which the updated resources will be added.
     * @param provenance        The ProvenanceDTO object containing provenance information for the transaction.
     */
    private void updatePatientStatus(Patient patient, RelatedPerson relatedPerson, Bundle transactionBundle, ProvenanceDTO provenance) {
        if (Objects.nonNull(patient)) {
            patient.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && (FhirConstants.SCREENED.equals(
                        identifier.getValue())) || FhirConstants.ASSESSED.equals(identifier.getValue())) {
                    identifier.setValue(FhirConstants.MEDICAL_REVIEWED);
                }
            });
        }
        if (Objects.nonNull(relatedPerson)) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && (FhirConstants.SCREENED.equals(
                        identifier.getValue())) || FhirConstants.ASSESSED.equals(identifier.getValue())) {
                    identifier.setValue(FhirConstants.MEDICAL_REVIEWED);
                }
            });
        }
        if (Objects.nonNull(patient) && Objects.nonNull(patient.getId())) {
            String id = fhirUtils.getIdFromHistoryUrl(patient.getId());
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, id),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, id),
                    Bundle.HTTPVerb.PUT, patient, transactionBundle, provenance);
        }
        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getId())) {
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, relatedPerson.getIdPart()),
                    Constants.EMPTY_SPACE,
                    Bundle.HTTPVerb.PUT, relatedPerson, transactionBundle, provenance);
        }
    }

    /**
     * Creates encounter for medical review.
     *
     * @param request
     * @param transcationBundle
     * @return encounter fullUrl
     */
    private String createEncounter(NCDMedicalReviewDTO request, Bundle transcationBundle) {
        Encounter encounter = encounterConverter.createEncounter(request.getPatientReference(), request.getMemberReference(), FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                Constants.NCD_MEDICAL_REVIEW_ENCOUNTER_TYPE, request.getProvenance());
        encounter.setPartOf(fhirUtils.getReferenceUrl(ResourceType.Encounter, request.getEncounterReference()));
        String uuid = fhirUtils.getUniqueId();
        String id = StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL,
                uuid);
        request.setEncounterReference(StringUtil.concatString(Constants.FHIR_BASE_URL, uuid));
        fhirUtils.setBundle(id, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid), Bundle.HTTPVerb.POST,
                encounter,
                transcationBundle,
                request.getProvenance());
        return StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);

    }

    /**
     * Creates initial encounter.
     *
     * @param request
     * @param transactionBundle
     * @param patient
     */
    public void createInitialEncounter(NCDMedicalReviewDTO request, Bundle transactionBundle, Patient patient, RelatedPerson relatedPerson) {
        List<Condition> exisitingPatientDiagnosis = getPatientDiagnosis(request.getPatientReference());
        if (!Objects.isNull(request.getInitialMedicalReview().getDiagnosis())) {
            request.getInitialMedicalReview().getDiagnosis().setProvenance(request.getProvenance());
            request.getInitialMedicalReview().getDiagnosis().setPatientReference(request.getPatientReference());
            request.getInitialMedicalReview().getDiagnosis().setEncounterReference(request.getEncounterReference());
            createPatientDiagnosis(exisitingPatientDiagnosis, request.getInitialMedicalReview().getDiagnosis(), transactionBundle);
        }
        createInitialMedicalReview(request, transactionBundle, patient, relatedPerson);
    }

    /**
     * Creates initial medical review.
     *
     * @param request
     * @param bundle
     */
    private void createInitialMedicalReview(NCDMedicalReviewDTO request, Bundle bundle, Patient patient, RelatedPerson relatedPerson) {
        InitialMedicalReviewDTO initialMedicalReview = request.getInitialMedicalReview();
        createCurrentMedication(initialMedicalReview.getCurrentMedications(), request, bundle);
        createComorbidity(initialMedicalReview.getComorbidities(), request, bundle);
        createComplications(initialMedicalReview.getComplications(), request, bundle);
        createLifestyle(initialMedicalReview.getLifestyle(), request, bundle);
        updatePatientStatus(patient, relatedPerson, bundle, request.getProvenance());
    }

    /**
     * Creates continuous medical review.
     *
     * @param request medical review request.
     * @param bundle  transcation bundle.
     */
    private void createContinuousMedicalReview(NCDMedicalReviewDTO request, Bundle bundle) {
        ContinuousMedicalReviewDTO continuousMedicalReview = request.getContinuousMedicalReview();
        createComorbidity(continuousMedicalReview.getComorbidities(), request, bundle);
        createComplications(continuousMedicalReview.getComplications(), request, bundle);
        createPhysicalExaminations(continuousMedicalReview.getPhysicalExams(), continuousMedicalReview.getPhysicalExamComments(), request, bundle);
        createComplaints(continuousMedicalReview.getComplaints(), continuousMedicalReview.getComplaintComments(), request, bundle);
        createClinicalNotes(continuousMedicalReview.getClinicalNote(), request, bundle);

    }

    /**
     * Creates condition.
     *
     * @param conditionName
     * @param identifierValue
     * @param request
     * @param bundle
     */
    private void createCondition(String conditionName, String identifierValue, NCDMedicalReviewDTO request,
                                 Bundle bundle) {
        Condition condition = new Condition();
        condition.addIdentifier().setSystem(FhirIdentifierConstants.CONDITION_TYPE_SYSTEM_URL).setValue(identifierValue);
        condition.setCode(fhirUtils.createCodeableConcept(conditionName));
        CodeableConcept codeableConcept = new CodeableConcept();
        Coding coding = new Coding(Constants.CONDITION_CLINCAL_STATUS_SYSTEM, Constants.CLINCAL_STATUS_CODE_ACTIVE, Constants.CLINCAL_STATUS_DISPLAY_ACTIVE);
        codeableConcept.setCoding(List.of(coding));
        condition.setClinicalStatus(codeableConcept);
        condition.setRecordedDate(new Date());
        condition.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, request.getPatientReference()));
        condition.setEncounter(fhirUtils.getReferenceUrl(ResourceType.Encounter, request.getEncounterReference()));
        condition.setAsserter(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, request.getMemberReference()));
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, condition, bundle, request.getProvenance());
    }

    /**
     * Creates medication statement.
     *
     * @param medicationName
     * @param identifierValue
     * @param request
     * @param bundle
     * @return MedicationStatement
     */
    private MedicationStatement createMedicationStatement(String medicationName, String identifierValue, NCDMedicalReviewDTO request, Bundle bundle) {
        MedicationStatement medicationStatement = new MedicationStatement();
        medicationStatement.addIdentifier().setSystem(FhirIdentifierConstants.MEDICATION_STATEMENT_TYPE_SYSTEM_URL).setValue(identifierValue);
        medicationStatement.setMedication(fhirUtils.createCodeableConcept(medicationName));
        medicationStatement.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, request.getPatientReference()));
        medicationStatement.setInformationSource(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, request.getMemberReference()));
        medicationStatement.setStatus(MedicationStatementStatus.ACTIVE);
        medicationStatement.setContext(fhirUtils.getReferenceUrl(ResourceType.Encounter, request.getEncounterReference()));
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.MedicationStatement), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, medicationStatement, bundle, request.getProvenance());

        return medicationStatement;
    }

    /**
     * Creates observation for medical reviews.
     *
     * @param observationName
     * @param request
     * @return Observation
     */
    private Observation createObservation(String observationName, NCDMedicalReviewDTO request) {
        Observation observation = new Observation();
        observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue(observationName);
        observation.setStatus(Observation.ObservationStatus.PRELIMINARY);
        observation.setEncounter(fhirUtils.getReferenceUrl(ResourceType.Encounter, request.getEncounterReference()));
        observation.getEffectiveDateTimeType().setValue(new Date());
        observation.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, request.getPatientReference()));
        observation.addPerformer(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, request.getMemberReference()));
        observation.setCode(fhirUtils.setCodes(observationName));
        return observation;
    }

    /**
     * Adds observation to bundle.
     *
     * @param observation
     * @param bundle
     * @param provenance
     * @return String
     */
    private String addObservationToBundle(Observation observation, Bundle bundle, ProvenanceDTO provenance) {
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle, provenance);
        return fullUrl;
    }

    /**
     * Creates currend medication.
     *
     * @param currentMedication
     * @param request
     * @param bundle
     */
    private void createCurrentMedication(CurrentMedicationDetailsDTO currentMedication, NCDMedicalReviewDTO request, Bundle bundle) {
        if (!Objects.isNull(currentMedication)
                && (!Objects.isNull(currentMedication.getMedications()))
                && !currentMedication.getMedications().isEmpty()) {

            Observation observation = createObservation(Constants.OBSERVATION_CURRENT_MEDICATION, request);

            CurrentMedicationDetailsDTO currentMedicationDetails = request.getInitialMedicalReview().getCurrentMedications();
            List<ObservationComponentComponent> componentComponents = new ArrayList<>();
            fhirAssessmentMapper.createObservationComponent(currentMedicationDetails.isAdheringCurrentMed(),
                    MetaCodeConstants.IS_ADHERING_CURRENT_MEDICATION, componentComponents);
            fhirAssessmentMapper.createObservationComponent(currentMedicationDetails.isDrugAllergies(),
                    MetaCodeConstants.IS_DRUG_ALLERGIES, componentComponents);
            fhirAssessmentMapper.createObservationComponent(currentMedicationDetails.getAdheringMedComment(),
                    MetaCodeConstants.ADHERING_MEDICATION_COMMENT, componentComponents);
            fhirAssessmentMapper.createObservationComponent(currentMedicationDetails.getAllergiesComment(), MetaCodeConstants.ALLERGIES_COMMENT,
                    componentComponents);
            observation.setComponent(componentComponents);
            String observationUrl = addObservationToBundle(observation, bundle, request.getProvenance());
            currentMedicationDetails.getMedications().stream().forEach(medication -> {
                MedicationStatement medicationStatement = createMedicationStatement(medication.getValue(), MetaCodeConstants.CURRUNT_MEDICATION, request, bundle);
                medicationStatement.addReasonReference(fhirUtils.getReferenceUrl(ResourceType.Observation, observationUrl));
            });
        }

    }

    /**
     * Creates patient lifestyle.
     *
     * @param lifestyles
     * @param request
     * @param bundle
     */
    private void createLifestyle(Set<MedicalReviewMetaDTO> lifestyles, NCDMedicalReviewDTO request, Bundle bundle) {
        if (!Objects.isNull(lifestyles) && !lifestyles.isEmpty()) {
            Observation observation = createObservation(Constants.OBSERVATION_LIFESTYLE, request);
            List<ObservationComponentComponent> componentComponents = new ArrayList<>();
            for (MedicalReviewMetaDTO lifestyle : lifestyles) {
                fhirAssessmentMapper.createObservationComponent(lifestyle.getAnswer().getName(),
                        lifestyle.getValue(), componentComponents);
                if (!StringUtils.isEmpty(lifestyle.getComments())) {
                    observation.addNote(new Annotation().setText(lifestyle.getComments()).setAuthor(new StringType(lifestyle.getValue())));
                }
            }
            observation.setComponent(componentComponents);
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            String url = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, observation, bundle, request.getProvenance());
        }
    }

    /**
     * Creates patient comorbidity.
     *
     * @param comorbidities
     * @param request
     * @param bundle
     */
    private void createComorbidity(Set<MedicalReviewMetaDTO> comorbidities, NCDMedicalReviewDTO request, Bundle bundle) {
        if (!Objects.isNull(comorbidities)
                && !comorbidities.isEmpty()) {
            comorbidities.stream().forEach(comorbidity ->
                    createCondition(comorbidity.getValue(), MetaCodeConstants.COMORBIDITIES, request, bundle)
            );
        }
    }

    /**
     * Creates patient complications.
     *
     * @param complications
     * @param request
     * @param bundle
     */
    private void createComplications(Set<MedicalReviewMetaDTO> complications, NCDMedicalReviewDTO request, Bundle bundle) {
        if (!Objects.isNull(complications)
                && !complications.isEmpty()) {
            complications.stream().forEach(complication ->
                    createCondition(complication.getValue(), MetaCodeConstants.COMPLICATIONS, request, bundle)
            );
        }
    }

    /**
     * Creats patient complaints.
     *
     * @param complaints
     * @param notes
     * @param request
     * @param bundle
     */
    private void createComplaints(Set<MedicalReviewMetaDTO> complaints, String notes, NCDMedicalReviewDTO request, Bundle bundle) {
        if ((!Objects.isNull(complaints) && !complaints.isEmpty()) || !StringUtil.isBlank(notes)) {
            Observation observation = createObservation(Constants.OBSERVATION_COMPLAINTS, request);

            if (!Objects.isNull(complaints) && !complaints.isEmpty()) {
                List<ObservationComponentComponent> componentComponents = new ArrayList<>();
                for (MedicalReviewMetaDTO complaint : complaints) {
                    fhirAssessmentMapper.createObservationComponent(complaint.getValue(),
                            componentComponents);
                    observation.setComponent(componentComponents);
                }
            }
            observation.setNote(List.of(new Annotation().setText(notes)));
            addObservationToBundle(observation, bundle, request.getProvenance());
        }
    }

    /**
     * Creates patient physical examinations.
     *
     * @param physicalExaminations
     * @param notes
     * @param request
     * @param bundle
     */
    private void createPhysicalExaminations(Set<MedicalReviewMetaDTO> physicalExaminations, String notes, NCDMedicalReviewDTO request, Bundle bundle) {
        if ((!Objects.isNull(physicalExaminations)
                && !physicalExaminations.isEmpty()) || !StringUtil.isBlank(notes)) {
            Observation observation = createObservation(Constants.OBSERVATION_PHYSICAL_EXAMINATION, request);

            if (!Objects.isNull(physicalExaminations) && !physicalExaminations.isEmpty()) {
                List<ObservationComponentComponent> componentComponents = new ArrayList<>();
                for (MedicalReviewMetaDTO physicalExamination : physicalExaminations) {
                    fhirAssessmentMapper.createObservationComponent(physicalExamination.getValue(),
                            componentComponents);
                }
                observation.setComponent(componentComponents);
            }
            observation.setNote(List.of(new Annotation().setText(notes)));
            addObservationToBundle(observation, bundle, request.getProvenance());
        }
    }

    /**
     * Creates notes mapping.
     */
    public void createClinicalNotes(String clinicalNotes, NCDMedicalReviewDTO request, Bundle bundle) {
        Observation observation = createObservation(Constants.OBSERVATION_CLINICAL_NOTES, request);
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(clinicalNotes);
        observation.setValue(codeableConcept);
        addObservationToBundle(observation, bundle, request.getProvenance());
    }

    /**
     * Gets NCD medical review summary.
     *
     * @param request .
     * @return NcdMedicalReviewResponse
     */
    public NcdMedicalReviewResponse ncdMedicalReviewSummary(MedicalReviewRequestDTO request) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_ENCOUNTER_DETAILS, request.getEncounterReference()));

        NcdMedicalReviewResponse response = new NcdMedicalReviewResponse();
        if (!bundle.getEntry().isEmpty()) {
            bundle.getEntry().forEach(resource -> {
                if (resource.getResource() instanceof Observation observation) {
                    if (Constants.OBSERVATION_PHYSICAL_EXAMINATION.equals(observation.getCode().getText())) {
                        response.getPhysicalExams().addAll(getComponentValuesFromObservation(observation));
                        response.setPhysicalExamComments(observation.getNote().isEmpty() ? null : observation.getNote().getFirst().getText());
                    } else if (Constants.OBSERVATION_COMPLAINTS.equals(observation.getCode().getText())) {
                        response.getComplaints().addAll(getComponentValuesFromObservation(observation));
                        response.setCompliantComments(observation.getNote().isEmpty() ? null : observation.getNote().getFirst().getText());
                    } else if (Constants.OBSERVATION_CLINICAL_NOTES.equals(observation.getCode().getText())) {
                        response.setClinicalNote(observation.getValueCodeableConcept().getText());
                    }
                }
                if (resource.getResource() instanceof Condition condition) {
                    if (condition.getIdentifier().stream().anyMatch(id -> MetaCodeConstants.COMORBIDITIES.equals(id.getValue()))) {
                        response.getComorbities().add(fhirUtils.getText(condition.getCode().getText()));
                    } else if (condition.getIdentifier().stream().anyMatch(id -> MetaCodeConstants.COMPLICATIONS.equals(id.getValue()))) {
                        response.getComplications().add(fhirUtils.getText(condition.getCode().getText()));
                    }
                }
                if (resource.getResource() instanceof Patient patient) {
                    if (Objects.nonNull(patient.getIdentifier())) {
                        patient.getIdentifier().forEach(identifier -> {
                            if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                                request.setVillageId(identifier.getValue());
                            }
                        });
                    }
                    if (resource.getResource() instanceof RelatedPerson relatedPerson && Objects.isNull(request.getVillageId()) &&
                            Objects.nonNull(relatedPerson.getIdentifier())) {
                        relatedPerson.getIdentifier().forEach(identifier -> {
                            if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                                request.setVillageId(identifier.getValue());
                            }
                        });
                    }
                }

            });
        }
        getMedicalReviewDetailsByVisit(response, request.getPatientVisitId());
        response.setConfirmDiagnosis(patientService.getPatientDiagnosisDetails(new RequestDTO(request.getPatientReference(), request.getDiagnosisType()), Boolean.FALSE));
        return response;
    }

    /**
     * Get Signs from Observation
     *
     * @param observation Observation Object
     * @return List of signs
     */
    private List<String> getComponentValuesFromObservation(Observation observation) {
        return observation.getComponent()
                .stream()
                .map(component -> Objects.isNull(fhirUtils.getText(component.getCode().getText())) ? component.getCode().getText() : fhirUtils.getText(component.getCode().getText()))
                .toList();
    }

    /**
     * <p>
     * Creates a new Patient resource from a RelatedPerson resource and updates the patient status.
     * </p>
     *
     * @param request           The NCDMedicalReviewDTO object containing the request details.
     * @param relatedPerson     The RelatedPerson object from which the Patient is created.
     * @param transactionBundle The Bundle object to which the created Patient resource will be added.
     * @return The created Patient object.
     */
    private Patient createPatient(NCDMedicalReviewDTO request, RelatedPerson relatedPerson, Bundle transactionBundle) {
        Patient patient = patientConverter.createPatientFromRelatedPerson(relatedPerson);
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Patient),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL,
                uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, patient, transactionBundle, request.getProvenance());
        request.setPatientReference(fullUrl);
        return patient;
    }

    /**
     * @param patientReference
     * @return
     */
    private List<Condition> getPatientDiagnosis(String patientReference) {
        List<Condition> conditions = new ArrayList<>();
        String url = String.format(GET_PATIENT_DIAGNOSIS, FhirIdentifierConstants.PATIENT_STATUS_DIAGNOSIS_SYSTEM_URL, patientReference);
        Bundle resultBundle = restApiUtil.getBatchRequest(url);
        if (!resultBundle.isEmpty()) {
            conditions = resultBundle.getEntry().stream().map(entry -> (Condition) entry.getResource()).toList();
        }
        return conditions;
    }

    private void createPatientDiagnosis(List<Condition> conditions, PatientStatusDTO patientStatusDTO, Bundle bundle) {
        List<Condition> transcationCondition = new ArrayList<>();
        if (!Objects.isNull(conditions) && !conditions.isEmpty()) {
            conditions.stream().forEach(condition -> {
                if (condition.getIdentifier().stream().anyMatch(identifer -> Constants.HYPERTENSION.equals(identifer.getValue()))) {
                    Condition hypertensionCondition = patientStatusConverter.createHypertensionStatus(patientStatusDTO, condition);
                    transcationCondition.add(hypertensionCondition);
                } else if (condition.getIdentifier().stream().anyMatch(identifer -> Constants.DIABETES.equals(identifer.getValue()))) {

                    Condition diabetesCondition = patientStatusConverter.createDiabetesStatus(patientStatusDTO, condition);
                    transcationCondition.add(diabetesCondition);
                }
            });
        } else {
            Condition condition = patientStatusConverter.createHypertensionStatus(patientStatusDTO, null);
            condition.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, patientStatusDTO.getPatientReference()));
            condition.setEncounter(fhirUtils.getReferenceUrl(ResourceType.Encounter, patientStatusDTO.getEncounterReference()));
            transcationCondition.add(condition);
            condition = patientStatusConverter.createDiabetesStatus(patientStatusDTO, null);
            condition.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, patientStatusDTO.getPatientReference()));
            condition.setEncounter(fhirUtils.getReferenceUrl(ResourceType.Encounter, patientStatusDTO.getEncounterReference()));
            transcationCondition.add(condition);
        }
        if (!transcationCondition.isEmpty()) {
            transcationCondition.stream().forEach(condition -> {
                if (Objects.nonNull(condition.getId())) {
                    String id = fhirUtils.getIdFromHistoryUrl(condition.getId());
                    fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH, id),
                            StringUtil.concatString(Constants.FHIR_BASE_URL, id),
                            Bundle.HTTPVerb.PUT, condition, bundle, patientStatusDTO.getProvenance());
                } else {
                    String uuid = fhirUtils.getUniqueId();
                    String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
                    String url = StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
                    fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, condition, bundle, patientStatusDTO.getProvenance());
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    public void addOrUpdateNextVisitDate(NCDMedicalReviewDTO ncdMedicalReviewDTO) {
        if (Objects.isNull(ncdMedicalReviewDTO.getNextMedicalReviewDate())) {
            throw new DataNotFoundException(2005);
        } else if (Objects.isNull(ncdMedicalReviewDTO.getMemberReference())) {
            throw new DataNotFoundException(2004);
        }
        patientTreatmentPlanService.createOrUpdateAppointment(
                Constants.FREQUENCY_MEDICAL_REVIEW,
                ncdMedicalReviewDTO.getNextMedicalReviewDate(),
                ncdMedicalReviewDTO.getMemberReference(),
                ncdMedicalReviewDTO.getPatientReference(),
                null,
                ncdMedicalReviewDTO.getProvenance(), Boolean.FALSE);

    }

    /**
     * {@inheritDoc}
     */
    public List<String> getEncounterIdsByVisit(String encounterId) {
        List<String> encounterIds = new ArrayList<>();
        Bundle bundleEncounters = restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_PART_OF, encounterId));
        if (!bundleEncounters.getEntry().isEmpty()) {
            bundleEncounters.getEntry().forEach(resource -> {
                if (resource.getResource() instanceof Encounter partOfEncounter) {
                    encounterIds.add(partOfEncounter.getIdPart());
                }
            });
        }
        return encounterIds;
    }

    /**
     * This function processes a batch request for medical review
     * history data, extracting observations, prescriptions, and investigations from the resources
     * obtained.
     *
     * @param response    It is used to store the medical review history
     *                    response data, including prescriptions and investigations related to the provided encounter IDs.
     * @param encounterId The encounterId are used to retrieve medical review history information
     *                    related to those specific encounters.
     */
    private void getMedicalReviewDetailsByVisit(NcdMedicalReviewResponse response, String encounterId) {
        if (!encounterId.isEmpty()) {
            Bundle batchRequest = restApiUtil.getBatchRequest(String.format(Constants.REV_INCLUDE_ENCOUNTER_IDS, encounterId,
                    Constants.OBSERVATION, Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT));

            if (!batchRequest.getEntry().isEmpty()) {
                batchRequest.getEntry().forEach(resource -> {

                    if (resource.getResource() instanceof MedicationRequest medicationRequest && !medicationRequest.getStatus().getDisplay().equals(
                            MedicationRequest.MedicationRequestStatus.CANCELLED.getDisplay()) && medicationRequest.hasMedicationCodeableConcept()) {
                        PrescriptionDTO prescriptionDTO = fhirMapper.mapPrescriptionDTO(medicationRequest);
                        prescriptionDTO.setPrescriptionId(medicationRequest.getIdPart());
                        response.getPrescriptions().add(prescriptionDTO);
                    }
                    if (!response.getPrescriptions().isEmpty()) {
                        prescriptionRequestService.setMedicationDetails(response.getPrescriptions());
                    }

                    if (resource.getResource() instanceof DiagnosticReport diagnosticReport && diagnosticReport.hasStatus() && !Objects.equals(DiagnosticReport.DiagnosticReportStatus.CANCELLED, diagnosticReport.getStatus())) {
                        response.getInvestigations().add(diagnosticReport.getCode().getText());
                    }
                });
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public List<LifestyleResponseDTO> getPatientLifestyle(RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getPatientReference())) {
            throw new SpiceValidation();
        }
        Bundle resultBundle = restApiUtil.getBatchRequest(String.format(GET_LIFESTYLE_DETAILS,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL, Constants.OBSERVATION_LIFESTYLE, requestDTO.getPatientReference()));
        List<LifestyleResponseDTO> lifestyleResponses = new ArrayList<>();
        Map<String, String> valueComments = new HashMap<>();

        if (!resultBundle.getEntry().isEmpty()) {
            Observation observation = (Observation) resultBundle.getEntry().getFirst().getResource();
            observation.getComponent().forEach(component -> {

                LifestyleResponseDTO lifestyleResponse = new LifestyleResponseDTO();
                lifestyleResponse.setValue(component.getCode().getText());
                lifestyleResponse.setLifestyle(fhirUtils.getText(component.getCode().getText()));
                lifestyleResponse.setLifestyleAnswer(component.getValueCodeableConcept().getText());
                lifestyleResponses.add(lifestyleResponse);

            });

            if (!observation.getNote().isEmpty()) {
                observation.getNote().stream().forEach(note -> valueComments.put(note.getAuthorStringType().getValue(), note.getText()));
            }

            lifestyleResponses.forEach(lifestyleResponse -> lifestyleResponse.setComments(valueComments.get(lifestyleResponse.getValue())));

        }
        return lifestyleResponses;
    }
}

