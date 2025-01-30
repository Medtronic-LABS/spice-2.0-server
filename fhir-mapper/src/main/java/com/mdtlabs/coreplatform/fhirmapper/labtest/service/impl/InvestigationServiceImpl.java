package com.mdtlabs.coreplatform.fhirmapper.labtest.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.Code;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestResultDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

/**
 * <p>
 * This is an Implementation of InvestigationService class.
 * This class provides methods to handle investigation request-related operations
 * </p>
 *
 * @author Jaganathan R created on Jul 30, 2024.
 */
@Service
public class InvestigationServiceImpl implements InvestigationService {

    private final PatientService patientService;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final RestApiUtil restApiUtil;

    private final AdminServiceApiInterface adminServiceApiInterface;

    private final CommonConverter commonConverter;

    private final PatientVisitService patientVisitService;

    private final PatientConverter patientConverter;

    private final FhirUtils fhirUtils;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    public InvestigationServiceImpl(CommonConverter commonConverter, PatientConverter patientConverter,
                                    PatientVisitService patientVisitService, FhirUtils fhirUtils,
                                    FhirAssessmentMapper fhirAssessmentMapper, RestApiUtil restApiUtil,
                                    PatientService patientService, AdminServiceApiInterface adminServiceApiInterface) {
        this.commonConverter = commonConverter;
        this.patientConverter = patientConverter;
        this.patientVisitService = patientVisitService;
        this.fhirUtils = fhirUtils;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.restApiUtil = restApiUtil;
        this.patientService = patientService;
        this.adminServiceApiInterface = adminServiceApiInterface;
    }

    /**
     * Creates an Encounter.
     *
     * @param encounterDetails Encounter Details dto
     * @param bundle           bundle object
     */
    private void createEncounter(EncounterDetailsDTO encounterDetails, Bundle bundle) {
        encounterDetails.setLabTest(Boolean.TRUE);
        if (StringUtils.isEmpty(encounterDetails.getId())) {
            encounterDetails.setStartTime(new Date());
            encounterDetails.setEndTime(new Date());
            encounterDetails.setId(fhirAssessmentMapper.createEncounter(encounterDetails, bundle,
                    null, encounterDetails.getVisitId()));
        } else {
            encounterDetails.setStartTime(null);
            encounterDetails.setEndTime(new Date());
            fhirAssessmentMapper.updateEncounter(encounterDetails, bundle, null, null);
            encounterDetails.setId(StringUtil.concatString(Constants.FHIR_RESOURCE_ENCOUNTER,
                    Constants.FORWARD_SLASH, encounterDetails.getId()));
        }
    }


    /**
     * <p>
     * This method is used to create a create labTest object.
     * </p>
     *
     * @param labTestRequest investigation details
     * @return response
     */
    @Override
    public Map<String, String> createOrUpdateInvestigation(LabTestRequestDTO labTestRequest) {
        List<LabTestDTO> labTests = Objects.nonNull(labTestRequest.getLabTests()) ?
                labTestRequest.getLabTests() : new ArrayList<>();
        if (labTests.isEmpty()) {
            throw new DataNotAcceptableException(1014);
        }
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        patientService.setPatientReferenceInEncounterDetails(labTestRequest.getEncounter(), bundle);
        createEncounter(labTestRequest.getEncounter(), bundle);
        String encounterId = labTestRequest.getEncounter().getId();
        Map<String, String> response = new HashMap<>();
        for (LabTestDTO labTest : labTests) {
            labTest.setPatientId(labTestRequest.getEncounter().getPatientReference());
            bundle = Objects.isNull(labTest.getId())
                    ? createInvestigation(labTest, bundle,
                    labTestRequest.getEncounter().getProvenance(), encounterId)
                    : updateInvestigationRequest(labTest, bundle,
                    encounterId, labTestRequest.getEncounter().getProvenance(), response);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(),
                restApiUtil.constructRequestEntity(bundle));
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        return mapResponseDetails(fhirResponse, response, encounterId, labTestRequest.getEncounter().getPatientId());
    }

    /**
     * <p>
     * This method is used to create a create labTest object.
     * </p>
     *
     * @param labTestRequest investigation details
     * @return response
     */
    @Override
    public Map<String, String> createOrUpdateNcdInvestigation(LabTestRequestDTO labTestRequest) {
        List<LabTestDTO> labTests = Objects.nonNull(labTestRequest.getLabTests()) ?
                labTestRequest.getLabTests() : new ArrayList<>();
        if (labTests.isEmpty()) {
            throw new DataNotAcceptableException(1014);
        }
        Set<String> uniqueLabTestNames = new HashSet<>();
        labTestRequest.getLabTests().stream().forEach(test -> uniqueLabTestNames.add(test.getTestName()));
        if (uniqueLabTestNames.size() != labTestRequest.getLabTests().size()) {
            throw new BadRequestException(1302);
        }
        Map<String, String> response = new HashMap<>();
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url = String.format(Constants.GET_MEMBER_ID, labTestRequest.getEncounter().getMemberId());
        Bundle resultBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) resultBundle.getEntry().getFirst().getResource();
        Patient patient = null;
        String visitId = labTestRequest.getEncounter().getVisitId();
        String patientReference = labTestRequest.getEncounter().getPatientReference();
        if (Objects.isNull(labTestRequest.getEncounter().getPatientReference())) {
            ScreeningLogRequestDTO screeningLogRequestDTO = commonConverter.setPatientDetails(relatedPerson,
                    labTestRequest.getEncounter().getProvenance().getOrganizationId());
            patient = patientConverter.createPatient(patient, screeningLogRequestDTO.getBioData(),
                    screeningLogRequestDTO.getBioMetrics(), relatedPerson.getBirthDate());
            patient.setManagingOrganization(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    labTestRequest.getEncounter().getProvenance().getOrganizationId())));
            patient.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                    .setValue(Constants.MEDICAL_REVIEW.toLowerCase());
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)) {
                    identifier.setValue(Constants.MEDICAL_REVIEW.toLowerCase());
                }
            });
            fhirUtils.setBundle(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, relatedPerson.getIdPart()),
                    Bundle.HTTPVerb.PUT, relatedPerson, bundle, labTestRequest.getEncounter().getProvenance());

            labTestRequest.getEncounter().setPatientReference(FhirConstants.PATIENT_IDENTIFIER_URL);
            String uuid = fhirUtils.getUniqueId();
            String id = StringUtil.concatString(Constants.PATIENT,
                    Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
            fhirUtils.setBundle(id, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                    Bundle.HTTPVerb.POST, patient, bundle, labTestRequest.getEncounter().getProvenance());
            labTestRequest.getEncounter().setPatientReference(id);
        } else {
            labTestRequest.getEncounter().setPatientReference(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                    Constants.FORWARD_SLASH,
                    labTestRequest.getEncounter().getPatientReference()));
        }
        Bundle transactionBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Encounter encounter = patientVisitService.updatePatientVisitStatus(visitId, false, false, true, patientReference);
        fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH, visitId),
                StringUtil.concatString(Constants.FHIR_BASE_URL, visitId),
                Bundle.HTTPVerb.PUT, encounter, transactionBundle, labTestRequest.getEncounter().getProvenance());
        ResponseEntity<FhirResponseDTO> responseData = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transactionBundle));
        if (Objects.isNull(responseData.getBody())) {
            throw new Validation(1006);
        }

        for (LabTestDTO labTest : labTests) {
            labTest.setPatientId(labTestRequest.getEncounter().getPatientReference());
            bundle = Objects.isNull(labTest.getId())
                    ? createInvestigation(labTest, bundle,
                    labTestRequest.getEncounter().getProvenance(), labTestRequest.getEncounter().getVisitId())
                    : updateInvestigationRequest(labTest, bundle,
                    labTestRequest.getEncounter().getVisitId(), labTestRequest.getEncounter().getProvenance(), response);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(),
                restApiUtil.constructRequestEntity(bundle));
        Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        return mapResponseDetails(fhirResponse, response, labTestRequest.getEncounter().getId(),
                labTestRequest.getEncounter().getPatientReference());
    }


    /**
     * <p>
     * To create the investigation details.
     * </p>
     *
     * @param labTest       investigation details
     * @param bundle        the bundle details
     * @param provenanceDTO provenance details
     * @param encounterId   encounter id
     * @return response of the given investigation details
     */
    private Bundle createInvestigation(LabTestDTO labTest, Bundle bundle, ProvenanceDTO provenanceDTO,
                                       String encounterId) {
        String uuid = fhirUtils.getUniqueId();
        String id = StringUtil.concatString(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT,
                Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        DiagnosticReport diagnosticReport = mapDiagnosticReport(labTest, encounterId);
        fhirUtils.setBundle(id, StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                Bundle.HTTPVerb.POST, diagnosticReport, bundle, provenanceDTO);
        if (Objects.nonNull(labTest.getLabTestResults())) {
            setIsReview(true, diagnosticReport);
            setResultInterpreter(diagnosticReport, provenanceDTO);
            for (LabTestResultDTO labTestResult : labTest.getLabTestResults()) {
                String observationId = createObservation(labTestResult, encounterId,
                        provenanceDTO, labTest.getPatientId(), bundle);
                DateTimeType effectiveTime = new DateTimeType(labTestResult.getTestedOn());
                diagnosticReport.setEffective(effectiveTime);
                diagnosticReport.addResult(new Reference(observationId));
                labTestResult.setId(observationId);
            }
        }
        return bundle;
    }

    /**
     * <p>
     * Used to set the result interpreter reference.
     * <p/>
     *
     * @param diagnosticReport - Patient's diagnosis report.
     * @param provenanceDTO    - User id to map the result interpreter.
     */
    private void setResultInterpreter(DiagnosticReport diagnosticReport, ProvenanceDTO provenanceDTO) {
        diagnosticReport.addResultsInterpreter(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Practitioner),
                Constants.FORWARD_SLASH, provenanceDTO.getUserId())));
    }


    /**
     * To form the response of the given investigation details.
     *
     * @param fhirResponse response from the restApi to fhirServer.
     * @param response     response of the details
     * @param encounterId  encounter id
     * @param patientId    patient id
     * @return response of the details.
     */
    private Map<String, String> mapResponseDetails(Map<String, List<String>> fhirResponse,
                                                   Map<String, String> response, String encounterId, String patientId) {
        response.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER))
                && !fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).isEmpty()
                ? fhirResponse.get(Constants.FHIR_RESOURCE_ENCOUNTER).getFirst()
                : fhirUtils.getIdFromReference(encounterId));
        response.put(Constants.KEY_PATIENT_REFERENCE, !Objects.isNull(fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT))
                && !fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty()
                ? fhirResponse.get(Constants.FHIR_RESOURCE_PATIENT).getFirst()
                : fhirUtils.getIdFromReference(patientId));
        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Bundle updateInvestigationRequest(LabTestDTO labTestDTO, Bundle bundle, String encounterId,
                                             ProvenanceDTO provenance, Map<String, String> response) {
        Bundle diagnosticReportBundle = restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY,
                labTestDTO.getId()));
        DiagnosticReport diagnosticReport = (DiagnosticReport) diagnosticReportBundle.getEntryFirstRep().getResource();
        diagnosticReport.setSubject(new Reference(labTestDTO.getPatientId()));
        diagnosticReport.setEncounter(new Reference(String.format(FhirConstants.ENCOUNTER_ID, encounterId)));
        diagnosticReport.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
        if (Objects.nonNull(labTestDTO.getComments())) {
            diagnosticReport.setConclusion(labTestDTO.getComments());
        }
        if (Objects.nonNull(labTestDTO.getLabTestResults()) && !labTestDTO.getLabTestResults().isEmpty()) {
            setResultInterpreter(diagnosticReport, provenance);
            labTestDTO.getLabTestResults().forEach(labTestResultDTO -> {
                String observationId = createObservation(labTestResultDTO, encounterId, provenance,
                        labTestDTO.getPatientId(), bundle);
                DateTimeType effectiveTime = new DateTimeType(labTestDTO.getTestedOn());
                diagnosticReport.setEffective(effectiveTime);
                diagnosticReport.addResult(new Reference(observationId));
            });
        }
        setIsReview(true, diagnosticReport);
        fhirUtils.setBundle(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT.concat(Constants.FORWARD_SLASH)
                        .concat(labTestDTO.getId()), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT,
                diagnosticReport, bundle, provenance);
        return bundle;
    }

    /**
     * <p>
     * Used to set the is review key based on the condition and retain the existing identifier.
     * <p/>
     *
     * @param isReview         - Review identification key
     * @param diagnosticReport - Patient diagnosis report
     */
    private void setIsReview(Boolean isReview, DiagnosticReport diagnosticReport) {
        List<Identifier> diagnosisIdentifiers = new ArrayList<>();
        Identifier reviewIdentifier = new Identifier();
        diagnosticReport.getIdentifier().forEach(identifier -> {
            if (!identifier.getSystem().equals(FhirIdentifierConstants.IS_REVIEW_SYSTEM_URL)) {
                diagnosisIdentifiers.add(identifier);
            }
        });
        if (Boolean.TRUE.equals(isReview)) {
            reviewIdentifier.setSystem(FhirIdentifierConstants.IS_REVIEW_SYSTEM_URL).
                    setValue(Boolean.TRUE.toString());
            diagnosisIdentifiers.add(reviewIdentifier);
        } else {
            reviewIdentifier.setSystem(FhirIdentifierConstants.IS_REVIEW_SYSTEM_URL).
                    setValue(Boolean.FALSE.toString());
            diagnosisIdentifiers.add(reviewIdentifier);
        }
        diagnosticReport.setIdentifier(diagnosisIdentifiers);
    }


    /**
     * <p>
     * This method to map the investigation details in DiagnosticReport.
     * </p>
     *
     * @param labTest     investigation details
     * @param encounterId encounter id
     * @return diagnostic report
     */
    private DiagnosticReport mapDiagnosticReport(LabTestDTO labTest, String encounterId) {
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        if (Objects.nonNull(labTest)) {
            diagnosticReport.setStatus((Objects.nonNull(labTest.getLabTestResults()))
                    && !(labTest.getLabTestResults().isEmpty())
                    ? DiagnosticReport.DiagnosticReportStatus.FINAL
                    : DiagnosticReport.DiagnosticReportStatus.REGISTERED);
            diagnosticReport.addIdentifier()
                    .setSystem(FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL)
                    .setValue(labTest.getTestName());
            diagnosticReport.setIssued(labTest.getRecommendedOn());
            diagnosticReport.setSubject(new Reference(labTest.getPatientId()));
            diagnosticReport.setEncounter(new Reference(String.format(FhirConstants.ENCOUNTER_ID, encounterId)));
            diagnosticReport.addPerformer().setDisplay(labTest.getRecommendedName())
                    .setReference(Constants.FHIR_RESOURCE_PRACTITIONER.concat(Constants.FORWARD_SLASH)
                            .concat(labTest.getRecommendedBy()));
            diagnosticReport.getCode().setText(labTest.getTestName());
            if (Objects.nonNull(labTest.getCodeDetails())) {
                diagnosticReport.getCode()
                        .addCoding().setDisplay(labTest.getTestName())
                        .setSystem(labTest.getCodeDetails().getUrl())
                        .setCode(labTest.getCodeDetails().getCode());
            }
            if (Objects.nonNull(labTest.getComments())) {
                diagnosticReport.setConclusion(labTest.getComments());
            }
        }
        return diagnosticReport;
    }

    /**
     * <p>
     * To map the investigation result details in Observation and add to bundle.
     * </p>
     *
     * @param labTestResult investigation result details
     * @param encounterId   encounter id
     * @param provenance    provence details
     * @param patientId     patient identifier
     * @param bundle        bundle reference
     * @return id of the investigation result details
     */
    private String createObservation(LabTestResultDTO labTestResult, String encounterId,
                                     ProvenanceDTO provenance, String patientId, Bundle bundle) {
        String uuid = fhirUtils.getUniqueId();
        Observation observation = new Observation();
        if (StringUtil.isNotBlank(encounterId)) {
            observation.setEncounter(new Reference(String.format(FhirConstants.ENCOUNTER_ID, encounterId)));
        }
        observation.setStatus(Observation.ObservationStatus.FINAL);
        observation.addIdentifier()
                .setSystem(FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL)
                .setValue(labTestResult.getName());
        observation.addPerformer().setDisplay(labTestResult.getPerformedName())
                .setReference(Constants.FHIR_RESOURCE_PRACTITIONER.concat(Constants.FORWARD_SLASH)
                        .concat(labTestResult.getPerformedBy()));
        observation.setSubject(new Reference().setReference(patientId));
        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(labTestResult.getName());
        if (Objects.nonNull(labTestResult.getCodeDetails())) {
            Coding coding = new Coding();
            coding.setSystem(labTestResult.getCodeDetails().getUrl());
            coding.setCode(labTestResult.getCodeDetails().getCode());
            coding.setDisplay(labTestResult.getName());
            codeableConcept.addCoding(coding);
        }
        observation.setCode(codeableConcept);
        observation.setIssued(labTestResult.getTestedOn());
        if (Objects.nonNull(labTestResult.getResource())) {
            switch (labTestResult.getResource()) {
                case Constants.STRING_NAME:
                    observation.setValue(new org.hl7.fhir.r4.model.StringType(labTestResult.getValue()));
                    break;
                case Constants.CODE_NAME:
                    observation.setValue(new org.hl7.fhir.r4.model.CodeableConcept().setText(labTestResult.getValue()));
                    break;
                case Constants.BOOLEAN_NAME:
                    observation.setValue(new org.hl7.fhir.r4.model
                            .BooleanType(Boolean.parseBoolean(labTestResult.getValue())));
                    break;
                case Constants.DATE_NAME:
                    observation.setValue(new DateTimeType(labTestResult.getValue()));
                    break;
                case Constants.QUANTITY_NAME:
                    Quantity value = new Quantity(null, Double.valueOf(labTestResult.getValue()),
                            null, null, labTestResult.getUnit());
                    observation.setValue(value);
                    break;
                case Constants.INTEGER_NAME:
                    observation.setValue(new org.hl7.fhir.r4.model
                            .IntegerType(Integer.parseInt(labTestResult.getValue())));
                    break;
                default:
                    break;
            }
        }
        String id = StringUtil.concatString(Constants.FHIR_RESOURCE_OBSERVATION,
                Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        fhirUtils.setBundle(id,
                StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                Bundle.HTTPVerb.POST, observation,
                bundle, provenance);
        return id;
    }

    /**
     * <p>
     * To map the diagnosticReport details in labTestDTO.
     * </p>
     *
     * @param diagnosticReport investigatedDetails
     * @return labTestDTO
     */
    private LabTestDTO mapDiagnosticReportToLabTestDTO(DiagnosticReport diagnosticReport, boolean showResult,
                                                       Bundle investigationBundle) {
        LabTestDTO labTestDTO = new LabTestDTO();
        if (Objects.nonNull(diagnosticReport) && diagnosticReport.hasStatus()
                && !diagnosticReport.getStatus().equals(DiagnosticReport.DiagnosticReportStatus.CANCELLED)) {
            Practitioner recommendedName = getPractitionerById(investigationBundle, fhirUtils.getIdFromReference
                    (diagnosticReport.getPerformer().getFirst().getReference()));
            labTestDTO.setId(diagnosticReport.getIdPart());
            labTestDTO.setRecommendedOn(diagnosticReport.getIssued());
            labTestDTO.setTestName(diagnosticReport.getIdentifier().getFirst().getValue());
            labTestDTO.setTestedOn(diagnosticReport.getEffectiveDateTimeType().getValue());
            labTestDTO.setRecommendedName(Objects.nonNull(recommendedName) ?
                    recommendedName.getName().getFirst().getText() : null);
            labTestDTO.setComments(diagnosticReport.getConclusion());
            labTestDTO.setRecommendedBy(fhirUtils.getIdFromReference(
                    diagnosticReport.getPerformer().getFirst().getReference()));
            labTestDTO.setRecommendedName(getPractitionerName(labTestDTO.getRecommendedBy()));
            labTestDTO.setPatientId(fhirUtils.getIdFromReference(diagnosticReport.getSubject().getReference()));
            diagnosticReport.getIdentifier().forEach(identifier -> {
                if (FhirIdentifierConstants.IS_REVIEW_SYSTEM_URL.equals(identifier.getSystem())) {
                    labTestDTO.setIsReview(Boolean.valueOf(identifier.getValue()));
                }
            });
            if (!diagnosticReport.getResultsInterpreter().isEmpty()) {
                Practitioner resultInterpreter = getPractitionerById(investigationBundle, fhirUtils.getIdFromReference
                        (diagnosticReport.getResultsInterpreter().getFirst().getReference()));
                labTestDTO.setResultUpdatedBy(Objects.nonNull(resultInterpreter) ?
                        resultInterpreter.getName().getFirst().getText() : null);
            }
            List<LabTestResultDTO> results = new ArrayList<>();
            if (showResult && Objects.nonNull(diagnosticReport.getResult()) && !diagnosticReport.getResult().isEmpty()) {
                diagnosticReport.getResult().stream().forEach(id -> {
                    String observationId = fhirUtils.getIdFromReference(id.getReference());
                    investigationBundle.getEntry().forEach(entry -> {
                        if (entry.getResource() instanceof Observation observation && observation.getIdPart().equals(observationId)) {
                            results.add(mapInvestigationResult(observation));
                        }
                    });
                });
            }
            labTestDTO.setLabTestResults(results.isEmpty() ? null : results);
            labTestDTO.setLabTestCustomization(showResult ? adminServiceApiInterface.getLabTestCustomizationByName(
                    CommonUtil.getAuthToken(), CommonUtil.getClient(), new SearchRequestDTO(labTestDTO.getTestName(),
                            UserContextHolder.getUserDto().getCountry().getId())) : null);
        }
        return labTestDTO;
    }

    /**
     * <p>
     * Get the practitioner by the id from bundle response.
     * <p/>
     *
     * @param response - FHIR raw bundle response
     * @param id       - Practitioner Id which is used to match with the bundle entity.
     * @return - {@link  Practitioner} - Matched practitioner entity.
     */
    private Practitioner getPractitionerById(Bundle response, String id) {
        if (Objects.nonNull(response)) {
            AtomicReference<Practitioner> practitioner = new AtomicReference<>();
            response.getEntry().stream()
                    .map(Bundle.BundleEntryComponent::getResource)
                    .filter(resource -> resource instanceof Practitioner && resource.getIdPart().equals(id)).
                    map(Practitioner.class::cast).forEach(practitioner::set);
            return practitioner.get();
        }
        return null;
    }

    /**
     * <p>
     * This method is used to get the practitioner name.
     * </p>
     *
     * @param recommendedBy id of the practitioner
     * @return String name of the practitioner
     */
    private String getPractitionerName(String recommendedBy) {
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.PRACTIIONER_PARAMS, recommendedBy));
        String practitionerName = null;
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            Practitioner practitioner = (Practitioner) entry.getResource();
            practitionerName = practitioner.getName().getFirst().getText();
        }
        return practitionerName;
    }

    /**
     * <p>
     * To map the investigated result details in labTestResultDetails.
     * </p>
     *
     * @param observation investigated result details
     * @return labTestResultDetails
     */
    private LabTestResultDTO mapInvestigationResult(Observation observation) {
        LabTestResultDTO labTestResult = new LabTestResultDTO();
        labTestResult.setId(observation.getId());
        labTestResult.setName(observation.getIdentifier().getFirst().getValue());
        labTestResult.setPatientId(fhirUtils.getIdFromReference(observation.getSubject().getReference()));
        labTestResult.setPerformedBy(fhirUtils.getIdFromReference(observation.getPerformer().getFirst().getReference()));
        labTestResult.setPerformedName(getPractitionerName(labTestResult.getPerformedBy()));
        labTestResult.setTestedOn(observation.getIssued());
        if (Boolean.TRUE.equals(observation.hasValueQuantity())) {
            labTestResult.setValue(String.valueOf(observation.getValueQuantity().getValue()));
        } else if (Boolean.TRUE.equals(observation.hasValueBooleanType())) {
            labTestResult.setValue(String.valueOf(observation.getValueBooleanType().getValue()));
        } else if (Boolean.TRUE.equals(observation.hasValueIntegerType())) {
            labTestResult.setValue(String.valueOf(observation.getValueIntegerType().getValue()));
        } else if (Boolean.TRUE.equals(observation.hasValueCodeableConcept())) {
            labTestResult.setValue(String.valueOf(observation.getValueCodeableConcept().getCoding().getFirst().getCode()));
        } else if (Boolean.TRUE.equals(observation.hasValueDateTimeType())) {
            labTestResult.setValue(String.valueOf(observation.getValueDateTimeType().getValue()));
        } else if (Boolean.TRUE.equals(observation.hasValueStringType())) {
            labTestResult.setValue(String.valueOf(observation.getValueStringType().getValue()));
        } else {
            labTestResult.setValue(null);
        }
        labTestResult.setUnit(observation.hasValueQuantity() ? observation.getValueQuantity().getUnit() : null);
        CodeableConcept code = observation.getCode();
        if (Objects.nonNull(code) && code.hasCoding()) {
            labTestResult.setCodeDetails(new Code(code.getCoding().get(0).getCode(), code.getCoding().get(0).getSystem()));
        }
        return labTestResult;
    }

    /**
     * <p>
     * This method to get the list of the investigatedDetails.
     * </p>
     *
     * @param requestDTO request details
     * @return list of the investigatedDetails
     */
    @Override
    public List<LabTestDTO> getListOfInvestigatedDetails(RequestDTO requestDTO) {
        List<LabTestDTO> investigations = new ArrayList<>();
        if (Objects.nonNull(requestDTO.getPatientReference())) {
            String encounterUrl;
            if (Objects.nonNull(requestDTO.getRoleName()) && requestDTO.getRoleName().equals(Constants.ROLE_LAB_TECHNICIAN)) {
                encounterUrl = String.format(Constants.DIAGNOSTIC_REPORT_QUERY_WITH_STATUS, FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL,
                        requestDTO.getPatientReference(), Constants.REGISTERED_STATUS);
            } else {
                encounterUrl = String.format(Constants.DIAGNOSTIC_REPORT_QUERY_WITH_STATUS, FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL,
                        requestDTO.getPatientReference(), String.join(Constants.COMMA,
                                List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS)));
            }
            Bundle investigationBundle = restApiUtil.getBatchRequest(encounterUrl);
            investigationBundle.getEntry().forEach(resource -> {
                if (resource.getResource() instanceof DiagnosticReport diagnosticReport) {
                    investigations.add(mapDiagnosticReportToLabTestDTO(diagnosticReport, Boolean.TRUE, investigationBundle));
                }
            });
        }
        return investigations;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> removeInvestigation(String id, ProvenanceDTO provenance) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        AtomicReference<String> encounterId = new AtomicReference<>();
        Bundle diagnosticReportBundle = restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY, id));
        diagnosticReportBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof DiagnosticReport diagnosticReport) {
                encounterId.set(diagnosticReport.getEncounter().getReferenceElement().getIdPart());
                diagnosticReport.setStatus(DiagnosticReport.DiagnosticReportStatus.CANCELLED);
                fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT,
                                Constants.FORWARD_SLASH, diagnosticReport.getIdPart()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, diagnosticReport.getIdPart()),
                        Bundle.HTTPVerb.PUT, diagnosticReport, bundle, provenance);
                restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
            }
        });
        Bundle requestBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Bundle diagnosticsBundle = restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY_WITH_ENCOUNTER, encounterId));
        if (Constants.ZERO == diagnosticsBundle.getTotal()) {
            Bundle encounterBundle = restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, encounterId));
            encounterBundle.getEntry().forEach(resource -> {
                if (resource.getResource() instanceof Encounter encounter) {
                    encounter.setStatus(Encounter.EncounterStatus.CANCELLED);
                    fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_ENCOUNTER,
                                    Constants.FORWARD_SLASH, encounter.getIdPart()),
                            StringUtil.concatString(Constants.FHIR_BASE_URL, encounter.getIdPart()),
                            Bundle.HTTPVerb.PUT, encounter, requestBundle, provenance);
                    restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(requestBundle));
                }
            });
        }
        return Map.of(Constants.ID, id);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public LabTestHistoryDTO getInvestigatedDetails(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference())) {
            throw new SpiceValidation(2003);
        }
        if (Objects.nonNull(request.getEncounterId())) {
            return getInvestigationHistoryByEncounter(request);
        } else {
            List<Map<String, Object>> encounters = getEncounterHistory(request);
            if (encounters.isEmpty()) {
                return new LabTestHistoryDTO();
            } else {
                String encounterId = (String) encounters.get(0).get(Constants.ID);
                request.setEncounterId(encounterId);
                LabTestHistoryDTO labTestHistoryDTO = getInvestigationHistoryByEncounter(request);
                labTestHistoryDTO.setHistory(encounters);
                return labTestHistoryDTO;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public LabTestHistoryDTO getNcdInvestigatedDetails(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference())) {
            throw new SpiceValidation(2003);
        }
        if (Objects.nonNull(request.getPatientVisitId())) {
            return getInvestigationHistoryByVisitId(request);
        } else {
            List<Map<String, Object>> encounters = getEncounterByVisit(request);
            if (encounters.isEmpty()) {
                return new LabTestHistoryDTO();
            } else {
                String encounterId = (String) encounters.getLast().get(Constants.ID);
                request.setPatientVisitId(encounterId);
                LabTestHistoryDTO labTestHistoryDTO = getInvestigationHistoryByVisitId(request);
                labTestHistoryDTO.setHistory(encounters);
                return labTestHistoryDTO;
            }
        }
    }


    /**
     * <p>
     * Gets investigation history of particular patient using patient visit id
     * and patient id.
     * </p>
     *
     * @param request it contains patient visit id and patient id
     * @return The lab test history entity
     */
    private LabTestHistoryDTO getInvestigationHistoryByVisitId(RequestDTO request) {
        LabTestHistoryDTO labTestHistoryDTO = new LabTestHistoryDTO();
        if (Objects.isNull(request.getPatientReference()) ||
                Objects.isNull(request.getPatientVisitId())) {
            throw new SpiceValidation(2003);
        }
        List<LabTestDTO> labTests = new ArrayList<>();
        String investigationHistoryUrl = String.format(Constants.INVESTIGATION_BY_ENCOUNTER,
                ResourceType.Encounter.name().concat(Constants.FORWARD_SLASH).concat(request.getPatientVisitId()))
                .concat(Constants.STATUS_NOT_CANCELLED);
        Bundle investigationHistoryBundle = restApiUtil.getBatchRequest(investigationHistoryUrl);
        investigationHistoryBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof DiagnosticReport diagnosticReport) {
                LabTestDTO labTestDTO = mapDiagnosticReportToLabTestDTO(diagnosticReport, Boolean.FALSE, investigationHistoryBundle);
                labTests.add(labTestDTO);
            }
        });
        labTestHistoryDTO.setInvestigations(labTests);
        labTestHistoryDTO.setPatientReference(request.getPatientReference());
        labTestHistoryDTO.setEncounterId(request.getPatientVisitId());
        return labTestHistoryDTO;
    }


    /**
     * Get investigation history by encounter
     *
     * @param requestDTO
     * @return labTestHistoryDTO
     */
    private LabTestHistoryDTO getInvestigationHistoryByEncounter(RequestDTO requestDTO) {
        LabTestHistoryDTO labTestHistoryDTO = new LabTestHistoryDTO();
        String encounterUrl = String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, requestDTO.getEncounterId());
        Bundle encounterBundle = restApiUtil.getBatchRequest(encounterUrl);
        encounterBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof Encounter encounter) {
                labTestHistoryDTO.setEncounterId(encounter.getIdPart());
                labTestHistoryDTO.setDateOfReview(encounter.getPeriod().getStart());
                labTestHistoryDTO.setPatientReference(encounter.getSubject().getReferenceElement().getIdPart());
            }
        });
        String investigationUrl = String.format(Constants.INVESTIGATION_LIST_PARAMS,
                FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL, requestDTO.getPatientReference(),
                String.join(Constants.COMMA, List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS)));
        Bundle investigationBundle = restApiUtil.getBatchRequest(investigationUrl);
        List<String> investigationIds = investigationBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(DiagnosticReport.class::isInstance)
                .map(Resource::getIdPart)
                .toList();
        labTestHistoryDTO.setInvestigations(getInvestigationHistoryByInvestigatedIds(investigationIds, requestDTO.getEncounterId()));

        return labTestHistoryDTO;
    }

    /**
     * Get investigation history by medication ids
     *
     * @param investigatedIds
     * @param encounterId
     * @return List of investigation details
     */
    private List<LabTestDTO> getInvestigationHistoryByInvestigatedIds(List<String> investigatedIds, String encounterId) {
        List<LabTestDTO> investigations = new ArrayList<>();
        for (String diagnosticReportId : investigatedIds) {
            String diagnosticReportUrl = String.format(Constants.INVESTIGATION_HISTORY, diagnosticReportId);
            Bundle diagnosticReportBundle = restApiUtil.getBatchRequest(diagnosticReportUrl);
            for (Bundle.BundleEntryComponent component : diagnosticReportBundle.getEntry()) {
                if (component.getResource() instanceof DiagnosticReport diagnosticReport
                        && encounterId.equals(fhirUtils.getIdFromReference(diagnosticReport.getEncounter().getReference()))) {
                    LabTestDTO labTestDTO = mapDiagnosticReportToLabTestDTO(diagnosticReport, Boolean.FALSE, null);
                    investigations.add(labTestDTO);
                    break;
                }
            }
        }
        return investigations;
    }

    /**
     * Get encounter history of patient.
     *
     * @param request
     * @return List<Map < String, Object>>
     */
    private List<Map<String, Object>> getEncounterHistory(RequestDTO request) {
        List<Map<String, Object>> encounterList = new ArrayList<>();
        String url = String.format(Constants.GET_INVESTIGATION_ENCOUNTER_QUERY, request.getPatientReference(),
                StringUtil.concatString(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL, Constants.VERTICAL_BAR, Constants.INVESTIGATED));
        Bundle bundle = restApiUtil.getBatchRequest(url);
        bundle.getEntry().stream().forEach(resource -> {
            Encounter encounter = (Encounter) resource.getResource();
            encounterList.add(Map.of(Constants.ID, encounter.getIdPart(), Constants.DATE, encounter.getPeriod().getStart()));
        });
        return encounterList;
    }

    /**
     * Get encounter history of patient.
     *
     * @param request
     * @return List<Map < String, Object>>
     */
    private List<Map<String, Object>> getEncounterByVisit(RequestDTO request) {
        List<Map<String, Object>> encounterList = new ArrayList<>();
        String url = String.format(Constants.GET_INVESTIGATION_ASC_ENCOUNTER_QUERY, request.getPatientReference(),
                StringUtil.concatString(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL, Constants.VERTICAL_BAR, Constants.INVESTIGATED));
        Bundle bundle = restApiUtil.getBatchRequest(url);
        bundle.getEntry().stream().forEach(resource -> {
            Encounter encounter = (Encounter) resource.getResource();
            encounterList.add(Map.of(Constants.ID, fhirUtils.getIdFromReference(encounter.getIdPart()),
                    Constants.DATE, encounter.getPeriod().getStart()));
        });
        return encounterList;
    }


    /**
     * {@inheritDoc}
     */
    public List<LabTestDTO> getInvestigationsByEncounter(String encounterId, String patientReference) {
        String investigationUrl = String.format(Constants.INVESTIGATION_LIST_PARAMS,
                FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL, patientReference,
                String.join(Constants.COMMA, List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS)));
        Bundle investigatedBundle = restApiUtil.getBatchRequest(investigationUrl);
        List<String> investigatedIds = investigatedBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(DiagnosticReport.class::isInstance)
                .map(Resource::getIdPart)
                .toList();
        return getInvestigationHistoryByInvestigatedIds(investigatedIds, encounterId);
    }

    @Override
    public void updateInvestigationResult(LabTestRequestDTO labTestRequest) {
        String patientReference = labTestRequest.getEncounter().getPatientReference();
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        for (LabTestDTO labTestDTO : labTestRequest.getLabTests()) {
            String investigationUrl = String.format(Constants.DIAGNOSIS_REPORT_QUERY_WITH_ID,
                    labTestDTO.getId());
            Bundle investigatedBundleResult = restApiUtil.getBatchRequest(investigationUrl);
            if (investigatedBundleResult.getEntry().isEmpty()) {
                throw new DataNotAcceptableException(1014);
            }
            String patientId = StringUtil.concatString(String.valueOf(ResourceType.Patient),
                    Constants.FORWARD_SLASH, patientReference);
            labTestRequest.getEncounter().setPatientReference(patientId);
            DiagnosticReport diagnosticReport = (DiagnosticReport) investigatedBundleResult.getEntry().getFirst().getResource();
            if (Objects.nonNull(labTestDTO.getLabTestResults())) {
                setResultInterpreter(diagnosticReport, labTestRequest.getEncounter().getProvenance());
                diagnosticReport.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
                for (LabTestResultDTO labTestResult : labTestDTO.getLabTestResults()) {
                    String observationId = createObservation(labTestResult, labTestRequest.getEncounter().getId(),
                            labTestRequest.getEncounter().getProvenance(), patientId, bundle);
                    DateTimeType effectiveTime = new DateTimeType(labTestResult.getTestedOn());
                    diagnosticReport.setEffective(effectiveTime);
                    diagnosticReport.addResult(new Reference(observationId));
                    labTestResult.setId(observationId);
                }
            }
            setIsReview(false, diagnosticReport);
            if (Objects.nonNull(labTestDTO.getComments())) {
                diagnosticReport.setConclusion(labTestDTO.getComments());
            }
            fhirUtils.setBundle(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT.concat(Constants.FORWARD_SLASH)
                            .concat(labTestDTO.getId()), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT,
                    diagnosticReport, bundle, labTestRequest.getEncounter().getProvenance());
            restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(),
                    restApiUtil.constructRequestEntity(bundle));
        }
    }

    /**
     * {@inheritDoc}
     */
    public void reviewInvestigation(RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getId())) {
            throw new BadRequestException(1009);
        }
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Bundle diagnosticReportBundle = restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY,
                requestDTO.getId()));
        if (diagnosticReportBundle.getEntry().isEmpty()) {
            throw new DataNotAcceptableException(1014);
        }
        DiagnosticReport diagnosticReport = (DiagnosticReport) diagnosticReportBundle.getEntry().getFirst().getResource();
        diagnosticReport.setConclusion(requestDTO.getComments());
        setIsReview(true, diagnosticReport);
        fhirUtils.setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT,
                        Constants.FORWARD_SLASH, diagnosticReport.getIdPart()),
                StringUtil.concatString(Constants.FHIR_BASE_URL, diagnosticReport.getIdPart()),
                Bundle.HTTPVerb.PUT, diagnosticReport, bundle, requestDTO.getProvenance());
        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * {@inheritDoc}
     */
    public List<LabTestDTO> getInvestigationByNames(RequestDTO requestDTO, List<String> names) {
        List<LabTestDTO> investigations = new ArrayList<>();
        String investigationUrl = String.format(Constants.INVESTIGATION_LIST_BY_NAME, requestDTO.getPatientReference(),
                String.join(Constants.COMMA, names), String.join(Constants.COMMA, List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS)));
        Bundle investigationBundle = restApiUtil.getBatchRequest(investigationUrl);
        investigationBundle.getEntry().forEach(resource -> {
            if (resource.getResource() instanceof DiagnosticReport diagnosticReport) {
                investigations.add(mapDiagnosticReportToLabTestDTO(diagnosticReport, Boolean.TRUE, investigationBundle));
            }
        });
        return investigations;
    }

    /**
     * {@inheritDoc}
     */
    public int getLabtestCount(RequestDTO request) {
        String investigationCountUrl = String.format(Constants.INVESTIGATION_COUNT_BY_PATIENT, Constants.STRING_FINAL,
                request.getPatientReference(), FhirIdentifierConstants.IS_REVIEW_SYSTEM_URL + Constants.VERTICAL_BAR + Boolean.FALSE);
        Bundle investigationBundle = restApiUtil.getBatchRequest(investigationCountUrl);
        return investigationBundle.getTotal();
    }
}
