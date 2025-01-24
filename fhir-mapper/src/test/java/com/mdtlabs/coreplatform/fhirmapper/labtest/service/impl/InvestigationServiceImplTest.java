package com.mdtlabs.coreplatform.fhirmapper.labtest.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.Code;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestCustomizationDTO;
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
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InvestigationServiceImplTest {

    @InjectMocks
    private InvestigationServiceImpl investigationService;

    @Mock
    private PatientVisitService patientVisitService;

    @Mock
    private PatientConverter patientConverter;

    @Mock
    private CommonConverter commonConverter;

    @Mock
    private PatientService patientService;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    private MockedStatic<UserContextHolder> userContextHolder;

    private static MockedStatic<CommonUtil> commonUtil;

    @Test
    void createOrUpdateInvestigation() {
        //given
        LabTestResultDTO labTestResultDTO = new LabTestResultDTO();
        labTestResultDTO.setId(TestConstants.STRING_THREE);
        labTestResultDTO.setPerformedName(TestConstants.NAME);
        labTestResultDTO.setPerformedBy(TestConstants.NAME);
        labTestResultDTO.setTestedOn(new Date());
        labTestResultDTO.setResource(Constants.STRING_NAME);
        labTestResultDTO.setValue(TestConstants.STRING_VALUE);
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDetailsData();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        labTestDTO.setPatientId(TestConstants.STRING_THREE);
        labTestDTO.setId(TestConstants.STRING_THREE);
        labTestDTO.setLabTestResults(List.of(labTestResultDTO));
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        labTestRequestDTO.setLabTests(List.of(labTestDTO));
        labTestRequestDTO.setEncounter(encounterDetailsDTO);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setSubject(new Reference(labTestDTO.getPatientId()));
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(diagnosticReport);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson), TestDataProvider.getHouseholdData());
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        Map<String, List<String>> fhirResponseMap = new HashMap<>();
        fhirResponseMap.put("Encounter", Arrays.asList("Encounter","Encounter"));
        fhirResponseMap.put("Patient", Arrays.asList("Patient","Patient"));

        //when
        doNothing().when(patientService).setPatientReferenceInEncounterDetails(labTestRequestDTO.getEncounter(), bundle);
        doNothing().when(fhirAssessmentMapper).updateEncounter(labTestRequestDTO.getEncounter(), bundle,
                null, null);
        when(restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY,
                labTestDTO.getId()))).thenReturn(bundle);
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.STRING_THREE);
        doNothing().when(fhirUtils).setBundle("Observation/urn:uuid:3", StringUtil.concatString(Constants.FHIR_BASE_URL, TestConstants.STRING_THREE),
                Bundle.HTTPVerb.POST, new Observation(),
                bundle, encounterDetailsDTO.getProvenance());
        doNothing().when(fhirUtils).setBundle(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT.concat(Constants.FORWARD_SLASH)
                        .concat(labTestDTO.getId()), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT,
                diagnosticReport, bundle, encounterDetailsDTO.getProvenance());

        when(fhirUtils.getFhirBaseUrl()).thenReturn(TestConstants.URL);
        when(restApiUtil.constructRequestEntity(bundle)).thenReturn(any());
        when(restApiUtil.postBatchRequest(TestConstants.URL, any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(fhirResponseMap);

        when(fhirUtils.getIdFromReference(labTestRequestDTO.getEncounter().getId())).thenReturn(TestConstants.STRING_VALUE);

        //then
        Map<String, String> response = investigationService.createOrUpdateInvestigation(labTestRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateInvestigationException() {
        //given
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        labTestRequestDTO.setLabTests(new ArrayList<>());

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, ()-> investigationService.createOrUpdateInvestigation(labTestRequestDTO));
    }

    @Test
    void getListOfInvestigatedDetails() {
        //given
        TestDataProvider.init();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.STRING_THREE);

        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL);
        identifier.setValue(Constants.APGAR_SCORE_TEN_MINUTE);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setSubject(new Reference(TestConstants.STRING_THREE));

        diagnosticReport.setIdentifier(List.of(identifier));
        diagnosticReport.setPerformer(List.of(reference));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(diagnosticReport);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        Practitioner practitioner = new Practitioner();
        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        practitioner.setName(List.of(name));

        bundleEntryComponent.setResource(practitioner);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        TestDataProvider.getStaticMock();
        when(restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY_WITH_STATUS, FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL,
                requestDTO.getPatientReference(), String.join(Constants.COMMA,
                        List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS))))).thenReturn(bundle);
        when(fhirUtils.getIdFromReference(diagnosticReport.getPerformer().getFirst().getReference())).thenReturn(TestConstants.STRING_THREE);
        when(fhirUtils.getIdFromReference(diagnosticReport.getSubject().getReference())).thenReturn(TestConstants.STRING_THREE);
        when(restApiUtil.getBatchRequest(TestConstants.STRING_THREE)).thenReturn(bundle);
        when(adminServiceApiInterface.getLabTestCustomizationByName(TestConstants.BEARER_TEST, TestConstants.ADMIN, new SearchRequestDTO(diagnosticReport.getIdentifier().getFirst().getValue(),
                TestConstants.ONE))).thenReturn(new LabTestCustomizationDTO());

        //then
        List<LabTestDTO> response = investigationService.getListOfInvestigatedDetails(requestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(response);
    }

    @Test
    void removeInvestigation() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL);
        identifier.setValue(Constants.APGAR_SCORE_TEN_MINUTE);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setSubject(new Reference(TestConstants.STRING_THREE));
        diagnosticReport.setId(TestConstants.STRING_THREE);

        diagnosticReport.setIdentifier(List.of(identifier));
        diagnosticReport.setPerformer(List.of(reference));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(diagnosticReport);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        ProvenanceDTO provenance = TestDataProvider.getProvenance();
        String url = "DiagnosticReport?encounter=null&status:not=cancelled";

        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                diagnosticReport);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        ReflectionTestUtils.setField(investigationService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.DIAGNOSTIC_REPORT_QUERY, TestConstants.STRING_THREE)))
                .thenReturn(bundle);
        when(restApiUtil.getBatchRequest(url)).thenReturn(TestDataProvider.getEncounterBundle());
        when(restApiUtil.getBatchRequest("Encounter?_id=null")).thenReturn(TestDataProvider.getEncounterBundle());
        doNothing().when(fhirUtils).setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_DIAGNOSTIC_REPORT,
                        Constants.FORWARD_SLASH, diagnosticReport.getIdPart()),
                StringUtil.concatString(Constants.FHIR_BASE_URL, diagnosticReport.getIdPart()),
                Bundle.HTTPVerb.PUT, diagnosticReport, bundle, provenance);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(responseEntity);

        //then
        Map<String,String> response = investigationService.removeInvestigation(TestConstants.STRING_THREE, provenance);
        Assertions.assertNotNull(response);
    }

    @Test
    void getInvestigatedDetails() {
        //given
        RequestDTO request = new RequestDTO();
        //then
        Assertions.assertThrows(SpiceValidation.class, () -> {
            investigationService.getInvestigatedDetails(request);
        });

        //given
        request.setPatientReference(TestConstants.STRING_THREE);
        request.setEncounterId(TestConstants.STRING_THREE);

        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.REFERRED);

        Period period = new Period();
        period.setStart(new Date());

        Encounter encounter = new Encounter();
        encounter.setIdentifier(List.of(identifier));
        encounter.setId(TestConstants.STRING_THREE);
        encounter.setPeriod(period);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(encounter);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, request.getEncounterId())))
                .thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.INVESTIGATION_LIST_PARAMS,
                FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL, request.getPatientReference(),
                String.join(Constants.COMMA, List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS))))).thenReturn(bundle);

        //then
        LabTestHistoryDTO response = investigationService.getInvestigatedDetails(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getInvestigatedDetailWithoutEncounterId() {
        //given
        RequestDTO request = new RequestDTO();
        request.setPatientReference(TestConstants.STRING_THREE);

        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.REFERRED);

        Period period = new Period();
        period.setStart(new Date());

        Encounter encounter = new Encounter();
        encounter.setIdentifier(List.of(identifier));
        encounter.setId(TestConstants.STRING_THREE);
        encounter.setPeriod(period);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(encounter);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_INVESTIGATION_ENCOUNTER_QUERY, request.getPatientReference(),
                StringUtil.concatString(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL, Constants.VERTICAL_BAR, Constants.INVESTIGATED))))
                .thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, TestConstants.STRING_THREE))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.INVESTIGATION_LIST_PARAMS,
                FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL, request.getPatientReference(),
                String.join(Constants.COMMA, List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS))))).thenReturn(bundle);

        //then
        LabTestHistoryDTO response = investigationService.getInvestigatedDetails(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getInvestigationsByEncounter() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL);
        identifier.setValue(Constants.APGAR_SCORE_TEN_MINUTE);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setSubject(new Reference(TestConstants.STRING_THREE));
        diagnosticReport.setId(TestConstants.STRING_THREE);

        diagnosticReport.setIdentifier(List.of(identifier));
        diagnosticReport.setPerformer(List.of(reference));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(diagnosticReport);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.INVESTIGATION_LIST_PARAMS,
                FhirIdentifierConstants.LAB_TEST_KEY_NAME_URL, TestConstants.PATIENT_REFERENCE,
                String.join(Constants.COMMA, List.of(Constants.FINAL_STATUS, Constants.REGISTERED_STATUS))))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.INVESTIGATION_HISTORY, TestConstants.STRING_THREE))).thenReturn(bundle);

        //then
        List<LabTestDTO> response = investigationService.getInvestigationsByEncounter(TestConstants.STRING_THREE, TestConstants.PATIENT_REFERENCE);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateNcdInvestigation() {
        //given
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        labTestDTO.setId(TestConstants.STRING_THREE);
        labTestDTO.setRecommendedBy(TestConstants.BLANK_STRING);
        labTestRequestDTO.setLabTests(List.of(labTestDTO));
        labTestRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(restApiUtil.getBatchRequest("DiagnosticReport?_id=3")).thenReturn(TestDataProvider.getDiagnosticReportBundle());
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));

        //then
        Map<String, String> response = investigationService.createOrUpdateNcdInvestigation(labTestRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateNcdInvestigationLabTestIsNull() {
        //given
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        labTestRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        labTestRequestDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        LabTestResultDTO labTestResultDTO = new LabTestResultDTO();
        labTestResultDTO.setPerformedBy(TestConstants.BEARER_TEST);
        Code code = new Code();
        code.setCode(TestConstants.CODE);
        code.setUrl(TestConstants.URL);
        labTestResultDTO.setCodeDetails(code);
        labTestResultDTO.setName(TestConstants.NAME);
        labTestDTO.setLabTestResults(Arrays.asList(labTestResultDTO));
        labTestDTO.setId(null);
        labTestDTO.setRecommendedBy(TestConstants.BLANK_STRING);
        labTestRequestDTO.setLabTests(List.of(labTestDTO));
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(restApiUtil.getBatchRequest("DiagnosticReport?_id=3")).thenReturn(TestDataProvider.getDiagnosticReportBundle());
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));

        //then
        Map<String, String> response = investigationService.createOrUpdateNcdInvestigation(labTestRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateNcdInvestigationException() {
        //given
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, ()-> investigationService.createOrUpdateNcdInvestigation(labTestRequestDTO));
    }

    @Test
    void createOrUpdateNcdInvestigationBadRequestException() {
        //given
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        LabTestDTO labTestDTO = new LabTestDTO();
        labTestRequestDTO.setLabTests(List.of(labTestDTO, labTestDTO));

        //then
        Assertions.assertThrows(BadRequestException.class, ()-> investigationService.createOrUpdateNcdInvestigation(labTestRequestDTO));
    }

    @Test
    void createOrUpdateNcdInvestigationAndPatientReferenceIsNull() {
        //given
        LabTestRequestDTO labTestRequestDTO = new LabTestRequestDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        labTestDTO.setRecommendedBy(TestConstants.BLANK_STRING);
        labTestRequestDTO.setLabTests(List.of(labTestDTO));
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        EncounterDetailsDTO encounter = new EncounterDetailsDTO();
        encounter.setProvenance(TestDataProvider.getProvenance());
        encounter.setMemberId(TestConstants.ONE_STR);
        labTestRequestDTO.setEncounter(encounter);
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        ScreeningLogRequestDTO screeningLogRequestDTO = TestDataProvider.getScreeningLogRequest();
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(commonConverter.setPatientDetails(any(), anyString())).thenReturn(screeningLogRequestDTO);
        when(patientConverter.createPatient(any(), any(), any(),any())).thenReturn(TestDataProvider.getPatient());
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));

        //then
        Map<String, String> response = investigationService.createOrUpdateNcdInvestigation(labTestRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdInvestigatedDetailsSpiceValidationException() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //then
        Assertions.assertThrows(SpiceValidation.class, ()->investigationService.getNcdInvestigatedDetails(requestDTO));
    }

    @Test
    void getNcdInvestigatedDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        requestDTO.setPatientVisitId(TestConstants.STRING_THREE);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        LabTestHistoryDTO response = investigationService.getNcdInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdInvestigatedDetailsPatientVisitIsNull() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = TestDataProvider.getDiagnosticReportBundle();
        Bundle  investigationHistoryBundle = new Bundle();

        //when
        when(restApiUtil.getBatchRequest("Encounter?_sort=-date&subject=Patient/Patient/urn:uuid:null&identifier=nullinvestigation-status|Investigated&_count=9999"))
                .thenReturn(TestDataProvider.getEncounterBundle());
        when(restApiUtil.getBatchRequest("Encounter?part-of=Encounter/Patient/urn:uuid:null&subject=Patient/Patient/urn:uuid:null&identifier=Investigated&_sort=-_lastUpdated"))
                .thenReturn(TestDataProvider.getEncounterBundle());
        when(restApiUtil.getBatchRequest("DiagnosticReport?encounter=Encounter/2&_sort=-_id"))
                .thenReturn(bundle);
        when(restApiUtil.getBatchRequest("Practitioner?_id=Patient/urn:uuid:null")).thenReturn(TestDataProvider.getPractitionerBundle());
        when(fhirUtils.getIdFromReference(any())).thenReturn(TestConstants.PATIENT_REFERENCE);
        when(restApiUtil.getBatchRequest(any())).thenReturn(investigationHistoryBundle);

        //then
        LabTestHistoryDTO response = investigationService.getNcdInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateInvestigationResult() {
        //given
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();
        labTestRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        LabTestDTO labTestDTO= TestDataProvider.getLabTestDTO();
        labTestDTO.setId(TestConstants.STRING_THREE);
        labTestRequestDTO.setLabTests(List.of(labTestDTO, labTestDTO));

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getDiagnosticReportBundle());

        //then
        investigationService.updateInvestigationResult(labTestRequestDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void updateInvestigationResultException() {
        //given
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();
        labTestRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        LabTestDTO labTestDTO= new LabTestDTO();
        labTestRequestDTO.setLabTests(List.of(labTestDTO, labTestDTO));

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());

        //then
        Assertions.assertThrows(DataNotAcceptableException.class ,()->investigationService.updateInvestigationResult(labTestRequestDTO));
    }

    @Test
    void updateInvestigationResultAndLabTestDtoIsNull() {
        //given
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();
        labTestRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        labTestRequestDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        LabTestDTO labTestDTO= new LabTestDTO();
        labTestDTO.setId(TestConstants.STRING_THREE);
        labTestDTO.setComments(TestConstants.BEARER_TEST);
        LabTestResultDTO labTest = new LabTestResultDTO();
        labTest.setId("1");
        labTest.setName("Blood Test");
        labTest.setValue("5.5");
        labTest.setUnit("mmol/L");
        labTest.setResource("Observation/1");
        labTest.setTestedOn(new Date()); // Current date
        labTest.setPatientId("patient123");
        labTest.setPerformedBy("doctor123");
        labTest.setPerformedName("Dr. John Doe");
        labTestDTO.setLabTestResults(Arrays.asList(labTest, labTest));
        labTestRequestDTO.setLabTests(Arrays.asList(labTestDTO, labTestDTO));

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getDiagnosticReportBundle());

        //then
        investigationService.updateInvestigationResult(labTestRequestDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void reviewInvestigation() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setId(TestConstants.STRING_THREE);

        //when
        when(restApiUtil.getBatchRequest("DiagnosticReport?_id=3")).thenReturn(TestDataProvider.getDiagnosticReportBundle());

        //then
        investigationService.reviewInvestigation(requestDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void reviewInvestigationBadRequestException() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(restApiUtil.getBatchRequest("DiagnosticReport?_id=3")).thenReturn(new Bundle());

        //then
        Assertions.assertThrows(BadRequestException.class, ()-> investigationService.reviewInvestigation(requestDTO));
    }

    @Test
    void reviewInvestigationDataNotAcceptableException() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setId(TestConstants.STRING_THREE);

        //when
        when(restApiUtil.getBatchRequest("DiagnosticReport?_id=3")).thenReturn(new Bundle().setEntry(new ArrayList<>()));

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, ()-> investigationService.reviewInvestigation(requestDTO));
    }

    @Test
    void getInvestigationByNames() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<String> names = Arrays.asList("name1","name2");
        Bundle bundle = TestDataProvider.getDiagnosticReportBundle();
        Observation observation = (Observation) TestDataProvider.getObservationBundle().getEntry().getFirst().getResource();
        Reference reference = new Reference();
        reference.setId(TestConstants.ONE_STR);
        List<Reference> references = new ArrayList<>();
        references.add(reference);
        observation.setPerformer(references);
        bundle.addEntry().setResource(observation);
        commonUtil = mockStatic(CommonUtil.class);
        userContextHolder = mockStatic(UserContextHolder.class);
        UserDTO userDTO = new UserDTO();
        Country country = new Country();
        country.setId(1L);
        country.setName("United States");
        userDTO.setCountry(country);
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setCountry(country);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(restApiUtil.getBatchRequest("Practitioner?_id=2")).thenReturn(TestDataProvider.getPractitionerBundle());
        when(fhirUtils.getIdFromReference(any())).thenReturn(TestConstants.TWO_STR);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);

        //then
        List<LabTestDTO> response = investigationService.getInvestigationByNames(requestDTO, names);
        Assertions.assertNotNull(response);
        commonUtil.close();
        userContextHolder.close();
    }

    @Test
    void getLabtestCount() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getPractitionerBundle());

        //then
        investigationService.getLabtestCount(requestDTO);
        Assertions.assertEquals(TestDataProvider.getPractitionerBundle().getTotal(), investigationService.getLabtestCount(requestDTO));
    }

    @Test
    void getListOfInvestigatedDetailsAndRoleNameNotNull() {
        //given
        TestDataProvider.init();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.STRING_THREE);
        requestDTO.setRoleName("LAB_TECHNICIAN");
        String url = "DiagnosticReport?identifier=nulllab-test-name|&subject=Patient/3&status=registered&_count=9999&_include=" +
                "DiagnosticReport:results-interpreter&_include=DiagnosticReport:performer&_include=DiagnosticReport:result&_sort=" +
                "-issued";

        //when
        TestDataProvider.getStaticMock();
        when(restApiUtil.getBatchRequest(url)).thenReturn(TestDataProvider.getDiagnosticReportBundle());
        when(fhirUtils.getIdFromReference("1")).thenReturn("diag-001");
        when(restApiUtil.getBatchRequest("Practitioner?_id=diag-001")).thenReturn(TestDataProvider.getPractitionerBundle());

        //then
        List<LabTestDTO> response = investigationService.getListOfInvestigatedDetails(requestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(response);
    }

}