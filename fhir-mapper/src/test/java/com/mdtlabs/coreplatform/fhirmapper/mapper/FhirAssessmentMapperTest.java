package com.mdtlabs.coreplatform.fhirmapper.mapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IQuery;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.DomainResource;
import org.hl7.fhir.r4.model.Dosage;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Timing;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.internal.stubbing.defaultanswers.ReturnsDeepStubs;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.APGARScoreDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.Code;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FhirAssessmentMapperTest {

    @InjectMocks
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private UserService userService;

    @Mock
    private RedisTemplate<String, Map<String,  List<MetaDataDTO>>> redisTemplate;

    @Mock
    private ValueOperations valueOperations;

    @Mock
    private RestApiUtil restApiUtil;

    @Test
    void setPatient() {
        //given
        Patient patient = new Patient();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setIsPregnant(Boolean.TRUE);

        //then
        Patient result = fhirAssessmentMapper.setPatient(patient, householdMemberDTO, TestConstants.ADMIN);
        assertNotNull(result);
    }

    @Test
    void createEncounter() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        ReflectionTestUtils.setField(fhirAssessmentMapper, TestConstants.FHIR_SERVER_URL, TestConstants.URL);
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getAssessmentDTO().getEncounter();
        String url = fhirAssessmentMapper.createEncounter(encounterDetailsDTO, new Bundle(),
                TestConstants.FHIR_SERVER_URL, null);
        encounterDetailsDTO.setReferred(Boolean.FALSE);
        String url2 = fhirAssessmentMapper.createEncounter(encounterDetailsDTO, new Bundle(),
                TestConstants.FHIR_SERVER_URL, null);
        assertNotNull(url);
        assertNotNull(url2);
        TestDataProvider.cleanUp();
    }

    @Test
    void createObservation() {
        Bundle bundle = new Bundle();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getAssessmentDTO().getEncounter();
        fhirAssessmentMapper.createObservation(encounterDetailsDTO, TestConstants.NAME, Boolean.TRUE,
                TestConstants.FHIR_SERVER_URL, bundle);
        fhirAssessmentMapper.createObservation(encounterDetailsDTO, TestConstants.NAME, Boolean.FALSE,
                TestConstants.FHIR_SERVER_URL, bundle);

        fhirAssessmentMapper.createObservation(encounterDetailsDTO, TestConstants.NAME, TestConstants.URL,
                TestConstants.FHIR_SERVER_URL, bundle);
        fhirAssessmentMapper.createObservation(encounterDetailsDTO, TestConstants.NAME, TestConstants.URL,
                TestConstants.FHIR_SERVER_URL, bundle);
        assertNotNull(bundle);
        assertNotNull(bundle.getEntry());
    }

    @Test
    void addObservationToBundle() {
        String url = fhirAssessmentMapper.addObservationToBundle(new Observation(), new Bundle(), new ProvenanceDTO());
        assertNotNull(url);
    }

    @Test
    void setPatientBasicDetails() {
        Map<String, Observation> map = new HashMap<>();
        Map<String, Observation> mapWithObservations = new HashMap<>();

        PregnancyDetailsDTO pregnancyDetailsDTO = fhirAssessmentMapper.setPatientBasicDetails(map);
        assertNotNull(pregnancyDetailsDTO);

        //set Values
        Observation observation = new Observation();
        observation.getValueDateTimeType().setValue(new Date());
        mapWithObservations.put(Constants.LAST_MENSTRUAL_PERIOD, observation);

        Observation observationAnc = new Observation();
        observationAnc.getValueIntegerType().setValue(TestConstants.TEN);
        mapWithObservations.put(Constants.ANC, observationAnc);
        mapWithObservations.put(Constants.PNC, observationAnc);
        mapWithObservations.put(Constants.CHILDHOOD_VISIT, observationAnc);
        PregnancyDetailsDTO pregnancyDetailsDTOAnc = fhirAssessmentMapper.setPatientBasicDetails(mapWithObservations);
        assertNotNull(pregnancyDetailsDTOAnc);

    }

    @Test
    void setValuesToObservation() {
        Observation observationString = new Observation();
        Observation observationBoolean = new Observation();
        Observation observationDate = new Observation();

        fhirAssessmentMapper.setValuesToObservation(observationString, TestConstants.ACTIVE_UPPER, null, null);
        fhirAssessmentMapper.setValuesToObservation(observationBoolean, null, Boolean.TRUE, null);
        fhirAssessmentMapper.setValuesToObservation(observationDate, null, null, new Date());

        assertNotNull(observationBoolean.getValue());
        assertNotNull(observationString.getValue());
        assertNotNull(observationDate.getValue());

    }

    @Test
    void setQuantityToObservation() {
        Observation observation = new Observation();
        fhirAssessmentMapper.setQuantityToObservation(observation, 11.2, TestConstants.URL);
        assertNotNull(observation);

        fhirAssessmentMapper.setQuantityToObservation(observation, null, TestConstants.URL);
        assertNotNull(observation);

        fhirAssessmentMapper.setQuantityToObservation(observation, new Quantity(11.2), null);
        assertNotNull(observation);

        fhirAssessmentMapper.setQuantityToObservation(observation, null, null);
        assertNotNull(observation);
    }

    @Test
    void setMedicationDispense() {
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        CodeableConcept codeableConcept = new CodeableConcept();
        when(fhirUtils.setCodes(TestConstants.NAME)).thenReturn(codeableConcept);
        MedicationDispense medicationDispense = fhirAssessmentMapper.setMedicationDispense(assessmentDTO.getEncounter(),
                TestConstants.NAME, "COMPLETED", assessmentDTO.getId());
        assertNotNull(medicationDispense);
        assessmentDTO.getEncounter().setReferred(Boolean.FALSE);
        MedicationDispense medicationDispenseResult = fhirAssessmentMapper.setMedicationDispense(assessmentDTO.getEncounter(),
                TestConstants.NAME, "COMPLETED", assessmentDTO.getId());
        assertNotNull(medicationDispenseResult);
    }

    @Test
    void setMedicationRequest() {
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        MedicationRequest medicationRequest = fhirAssessmentMapper.setMedicationRequest(assessmentDTO,
                TestConstants.NAME, List.of(), TestConstants.ACTIVE_UPPER);
        assertNotNull(medicationRequest);

        assessmentDTO.getEncounter().setReferred(Boolean.FALSE);
        MedicationRequest medicationRequestFalse = fhirAssessmentMapper.setMedicationRequest(assessmentDTO,
                TestConstants.NAME, List.of(), TestConstants.ACTIVE_UPPER);
        assertNotNull(medicationRequestFalse);
    }

    @Test
    void createObservationComponent() {
        Observation.ObservationComponentComponent value = fhirAssessmentMapper.createObservationComponent(
                TestConstants.FHIR_SERVER_URL);
        assertNotNull(value);
    }

    @Test
    void testCreateObservationComponent() {
        List<Observation.ObservationComponentComponent> observation = new ArrayList<>();
        List<Observation.ObservationComponentComponent> observationDate = new ArrayList<>();
        List<Observation.ObservationComponentComponent> observationString = new ArrayList<>();
        List<Observation.ObservationComponentComponent> observationNullTest = new ArrayList<>();

        fhirAssessmentMapper.createObservationComponent(TestConstants.FHIR_SERVER_URL, TestConstants.URL, observation);
        fhirAssessmentMapper.createObservationComponent(new Date(), TestConstants.URL, observationDate);
        fhirAssessmentMapper.createObservationComponent(Boolean.TRUE, TestConstants.URL, observationString);
        fhirAssessmentMapper.createObservationComponent(Boolean.FALSE, TestConstants.URL, observationNullTest);

        assertEquals(TestConstants.ONE, observationNullTest.size());
        assertEquals(TestConstants.ONE, observation.size());
        assertEquals(TestConstants.ONE, observationNullTest.size());
        assertEquals(TestConstants.ONE, observationNullTest.size());
    }

    @Test
    void mapDiagnosticReport() {
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
        DiagnosticReport report = fhirAssessmentMapper.mapDiagnosticReport(TestConstants.NAME, assessmentDTO,
                TestConstants.NOT_APPLICABLE, TestConstants.ENCOUNTER_REFERENCE);
        assertNotNull(report);
        assessmentDTO.getEncounter().setReferred(Boolean.FALSE);
        DiagnosticReport reportTwo = fhirAssessmentMapper.mapDiagnosticReport(TestConstants.NAME, assessmentDTO,
                TestConstants.URL, TestConstants.ENCOUNTER_REFERENCE);
        assertNotNull(reportTwo);
    }

    @Test
    void setReferralTicket() {
        ServiceRequest serviceRequest = new ServiceRequest();
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        serviceRequest.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                .setValue(assessmentDTO.getPatientStatus());
        serviceRequest.setSubject(new Reference(assessmentDTO.getEncounter().getPatientReference()));
        serviceRequest.setAuthoredOn(assessmentDTO.getEncounter().getProvenance().getModifiedDate());
        serviceRequest.setPatientInstruction(assessmentDTO.getReferredReasons());
        serviceRequest.addPerformer(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Practitioner), Constants.FORWARD_SLASH,
                        assessmentDTO.getEncounter().getProvenance().getUserId())));
        serviceRequest.setEncounter(new Reference(assessmentDTO.getId()));
        serviceRequest.setOccurrence(new DateTimeType(assessmentDTO.getSummary().getNextVisitDate()));
        serviceRequest.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
        serviceRequest.setIntent(ServiceRequest.ServiceRequestIntent.ORDER);
        // Add referred organization
        serviceRequest.addPerformer(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Organization), Constants.FORWARD_SLASH,
                        TestConstants.ACTIVE_UPPER)));
        //Add current organization
        serviceRequest.setRequester(new Reference(
                StringUtil.concatString(String.valueOf(ResourceType.Organization), Constants.FORWARD_SLASH,
                        assessmentDTO.getEncounter().getProvenance().getOrganizationId())));

        Provenance provenance = new Provenance();
        Provenance.ProvenanceAgentComponent provenanceAgentComponent = new Provenance.ProvenanceAgentComponent();
        provenanceAgentComponent.setWho(new Reference(TestConstants.TWO_STR));
        provenanceAgentComponent.setOnBehalfOf(new Reference(TestConstants.TWO_STR));
        List<Provenance.ProvenanceAgentComponent> provenanceAgentComponents = new ArrayList<>();
        provenanceAgentComponents.add(provenanceAgentComponent);
        provenance.setAgent(provenanceAgentComponents);

        Map<String, Resource> resourceMap = new HashMap<>();
        Practitioner practitioner = new Practitioner();
        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        practitioner.setName(List.of(name));
        ContactPoint contactPoint = new ContactPoint();
        contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
        contactPoint.setValue(TestConstants.ONE_STR);
        //Add Phone number Category
        practitioner.addTelecom(contactPoint);
        resourceMap.put(serviceRequest.getRequester().getReference(), new Organization());
        resourceMap.put(null, new Organization());
        resourceMap.put(serviceRequest.getPerformer().getFirst().getReference(), practitioner);
        resourceMap.put(StringUtil.concatString(String.valueOf(ResourceType.Organization), Constants.FORWARD_SLASH,
                TestConstants.ACTIVE_UPPER), new Organization());
        Map<String, Provenance> provenanceMap = new HashMap<>();
        provenanceMap.put(serviceRequest.getId(), provenance);

        //when
        when(fhirUtils.getIdFromHistoryUrl(serviceRequest.getId())).thenReturn(TestConstants.TWO_STR);
        when(userService.getUserById(provenanceMap.get(serviceRequest.getIdPart()).getAgent().getFirst().getWho().getReference())).thenReturn(practitioner);

        //then
        ReferralTicketDTO referralTicketDTO = fhirAssessmentMapper.setReferralTicket(serviceRequest,
                new ReferralTicketDTO(), resourceMap, provenanceMap);
        assertNotNull(referralTicketDTO);
        serviceRequest.setIdentifier(List.of());
        practitioner.setTelecom(List.of());
        ReferralTicketDTO referralTicketDTOTwo = fhirAssessmentMapper.setReferralTicket(serviceRequest,
                new ReferralTicketDTO(), resourceMap, provenanceMap);
        assertNotNull(referralTicketDTOTwo);
    }

    @Test
    void setValuesToObservationTest() {
        TestDataProvider.init();
        ObservationDTO observationDTO = new ObservationDTO();
        observationDTO.setType(Constants.WEIGHT);
        observationDTO.setWeight(11.2);
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setMemberId(TestConstants.TWO_STR);
        encounterDetailsDTO.setProvenance(new ProvenanceDTO());
        observationDTO.setEncounter(encounterDetailsDTO);
        ReflectionTestUtils.setField(fhirAssessmentMapper, TestConstants.FHIR_SERVER_URL, TestConstants.URL);
        // Mock the behavior of the client search
        IGenericClient client = mock(IGenericClient.class, new ReturnsDeepStubs());
        Bundle bundle = new Bundle();
        bundle.setTotal(0);
        IUntypedQuery<IBaseBundle> mockClient = client.search();
        IQuery<IBaseBundle> resource = mockClient.byUrl(TestConstants.FHIR_SERVER_URL);

        //when
        TestDataProvider.getStaticMock();
        when(client.search()).thenReturn((mockClient));
        when(mockClient.byUrl(any())).thenReturn(resource);
        when(resource.returnBundle(any(Class.class)).execute()).thenReturn(bundle);
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.TWO_STR);
        when(fhirUtils.getClient(TestConstants.URL, TestConstants.ADMIN, TestConstants.BEARER_TEST)).thenReturn(client);
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        fhirAssessmentMapper.createValueObservation(observationDTO, bundle);

        observationDTO.setType(Constants.LAST_MENSTRUAL_PERIOD);
        observationDTO.setDateValue(new Date());
        fhirAssessmentMapper.createValueObservation(observationDTO, new Bundle());

        observationDTO.setType(Constants.ANC_VISIT_NUMBER);
        observationDTO.setNumberValue(TestConstants.ONE.intValue());
        fhirAssessmentMapper.createValueObservation(observationDTO, new Bundle());

        observationDTO.setType(Constants.BP);
        Boolean result = fhirAssessmentMapper.createValueObservation(observationDTO, new Bundle());
        Assertions.assertTrue(result);
        TestDataProvider.cleanUp();
    }

    @Test
    void createVitalObservation() {
        ObservationDTO observationDTO = new ObservationDTO();
        observationDTO.setType(Constants.WEIGHT);
        observationDTO.setWeight(11.2);
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setProvenance(new ProvenanceDTO());
        observationDTO.setEncounter(encounterDetailsDTO);

        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        ReflectionTestUtils.setField(fhirAssessmentMapper, TestConstants.FHIR_SERVER_URL, TestConstants.URL);
        // Mock the behavior of the client search
        IGenericClient client = mock(IGenericClient.class, new ReturnsDeepStubs());
        Bundle bundle = new Bundle();
        bundle.setEntry(List.of(new Bundle.BundleEntryComponent()));
        IUntypedQuery<IBaseBundle> mockClient = client.search();
        IQuery<IBaseBundle> resource = mockClient.byUrl(TestConstants.FHIR_SERVER_URL);
        when(client.search()).thenReturn((mockClient));
        when(mockClient.byUrl(any())).thenReturn(resource);
        when(resource.returnBundle(any(Class.class)).execute()).thenReturn(bundle);
        when(fhirUtils.getClient(TestConstants.URL, TestConstants.ADMIN, TestConstants.BEARER_TEST)).thenReturn(client);
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        fhirAssessmentMapper.createVitalObservation(new Bundle(), encounterDetailsDTO,"", 1,
                TestConstants.URL);

        fhirAssessmentMapper.createVitalObservation(new Bundle(), encounterDetailsDTO,"", new Date(),
                TestConstants.URL);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());

        TestDataProvider.cleanUp();
    }

    @Test
    void mapMedicationRequest() {
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        prescriptionDTO.setCodeDetails(new Code(TestConstants.CODE, TestConstants.URL));
        MedicationRequest response = fhirAssessmentMapper.mapMedicationRequest(prescriptionDTO, new MedicationRequest());
        Assertions.assertNotNull(response);
    }

    @Test
    void setIccmStatus() {
        //given
        String iccmStatus = TestConstants.ICCM;
        String status = TestConstants.ICCM;

        //then
        String result = fhirAssessmentMapper.setIccmStatus(iccmStatus, status);
        Assertions.assertNotNull(result);
    }

    @Test
    void changePatientStatusToDisplayFormat() {
        //given
        String pregnantStatus = "Pregnant";
        String onTreatmentStatus = "OnTreatment";

        //then
        String pregnantStatusResult = fhirAssessmentMapper.changePatientStatusToDisplayFormat(pregnantStatus);
        String onTreatmentStatusResult = fhirAssessmentMapper.changePatientStatusToDisplayFormat(onTreatmentStatus);
        Assertions.assertNotNull(pregnantStatusResult);
        Assertions.assertNotNull(onTreatmentStatusResult);
    }

    @Test
    void getSummaryStatus() {
        //given
        Patient patient = TestDataProvider.getPatient();

        //then
        List<Map<String, String>> result = fhirAssessmentMapper.getSummaryStatus(patient);
        Assertions.assertNotNull(result);
    }

    @Test
    void updateEncounterStatusDetails() {
        //given
        String id = TestConstants.STRING_THREE;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        String status = TestConstants.ACTIVE_UPPER;
        Bundle updateBundle = new Bundle();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        Encounter result = fhirAssessmentMapper.updateEncounterStatusDetails(id, provenanceDTO, status, updateBundle);
        Assertions.assertNotNull(result);
    }

    @Test
    void getEncounterDetails() {
        //given
        String id = TestConstants.STRING_THREE;
        boolean includeObservation = Boolean.TRUE;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        Bundle result = fhirAssessmentMapper.getEncounterDetails(id, includeObservation);
        Assertions.assertNotNull(result);
    }

    @Test
    void setPregnancyStatus() {
        //given
        Boolean status = Boolean.TRUE;
        DomainResource resource = new Patient();
        resource.setExtension(List.of(new Extension("nullfhir/StructureDefinition/pregnant-status")));

        //then
        DomainResource result = fhirAssessmentMapper.setPregnancyStatus(status, resource);
        Assertions.assertNotNull(result);
    }

    @Test
    void updateEncounter() {
        //given
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        Bundle bundle = new Bundle();
        String encounterType = TestConstants.ENCOUNTER;
        String partOfEncounter = TestConstants.ENCOUNTER;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        fhirAssessmentMapper.updateEncounter(encounterDetailsDTO, bundle, encounterType, partOfEncounter);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createBirthHistory() {
        //given
        List<String> signs = new ArrayList<>();
        Double weight = TestConstants.ONE_DOUBLE;
        Integer gestationalAge = TestConstants.INT_ONE;
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        Bundle bundle = new Bundle();

        //when
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        fhirAssessmentMapper.createBirthHistory(signs, weight, gestationalAge, encounterDetailsDTO, bundle);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void updatePreviousEncounters() {
        //given
        List<String> identifier = new ArrayList<>();
        Bundle bundle = new Bundle();
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        String patientReference = TestConstants.PATIENT_REFERENCE;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        fhirAssessmentMapper.updatePreviousEncounters(identifier, bundle, provenanceDTO, patientReference);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void setEncounter() {
        //given
        Encounter encounter = TestDataProvider.getEncounter();
        EncounterDetailsDTO encounterDetails = TestDataProvider.getEncounterDetailsData();
        encounterDetails.setPrescribed(Boolean.TRUE);
        encounterDetails.setDispensed(Boolean.FALSE);
        encounterDetails.setDiagnosisType(TestConstants.DIAGNOSIS_TYPE);
        String encounterType = TestConstants.ENCOUNTER;
        boolean isEncounterCreate = Boolean.TRUE;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        Encounter result = fhirAssessmentMapper.setEncounter(encounter, encounterDetails, encounterType, isEncounterCreate);
        Assertions.assertNotNull(result);
    }

    @Test
    void mapToDiagnosis() {
        //given
        Condition condition = TestDataProvider.getCondition();
        boolean isMedicalReviewSummary = Boolean.TRUE;
        String type = TestConstants.DIAGNOSIS_TYPE;

        //then
        DiagnosisDTO.DiseaseDTO result = fhirAssessmentMapper.mapToDiagnosis(condition, isMedicalReviewSummary, type);
        Assertions.assertNotNull(result);
    }

    @Test
    void getProvisionalDiagnosisDetails() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        bundle.getEntry().getFirst().setResource(TestDataProvider.getCondition());

        //then
        List<String> result = fhirAssessmentMapper.getProvisionalDiagnosisDetails(bundle);
        Assertions.assertNotNull(result);
    }

    @Test
    void mapToDiagnosisBundle() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        bundle.getEntry().getFirst().setResource(TestDataProvider.getCondition());
        Map<String, MetaCodeDetails> codeDetailsMap = new HashMap<>();
        codeDetailsMap.put("Problem List Item" , new MetaCodeDetails());

        //when
        when(fhirUtils.getCodeDetails()).thenReturn(codeDetailsMap);

        //then
        ConfirmDiagnosisDTO result = fhirAssessmentMapper.mapToDiagnosis(bundle);
        Assertions.assertNotNull(result);
    }

    @Test
    void createNotes() {
        //given
        String title = TestConstants.BLANK_STRING;
        String notes = TestConstants.BLANK_STRING;
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());

        //then
        Observation result = fhirAssessmentMapper.createNotes(title, notes, encounterDetailsDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createObservationComponentTest() {
        //given
        Boolean value = Boolean.TRUE;
        String question = TestConstants.BLANK_STRING;

        //then
        Observation.ObservationComponentComponent result = fhirAssessmentMapper.createObservationComponent(value, question);
        Assertions.assertNotNull(result);
    }

    @Test
    void createSignsObservationNull() {
        //given
        List<String> signs = new ArrayList<>();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        String identifier = TestConstants.BLANK_STRING;
        String note = TestConstants.BLANK_STRING;

        //then
        Observation result = fhirAssessmentMapper.createSignsObservation(signs, encounterDetailsDTO, identifier, note);
        Assertions.assertNull(result);
    }

    @Test
    void createSignsObservation() {
        //given
        List<String> signs = TestDataProvider.getSymptomName();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        String identifier = TestConstants.BLANK_STRING;
        String note = TestConstants.BLANK_STRING;

        //then
        Observation result = fhirAssessmentMapper.createSignsObservation(signs, encounterDetailsDTO, identifier, note);
        Assertions.assertNotNull(result);
    }

    @Test
    void createApgarObservationComponent() {
        //given
        APGARScoreDTO apgarScoreDTO = new APGARScoreDTO();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        String identifier = TestConstants.BLANK_STRING;

        //then
        Observation result = fhirAssessmentMapper.createApgarObservationComponent(apgarScoreDTO, encounterDetailsDTO, identifier);
        Assertions.assertNotNull(result);
    }

    @Test
    void createReferralTicket() {
        //given
        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();
        referralDetailsDTO.setPatientStatus(TestConstants.PATIENT);
        referralDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        referralDetailsDTO.setNextVisitDate(new Date());

        //then
        ServiceRequest serviceRequest = fhirAssessmentMapper.createReferralTicket(referralDetailsDTO);
        Assertions.assertNotNull(serviceRequest);
    }

    @Test
    void saveObservationWithList() {
        //given
        List<String> values = new ArrayList<>();
        values.add(TestConstants.BLANK_STRING);

        //then
        fhirAssessmentMapper.saveObservationWithList(values);
        Assertions.assertNotNull(values);
    }

    @Test
    void createObservationComponents() {
        //given
        Double value = TestConstants.ONE_DOUBLE;
        String unit = TestConstants.BLANK_STRING;
        String question = TestConstants.BLANK_STRING;
        List< Observation.ObservationComponentComponent > components = new ArrayList<>();

        //then
        fhirAssessmentMapper.createObservationComponent(value, unit, question, components);
        Assertions.assertNotNull(components);
    }

    @Test
    void mapMedicationDispense() {
        //given
        PrescriptionDTO prescriptionRequest = TestDataProvider.getPrescriptionDTO();
        MedicationRequest medicationRequest = TestDataProvider.getMedicationRequest();
        medicationRequest.setDosageInstruction(List.of(new Dosage().setTiming(new Timing().setRepeat(new Timing.TimingRepeatComponent().setPeriod(TestConstants.ONE_DOUBLE)))));
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());

        //then
        MedicationDispense result = fhirAssessmentMapper.mapMedicationDispense(prescriptionRequest, medicationRequest, encounterDetailsDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void UpdateDispenseRequest() {
        //given
        PrescriptionDTO prescriptionRequest = TestDataProvider.getPrescriptionDTO();
        MedicationRequest medicationRequest = TestDataProvider.getMedicationRequest();
        medicationRequest.setDosageInstruction(List.of(new Dosage().setTiming(new Timing().setRepeat(new Timing.TimingRepeatComponent().setPeriod(TestConstants.ONE_DOUBLE)))));

        //then
        MedicationRequest result = fhirAssessmentMapper.updateDispenseRequest(prescriptionRequest, medicationRequest);
        Assertions.assertNotNull(result);
    }

    @Test
    void updateVitalObservationsStatus() {
        //given
        List<String> types = new ArrayList<>();
        Bundle bundle = new Bundle();
        String memberId = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        fhirAssessmentMapper.updateVitalObservationsStatus(types, bundle, memberId, provenanceDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void getPatientBasicDetails() {
        //given
        List<String> types = new ArrayList<>();
        String patientReference = TestConstants.PATIENT_REFERENCE;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        Bundle result = fhirAssessmentMapper.getPatientBasicDetails(types, patientReference);
        Assertions.assertNotNull(result);
    }

    @Test
    void createVitalObservations() {
        //given
        Bundle bundle = new Bundle();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        String type = TestConstants.DIAGNOSIS_TYPE;
        Boolean value = Boolean.TRUE;
        String patient = TestConstants.PATIENT;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        fhirAssessmentMapper.createVitalObservation(bundle, encounterDetailsDTO, type, value, patient);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createVitalObservationAndStringValue() {
        //given
        Bundle bundle = new Bundle();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        String type = TestConstants.DIAGNOSIS_TYPE;
        String value = TestConstants.BLANK_STRING;
        String patient = TestConstants.PATIENT;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        fhirAssessmentMapper.createVitalObservation(bundle, encounterDetailsDTO, type, value, patient);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createVitalObservationAndDoubleValue() {
        //given
        Bundle bundle = new Bundle();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        String type = TestConstants.DIAGNOSIS_TYPE;
        Double value = TestConstants.ONE_DOUBLE;
        String patient = TestConstants.PATIENT;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        fhirAssessmentMapper.createVitalObservation(bundle, encounterDetailsDTO, type, value, patient);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createVitalObservationAndSys() {
        //given
        Bundle bundle = new Bundle();
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        encounterDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        Double sys = TestConstants.ONE_DOUBLE;
        Double dis = TestConstants.ONE_DOUBLE;
        Double pulse = TestConstants.ONE_DOUBLE;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        fhirAssessmentMapper.createVitalObservation(bundle, encounterDetailsDTO, sys, dis, pulse);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createObservationQuestionAndComponent() {
        //given
        String question = TestConstants.BLANK_STRING;
        List<Observation.ObservationComponentComponent> components = new ArrayList<>();

        //then
        fhirAssessmentMapper.createObservationComponent(question, components);
        Assertions.assertNotNull(components);
    }

    @Test
    void createNoteObservation() {
        //given
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        String identifier = TestConstants.BLANK_STRING;
        String code = TestConstants.CODE;
        String note = TestConstants.BLANK_STRING;
        boolean isCounselorAssessment = Boolean.TRUE;
        Long noteObservationId = TestConstants.ONE;

        //then
        Observation result = fhirAssessmentMapper.createNoteObservation(encounterDetailsDTO, provenanceDTO, identifier, code, note, isCounselorAssessment, noteObservationId);
        Assertions.assertNotNull(result);
    }
}