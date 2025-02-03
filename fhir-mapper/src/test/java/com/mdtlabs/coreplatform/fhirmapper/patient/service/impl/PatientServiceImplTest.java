package com.mdtlabs.coreplatform.fhirmapper.patient.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Coverage;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.IntegerType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdPatientStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientFilterDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.ConditionConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SearchPersonDetailsConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientServiceImplTest {

    @InjectMocks
    private PatientServiceImpl patientService;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private PatientConverter patientConverter;

    @Mock
    private HouseholdService householdService;

    @Mock
    private FhirMapper fhirMapper;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private HttpEntity<?> httpEntity;

    @Mock
    PregnancyConverter pregnancyConverter;

    @Mock
    private CommonConverter commonConverter;

    @Mock
    private VitalSignsConverter vitalSignsConverter;

    @Mock
    private SpiceConverter spiceConverter;

    @Mock
    private SearchPersonDetailsConverter searchPersonDetailsConverter;

    @Mock
    private PatientVisitService patientVisitService;

    @Mock
    private EncounterConverter encounterConverter;

    @Mock
    private ConditionConverter conditionConverter;

    @Test
    void getPatientListIsOne() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setReferencePatientId(TestConstants.PATIENT_ID);
        patientRequestDTO.setSearchText(TestConstants.DIAGNOSIS_TYPE);
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getRelatedPerson());
        String url = "ServiceRequest?priority=urgent&_elements=subject&_count=99999&requisition=medicalReview&subject.active=true&_id:not=null,abcd-efgh-ijkl-mnop&subject.pregnancy-extension-search=NO,NA&status=active,on-hold&subject.identifier=nullvillage-id|1L&subject.name=malaria";
        String urlList = "RelatedPerson?_count=10&_getpagesoffset=0&pregnancy-extension-search=YES&identifier=nullvillage-id|1L&active=true";

        //when
        when(restApiUtil.getBatchRequest("ServiceRequest?_count=9999&_sort=-_id&subject=Patient/patientId")).thenReturn(TestDataProvider.getRelatedPersonBundle());
        when(restApiUtil.getBatchRequest(url)).thenReturn(TestDataProvider.getRelatedPersonBundle());
        when(restApiUtil.getBatchRequest(urlList)).thenReturn(bundle);
        when(restApiUtil.getBatchRequest("RelatedPerson?_count=0&pregnancy-extension-search=YES&active=true&identifier=nullvillage-id|1L")).thenReturn(bundle);

        //then
        Map<String, Object> response = patientService.getPatientList(patientRequestDTO);
        assertFalse(response.isEmpty());
    }

    @Test
    void getPatientListAndSizeIsZero() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setReferencePatientId(TestConstants.PATIENT_ID);
        patientRequestDTO.setSkip(50);
        patientRequestDTO.setSearchText(TestConstants.ONE_STR);
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getPatient());
        bundle.addEntry().setResource(TestDataProvider.getPatient());

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirMapper.toPatient(any(), any())).thenReturn(TestDataProvider.getPatientDTO());
        //then
        Map<String, Object> response = patientService.getPatientList(patientRequestDTO);
        assertFalse(response.isEmpty());
    }

    @Test
    void getPatientList() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        PatientFilterDTO filterDTO = new PatientFilterDTO();
        patientRequestDTO.setSkip(100);
        filterDTO.setPatientStatus(List.of("Recovered"));
        filterDTO.setVisitDate(List.of(TestConstants.ONE_STR, TestConstants.TWO_STR));
        patientRequestDTO.setFilter(filterDTO);
        Bundle bundle = new Bundle();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        Map<String, Object> response = patientService.getPatientList(patientRequestDTO);
        assertFalse(response.isEmpty());
    }

    @Test
    void getPatientDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(TestConstants.TWO_STR);
        Bundle bundle = new Bundle();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setId(TestConstants.TWO_STR);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_ID, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, requestDTO.getPatientId()))).thenReturn(bundle);
        when(householdService.getHouseholdMemberByPatientId(requestDTO.getPatientId())).thenReturn(null);

        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> patientService.getPatientDetails(requestDTO));

        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        RequestDTO householdRequestDTO = new RequestDTO();
        householdRequestDTO.setId(householdMemberDTO.getHouseholdId());

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.ANC_VISIT_NUMBER);
        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_ID, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, requestDTO.getPatientId()))).thenReturn(bundle);
        when(householdService.getHouseholdMemberByPatientId(requestDTO.getPatientId())).thenReturn(householdMemberDTO);
        when(householdService.getHouseholdList(householdRequestDTO)).thenReturn(List.of(householdDTO));
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirUtils.getIdFromReference(observation.getPerformer().getFirst().getReference())).thenReturn(TestConstants.TWO_STR);

        //then
        PatientDetailsDTO response = patientService.getPatientDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

//    @Test
    void getPatientDetailsWithBundleTotalOne() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getPatient());
        bundle.setTotal(TestConstants.TEN);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(householdService.getHouseholdMemberByPatientId(requestDTO.getPatientId())).thenReturn(householdMemberDTO);

        //then
        PatientDetailsDTO response = patientService.getPatientDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

   @Test
    void getPatientDetailsByPatientReference() {
        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_PATIENT_REFERENCE, TestConstants.STRING_THREE).concat("&active=true"))).thenReturn(new Bundle());

        //then
        Bundle response = patientService.getPatientDetailsByPatientReference(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void searchPatient() {
        //given
        Bundle bundle = new Bundle();
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSearchText(TestConstants.TEXT);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        Map<String, Object> response = patientService.searchPatient(patientRequestDTO);
        Assertions.assertNotNull(response);

        //given
        patientRequestDTO.setSearchText(TestConstants.ONE_TWO_THREE_FOUR);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        response = patientService.searchPatient(patientRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientStatus() {
        //given
        Bundle bundle = new Bundle();
        bundle.setTotal(Constants.ONE);
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(TestConstants.TWO_STR);
        requestDTO.setCategory(TestConstants.ICCM);
        requestDTO.setTicketType(TestConstants.ASSESSMENT);
        requestDTO.setMemberId(TestConstants.PATIENT_ID);
        requestDTO.setGender(TestConstants.FEMALE);

        Patient patient = new Patient();
        patient.setId(TestConstants.TWO_STR);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(patient);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_ID,
                FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, TestConstants.TWO_STR))).thenReturn(bundle);
        when(fhirAssessmentMapper.extractStatus("", Constants.SKIP_STATUS)).thenReturn(Constants.REFERRED);
        when(restApiUtil.getBatchRequest("Observation?identifier=nullobservation-type|PatientVitals&performer=RelatedPerson/patientId&status=preliminary&_count=9999"))
                .thenReturn(bundle);

        //then
        Map<String, Object> response = patientService.getPatientStatus(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientPregnancyDetails() {
        //given
        Bundle bundle = new Bundle();
        Observation observation = new Observation();
        CodeableConcept codeableConcept = new CodeableConcept();
        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);
        reference.setReference(TestConstants.TEXT);
        codeableConcept.setText(Constants.ANC_VISIT_NUMBER);
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));
        observation.setValue(new IntegerType(TestConstants.TEN));
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirUtils.getIdFromReference(anyString())).thenReturn(TestConstants.STRING_THREE);

        //then
        PregnancyDetailsDTO response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.WEIGHT);
        observation.setValue(new Quantity(TestConstants.TEN));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.LAST_MENSTRUAL_PERIOD);
        observation.setValue(new DateTimeType(new Date()));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.ESTIMATED_DELIVERY_DATE);
        observation.setValue(new DateTimeType(new Date()));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.PNC_VISIT_NUMBER);
        observation.setValue(new IntegerType(TestConstants.TEN));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.DATE_OF_DELIVERY);
        observation.setValue(new DateTimeType(new Date()));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.NO_OF_NEONATES);
        observation.setValue(new IntegerType(TestConstants.TEN));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.CHILDHOOD_VISIT_NUMBER);
        observation.setValue(new IntegerType(TestConstants.TEN));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.PREGNANCY_ANC_MEDICAL_REVIEW);
        observation.setValue(new IntegerType(TestConstants.TEN));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.PNC_MOTHER_MEDICAL_REVIEW);
        observation.setValue(new IntegerType(TestConstants.TEN));
        //then
        response = patientService.getPatientPregnancyDetails(TestConstants.TWO_STR, TestConstants.STRING_THREE, TestConstants.PATIENT_ID);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateStatusOfServiceRequest() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);
        requestDTO.setCategory(TestConstants.ASSESSMENT);
        requestDTO.setTicketType(TestConstants.ICCM);
        requestDTO.setMemberId(TestConstants.TWO_STR);
        requestDTO.setPatientStatus(TestConstants.RECOVERED);
        requestDTO.setProvenance(provenanceDTO);
        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(List.of(identifier));
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(serviceRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(serviceRequest.getId())).thenReturn(TestConstants.TWO_STR);
        doNothing().when(fhirUtils).setBundle(TestConstants.URL, TestConstants.URL, Bundle.HTTPVerb.PUT, serviceRequest, bundle, requestDTO.getProvenance());

        //then
        ResponseEntity<FhirResponseDTO> response = patientService.updateStatusOfServiceRequest(requestDTO);
        Assertions.assertNull(response);
    }

    @Test
    void createReferralTicket() {
        //given
        ReflectionTestUtils.setField(patientService, "fhirServerUrl" , "fhirServerUrl");
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();
        referralDetailsDTO.setPatientStatus(Constants.REFERRED);
        referralDetailsDTO.setPatientId(TestConstants.TWO_STR);
        referralDetailsDTO.setProvenance(provenanceDTO);

        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setPatientReference(TestConstants.TWO_STR);
        householdMemberDTO.setPatientId(TestConstants.TWO_STR);
        Patient patient = TestDataProvider.getPatient();
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(patient);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(List.of(identifier));

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(householdService.getHouseholdMemberByPatientId(TestConstants.TWO_STR)).thenReturn(householdMemberDTO);
        when(fhirUtils.getIdFromHistoryUrl(TestConstants.TWO_STR)).thenReturn(TestConstants.TWO_STR);
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.TWO_STR);
        when(fhirAssessmentMapper.createReferralTicket(referralDetailsDTO)).thenReturn(serviceRequest);
        doNothing().when(fhirUtils).setBundle("", "", Bundle.HTTPVerb.POST, serviceRequest, bundle, referralDetailsDTO.getProvenance());
        when(restApiUtil.constructRequestEntity(bundle)).thenReturn(any());
        when(restApiUtil.postBatchRequest("fhirServerUrl", any())).thenReturn(any());

        //then
        ReferralDetailsDTO response = patientService.createReferralTicket(referralDetailsDTO, bundle, true, false, Boolean.TRUE);
        Assertions.assertNotNull(response);
    }

    @Test
    void createReferralTicketForMedicalReview() {
        //given
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();
        referralDetailsDTO.setPatientStatus(Constants.REFERRED);
        referralDetailsDTO.setPatientId(TestConstants.TWO_STR);
        referralDetailsDTO.setMemberId(TestConstants.TWO_STR);
        referralDetailsDTO.setEncounterId(TestConstants.TWO_STR);
        referralDetailsDTO.setCategory(TestConstants.ASSESSMENT);
        referralDetailsDTO.setProvenance(provenanceDTO);

        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(List.of(identifier));
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(serviceRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setPatientReference(TestConstants.TWO_STR);
        householdMemberDTO.setPatientId(TestConstants.TWO_STR);

        Bundle patientBundle = new Bundle();
        List<Bundle.BundleEntryComponent> patientEntry = new ArrayList<>();
        Bundle.BundleEntryComponent patientBundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(patientBundle);
        entry.add(patientBundleEntryComponent);
        bundle.setEntry(patientEntry);

        //when
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.TWO_STR);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirUtils.getIdFromResourceUrl(referralDetailsDTO.getEncounterId())).thenReturn("Encounter/2");
        when(fhirAssessmentMapper.updateEncounterStatusDetails("Encounter/2", referralDetailsDTO.getProvenance(),  Constants.REFERRED, bundle)).thenReturn(new Encounter());

        when(restApiUtil.getBatchRequest(anyString())).thenReturn(patientBundle);
        when(householdService.getHouseholdMemberByPatientId(TestConstants.TWO_STR)).thenReturn(householdMemberDTO);
        when(fhirUtils.getIdFromHistoryUrl(TestConstants.TWO_STR)).thenReturn(TestConstants.TWO_STR);
        when(householdService.getHouseholdMemberById(referralDetailsDTO.getMemberId())).thenReturn(householdMemberDTO);
        when(fhirAssessmentMapper.createReferralTicket(referralDetailsDTO)).thenReturn(serviceRequest);

        //then
        ReferralDetailsDTO response = patientService.createReferralTicketForMedicalReview(referralDetailsDTO, bundle, true, true);
        Assertions.assertNotNull(response);
    }

    @Test
    void closePncNeonateDetails() {
        //given
        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(new ArrayList<>(Arrays.asList(identifier)));
        serviceRequest.setPatientInstruction(TestConstants.STRING_VALUE);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(serviceRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setCategory(StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.RMNCH_VISIT));
        requestDTO.setClosedReason(Constants.MISCARRIAGE);

        //when
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.PATIENT_REFERENCE);
        when(fhirUtils.getIdFromHistoryUrl(serviceRequest.getId())).thenReturn(TestConstants.TWO_STR);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        patientService.closePncNeonateDetails(bundle, requestDTO);
        verify(restApiUtil).getBatchRequest(anyString());
    }

    @Test
    void closeAncDetails() {
        //given
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(new ArrayList<>(Arrays.asList(identifier)));
        serviceRequest.setPatientInstruction(TestConstants.STRING_VALUE);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(serviceRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setClosedReason(Constants.MISCARRIAGE);

        //when
        doNothing().when(fhirAssessmentMapper).updateVitalObservationsStatus(List.of(Constants.LAST_MENSTRUAL_PERIOD, Constants.ANC_VISIT_NUMBER, Constants.ESTIMATED_DELIVERY_DATE,
                Constants.PREGNANCY_ANC_MEDICAL_REVIEW), bundle, TestConstants.TWO_STR, provenanceDTO);

        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.PATIENT_REFERENCE);
        when(fhirUtils.getIdFromHistoryUrl(serviceRequest.getId())).thenReturn(TestConstants.TWO_STR);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        patientService.closeAncDetails(bundle, requestDTO);
        verify(fhirUtils).getIdFromHistoryUrl(serviceRequest.getId());
    }

    @Test
    void closePncDetails() {
        //given
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(new ArrayList<>(Arrays.asList(identifier)));
        serviceRequest.setPatientInstruction(TestConstants.STRING_VALUE);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(serviceRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setClosedReason(Constants.MISCARRIAGE);

        //closeChildhoodDetails
        //when
        doNothing().when(fhirAssessmentMapper).updateVitalObservationsStatus(List.of(Constants.CHILDHOOD_VISIT_NUMBER), bundle, TestConstants.TWO_STR, provenanceDTO);

        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.PATIENT_REFERENCE);
        when(fhirUtils.getIdFromHistoryUrl(serviceRequest.getId())).thenReturn(TestConstants.TWO_STR);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        patientService.closeChildhoodDetails(bundle, requestDTO);
        verify(fhirUtils).getIdFromHistoryUrl(serviceRequest.getId());
    }

    @Test
    void closePncDetailsWithData() {
        //given
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.RECOVERED);
        serviceRequest.setId(TestConstants.TWO_STR);
        serviceRequest.setIdentifier(new ArrayList<>(Arrays.asList(identifier)));
        serviceRequest.setPatientInstruction(TestConstants.STRING_VALUE);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(serviceRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setClosedReason(Constants.MISCARRIAGE);

        //closePncDetails
        //when
        doNothing().when(fhirAssessmentMapper).updateVitalObservationsStatus(List.of(Constants.PNC_VISIT_NUMBER, Constants.DATE_OF_DELIVERY, Constants.PNC_MOTHER_MEDICAL_REVIEW,
                Constants.NO_OF_NEONATES), bundle, TestConstants.TWO_STR, provenanceDTO);

        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.PATIENT_REFERENCE);
        when(fhirUtils.getIdFromHistoryUrl(serviceRequest.getId())).thenReturn(TestConstants.TWO_STR);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        patientService.closePncDetails(bundle, requestDTO);
        verify(restApiUtil).getBatchRequest(anyString());
    }

    @Test
    void getPatientById() {
        //given
        Patient patient = TestDataProvider.getPatient();
        patient.setId(TestConstants.TWO_STR);
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(patient);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        Patient response = patientService.getPatientById(TestConstants.TWO_STR);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPatientByPatientId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(null);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientService.createPatientByPatientId(requestDTO));

        //given
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);
        requestDTO.setPatientId(TestConstants.TWO_STR);
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setPatientReference(null);
        requestDTO.setProvenance(provenanceDTO);
        Patient patient = TestDataProvider.getPatient();

        //when
        when(householdService.getHouseholdMemberByPatientId(TestConstants.TWO_STR)).thenReturn(householdMemberDTO);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.UNIQUE_ID);
        when(fhirAssessmentMapper.setPatient(new Patient(), householdMemberDTO, provenanceDTO.getUserId())).thenReturn(patient);
        when(fhirUtils.getIdFromResourceUrl(TestConstants.TWO_STR)).thenReturn(TestConstants.TWO_STR);

        //then
        String response = patientService.createPatientByPatientId(requestDTO);
        Assertions.assertNull(response);
    }

    @Test
    void updatePregnancyStatusWithEmptyBundle() {
        //given
        Patient patient = TestDataProvider.getPatient();
        patient.setId(TestConstants.TWO_STR);
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(patient);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setId(TestConstants.TWO_STR);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirAssessmentMapper.setPregnancyStatus(Boolean.TRUE, patient)).thenReturn(patient);
        when(fhirAssessmentMapper.setPregnancyStatus(Boolean.TRUE, relatedPerson)).thenReturn(relatedPerson);
        when(householdService.getRelatedPersonByPatientId(TestConstants.TWO_STR)).thenReturn(relatedPerson);
        doNothing().when(fhirUtils).setBundleUsingId(relatedPerson.getId(), TestConstants.FHIR_SERVER_URL, Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);

        //then
        patientService.updatePatientStatus(new Bundle(), Boolean.TRUE, provenanceDTO, TestConstants.TWO_STR, Boolean.TRUE);
        verify(householdService).getRelatedPersonByPatientId(TestConstants.TWO_STR);
    }

    @Test
    void updatePregnancyStatus() {
        //given
        Patient patient = TestDataProvider.getPatient();
        patient.setId(TestConstants.TWO_STR);
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(patient);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setId(TestConstants.TWO_STR);

        //when
        when(fhirAssessmentMapper.setPregnancyStatus(Boolean.TRUE, patient)).thenReturn(patient);
        when(householdService.getRelatedPersonByPatientId(TestConstants.TWO_STR)).thenReturn(relatedPerson);
        when(fhirAssessmentMapper.setPregnancyStatus(Boolean.TRUE, relatedPerson)).thenReturn(relatedPerson);
        doNothing().when(fhirUtils).setBundleUsingId(relatedPerson.getId(), TestConstants.FHIR_SERVER_URL, Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);

        //then
        patientService.updatePatientStatus(bundle, Boolean.TRUE, provenanceDTO, TestConstants.TWO_STR, Boolean.TRUE);
        verify(householdService).getRelatedPersonByPatientId(TestConstants.TWO_STR);
    }

    @Test
    void createDiagnosis() {
        //given
        DiagnosisDTO diagnosisDTO = TestDataProvider.getDiagnosisDTO();

        //then
        Assertions.assertThrows(SpiceValidation.class, () -> patientService.createDiagnosis(diagnosisDTO));

        //given
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(TestConstants.TWO_STR);
        provenanceDTO.setOrganizationId(TestConstants.TWO_STR);

        diagnosisDTO.setPatientId(TestConstants.TWO_STR);
        diagnosisDTO.setOtherNotes(TestConstants.TEXT);
        diagnosisDTO.setProvenance(provenanceDTO);
        diagnosisDTO.setType(TestConstants.ASSESSMENT);
        diagnosisDTO.setDiseases(new ArrayList<>());
        Condition condition = new Condition();
        Bundle bundle = new Bundle();

        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(TestConstants.TEXT);

        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Organization),
                TestDataProvider.getHealthFacilityRequestDTO());
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);

        //when
        when(householdService.getHouseholdMemberByPatientId(TestConstants.TWO_STR)).thenReturn(householdMemberDTO);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());
        when(fhirUtils.getIdFromResourceUrl(TestConstants.TWO_STR)).thenReturn(TestConstants.TWO_STR);
        when(fhirUtils.createCodeableConcept("otherNotes")).thenReturn(codeableConcept);

        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.UNIQUE_ID);
        doNothing().when(fhirUtils).setBundle("", "", Bundle.HTTPVerb.POST, condition, bundle, diagnosisDTO.getProvenance());
        when(fhirUtils.getFhirBaseUrl()).thenReturn(TestConstants.URL);
        when(restApiUtil.constructRequestEntity(bundle)).thenReturn(any());
        when(restApiUtil.postBatchRequest(TestConstants.URL, any())).thenReturn(responseEntity);
        //then
        patientService.createDiagnosis(diagnosisDTO);
    }

    @Test
    void listPatientsWithValidRequest() {
        // given
        TestDataProvider.init();
        TestDataProvider.dateUtilInit();
        PatientRequestDTO requestDTO = new PatientRequestDTO();
        PatientFilterDTO patientFilterDTO = new PatientFilterDTO();
        patientFilterDTO.setEnrollmentStatus(TestConstants.ACTIVE_UPPER);
        patientFilterDTO.setIsRedRiskPatient(Boolean.TRUE);
        patientFilterDTO.setIsRedRiskPatient(Boolean.TRUE);
        patientFilterDTO.setCvdRiskLevel(TestConstants.RISK_LEVEL);
        patientFilterDTO.setAssessmentDate(TestConstants.TEST_DATE);
        patientFilterDTO.setMedicalReviewDate("today");
        patientFilterDTO.setAssessmentDate("today");
        patientFilterDTO.setLabTestReferredOn("today");
        patientFilterDTO.setPrescriptionReferredOn("today");
        patientFilterDTO.setEnrollmentStatus("ENROLLED");
        requestDTO.setFilter(patientFilterDTO);
        requestDTO.setSiteId("siteId");
        requestDTO.setLimit(10);
        requestDTO.setType("nutritionLifestyle");
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getPatient());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        // when
        TestDataProvider.getStaticMock();
        TestDataProvider.getDateUtilStaticMock();
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(patientConverter.convertToPatientDetails(any(Patient.class), any(PatientDetailsDTO.class))).thenReturn(new PatientDetailsDTO());

        // then
        Map<String, Object> response = patientService.listPatients(requestDTO);
        Assertions.assertNotNull(response);
        assertTrue(response.containsKey(Constants.PATIENT_LIST));
        assertTrue(response.containsKey(Constants.TOTAL_COUNT));
        TestDataProvider.dateUtilCleanUp();
        TestDataProvider.cleanUp();
    }

    @Test
    void listPatientsWithValidRequestWithTypeInvestigation() {
        // given
        TestDataProvider.init();
        TestDataProvider.dateUtilInit();
        PatientRequestDTO requestDTO = new PatientRequestDTO();
        PatientFilterDTO patientFilterDTO = new PatientFilterDTO();
        patientFilterDTO.setMedicalReviewDate("tomorrow");
        patientFilterDTO.setAssessmentDate("tomorrow");
        patientFilterDTO.setLabTestReferredOn("tomorrow");
        patientFilterDTO.setPrescriptionReferredOn("tomorrow");
        patientFilterDTO.setEnrollmentStatus("NOT_ENROLLED");
        requestDTO.setFilter(patientFilterDTO);
        requestDTO.setSiteId("siteId");
        requestDTO.setLimit(10);
        requestDTO.setType("investigation");
        requestDTO.setFilter(patientFilterDTO);
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getPatient());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        // when
        TestDataProvider.getStaticMock();
        TestDataProvider.getDateUtilStaticMock();
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(patientConverter.convertToPatientDetails(any(Patient.class), any(PatientDetailsDTO.class))).thenReturn(new PatientDetailsDTO());

        // then
        Map<String, Object> response = patientService.listPatients(requestDTO);
        Assertions.assertNotNull(response);
        assertTrue(response.containsKey(Constants.PATIENT_LIST));
        assertTrue(response.containsKey(Constants.TOTAL_COUNT));
        TestDataProvider.dateUtilCleanUp();
        TestDataProvider.cleanUp();
    }

    @Test
    void listPatientsWithValidRequestWithTypeDispense() {
        // given
        TestDataProvider.init();
        TestDataProvider.dateUtilInit();
        PatientRequestDTO requestDTO = new PatientRequestDTO();
        PatientFilterDTO patientFilterDTO = new PatientFilterDTO();
        patientFilterDTO.setPrescriptionReferredOn(TestConstants.REFERRED);
        requestDTO.setFilter(patientFilterDTO);
        requestDTO.setSiteId("siteId");
        requestDTO.setLimit(10);
        requestDTO.setType("dispense");
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getPatient());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        // when
        TestDataProvider.getStaticMock();
        TestDataProvider.getDateUtilStaticMock();
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(patientConverter.convertToPatientDetails(any(Patient.class), any(PatientDetailsDTO.class))).thenReturn(new PatientDetailsDTO());

        // then
        Map<String, Object> response = patientService.listPatients(requestDTO);
        Assertions.assertNotNull(response);
        assertTrue(response.containsKey(Constants.PATIENT_LIST));
        assertTrue(response.containsKey(Constants.TOTAL_COUNT));
        TestDataProvider.dateUtilCleanUp();
        TestDataProvider.cleanUp();
    }

    @Test
    void updatePatient(){
        // given
        EnrollmentRequestDTO enrollmentRequestDTO = TestDataProvider.getEnrollmentRequestDTO();
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(TestDataProvider.getPatient());
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        // when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        // then
        EnrollmentResponseDTO enrollmentResponseDto = patientService.updatePatient(enrollmentRequestDTO);
        Assertions.assertNotNull(enrollmentResponseDto);
    }


    @Test
    void testUpdatePregnancyANCRisk_WhenIsPregnancyRiskIsNull() {
        when(fhirUtils.initiateCodesMap()).thenReturn(null);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(httpEntity);
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setIsPregnancyRisk(null);
        Boolean result = patientService.updatePregnancyANCRisk(pregnancyDetailsDTO);
        assertFalse(result, "Expected result to be false when IsPregnancyRisk is null");
        verify(restApiUtil, never()).getBatchRequest(anyString());
    }

    @Test
    void testUpdatePregnancyANCRisk_WhenPregnancyObservationNotFound() {
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId("106");
        provenanceDTO.setOrganizationId(String.valueOf(30));
        provenanceDTO.setModifiedDate(new Date());

        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setIsPregnancyRisk(true);
        pregnancyDetailsDTO.setMemberReference("123");
        pregnancyDetailsDTO.setIsPregnant(true);
        pregnancyDetailsDTO.setPatientReference("23005");
        pregnancyDetailsDTO.setProvenance(provenanceDTO);
        when(fhirUtils.initiateCodesMap()).thenReturn(null);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(httpEntity);

        Bundle pregnancyBundle = new Bundle();
        pregnancyBundle.setEntry(new ArrayList<>());
        Bundle relatedPersonBundle = getBundle(new RelatedPerson());
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(relatedPersonBundle);
        when(restApiUtil.getBatchRequest("Observation?code:text=pregnancy&status=preliminary&performer=RelatedPerson/123&_sort=-date&_count=1")).thenReturn(pregnancyBundle);

        // When
        Boolean result = patientService.updatePregnancyANCRisk(pregnancyDetailsDTO);

        // Then
        assertFalse(result, "Expected result to be false when no pregnancy observation is found");
        verify(restApiUtil, never()).postBatchRequest(anyString(), any());  // No batch request sent
    }

    @Test
    void updatePregnancyANCRiskWhnPregnancyDetailIsValid() {
        when(fhirUtils.initiateCodesMap()).thenReturn(null);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(httpEntity);
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId("106");
        provenanceDTO.setOrganizationId(String.valueOf(30));
        provenanceDTO.setModifiedDate(new Date());

        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setIsPregnancyRisk(true);
        pregnancyDetailsDTO.setMemberReference("123");
        pregnancyDetailsDTO.setIsPregnant(true);
        pregnancyDetailsDTO.setPatientReference("23005");
        pregnancyDetailsDTO.setProvenance(provenanceDTO);

        Bundle relatedPersonBundle = getBundle(new RelatedPerson());
        Bundle pregnancyBundle = getBundle(new Observation());

        when(restApiUtil.getBatchRequest(anyString())).thenReturn(relatedPersonBundle);

        when(restApiUtil.getPatientById(anyString())).thenReturn(new Patient());
        when(restApiUtil.getBatchRequest("Observation?code:text=pregnancy&status=preliminary&performer=RelatedPerson/123&_sort=-date&_count=1")).thenReturn(pregnancyBundle);
        when(pregnancyConverter.updatePregnancyObservation(any(), any())).thenReturn(new Observation());
        Boolean status = patientService.updatePregnancyANCRisk(pregnancyDetailsDTO);
        assertNotNull(status);
        assertTrue(status);
    }

    private Bundle getBundle(Resource resource) {
        Bundle bundle = new Bundle();
        resource.setId(TestConstants.TWO_STR);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();

        bundleEntryComponent.setResource(resource);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        return bundle;
    }

    @Test
    void deletePatientByPatientIdThrowAnExceptionWhenRequsetObjectIsNull() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientId(null);
        assertThrows(DataNotAcceptableException.class, () -> {
            patientService.deletePatientByPatientId(requestDTO);
        });
    }

    @Test
    void deletePatientByPatientId() {
        Patient patient = TestDataProvider.getPatient();
                new Patient();
        patient.setId("patient123");
        patient.addIdentifier().setSystem("http://example.com").setValue("patient123");

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setId("relatedPerson123");
        relatedPerson.addIdentifier().setSystem("http://example.com").setValue("relatedPerson123");


        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(TestConstants.TWO_STR);
        requestDTO.setReason(Constants.PATIENT_DEAD);
        requestDTO.setProvenance(TestDataProvider.getProvenance());

        Bundle relatedPersonBundle = getBundle(new RelatedPerson());
        Bundle patientBundle = new Bundle();
        patientBundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(TestDataProvider.getPatient()));

        when(restApiUtil.getBatchRequest("Patient?_id=2")).thenReturn(patientBundle);
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=1234")).thenReturn(relatedPersonBundle);

        patientService.deletePatientByPatientId(requestDTO);
        assertFalse(relatedPerson.getActive());
    }

    @Test
    void listNcdPatients(){
        PatientRequestDTO requestDTO = TestDataProvider.getPatientRequestDTO();
        requestDTO.setId(TestConstants.TWO_STR);
        Bundle bundle = new Bundle();
        bundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(TestDataProvider.getPatient()));

        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        Map<String, PatientDetailsDTO> response = patientService.listNcdPatients(requestDTO);
        assertNotNull(response);
        assertEquals(1, response.size());
    }

    @Test
    void searchPatientDetails() {
        when(fhirUtils.initiateCodesMap()).thenReturn(null);
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(TestConstants.TWO_STR);
        requestDTO.setReason(Constants.PATIENT_DEAD);
        requestDTO.setProvenance(TestDataProvider.getProvenance());
        requestDTO.setType("");

        Bundle bundle = getBundle(new Patient());
        bundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(TestDataProvider.getRelatedPerson()));
        Bundle vitalResponsBundle = getBundle(new Observation());

        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();
        patientDetailsDTO.setId(TestConstants.TWO_STR);
        patientDetailsDTO.setPhq4score("1");
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();
        searchPersonDetailsDTO.setPatient((Patient) bundle.getEntry().getFirst().getResource());
        searchPersonDetailsDTO.setRelatedPerson((RelatedPerson) bundle.getEntry().getLast().getResource());

        when(restApiUtil.getBatchRequest("RelatedPerson?active=true&_id=2&_sort=-_lastUpdated&_count=1&_revinclude=Patient:link")).thenReturn(bundle);
        when((restApiUtil.getBatchRequest("Observation?identifier=nullobservation-type|vitalsigns&performer=RelatedPerson/2&_sort=-_lastUpdated&_count=1&_include=Observation:derived-from&_include=Observation:subject"))).thenReturn(vitalResponsBundle);
        when(searchPersonDetailsConverter.constructSearchPersonDetails(searchPersonDetailsDTO , requestDTO.getType())).thenReturn(patientDetailsDTO);
        when(restApiUtil.getBatchRequest("Condition?subject=Patient/2&verification-status:code=confirmed&_count=9999")).thenReturn(vitalResponsBundle);
        when(restApiUtil.getBatchRequest("Condition?asserter=RelatedPerson/abcd-efgh-ijkl-mnop&verification-status:text=provisional&_count=9999")).thenReturn(bundle);

        PatientDetailsDTO response = patientService.searchPatientDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void testGetPatientDiagnosisDetails_noPatientReference() {
        RequestDTO request = new RequestDTO();
        request.setPatientReference(null);
        ConfirmDiagnosisDTO result = patientService.getPatientDiagnosisDetails(request, false);
        assertNotNull(result);
    }

    @Test
    void testGetPatientDiagnosisDetails_isFromDetailsTrue() {
        RequestDTO request = new RequestDTO();
        request.setPatientReference(TestConstants.PATIENT_ID);
        request.setDiagnosisType(Arrays.asList(TestConstants.DIAGNOSIS_TYPE, TestConstants.DIAGNOSIS_TYPE));

        String patientReference = TestConstants.PATIENT_ID;
        String url = String.format(Constants.CONDITION_QUERY_VERIFICATION_STATUS, patientReference);

        Bundle mockBundle = mock(Bundle.class);
        when(restApiUtil.getBatchRequest(url)).thenReturn(mockBundle);
        when(mockBundle.getEntry()).thenReturn(Collections.emptyList()); // Ensure the entry list is empty

        ConfirmDiagnosisDTO result = patientService.getPatientDiagnosisDetails(request, true);

        assertNotNull(result);
        verify(restApiUtil).getBatchRequest(url);
        verify(fhirAssessmentMapper, never()).mapToDiagnosis(any()); // Ensure mapToDiagnosis is not called when bundle entry is empty
    }

    @Test
    void createPatientByMemberReference() {
        //given
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        String memberReference = TestConstants.STRING_VALUE;
        Patient patient = TestDataProvider.getPatient();
        patient.setId(TestConstants.UNIQUE_ID);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = TestDataProvider.getBundle(relatedPerson);
        bundle.getEntry().getFirst().setResource(relatedPerson);
        relatedPerson.setPatient(TestDataProvider.getRelatedPerson().getPatient());

        //when
        fhirUtils.setBundle(TestConstants.URL,TestConstants.URL, Bundle.HTTPVerb.POST, relatedPerson, bundle, provenanceDTO);
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.UNIQUE_ID);
        when(patientConverter.createPatientFromRelatedPerson(relatedPerson)).thenReturn(patient);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(ResourceType.RelatedPerson.name(),
                Constants.QUESTION_MARK, RelatedPerson.SP_RES_ID, Constants.EQUAL_SYMBOL,
                memberReference))).thenReturn(bundle);
        Patient result = patientService.createPatientByMemberReference(memberReference, provenanceDTO, bundle);
        Assertions.assertNotNull(result);
    }

    @Test
    void updatePregnancyANCRisk() {
        PregnancyDetailsDTO pregnancyDetailsDTO = TestDataProvider.getPregnancyDetailsDTO();
        pregnancyDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        pregnancyDetailsDTO.setIsPregnancyRisk(true);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle =  new Bundle();
        Bundle relatedPersonBundle = TestDataProvider.getBundle(relatedPerson);
        relatedPersonBundle.addEntry().setResource(relatedPerson);
        bundle.addEntry().setResource(relatedPerson);
        pregnancyDetailsDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Patient patient = TestDataProvider.getPatient();
        Bundle pregnancyBundle = TestDataProvider.getBundle(patient);
        Observation pregnancyObservation = TestDataProvider.getPregnancyAncObservation();
        pregnancyBundle.getEntry().getFirst().setResource(pregnancyObservation);
        ProvenanceDTO pregnancyProvenanceDTO = TestDataProvider.getProvenance();
        pregnancyProvenanceDTO.setOrganizationId(TestConstants.UNIQUE_ID);
        pregnancyObservation.addPerformer(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                pregnancyProvenanceDTO.getOrganizationId())));

        //when
        when(restApiUtil.getPatientById(StringUtil.concatString(any(), FhirConstants.PATIENT,
                Constants.FORWARD_SLASH, pregnancyDetailsDTO.getPatientReference()))).thenReturn(patient);
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null")).thenReturn(relatedPersonBundle);
        when(restApiUtil.getBatchRequest("Observation?code:text=pregnancy&status=preliminary&performer=RelatedPerson/null&_sort=-date&_count=1")).thenReturn(pregnancyBundle);
        when(pregnancyConverter.updatePregnancyObservation(pregnancyObservation, pregnancyDetailsDTO)).thenReturn(pregnancyObservation);
        Boolean result = patientService.updatePregnancyANCRisk(pregnancyDetailsDTO);
        Assertions.assertTrue(result);
    }

    @Test
    void updateReferredSite() {
        ScreeningLogRequestDTO requestDTO = TestDataProvider.getScreeningLogRequest();
        String memberFhirId = requestDTO.getMemberReference();
        String patientFhirId = TestConstants.PATIENT_ID;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        provenanceDTO.setUserId(TestConstants.UNIQUE_ID);
        provenanceDTO.setOrganizationId(TestConstants.UNIQUE_ID);
        provenanceDTO.setModifiedDate(new Date());
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle relatedPersonbundle = TestDataProvider.getRelatedPersonBundle();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier identifier = new Identifier();
        relatedPersonbundle.setIdentifier(identifier);
        identifier.setSystem("ORGANIZATION_ID_SYSTEM_URL");
        identifiers.add(identifier);
        relatedPerson.setIdentifier(identifiers);
        relatedPersonbundle.addEntry()
                .setResource(relatedPersonbundle);
        relatedPersonbundle.setEntry(TestDataProvider.getRelatedPersonBundle().getEntry());
        relatedPersonbundle.getEntry().getFirst().setResource(relatedPerson);
        Patient patient = TestDataProvider.getPatient();
        patient.setIdentifier(identifiers);
        patient.setId(patientFhirId);
        relatedPerson.setPatient(TestDataProvider.getRelatedPerson().getPatient());
        Bundle patientBundle = TestDataProvider.getBundle(patient);
        patientBundle.addEntry().setResource(patient);
        List<Bundle.BundleEntryComponent> patientBundleEntries = patientBundle.getEntry();
        patientBundle.setEntry(patientBundleEntries);
        patientBundle.setIdentifier(identifier);

        //when
        fhirUtils.setBundle(StringUtil.concatString(FhirConstants.RELATED_PERSON,
                        Constants.FORWARD_SLASH, memberFhirId),
                StringUtil.concatString(Constants.FHIR_BASE_URL, memberFhirId),
                Bundle.HTTPVerb.PUT, relatedPerson, relatedPersonbundle, provenanceDTO);
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_MEMBER_ID, memberFhirId))).thenReturn(relatedPersonbundle);
        when(restApiUtil.getBatchRequest("Patient?_id=urn:uuid:null&active=true")).thenReturn(patientBundle);
        patientService.updateReferredSite(requestDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void getPregnancyDetails() {
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        PregnancyDetailsDTO pregnancyDetailsDTO = TestDataProvider.getPregnancyDetailsDTO();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Patient patient = TestDataProvider.getPatient();
        pregnancyDetailsDTO.setMemberReference(requestDTO.getId());
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getRelatedPerson());
        Observation observation = TestDataProvider.getHeightObservation();
        observation.setCode(new CodeableConcept().setText("pregnancy"));
        bundle.addEntry().setResource(observation);
        bundle.addEntry().setResource(TestDataProvider.getPatient());
        Bundle conditionBundle = new Bundle();
        conditionBundle.addEntry().setResource(TestDataProvider.getCondition());
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();
        searchPersonDetailsDTO.setRelatedPerson(relatedPerson);
        searchPersonDetailsDTO.setPatient(patient);
        searchPersonDetailsDTO.setPregnancyObservation(TestDataProvider.getPregnancyAncObservation());
        searchPersonDetailsDTO.setWeightObservation(TestDataProvider.getWeightObservation());
        String url = "Observation?identifier=nullobservation-type|vitalsigns&performer=RelatedPerson/2&_sort=-_lastUpdated&_count=1&_include=Observation:derived-from&_include=Observation:subject";

        //when
        when(restApiUtil.getBatchRequest("Condition?identifier=Pregnancy&asserter=RelatedPerson/2")).thenReturn(conditionBundle);
        when(restApiUtil.getBatchRequest(url)).thenReturn(bundle);
        PregnancyDetailsDTO result = patientService.getPregnancyDetails(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void updatePatientOverallStatus() {
        Bundle bundle = new Bundle();
        String patientStatus = TestConstants.PATIENT_ID;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        String memberId = TestConstants.UNIQUE_ID;
        Patient patient = TestDataProvider.getPatient();
        bundle.addEntry().setResource(patient);
        patient.setId(TestConstants.PATIENT_ID);

        patientService.updatePatientOverallStatus(bundle,patientStatus,provenanceDTO,memberId);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void updatePatientOverallStatus_NUll_Patient() {
        Bundle bundle = new Bundle();
        String patientStatus = TestConstants.PATIENT_ID;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        String memberId = TestConstants.UNIQUE_ID;
        Patient patient = null;
        bundle.addEntry().setResource(patient);

        patientService.updatePatientOverallStatus(bundle,patientStatus,provenanceDTO,memberId);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void updateCoverage() {
        //given
        Patient patient = TestDataProvider.getPatient();
        EnrollmentRequestDTO enrollmentRequestDTO = TestDataProvider.getEnrollmentRequestDTO();
        Bundle transactionBundle = new Bundle();
        Coverage coverage = new Coverage();
        transactionBundle.addEntry().setResource(coverage);

        //when
        when(restApiUtil.getBatchRequest("Coverage?patient=2")).thenReturn(transactionBundle);

        //then
        patientService.updateCoverage(patient, enrollmentRequestDTO, transactionBundle);
        verify(restApiUtil, atLeastOnce()).getBatchRequest("Coverage?patient=2");
    }

    @Test
    void updateCoverageWithNewBundle() {
        //given
        Patient patient = TestDataProvider.getPatient();
        EnrollmentRequestDTO enrollmentRequestDTO = TestDataProvider.getEnrollmentRequestDTO();
        Bundle transactionBundle = new Bundle();

        //when
        when(restApiUtil.getBatchRequest("Coverage?patient=2")).thenReturn(transactionBundle);

        //then
        patientService.updateCoverage(patient, enrollmentRequestDTO, transactionBundle);
        verify(restApiUtil, atLeastOnce()).getBatchRequest("Coverage?patient=2");
    }

    @Test
    void getReferralTicketes() {
        //given
        String patientId = TestConstants.TWO_STR;
        String serviceRequestId = TestConstants.TWO_STR;
        String type = TestConstants.DIAGNOSIS_TYPE;
        String assessmentType = TestConstants.ASSESSMENT;
        String url = "ServiceRequest?_count=9999&_sort=-_id&subject=Patient/2&_include=ServiceRequest:requester&_include=" +
                "ServiceRequest:performer&_include=ServiceRequest:encounter&_include=ServiceRequest:subject&_revinclude=" +
                "Provenance:target&identifier:not=nullpatient-status|Recovered&_id=ServiceRequest/2&requisition=nullticket-type|malaria";
        Bundle bundle = new Bundle();
        Provenance provenance = new Provenance();
        List<Reference> theReference = new ArrayList<>();
        Reference reference = new Reference();
        reference.setReferenceElement(new StringType(TestConstants.ONE_STR));
        theReference.add(reference);
        provenance.setTarget(theReference);
        provenance.setAgent(List.of(new Provenance.ProvenanceAgentComponent().setRole(List.of(new CodeableConcept().setText("POST")))));
        ServiceRequest serviceRequest = new ServiceRequest();
        serviceRequest.setId(TestConstants.ONE_STR);
        serviceRequest.setIdentifier(List.of(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue("Referred")));
        bundle.addEntry().setResource(provenance);
        bundle.addEntry().setResource(serviceRequest);

        //when
        when(restApiUtil.getBatchRequest(url)).thenReturn(bundle);
        when(fhirAssessmentMapper.setReferralTicket(any(), any(), any(), any())).thenReturn(TestDataProvider.getReferralTicketDTO());

        //then
        List<ReferralTicketDTO> result = patientService.getReferralTicketes(patientId, serviceRequestId, type, assessmentType);
        Assertions.assertNotNull(result);
    }

    @Test
    void getReferralTicketesForServiceRequest() {
        //given
        String patientId = TestConstants.TWO_STR;
        String serviceRequestId = TestConstants.TWO_STR;
        String type = TestConstants.DIAGNOSIS_TYPE;
        String assessmentType = TestConstants.ASSESSMENT;
        String url = "ServiceRequest?_count=9999&_sort=-_id&subject=Patient/2&_include=ServiceRequest:requester&_include=" +
                "ServiceRequest:performer&_include=ServiceRequest:encounter&_include=ServiceRequest:subject&_revinclude=" +
                "Provenance:target&identifier:not=nullpatient-status|Recovered&_id=ServiceRequest/2&requisition=nullticket-type|malaria";
        Bundle bundle = new Bundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        bundle.addEntry().setResource(serviceRequest);

        //when
        when(restApiUtil.getBatchRequest(url)).thenReturn(bundle);

        //then
        List<ReferralTicketDTO> result = patientService.getReferralTicketes(patientId, serviceRequestId, type, assessmentType);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPatientDiagnosisAndPatientReferenceNull() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        Boolean isMedicalReviewSummary = Boolean.TRUE;
        boolean includeAllCategories = Boolean.TRUE;

        //then
        List<DiagnosisDTO.DiseaseDTO> result = patientService.getPatientDiagnosis(request, isMedicalReviewSummary, includeAllCategories);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPatientDiagnosis() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Boolean isMedicalReviewSummary = Boolean.TRUE;
        boolean includeAllCategories = Boolean.TRUE;
        Bundle bundle = new Bundle();
        Condition condition = TestDataProvider.getCondition();
        bundle.addEntry().setResource(condition);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirAssessmentMapper.mapToDiagnosis(condition, isMedicalReviewSummary, request.getType())).thenReturn(new DiagnosisDTO.DiseaseDTO());

        //then
        List<DiagnosisDTO.DiseaseDTO> result = patientService.getPatientDiagnosis(request, isMedicalReviewSummary, includeAllCategories);
        Assertions.assertNotNull(result);
    }

    @Test
    void setPatientReferenceInEncounterDetails() {
        //given
        EncounterDetailsDTO encounterDetails = TestDataProvider.getEncounterDetailsData();
        Bundle bundle = new Bundle();

        //then
        patientService.setPatientReferenceInEncounterDetails(encounterDetails, bundle);
        Assertions.assertNotNull(encounterDetails);
    }

    @Test
    void setPatientReferenceInEncounterDetailsNull() {
        //given
        EncounterDetailsDTO encounterDetails = TestDataProvider.getEncounterDetailsData();
        encounterDetails.setPatientReference(null);
        encounterDetails.setMemberId(null);
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getPatient());

        //when
        when(householdService.getHouseholdMemberByPatientId(any())).thenReturn(TestDataProvider.getHouseHoldMember());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        patientService.setPatientReferenceInEncounterDetails(encounterDetails, bundle);
        Assertions.assertNotNull(encounterDetails);
    }

    @Test
    void createOrUpdateMedicalReviewEncounter() {
        //given
        String encounterId = TestConstants.STRING_THREE;
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        String encounterType = TestConstants.ENCOUNTER;
        String partOfEncounter = TestConstants.ENCOUNTER;
        Bundle bundle = new Bundle();

        //then
        String result = patientService.createOrUpdateMedicalReviewEncounter(encounterId, encounterDetailsDTO, encounterType, partOfEncounter, bundle);
        Assertions.assertNotNull(result);
    }

    @Test
    void createOrUpdateMedicalReviewEncounterIdIsNull() {
        //given
        String encounterId = null;
        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDTO();
        String encounterType = TestConstants.ENCOUNTER;
        String partOfEncounter = TestConstants.ENCOUNTER;
        Bundle bundle = new Bundle();

        //when
        when(fhirAssessmentMapper.createEncounter(encounterDetailsDTO, bundle, encounterType, partOfEncounter)).thenReturn(TestConstants.TWO_STR);

        //then
        String result = patientService.createOrUpdateMedicalReviewEncounter(encounterId, encounterDetailsDTO, encounterType, partOfEncounter, bundle);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPregnancyInfoByVillages() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        String url = "Observation?identifier=nullobservation-type|PatientVitals&performer=RelatedPerson/1,RelatedPerson/2&status=preliminary&_count=9999";
        Bundle bundle = new Bundle();
        Observation observation = TestDataProvider.getHeightObservation();
        bundle.addEntry().setResource(observation);

        //when
        when(householdService.getHouseholdMemberByVillagesWithPatientVitals(request.getVillageIds(), request.getLastSyncTime(),
                request.getCurrentSyncTime(), List.of(Constants.ID), request.getSkip(), request.getLimit())).
                thenReturn(List.of(TestConstants.ONE_STR, TestConstants.TWO_STR));
        when(restApiUtil.getBatchRequest(url)).thenReturn(bundle);
        when(fhirUtils.getIdFromReference(any())).thenReturn(TestConstants.TWO_STR);

        //then
        List<PregnancyInfo> result = patientService.getPregnancyInfoByVillages(request);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPatientVitals() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setMemberId(TestConstants.TWO_STR);
        Bundle bundle = new Bundle();
        Observation observation = TestDataProvider.getHeightObservation();
        bundle.addEntry().setResource(observation);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        PregnancyInfo result = patientService.getPatientVitals(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void handlePatientDeath() {
        //given
        String assessmentType = TestConstants.ASSESSMENT;
        Bundle bundle = new Bundle();
        String memberId = TestConstants.TWO_STR;
        String encounterId = TestConstants.STRING_THREE;
        ProvenanceDTO provenance = TestDataProvider.getProvenance();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(new Bundle());

        //then
        patientService.handlePatientDeath(assessmentType, bundle, memberId, encounterId, provenance);
        verify(fhirAssessmentMapper, atLeastOnce()).updateVitalObservationsStatus(any(), any(), any(), any());
    }

    @Test
    void searchPatientsContainTotalCount() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSearchText(TestConstants.TWO_STR);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(new Bundle());

        //then
        Map<String, Object> result = patientService.searchPatients(patientRequestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void searchPatients() {
        //given
        PatientRequestDTO patientRequestDTO = TestDataProvider.getPatientRequestDTO();
        patientRequestDTO.setSearchText(TestConstants.TWO_STR);
        patientRequestDTO.setSkip(TestConstants.INT_ONE);
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getRelatedPerson());

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(new Bundle());
        when(restApiUtil.getBatchRequest("RelatedPerson?active=true&national-id-virtual-id-phone-search:contains=2&_sort=-_lastUpdated&_count=10&_getpagesoffset=1")).thenReturn(bundle);

        //then
        Map<String, Object> result = patientService.searchPatients(patientRequestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPatientDetailsByVillageIds() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setVillageIds(List.of(TestConstants.ONE_STR, TestConstants.TWO_STR));
        requestDTO.setCurrentSyncTime(new Date());
        requestDTO.setLastSyncTime(new Date());
        String url = "Observation?identifier=nullobservation-type|vitalsigns&performer=RelatedPerson/abcd-efgh-ijkl-mnop&_sort=-_lastUpdated&_include=Observation:derived-from&_count=1";
        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();
        Bundle bundle = new Bundle();
        Observation observation = TestDataProvider.getHeightObservation();

        bundle.addEntry().setResource(observation);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getRelatedPersonBundle());
        when(restApiUtil.getBatchRequest(url)).thenReturn(bundle);
        when(searchPersonDetailsConverter.constructSearchPersonDetails(any(), anyString())).thenReturn(patientDetailsDTO);

        //then
        List<PatientDetailsDTO> result = patientService.getPatientDetailsByVillageIds(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createPregnancyDetails() {
        //given
        PregnancyDetailsDTO pregnancyDetailsDTO = TestDataProvider.getPregnancyDetailsDTO();
        NcdPatientStatus ncdPatientStatus = new NcdPatientStatus();
        ncdPatientStatus.setDiabetesStatus("Known Patient");
        ncdPatientStatus.setHypertensionStatus("Known Patient");
        pregnancyDetailsDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        pregnancyDetailsDTO.setNcdPatientStatus(ncdPatientStatus);
        pregnancyDetailsDTO.setActualDeliveryDate(new Date());
        pregnancyDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(relatedPerson);
        Condition condition = new Condition();
        condition.setIdentifier(List.of(new Identifier().setValue("Diabetes").setSystem(FhirIdentifierConstants.PATIENT_DIAGNOSIS_NCD_IDENTIFIER_SYSTEM_URL)));
        Condition hypertensionCondition = new Condition();
        hypertensionCondition.setIdentifier(List.of(new Identifier().setValue("Hypertension").setSystem(FhirIdentifierConstants.PATIENT_DIAGNOSIS_PREGNANCY_IDENTIFIER_URL)));
        Bundle conditionBundle = new Bundle();
        conditionBundle.addEntry().setResource(condition);
        conditionBundle.addEntry().setResource(hypertensionCondition);
        Encounter encounter = TestDataProvider.getEncounter();

        //when
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null")).thenReturn(bundle);
        when(restApiUtil.getPatientById("nullPatient/Patient/urn:uuid:null")).thenReturn(patient);
        when(restApiUtil.getBatchRequest("Condition?subject=Patient/Patient/urn:uuid:null&identifier=Hypertension,Diabetes,Substance Disorder,Mental Health Status")).thenReturn(conditionBundle);
        when(encounterConverter.createEncounter((Patient) any(), any(), any(), any(), any())).thenReturn(encounter);
        when(restApiUtil.getBatchRequest("Observation?code:text=pregnancy&status=preliminary&performer=RelatedPerson/null&_sort=-date&_count=1")).thenReturn(TestDataProvider.getObservationBundle());
        when(restApiUtil.getBatchRequest("Condition?identifier=Pregnancy&asserter=RelatedPerson/null")).thenReturn(conditionBundle);
        when(restApiUtil.getBatchRequest("Condition?subject=Patient/Patient/urn:uuid:null&verification-status:code=confirmed&_count=9999")).thenReturn(conditionBundle);

        //then
        PregnancyDetailsDTO result = patientService.createPregnancyDetails(pregnancyDetailsDTO);
        Assertions.assertNotNull(result);
    }
}

