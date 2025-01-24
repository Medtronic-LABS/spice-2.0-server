package com.mdtlabs.coreplatform.fhirmapper.registration.service;

import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CarePlan;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
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
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;

import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.fhirmapper.registration.service.impl.RegistrationServiceImpl;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RegistrationServiceTest {

    @InjectMocks
    private RegistrationServiceImpl registrationService;

    @Mock
    private PatientConverter patientConverter;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private VitalSignsConverter vitalSignsConverter;

    @Mock
    private PregnancyConverter converter;

    @Mock
    private CommonConverter commonConverter;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private DataSource dataSource;

    @Mock
    private OrganizationRepository organizationRepository;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    private SimpleJdbcCall jdbcCall;

    @Mock
    private PatientTreatmentPlanService patientTreatmentPlanService;

//    @Test
    void testRegisterPatientAndMember() {
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDTO();
        Bundle memberbundle = new Bundle().setEntry(new ArrayList<>());
        Bundle patientbundle = new Bundle().setEntry(new ArrayList<>());

        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        String uniqueId = fhirUtils.getUniqueId();
        when(restApiUtil.getBatchRequest(any())).thenReturn(memberbundle);
        when(restApiUtil.getBatchRequest(any())).thenReturn(patientbundle);
        when(fhirUtils.getUniqueId()).thenReturn(uniqueId);
        when(restApiUtil.postBatchRequest(null, null)).thenReturn(responseEntity);
        when(commonConverter.createHumanName(request.getBioData().getFirstName(), request.getBioData().getMiddleName(),
                request.getBioData().getLastName(), null)).thenReturn(new HumanName());
        when(converter.createPregnancyObservation(any(PregnancyDetailsDTO.class))).thenReturn(new Observation());
        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.findByFormDataIdAndFormName(any(),any())).thenReturn(organization);
        doAnswer(invocation -> {
            Patient patient = invocation.getArgument(0);
            patient.setName(List.of(new HumanName().setText("hey")));
            patient.setGender(Enumerations.AdministrativeGender.MALE);

            return null;
        }).when(patientConverter).mapPatient(any(Patient.class), any(EnrollmentRequestDTO.class), anyString());

        EnrollmentResponseDTO response = registrationService.registerPatientAndMember(request);
        Assertions.assertNotNull(response);
        
        request.getBioData().setMiddleName("V");
        request.getBioData().setInsuranceStatus(Boolean.FALSE);
        when(commonConverter.createHumanName(request.getBioData().getFirstName(), request.getBioData().getMiddleName(),
                request.getBioData().getLastName(), null)).thenReturn(new HumanName());
        response = registrationService.registerPatientAndMember(request);
        Assertions.assertNotNull(response);

        request.getBioData().setInsuranceId(null);
        response = registrationService.registerPatientAndMember(request);
        Assertions.assertNotNull(response);

        memberbundle.addEntry(new Bundle.BundleEntryComponent().setResource(new RelatedPerson()));
        Patient patient = new Patient();
        patient.addIdentifier(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
        .setValue(Constants.ENROLLED));
        patient.addName(new HumanName().setText(""));
        when(restApiUtil.getBatchRequest(any())).thenReturn(memberbundle);
        when(restApiUtil.getBatchRequest(any())).thenReturn(patientbundle);
        CarePlan careplan= getCarePlan();
        when(patientTreatmentPlanService.getCarePlanForPatient(any())).thenReturn(careplan);
        response = registrationService.registerPatientAndMember(request);
        Assertions.assertNotNull(response);
    }

    private CarePlan getCarePlan() {
        Bundle carePlanBundle = TestDataProvider.getCarePlanBundle();
        return (CarePlan) carePlanBundle.getEntry().getFirst().getResource();
    }


    @Test
    void testIsPatientRegisteredInRelatedPerson() {
        // Setup
        BioDataDTO request = new BioDataDTO();
        BioDataDTO.DataDTO dataDTO = new BioDataDTO.DataDTO();
        dataDTO.setId(TestConstants.ONE);
        request.setCountry(dataDTO);
        PatientValidationResponseDTO responseDTO = new PatientValidationResponseDTO();

        // Call the method
        assertThrows(DataNotAcceptableException.class, () -> registrationService.validatePatient(request));

        request.setIdentityValue(TestConstants.NATIONAL_ID);
        request.setMemberReference(TestConstants.ONE_STR);

        when(restApiUtil.getBatchRequest(any())).thenReturn(new Bundle());

        PatientValidationResponseDTO response = registrationService.validatePatient(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(responseDTO, response);
    }

    @Test
    void updateMemberSignature() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setMemberId(TestConstants.ONE_STR);
        Bundle responseBundle = mock(Bundle.class);
        RelatedPerson relatedPerson = new RelatedPerson();
        Bundle.BundleEntryComponent person = mock(Bundle.BundleEntryComponent.class);
        relatedPerson.addPhoto();
        HttpEntity<Bundle> http = new HttpEntity<>(responseBundle);
        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.RELATED_PERSON_QUERY_ID, requestDTO.getMemberId()))).thenReturn(responseBundle);
        when(person.getResource()).thenReturn(relatedPerson);
        when(responseBundle.getEntry()).thenReturn(List.of(person));
        doNothing().when(commonConverter).setRelatedPersonDetailsInBundle(any(), any(), any(), any());
        when(fhirUtils.getFhirBaseUrl()).thenReturn(Constants.LINK_FHIR_BASE);
        when(restApiUtil.constructRequestEntity(any())).thenReturn(http);
        when(restApiUtil.postBatchRequest(Constants.LINK_FHIR_BASE, http)).thenReturn(new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK));
        //then
        boolean response = registrationService.updateMemberSignature(requestDTO);
        Assertions.assertTrue(response);
        //given
        requestDTO.setMemberId(null);
        //then
        boolean errorResponse = registrationService.updateMemberSignature(requestDTO);
        Assertions.assertFalse(errorResponse);
    }

    @Test
    void isPatientPresentInRelatedPerson() {
        //given
        BioDataDTO request = TestDataProvider.BioDataDTO();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle relatedPersonBundle = new Bundle();
        relatedPersonBundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(relatedPersonBundle);

        //then
        PatientValidationResponseDTO response = registrationService.isPatientPresentInRelatedPerson(request);
        Assertions.assertNull(response);
    }

    @Test
    void isPatientPresentInRelatedPersonThrowValidationException() {
        //given
        BioDataDTO request = TestDataProvider.BioDataDTO();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        relatedPerson.addIdentifier().setSystem(Constants.PATIENT_STATUS_IDENTIFIER)
                .setValue(TestConstants.ENROLLED);
        Bundle relatedPersonBundle = new Bundle();
        relatedPersonBundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(relatedPersonBundle);
        assertThrows(Validation.class, () -> registrationService.isPatientPresentInRelatedPerson(request));
    }

//    @Test
    void registerPatientAndMember() {
        //given
        EnrollmentRequestDTO enrollmentRequestDTO = TestDataProvider.getEnrollmentRequestDTO();
        Bundle bundle = new Bundle();
        Organization organization = new Organization();
        organization.setId(TestConstants.ONE);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        SqlParameterSource in = new MapSqlParameterSource().addValue(Constants.INPUT_TENANT_ID, organization.getId());

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(organizationRepository.findByFormDataIdAndFormName(any(), any())).thenReturn(organization);
        when(jdbcCall.executeFunction(Long.class, in)).thenReturn(TestConstants.ONE);

        //then
        registrationService.registerPatientAndMember(enrollmentRequestDTO);
    }

//    @Test
    void testRegisterPatientAndMemberHasRelatedPersonResource() {
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDTO();
        request.setPatientId(String.valueOf(123));
        request.getBioMetrics().setIsPregnant(true);
        List<Bundle.BundleEntryComponent> memberResourceEntry = new ArrayList<>();
        Bundle.BundleEntryComponent memberComponent = new Bundle.BundleEntryComponent();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                .setValue(TestConstants.ENROLLED);
        memberComponent.setResource(relatedPerson);
        memberResourceEntry.add(memberComponent);
        Bundle memberbundle = new Bundle().setEntry(memberResourceEntry);
        
        Bundle patientbundle = new Bundle().setEntry(new ArrayList<>());

        List<Bundle.BundleEntryComponent> pregnancyEntry= new ArrayList<>();
        Bundle.BundleEntryComponent pregnancyComponent = new Bundle.BundleEntryComponent();
        pregnancyComponent.setResource(relatedPerson);
        pregnancyEntry.add(pregnancyComponent);

        Bundle patientPregnancybundle = new Bundle();
        patientPregnancybundle.setEntry(pregnancyEntry);

        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        when(restApiUtil.getBatchRequest(String.format("RelatedPerson?identifier=%s|%s&identifier=%s|%s", FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID, request.getBioData().getIdentityValue(),
                FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL, request.getBioData().getCountry().getId()).concat(Constants.PATIENT_ACTIVE_STATUS))).thenReturn(memberbundle);
        String uniqueId = fhirUtils.getUniqueId();
        when(restApiUtil.getBatchRequest(String.format("RelatedPerson?_id=%s", request.getId()).concat(Constants.PATIENT_ACTIVE_STATUS))).thenReturn(patientbundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.FETCH_LATEST_OBSERVATION_QUERY, FhirConstants.PREGNANCY.toLowerCase(),
                Observation.ObservationStatus.PRELIMINARY.name().toLowerCase(), request.getMemberId()))).thenReturn(patientPregnancybundle);

        when(fhirUtils.getUniqueId()).thenReturn(uniqueId);


        Observation observation = new Observation();
        when(vitalSignsConverter.createOrUpdateVitalSigns(any(),any())).thenReturn(observation);
        when(restApiUtil.postBatchRequest(null, null)).thenReturn(responseEntity);
        when(commonConverter.createHumanName(request.getBioData().getFirstName(), request.getBioData().getMiddleName(),
                request.getBioData().getLastName(), null)).thenReturn(new HumanName());
        when(converter.createPregnancyObservation(any(PregnancyDetailsDTO.class))).thenReturn(new Observation());
        Organization organization = TestDataProvider.getOrganization();
        when(organizationRepository.findByFormDataIdAndFormName(any(), any())).thenReturn(organization);
        doAnswer(invocation -> {
            Patient patient = invocation.getArgument(0);
            patient.setName(List.of(new HumanName().setText("hey")));
            patient.setGender(Enumerations.AdministrativeGender.MALE);

            return null;
        }).when(patientConverter).mapPatient(any(Patient.class), any(EnrollmentRequestDTO.class), anyString());

        EnrollmentResponseDTO response = registrationService.registerPatientAndMember(request);
        Assertions.assertNotNull(response);
    }
}
