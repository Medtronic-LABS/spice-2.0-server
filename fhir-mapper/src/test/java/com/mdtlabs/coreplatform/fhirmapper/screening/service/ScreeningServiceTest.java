package com.mdtlabs.coreplatform.fhirmapper.screening.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
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

import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.PatientStatusConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodGlucoseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodPressureConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.LocationConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.QuestionnaireResponseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.RelatedPersonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.screening.service.impl.ScreeningServiceImpl;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ScreeningServiceTest {
    @Mock
    FhirUtils fhirUtils;
    @Mock
    RestApiUtil restApiUtil;
    @Mock
    RelatedPersonConverter relatedPersonConverter;
    @Mock
    PatientConverter patientConverter;
    @Mock
    QuestionnaireResponseConverter questionnaireResponseConverter;
    @Mock
    BloodPressureConverter bloodPressureConverter;
    @Mock
    BloodGlucoseConverter bloodGlucoseConverter;
    @Mock
    PregnancyConverter pregnancyConverter;
    @Mock
    EncounterConverter encounterConverter;
    @Mock
    LocationConverter locationConverter;
    @Mock
    CommonConverter commonConverter;
    @Mock
    VitalSignsConverter vitalSignsConverter;
    @Mock
    SymptomConverter symptomConverter;

    private MockedStatic<DateUtil> dateUtil;

    private MockedStatic<UserContextHolder> userContextHolder;

    @InjectMocks
    ScreeningServiceImpl screeningServiceImpl;

    @Test
    void processScreeningLogTest() {
        ScreeningLogRequestDTO requestDTO = TestDataProvider.getScreeningLogRequest();
        requestDTO.setIsReferAssessment(Boolean.TRUE);
        requestDTO.setReferredReasons(List.of(TestConstants.BLANK_STRING));
        Patient patient = TestDataProvider.getFhirPatient();
        patient.setId(TestConstants.TWO_STR);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        List<Identifier> theIdentifiers = new ArrayList<>();
        theIdentifiers.add(new Identifier().setSystem("patient-status"));
        relatedPerson.setIdentifier(theIdentifiers);
        userContextHolder = mockStatic(UserContextHolder.class);
        Country country = new Country();
        country.setId(1L);
        country.setName("United States");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setCountry(country);
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(patient.getId())
                .setResource(patient)
                .getRequest()
                .setUrl(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Observation substanceAbuseObservation = TestDataProvider.getSubstanceAbuseObservation();
        CodeableConcept valueCodeableConcept = new CodeableConcept();
        Coding coding = new Coding()
                .setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                .setCode(Constants.YES.equalsIgnoreCase(requestDTO.getSuicidalIdeation()) ? Constants.YES_CODE : Constants.NO_CODE)
                .setDisplay(Constants.YES.equalsIgnoreCase(requestDTO.getSuicidalIdeation()) ? Constants.YES : Constants.NO);
        valueCodeableConcept.addCoding(coding);
        substanceAbuseObservation.setValue(valueCodeableConcept);
        Location location = TestDataProvider.getLocation();
        Encounter encounter = TestDataProvider.getEncounter();
        BioDataDTO response = null;
        String fhirServerUrl = TestConstants.FHIR_SERVER_URL;
        HttpEntity httpEntity = new HttpEntity<>(bundle, new HttpHeaders());
        ResponseEntity<FhirResponseDTO> responseDTOResponseEntity = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        Bundle serviceRequestBundle = new Bundle();
        serviceRequestBundle.addEntry().setResource(new ServiceRequest());

        // set
        ReflectionTestUtils.setField(screeningServiceImpl,fhirServerUrl, fhirServerUrl);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        when(locationConverter.createLocation(anyString(), anyString(), anyString())).thenReturn(location);
        when(patientConverter.createPatient(patient, requestDTO.getBioData(), requestDTO.getBioMetrics(),
                requestDTO.getBioMetrics().getDateOfBirth())).thenReturn(patient);
        when(relatedPersonConverter.createRelatedPerson(any(), any(), any(), any(), any(), any(), any())).thenReturn(relatedPerson);
        when(encounterConverter.createEncounter(patient, relatedPerson, location,
                requestDTO.getCategory(), requestDTO.getScreeningDateTime())).thenReturn(encounter);
        when(commonConverter.createSubstanceAbuseObservation(requestDTO.getSubstanceAbuse(),
                requestDTO.getCageAid(),
                requestDTO.getScreeningDateTime())).thenReturn(substanceAbuseObservation);
        when(commonConverter.createSuicideScreenerObservation(requestDTO.getSuicideScreener(),
                requestDTO.getScreeningDateTime())).thenReturn(substanceAbuseObservation);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(httpEntity);
        when(restApiUtil.postBatchRequest(fhirServerUrl,
                httpEntity)).thenReturn(responseDTOResponseEntity);
        when(restApiUtil.getBatchRequest("Observation?code:text=pregnancy&status=preliminary&performer=RelatedPerson/abcd-efgh-ijkl-mnop&_sort=-date&_count=1")).thenReturn(new Bundle());
        when(fhirUtils.getFhirIdsFromResponse(responseDTOResponseEntity.getBody())).thenReturn(new HashMap<>());
        when(restApiUtil.getBatchRequest("ServiceRequest?subject=2&status=active&_sort=-_lastUpdated")).thenReturn(serviceRequestBundle);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);

        //then
        response = screeningServiceImpl.processScreeningLog(requestDTO);
        Assertions.assertNotNull(response);
        userContextHolder.close();
    }

    @Test
    void processScreeningLogCheckDuplicateNudgeTest() {
        //given
        ScreeningLogRequestDTO requestDTO = TestDataProvider.getScreeningLogRequest();
        requestDTO.setCountryId(TestConstants.NATIONAL_ID);
        String relatedPersonByNationalIdUrl = "RelatedPerson?identifier=nullnational-id|national-id&identifier=nullcountry-id|nationalId&&active=true";
        String relatedPersonByBasicDetailsUrl = "RelatedPerson?name:contains=TEXT&name:contains=TEXT&telecom=Text&active=true&identifier=nullcountry-id|nationalId&&active=true";
        Patient patient = TestDataProvider.getFhirPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(patient.getId())
                .setResource(patient)
                .getRequest()
                .setUrl(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Observation substanceAbuseObservation = TestDataProvider.getSubstanceAbuseObservation();
        CodeableConcept valueCodeableConcept = new CodeableConcept();
        Coding coding = new Coding()
                .setSystem(FhirIdentifierConstants.FHIR_YES_NO_CODE)
                .setCode(Constants.YES.equalsIgnoreCase(requestDTO.getSuicidalIdeation()) ? Constants.YES_CODE : Constants.NO_CODE)
                .setDisplay(Constants.YES.equalsIgnoreCase(requestDTO.getSuicidalIdeation()) ? Constants.YES : Constants.NO);
        valueCodeableConcept.addCoding(coding);
        substanceAbuseObservation.setValue(valueCodeableConcept);
        Location location = TestDataProvider.getLocation();
        Encounter encounter = TestDataProvider.getEncounter();
        BioDataDTO response = null;
        String fhirServerUrl = TestConstants.FHIR_SERVER_URL;
        HttpEntity httpEntity = new HttpEntity<>(bundle, new HttpHeaders());
        ResponseEntity<FhirResponseDTO> responseDTOResponseEntity = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        ReflectionTestUtils.setField(screeningServiceImpl,fhirServerUrl, fhirServerUrl);
        Map<String, Object> siteDetails = new HashMap<>();
        Patient patientObject = TestDataProvider.getFhirPatient();
        patientObject.setId(TestConstants.TWO_STR);
        List<Identifier> theIdentifiers = new ArrayList<>();
        theIdentifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue("ENROLLED"));
        theIdentifiers.add(new Identifier().setSystem(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL).setValue(TestConstants.TWO_STR));
        patientObject.setIdentifier(theIdentifiers);
        siteDetails.put("Organization" , new Organization());
        siteDetails.put("Patient", patientObject);
        RelatedPerson relatedPersonByNationalId = TestDataProvider.getRelatedPerson();
        List<StringType> theGiven = new ArrayList<>();
        theGiven.add(new StringType("John"));
        theGiven.add(new StringType("Doe"));
        relatedPersonByNationalId.getName().getFirst().setGiven(theGiven);
        TestDataProvider.init();

        //when
        when(restApiUtil.getRelatedPerson(relatedPersonByNationalIdUrl)).thenReturn(relatedPersonByNationalId);
        when(restApiUtil.getRelatedPerson(relatedPersonByBasicDetailsUrl)).thenReturn(relatedPersonByNationalId);
        when(fhirUtils.getOldestEncounter(any())).thenReturn(new Encounter());
        when(fhirUtils.getRelatedPersonIdFromEncounter(any())).thenReturn("abcd-efgh-ijkl-mnop");
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        when(locationConverter.createLocation(anyString(), anyString(), anyString())).thenReturn(location);
        when(patientConverter.createPatient(patient, requestDTO.getBioData(), requestDTO.getBioMetrics(),
                requestDTO.getBioMetrics().getDateOfBirth())).thenReturn(patient);
        when(relatedPersonConverter.createRelatedPerson(any(), any(), any(), any(), any(), any(), any())).thenReturn(relatedPerson);
        when(encounterConverter.createEncounter(patient, relatedPerson, location,
                requestDTO.getCategory(), requestDTO.getScreeningDateTime())).thenReturn(encounter);
        when(commonConverter.createSubstanceAbuseObservation(requestDTO.getSubstanceAbuse(),
                requestDTO.getCageAid(),
                requestDTO.getScreeningDateTime())).thenReturn(substanceAbuseObservation);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(httpEntity);
        when(restApiUtil.postBatchRequest(fhirServerUrl,
                httpEntity)).thenReturn(responseDTOResponseEntity);
        when(restApiUtil.getBatchRequest("Observation?code:text=pregnancy&status=preliminary&performer=RelatedPerson/abcd-efgh-ijkl-mnop&_sort=-date&_count=1")).thenReturn(new Bundle());
        when(fhirUtils.getFhirIdsFromResponse(responseDTOResponseEntity.getBody())).thenReturn(new HashMap<>());
        when(restApiUtil.getSiteDetails(anyString())).thenReturn(siteDetails);
        when(commonConverter.createSuicideScreenerObservation(any(), any())).thenReturn(substanceAbuseObservation);
        TestDataProvider.getStaticMock();

        //then
        response = screeningServiceImpl.processScreeningLog(requestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetPatientCountOfUsersToday() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        userContextHolder = mockStatic(UserContextHolder.class);
        dateUtil = mockStatic(DateUtil.class);
        UserDTO userDTO = new UserDTO();
        userDTO.setTimezone(new Timezone());
        userDTO.getTimezone().setOffset("TimeZone");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTimezone(new Timezone());
        userContextDTO.getTimezone().setOffset("TimeZone");
        requestDTO.setUserId("user123");
        requestDTO.setSortField(Constants.TODAY);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(getSampleBundle());
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        dateUtil.when(()-> DateUtil.getUserTimezoneTime(userContextDTO.getTimezone().getOffset(), TestConstants.INT_ONE, Boolean.TRUE)).thenReturn(TestConstants.BLANK_STRING);

        DashboardDetails result = screeningServiceImpl.getPatientCountOfUsers(requestDTO);

        Assertions.assertNotNull(result);
        userContextHolder.close();
        dateUtil.close();
    }

    @Test
    void testGetPatientCountOfUsersYesterday() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        userContextHolder = mockStatic(UserContextHolder.class);
        dateUtil = mockStatic(DateUtil.class);
        UserDTO userDTO = new UserDTO();
        userDTO.setTimezone(new Timezone());
        userDTO.getTimezone().setOffset("TimeZone");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTimezone(new Timezone());
        userContextDTO.getTimezone().setOffset("TimeZone");
        requestDTO.setUserId("user123");
        requestDTO.setSortField(Constants.YESTERDAY);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(getSampleBundle());
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        dateUtil.when(()-> DateUtil.getUserTimezoneTime(userContextDTO.getTimezone().getOffset(), TestConstants.INT_ONE, Boolean.TRUE)).thenReturn(TestConstants.BLANK_STRING);
        DashboardDetails result = screeningServiceImpl.getPatientCountOfUsers(requestDTO);

        Assertions.assertNotNull(result);
        dateUtil.close();
        userContextHolder.close();
    }

    @Test
    void testGetPatientCountOfUsersThisWeek() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        userContextHolder = mockStatic(UserContextHolder.class);
        dateUtil = mockStatic(DateUtil.class);
        UserDTO userDTO = new UserDTO();
        userDTO.setTimezone(new Timezone());
        userDTO.getTimezone().setOffset("TimeZone");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTimezone(new Timezone());
        userContextDTO.getTimezone().setOffset("TimeZone");
        requestDTO.setUserId("user123");
        requestDTO.setSortField(Constants.THIS_WEEK);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(getSampleBundle());
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        dateUtil.when(()-> DateUtil.getUserTimezoneTime(userContextDTO.getTimezone().getOffset(), TestConstants.INT_ONE, Boolean.TRUE)).thenReturn(TestConstants.BLANK_STRING);

        DashboardDetails result = screeningServiceImpl.getPatientCountOfUsers(requestDTO);

        Assertions.assertNotNull(result);
        userContextHolder.close();
        dateUtil.close();
    }

    @Test
    void testGetPatientCountOfUsersThisMonth() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        userContextHolder = mockStatic(UserContextHolder.class);
        dateUtil = mockStatic(DateUtil.class);
        UserDTO userDTO = new UserDTO();
        userDTO.setTimezone(new Timezone());
        userDTO.getTimezone().setOffset("TimeZone");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTimezone(new Timezone());
        userContextDTO.getTimezone().setOffset("TimeZone");
        requestDTO.setUserId("user123");
        requestDTO.setSortField(Constants.THIS_MONTH);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(getSampleBundle());
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        dateUtil.when(()-> DateUtil.getUserTimezoneTime(userContextDTO.getTimezone().getOffset(), TestConstants.INT_ONE, Boolean.TRUE)).thenReturn(TestConstants.BLANK_STRING);

        DashboardDetails result = screeningServiceImpl.getPatientCountOfUsers(requestDTO);

        Assertions.assertNotNull(result);
        dateUtil.close();
        userContextHolder.close();
    }

    @Test
    void testGetPatientCountOfUsers() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        requestDTO.setUserId(null);

        assertThrows(BadRequestException.class, () -> {
            screeningServiceImpl.getPatientCountOfUsers(requestDTO);
        });
    }

    @Test
    void testGetPatientCountOfUsersCustomDateRange() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        userContextHolder = mockStatic(UserContextHolder.class);
        dateUtil = mockStatic(DateUtil.class);
        UserDTO userDTO = new UserDTO();
        userDTO.setTimezone(new Timezone());
        userDTO.getTimezone().setOffset("TimeZone");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTimezone(new Timezone());
        userContextDTO.getTimezone().setOffset("TimeZone");
        requestDTO.setUserId("user123");
        requestDTO.setCustomDate(Map.of(
                Constants.START_DATE, new Date(),
                Constants.END_DATE, new Date()));
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(getSampleBundle());
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        dateUtil.when(()-> DateUtil.getUserTimezoneTime(userContextDTO.getTimezone().getOffset(), TestConstants.INT_ONE, Boolean.TRUE)).thenReturn(TestConstants.BLANK_STRING);

        DashboardDetails result = screeningServiceImpl.getPatientCountOfUsers(requestDTO);

        Assertions.assertNotNull(result);
        dateUtil.close();
        userContextHolder.close();
    }

    @Test
    void testGetPatientCountOfUsersThrowsBadRequestException() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();

        assertThrows(BadRequestException.class, () -> {
            screeningServiceImpl.getPatientCountOfUsers(requestDTO);
        });
    }

    private Bundle getSampleBundle() {
        Bundle bundle = new Bundle();
        Bundle.BundleEntryComponent entry = new Bundle.BundleEntryComponent();
        RelatedPerson relatedPerson = new RelatedPerson();
        Identifier identifier = new Identifier();
        identifier.setSystem(Constants.PATIENT_STATUS_TYPE);
        identifier.setValue(PatientStatusConstants.ENROLLED);
        relatedPerson.setIdentifier(List.of(identifier));
        entry.setResource(relatedPerson);
        List<Bundle.BundleEntryComponent> entryComponents = new ArrayList<>();
        entryComponents.add(entry);
        bundle.setEntry(entryComponents);
        return bundle;
    }

    @Test
    void testGetCountOfPatientsWithEntries() {
        Bundle bundle = getSampleBundle();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Encounter encounter = TestDataProvider.getEncounter();
        Observation observation = TestDataProvider.getHeightObservation();
        MedicationDispense medicationDispense = new MedicationDispense();
        medicationDispense.setIdentifier(List.of(new Identifier().setSystem("prescription-filled-days").setValue(TestConstants.TWO_STR)));
        relatedPerson.setIdentifier(List.of(new Identifier().setSystem(Constants.PATIENT_STATUS_TYPE).setValue(PatientStatusConstants.SCREENING), new Identifier().setSystem("is-patient-referred").setValue("YES")));
        observation.setIdentifier(List.of(new Identifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue("psychologyAssessment"), new Identifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue("nutritionLifestyle")));
        encounter.setIdentifier(List.of(new Identifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue(FhirConstants.SCREENING), new Identifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue("assessment"), new Identifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue("enrollment")));
        bundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(relatedPerson));
        bundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(encounter));
        bundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(observation));
        bundle.getEntry().add(new Bundle.BundleEntryComponent().setResource(medicationDispense));
        DashboardDetails result = screeningServiceImpl.getCountOfPatients(bundle, bundle);

        Assertions.assertEquals(1, result.getAssessed());
        Assertions.assertEquals(1, result.getScreened());
        Assertions.assertEquals(1, result.getRegistered());
    }

    @Test
    void testGetCountOfPatientsEmptyBundle() {
        Bundle bundle = new Bundle();

        DashboardDetails result = screeningServiceImpl.getCountOfPatients(bundle, bundle);

        Assertions.assertEquals(0, result.getScreened());
        Assertions.assertEquals(0, result.getAssessed());
        Assertions.assertEquals(0, result.getRegistered());
    }

    @Test
    void testGetCountOfPatientsWithNullIdentifier() {
        Bundle bundle = new Bundle();
        Bundle.BundleEntryComponent entry = new Bundle.BundleEntryComponent();
        RelatedPerson relatedPerson = new RelatedPerson();
        entry.setResource(relatedPerson);
        bundle.setEntry(List.of(entry));

        DashboardDetails result = screeningServiceImpl.getCountOfPatients(bundle, bundle);

        Assertions.assertEquals(0, result.getScreened());
        Assertions.assertEquals(0, result.getAssessed());
        Assertions.assertEquals(0, result.getRegistered());
    }

    @Test
    void testGetPatientCountOfUsersThrowWhenInputSortFieldIsNull() {
        DashboardDetailsRequestDTO requestDTO = new DashboardDetailsRequestDTO();
        userContextHolder = mockStatic(UserContextHolder.class);
        UserDTO userDTO = new UserDTO();
        userDTO.setTimezone(new Timezone());
        userDTO.getTimezone().setOffset("TimeZone");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTimezone(new Timezone());
        userContextDTO.getTimezone().setOffset("TimeZone");
        requestDTO.setSortField(null);
        requestDTO.setUserId("abc");
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
        String errorMessage = assertThrows(BadRequestException.class, () -> screeningServiceImpl.getPatientCountOfUsers(requestDTO)).getMessage();
        Assertions.assertEquals("Date is not valid", errorMessage);
        userContextHolder.close();
    }

    @Test
    void getScreeningLog() {
        //given
        ScreeningLogRequestDTO screeningLogRequestDTO = TestDataProvider.getScreeningLogRequest();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle observationBundle = TestDataProvider.getObservationBundle();
        Observation observation = new Observation();
        CodeableConcept code = new CodeableConcept();
        code.setText("pregnancy");
        observation.setCode(code);
        observation.setId(TestConstants.TWO_STR);
        Observation observationOne = new Observation();
        CodeableConcept codeOne = new CodeableConcept();
        codeOne.setText("riskLevel");
        observationOne.setCode(codeOne);
        observationOne.setId(TestConstants.ONE_STR);
        Bundle.BundleEntryComponent observationEntry = new Bundle.BundleEntryComponent();
        Bundle.BundleEntryComponent observationEntryOne = new Bundle.BundleEntryComponent();
        observationEntry.setResource(observation);
        observationEntryOne.setResource(observationOne);
        observationEntry.getRequest().setMethod(Bundle.HTTPVerb.POST).setUrl("Observation");
        observationBundle.addEntry(observationEntry);
        observationBundle.addEntry(observationEntryOne);
        observationBundle.getEntry().removeFirst();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        String url = "Observation?code:text=riskLevel,pregnancy&performer=RelatedPerson/abcd-efgh-ijkl-mnop&_sort=-_lastUpdated";

        //when
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null")).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(url)).thenReturn(observationBundle);

        //then
        ScreeningLog response = screeningServiceImpl.getScreeningLog(screeningLogRequestDTO);
        Assertions.assertNotNull(response);
    }
}
