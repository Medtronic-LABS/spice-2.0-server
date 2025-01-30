package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdPatientStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RiskDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CommonConverterTest {

    @Mock
    FhirUtils fhirUtils;

    @Mock
    RestApiUtil restApiUtil;

    @Mock
    private PatientStatusConverter patientStatusConverter;

    @InjectMocks
    CommonConverter commonConverter;

    @Mock
    CommonConverter commonConverterMock;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Test
    void createHeightObservationTest() {
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createBasicObservation(bioMetricsDTO, new Date(), FhirConstants.HEIGHT);

        // Assertions
        Assertions.assertNotNull(observation);
        Assertions.assertEquals(FhirConstants.CM_CODE, observation.getValueQuantity().getUnit());
    }

    @Test
    void createWeightObservationTest() {
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createBasicObservation(bioMetricsDTO, new Date(), FhirConstants.WEIGHT);

        // Assertions
        Assertions.assertNotNull(observation);
        Assertions.assertEquals(FhirConstants.KG_CODE, observation.getValueQuantity().getUnit());
    }

    @Test
    void createBmiObservationTest() {
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createBasicObservation(bioMetricsDTO, new Date(), FhirConstants.BMI);

        // Assertions
        Assertions.assertNotNull(observation);
        Assertions.assertEquals(FhirConstants.KG_PER_M2_CODE, observation.getValueQuantity().getUnit());
    }

    @Test
    void createBmiObservationTestWithDateAsNull() {
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createBasicObservation(bioMetricsDTO, null, FhirConstants.BMI);

        // Assertions
        Assertions.assertNotNull(observation);
        Assertions.assertEquals(FhirConstants.KG_PER_M2_CODE, observation.getValueQuantity().getUnit());
    }

    @Test
    void createRegularSmokerObservationTest() {
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createRegularSmokerObservation(true, new Date());

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createRegularSmokerObservationTestWithDateAsNull() {
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createRegularSmokerObservation(false,null);

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createRiskDetailsObservationTest() {
        RiskDetailsRequestDTO riskDetailsRequestDTO = TestDataProvider.getRiskDetailsRequest();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createRiskDetailsObservation(riskDetailsRequestDTO, new Date(),
                TestConstants.TEXT);

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createRiskDetailsObservationTestWithDateAsNull() {
        RiskDetailsRequestDTO riskDetailsRequestDTO = TestDataProvider.getRiskDetailsRequest();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createRiskDetailsObservation(riskDetailsRequestDTO, null,
                TestConstants.TEXT);

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createSuicideScreenerObservationTest() {
        Map<String, String> suicideScreenerDetails = TestDataProvider.getSuicideScreenerDetails();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createSuicideScreenerObservation(suicideScreenerDetails, new Date());

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createSuicideScreenerObservationTestWithDateAsNull() {
        Map<String, String> suicideScreenerDetails = TestDataProvider.getSuicideScreenerDetails();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createSuicideScreenerObservation(suicideScreenerDetails, null);

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createSubstanceAbuseObservationTest() {
        Map<String, String> substanceAbuseDetails = TestDataProvider.getSubstanceAbuseDetails();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createSubstanceAbuseObservation(substanceAbuseDetails, TestConstants.ONE_DOUBLE,
                new Date());

        // Assertions
        Assertions.assertNotNull(observation);
    }

    @Test
    void createSubstanceAbuseObservationTestWithDateAsNull() {
        Map<String, String> substanceAbuseDetails = TestDataProvider.getSubstanceAbuseDetails();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = commonConverter.createSubstanceAbuseObservation(substanceAbuseDetails, TestConstants.ONE_DOUBLE,
                null);

        // Assertions
        Assertions.assertNotNull(observation);
    }


    @Test
    void setPatientReferenceTest() {
        Patient patient = new Patient();
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setPatientOrganization(patient, organization);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                + Constants.FORWARD_SLASH + organization.getIdPart(),
                patient.getManagingOrganization().getReference());
    }


    @Test
    void setEncounterOrganizationTest() {
        Encounter encounter = new Encounter();
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setEncounterOrganization(encounter, organization);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                + Constants.FORWARD_SLASH + organization.getIdPart(),
                encounter.getServiceProvider().getReference());
    }

    @Test
    void setLocationOrganizationTest() {
        Location location = new Location();
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setLocationOrganization(location, organization);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                        + Constants.FORWARD_SLASH + organization.getIdPart(),
                location.getManagingOrganization().getReference());
    }

    @Test
    void setQuestionnaireResponseReferenceTest() {
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();
        Encounter encounter =  Mockito.mock(Encounter.class);
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(encounter.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setQuestionnaireResponseReference(questionnaireResponse, organization,
                encounter);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                        + Constants.FORWARD_SLASH + organization.getIdPart(),
                questionnaireResponse.getAuthor().getReference());
    }

    @Test
    void setQuestionnaireResponseReferenceTestWithEmptyEncounter() {
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();
        Encounter encounter =  new Encounter();
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setQuestionnaireResponseReference(questionnaireResponse, organization,
                encounter);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                        + Constants.FORWARD_SLASH + organization.getIdPart(),
                questionnaireResponse.getAuthor().getReference());
        Assertions.assertEquals(FhirConstants.ENCOUNTER_IDENTIFIER_URL,
                questionnaireResponse.getEncounter().getReference());
    }

    @Test
    void setObservationEncounterAndOrganizationTest() {
        Observation observation = new Observation();
        Encounter encounter =  Mockito.mock(Encounter.class);
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(encounter.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setObservationEncounterAndOrganization(observation, organization,
                encounter);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                        + Constants.FORWARD_SLASH + organization.getIdPart(),
                observation.getPerformer().get(TestConstants.ZERO.intValue()).getReference());
    }

    @Test
    void setObservationEncounterAndOrganizationTestWithEmptyEncounter() {
        Observation observation = new Observation();
        Encounter encounter =  new Encounter();
        Organization organization = Mockito.mock(Organization.class);

        // when
        when(organization.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setObservationEncounterAndOrganization(observation, organization,
                encounter);

        // Assertions
        Assertions.assertEquals(TestConstants.ORGANIZATION
                        + Constants.FORWARD_SLASH + organization.getIdPart(),
                observation.getPerformer().get(TestConstants.ZERO.intValue()).getReference());
        Assertions.assertEquals(FhirConstants.ENCOUNTER_IDENTIFIER_URL,
                observation.getEncounter().getReference());
    }

    @Test
    void setObservationReferenceTest() {
        Observation observation = new Observation();
        Patient patient =  Mockito.mock(Patient.class);
        RelatedPerson relatedPerson = Mockito.mock(RelatedPerson.class);

        // when
        when(patient.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(relatedPerson.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setObservationReference(observation, patient,
                relatedPerson);

        // Assertions
        Assertions.assertEquals(TestConstants.PATIENT
                + Constants.FORWARD_SLASH + relatedPerson.getIdPart(),
                observation.getPerformer().get(TestConstants.ZERO.intValue()).getReference());
    }

    @Test
    void setObservationReferenceTestWithEmptyPatientDetails() {
        Observation observation = new Observation();
        Patient patient =  new Patient();
        RelatedPerson relatedPerson = new RelatedPerson();

        // when
        commonConverter.setObservationReference(observation, patient,
                relatedPerson);

        // Assertions
        Assertions.assertEquals(FhirConstants.PATIENT_IDENTIFIER_URL,
                observation.getSubject().getReference());
        Assertions.assertEquals(FhirConstants.PATIENT_IDENTIFIER_URL,
                observation.getPerformer().get(TestConstants.ZERO.intValue()).getReference());
    }

    @Test
    void setObservationInBundleTest() {
        Bundle bundle = new Bundle();
        Observation observation =  new Observation();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        // when
        commonConverter.setObservationDetailsInBundle(bundle, observation,
                FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.Observation),
                        Constants.FORWARD_SLASH, FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL), FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL, Bundle.HTTPVerb.POST,
                observation, bundle, provenanceDTO);
    }

    @Test
    void setLocationInBundleTest() {
        Bundle bundle = new Bundle();
        Location location =  new Location();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        // when
        commonConverter.setLocationDetailsInBundle(bundle, location,
                FhirConstants.LOCATION_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.Location),
                        Constants.FORWARD_SLASH, FhirConstants.LOCATION_IDENTIFIER_URL), FhirConstants.LOCATION_IDENTIFIER_URL,
                Bundle.HTTPVerb.POST, location, bundle, provenanceDTO);
    }

    @Test
    void setPatientDetailsInBundleTest() {
        Bundle bundle = new Bundle();
        Patient patient =  Mockito.mock(Patient.class);
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        // when
        when(patient.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(patient.getId()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(patient.getMeta()).thenReturn(new Meta().setVersionId("sot"));
        commonConverter.setPatientDetailsInBundle(bundle, patient,
                FhirConstants.PATIENT_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(String.format(FhirConstants.PATIENT_ID, patient.getIdPart()),
                StringUtil.concatString(Constants.FHIR_BASE_URL, patient.getIdPart()),
                Bundle.HTTPVerb.PUT, patient, bundle, provenanceDTO);
    }

    @Test
    void setPatientDetailsInBundleTestWithEmptyPatient() {
        Bundle bundle = new Bundle();
        Patient patient =  new Patient();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        // when
        commonConverter.setPatientDetailsInBundle(bundle, patient,
                FhirConstants.PATIENT_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                        Constants.FORWARD_SLASH, FhirConstants.PATIENT_IDENTIFIER_URL), FhirConstants.PATIENT_IDENTIFIER_URL,
                Bundle.HTTPVerb.POST, patient, bundle, provenanceDTO);
    }

    @Test
    void setRelatedPersonDetailsInBundleTest() {
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson =  Mockito.mock(RelatedPerson.class);
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        // when
        when(relatedPerson.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(relatedPerson.getId()).thenReturn(TestConstants.ONE_TWO_THREE);
        when(relatedPerson.getMeta()).thenReturn(new Meta().setVersionId("sot"));
        commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPerson,
                FhirConstants.RELATED_PERSON_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()),
                StringUtil.concatString(Constants.FHIR_BASE_URL, relatedPerson.getIdPart()),
                Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);
    }

    @Test
    void setRelatedPersonDetailsInBundleTestWithEmptyPerson() {
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson =  new RelatedPerson();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPerson,
                FhirConstants.RELATED_PERSON_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                        Constants.FORWARD_SLASH, FhirConstants.RELATED_PERSON_IDENTIFIER_URL), FhirConstants.RELATED_PERSON_IDENTIFIER_URL,
                Bundle.HTTPVerb.POST, relatedPerson, bundle, provenanceDTO);
    }

    @Test
    void setEncounterDetailsInBundleTest() {
        Bundle bundle = new Bundle();
        Encounter encounter =  new Encounter();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        commonConverter.setEncounterDetailsInBundle(bundle, encounter,
                FhirConstants.ENCOUNTER_IDENTIFIER_URL, provenanceDTO);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                        Constants.FORWARD_SLASH, FhirConstants.ENCOUNTER_IDENTIFIER_URL), FhirConstants.ENCOUNTER_IDENTIFIER_URL,
                Bundle.HTTPVerb.POST, encounter, bundle, provenanceDTO);
    }

    @Test
    void setQuestionnarieDetailsInBundleTest() {
        Bundle bundle = new Bundle();
        QuestionnaireResponse questionnaireResponse =  new QuestionnaireResponse();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        commonConverter.setQuestionnarieDetailsInBundle(bundle, questionnaireResponse,
                FhirConstants.QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO, Boolean.FALSE);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.QuestionnaireResponse),
                        Constants.FORWARD_SLASH, FhirConstants.QUESTIONNAIRERESPONSE_IDENTIFIER_URL), FhirConstants.QUESTIONNAIRERESPONSE_IDENTIFIER_URL, Bundle.HTTPVerb.POST,
                questionnaireResponse, bundle, provenanceDTO);
    }

    @Test
    void setObservationTextTest() {
        Observation observation = new Observation();

        // when
        commonConverter.setObservationText(observation, TestConstants.TEXT);

        // Assertions
        Assertions.assertNotNull(observation.getText());
    }

    @Test
    void setObservationCodeTest() {
        Observation observation = new Observation();
        CodeableConcept codeableConcept = TestDataProvider.getCodeableConceptTest();

        commonConverter.setObservationCode(observation, codeableConcept);

        // Assertions
        Assertions.assertNotNull(observation.getCode());
    }

    @Test
    void setObservationNoteTest() {
        Observation observation = new Observation();

        commonConverter.setObservationNote(observation, TestConstants.TEXT,
                TestConstants.ONE_TWO_THREE);

        // Assertions
        Assertions.assertNotNull(observation.getNote());
    }

    @Test
    void calculateBirthDateTest() {
        Date dateOfBirth = null;

        dateOfBirth = commonConverter.calculateBirthDate(new Date(), TestConstants.ONE.intValue());

        // Assertions
        Assertions.assertNotNull(dateOfBirth);
    }

    @Test
    void setPatientLinkComponentReferenceTest() {
        Patient patient = new Patient();
        RelatedPerson relatedPerson =  Mockito.mock(RelatedPerson.class);

        // when
        when(relatedPerson.getIdPart()).thenReturn(TestConstants.ONE_TWO_THREE);
        commonConverter.setPatientLinkComponentReference(patient, relatedPerson);

        // Assertions
        Assertions.assertEquals(TestConstants.RELATED_PERSON + Constants.FORWARD_SLASH +
                        TestConstants.ONE_TWO_THREE,
                patient.getLinkFirstRep().getOther().getReference());
    }

    @Test
    void setPatientLinkComponentReferenceTestWithEmptyPerson() {
        Patient patient = new Patient();
        RelatedPerson relatedPerson =  new RelatedPerson();

        // when
        commonConverter.setPatientLinkComponentReference(patient, relatedPerson);

        // Assertions
        Assertions.assertEquals(FhirConstants.RELATED_PERSON_IDENTIFIER_URL,
                patient.getLinkFirstRep().getOther().getReference());
    }

    @Test
    void getFhirPhoneNumberCategoryTest() {

        String mobile = commonConverter.getFhirPhoneNumberCategory(Constants.PERSONAL)
                .getDisplay();
        String home = commonConverter.getFhirPhoneNumberCategory(Constants.FAMILY_MEMBER)
                .getDisplay();
        String temp = commonConverter.getFhirPhoneNumberCategory(Constants.FRIEND)
                .getDisplay();

        // Assertions
        Assertions.assertEquals(ContactPoint.ContactPointUse.MOBILE.getDisplay(), mobile);
        Assertions.assertEquals(ContactPoint.ContactPointUse.HOME.getDisplay(), home);
        Assertions.assertEquals(ContactPoint.ContactPointUse.TEMP.getDisplay(), temp);
        Assertions.assertNull(commonConverter.getFhirPhoneNumberCategory(TestConstants.TEXT));
    }

    @Test
    void createHumanNameTest() {
        HumanName humanName = commonConverter.createHumanName(TestConstants.FIRST_NAME,
                TestConstants.TEXT, TestConstants.LAST_NAME, null);

        // Assertions
        Assertions.assertEquals(TestConstants.FIRST_NAME, humanName.getGiven().getFirst().getValue());
    }

    @Test
    void createServiceRequestTest() {
        ServiceRequest response;

        response = commonConverter.createServiceRequest(TestConstants.ONE_TWO_THREE,
                TestConstants.ONE_TWO_THREE, TestConstants.TEXT, new Date());
        // Assertions
        Assertions.assertNotNull(response);
    }

    @Test
    void createBasicObservation() {
        //given
        Double value = TestConstants.ONE_DOUBLE;
        Date createdAt = new Date();
        String metaCode = TestConstants.BLANK_STRING;
        String quantityCode = TestConstants.BLANK_STRING;
        String narrativeCode = TestConstants.BLANK_STRING;

        //then
        Observation response = commonConverter.createBasicObservation(value, createdAt, metaCode, quantityCode, narrativeCode, Constants.SUBSTANCE_DISORDER);
        Assertions.assertNotNull(response);
    }

    @Test
    void setServiceRequestReference() {
        //given
        ServiceRequest serviceRequest = new ServiceRequest();
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Encounter encounter = TestDataProvider.getEncounter();

        //then
        commonConverter.setServiceRequestReference(serviceRequest, patient, relatedPerson, encounter);
        Assertions.assertNotNull(serviceRequest.getEncounter());
    }

    @Test
    void setServiceRequestReferenceNotNull() {
        //given
        ServiceRequest serviceRequest = new ServiceRequest();
        Patient patient = new Patient();
        RelatedPerson relatedPerson = new RelatedPerson();
        Encounter encounter = new Encounter();

        //then
        commonConverter.setServiceRequestReference(serviceRequest, patient, relatedPerson, encounter);
        Assertions.assertNotNull(serviceRequest.getEncounter());
    }

    @Test
    void setServiceRequestInBundle() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        ServiceRequest serviceRequest = new ServiceRequest();
        String identifier = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();

        //then
        commonConverter.setServiceRequestInBundle(bundle, serviceRequest, identifier, provenanceDTO);
        Assertions.assertNotNull(serviceRequest.getEncounter());
    }

    @Test
    void setHIVQuestionnarieDetailsInBundle() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();
        String identifier = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();

        //then
        commonConverter.setHIVQuestionnarieDetailsInBundle(bundle, questionnaireResponse, identifier, provenanceDTO);
        Assertions.assertNotNull(questionnaireResponse.getEncounter());
    }

    @Test
    void setResourceInBundle() {
        //given
        Bundle bundle = new Bundle();
        String identifierUrl = TestConstants.URL;
        String resIdUrl = TestConstants.URL;
        String resType = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        //then
        commonConverter.setResourceInBundle(bundle, new Bundle(), identifierUrl, resIdUrl, resType, provenanceDTO);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void setPhoneNumberCategory() {
        //given
        List<String> phoneNumberCategory = new ArrayList<>();
        phoneNumberCategory.add("mobile");
        phoneNumberCategory.add("home");
        phoneNumberCategory.add("temp");
        BioDataDTO bioDataDTO = TestDataProvider.getBioData();

        //then
        for(String catogory : phoneNumberCategory) {
            commonConverter.setPhoneNumberCategory(catogory, bioDataDTO);
            Assertions.assertNotNull(catogory);
        }
    }

    @Test
    void setPatientDetails() {
        //given
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        String tenantId = TestConstants.TWO_THREE_FOUR;

        //then
        ScreeningLogRequestDTO response = commonConverter.setPatientDetails(relatedPerson, tenantId);
        Assertions.assertNotNull(response);
    }

    @Test
    void setConditionInBundle() {
        //given
        Bundle bundle = new Bundle();
        Condition condition = new Condition();
        String identifier = TestConstants.BLANK_STRING;
        Boolean isUpdate = true;
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        //then
        commonConverter.setConditionInBundle(bundle, condition, identifier, isUpdate, provenanceDTO);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void setConditionInBundleIsUpdateFalse() {
        //given
        Bundle bundle = new Bundle();
        Condition condition = new Condition();
        String identifier = TestConstants.BLANK_STRING;
        Boolean isUpdate = false;
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        //then
        commonConverter.setConditionInBundle(bundle, condition, identifier, isUpdate, provenanceDTO);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void createContactPoint() {
        //given
        String phoneNumber = TestConstants.PHONE_NUMBER;
        String phoneNumberCategory = TestConstants.PHONE_NUMBER_CATEGORY;

        //then
        ContactPoint response = commonConverter.createContactPoint(phoneNumber, phoneNumberCategory);
        Assertions.assertNotNull(response);
    }

    @Test
    void createAttachment() {
        //given
        String qrCode = TestConstants.CODE;

        //then
        Attachment response = commonConverter.createAttachment(qrCode);
        Assertions.assertNotNull(response);
    }

    @Test
    void createAddress() {
        //given
        BioDataDTO.DataDTO country = new BioDataDTO.DataDTO();
        country.setName(TestConstants.COUNTRY);
        BioDataDTO.DataDTO district = new BioDataDTO.DataDTO();
        district.setName(TestConstants.DISTRICT);
        BioDataDTO.DataDTO chiefdom = new BioDataDTO.DataDTO();
        chiefdom.setName(TestConstants.BLANK_STRING);
        BioDataDTO.DataDTO village = new BioDataDTO.DataDTO();
        village.setName(TestConstants.BLANK_STRING);

        //then
        Address response = commonConverter.createAddress(country, district, chiefdom, village, null);
        Assertions.assertNotNull(response);
    }

    @Test
    void getResourceFromBundleByIdentifier() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        String identifier = TestConstants.BLANK_STRING;

        //then
        Optional<Resource> response = commonConverter.getResourceFromBundleByIdentifier(bundle, identifier);
        Assertions.assertNotNull(response);
    }

    @Test
    void getObservationFromBundleByIdentifier() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        String identifier = TestConstants.BLANK_STRING;

        //then
        Observation response = commonConverter.getObservationFromBundleByIdentifier(bundle, identifier);
        Assertions.assertNull(response);
    }

    @Test
    void getLocationFromBundle() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        String identifier = TestConstants.BLANK_STRING;

        //then
        Location response = commonConverter.getLocationFromBundle(bundle, identifier);
        Assertions.assertNull(response);
    }

    @Test
    void getQuestionnaireResponseFromBundle() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        String identifier = TestConstants.BLANK_STRING;

        //then
        QuestionnaireResponse response = commonConverter.getQuestionnaireResponseFromBundle(bundle, identifier);
        Assertions.assertNull(response);
    }

    @Test
    void getLocationFromBundleByIdentifier() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();
        String identifier = TestConstants.BLANK_STRING;

        //then
        Location response = commonConverter.getLocationFromBundleByIdentifier(bundle, identifier);
        Assertions.assertNull(response);
    }

    @Test
    void setConditionMetaDetails() {
        //given
        Condition condition = new Condition();
        Encounter encounter = TestDataProvider.getEncounter();
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();

        //then
        commonConverter.setConditionMetaDetails(condition, encounter, patient, relatedPerson);
        Assertions.assertNotNull(relatedPerson);
    }

    @Test
    void updateRiskLevel() {
        //given
        String riskLevel = TestConstants.RISK_LEVEL;
        Patient patient = TestDataProvider.getPatient();

        //then
        commonConverter.updateRiskLevel(riskLevel, patient);
        Assertions.assertNotNull(patient);
    }

    @Test
    void setPatientDetailsInBundle() {
        //given
        Bundle bundle = new Bundle();
        Patient patient = new Patient();
        patient.setId(TestConstants.ONE_STR);
        String identifier = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        String countryId = TestConstants.BLANK_STRING;
        String userCountryId = TestConstants.BLANK_STRING;

        //then
        commonConverter.setPatientDetailsInBundle(bundle, patient, identifier, provenanceDTO, countryId, userCountryId);
        Assertions.assertNotNull(patient);
    }

    @Test
    void setPatientDetailsInBundleAndPatientNotNull() {
        //given
        Bundle bundle = new Bundle();
        Patient patient = new Patient();
        String identifier = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        String countryId = TestConstants.BLANK_STRING;
        String userCountryId = TestConstants.BLANK_STRING;

        //then
        commonConverter.setPatientDetailsInBundle(bundle, patient, identifier, provenanceDTO, countryId, userCountryId);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void setRelatedPersonDetailsInBundle() {
        //given
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setId(TestConstants.ONE_STR);
        String identifier = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO =new ProvenanceDTO();
        String countryId = TestConstants.BLANK_STRING;
        String userCountryId = TestConstants.BLANK_STRING;

        //then
        commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPerson, identifier, provenanceDTO, countryId, userCountryId);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void setRelatedPersonDetailsInBundleAndRelatedPersonNotNull() {
        //given
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson = new RelatedPerson();
        String identifier = TestConstants.BLANK_STRING;
        ProvenanceDTO provenanceDTO =new ProvenanceDTO();
        String countryId = TestConstants.BLANK_STRING;
        String userCountryId = TestConstants.BLANK_STRING;

        //then
        commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPerson, identifier, provenanceDTO, countryId, userCountryId);
        Assertions.assertNotNull(provenanceDTO);
    }

    @Test
    void setQuestionnarieDetailsAndResponseNotNull() {
        Bundle bundle = new Bundle();
        QuestionnaireResponse questionnaireResponse =  new QuestionnaireResponse();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();

        commonConverter.setQuestionnarieDetailsInBundle(bundle, questionnaireResponse,
                FhirConstants.QUESTIONNAIRERESPONSE_IDENTIFIER_URL, provenanceDTO, Boolean.TRUE);

        // Assertions
        verify(fhirUtils, atLeastOnce()).setBundle(StringUtil.concatString(String.valueOf(ResourceType.QuestionnaireResponse),
                        Constants.FORWARD_SLASH, questionnaireResponse.getIdPart()), FhirConstants.QUESTIONNAIRERESPONSE_IDENTIFIER_URL,
                Bundle.HTTPVerb.PUT, questionnaireResponse, bundle, provenanceDTO);
    }

    @Test
    void setPatientAndRelatedPersonLink() {
        //given
        Patient patient = new Patient();
        patient.setId(TestConstants.ONE_STR);
        RelatedPerson relatedPerson = new RelatedPerson();

        //then
        commonConverter.setPatientAndRelatedPersonLink(patient, relatedPerson);
        Assertions.assertNotNull(relatedPerson);
    }

    @Test
    void setPatientAndRelatedPersonLinkAndPatientNotNull() {
        //given
        Patient patient = new Patient();
        RelatedPerson relatedPerson = new RelatedPerson();

        //then
        commonConverter.setPatientAndRelatedPersonLink(patient, relatedPerson);
        Assertions.assertNotNull(relatedPerson);
    }

    @Test
    void createRiskLevelObservation() {
        //given
        String riskLevel = TestConstants.RISK_LEVEL;
        String riskMessage = TestConstants.RISK_LEVEL;
        String memberReference = TestConstants.PATIENT_REFERENCE;
        String url = "Observation?identifier=nullobservation-type|redRiskDetails&performer=RelatedPerson/Patient/urn:uuid:null&_sort=-_lastUpdated";

        //when
        when(restApiUtil.getBatchRequest(url)).thenReturn(TestDataProvider.getObservationBundle());

        //then
        Observation response = commonConverter.createRiskLevelObservation(riskLevel, riskMessage, memberReference);
        Assertions.assertNotNull(response);
    }

    @Test
    void createRiskLevelObservationAndBundleIsEmpty() {
        //given
        String riskLevel = TestConstants.RISK_LEVEL;
        String riskMessage = TestConstants.RISK_LEVEL;
        String memberReference = TestConstants.PATIENT_REFERENCE;

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());

        //then
        Observation response = commonConverter.createRiskLevelObservation(riskLevel, riskMessage, memberReference);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateRiskLevelForRelatedPerson() {
        //given
        String riskLevel = TestConstants.RISK_LEVEL;
        RelatedPerson relatedPerson = new RelatedPerson();

        //then
        commonConverter.updateRiskLevel(riskLevel, relatedPerson);
        Assertions.assertNotNull(riskLevel);
    }

    @Test
    void updateRiskLevelHasRiskIdentifierTrue() {
        //given
        String riskLevel = TestConstants.RISK_LEVEL;
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        relatedPerson.getIdentifier().getFirst().setSystem("nullrisk-level");

        //then
        commonConverter.updateRiskLevel(riskLevel, relatedPerson);
        Assertions.assertNotNull(riskLevel);
    }

    @Test
    void createOrUpdateDiabetesAndHypertensionPatientStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        patientStatusDto.setNcdPatientStatus(new NcdPatientStatus());
        patientStatusDto.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = new Bundle();
        AtomicReference<Condition> diabetesConditionReference = new AtomicReference<>();
        AtomicReference<Condition> hypertensionConditionReference = new AtomicReference<>();
        Boolean isUpdate = Boolean.TRUE;
        Condition condition = new Condition();
        condition.setId(TestConstants.ONE_STR);

        //when
        when(patientStatusConverter.createDiabetesStatus(any(), any())).thenReturn(condition);
        when(patientStatusConverter.createHypertensionStatus(any(), any())).thenReturn(condition);

        //then
        commonConverter.createOrUpdateDiabetesAndHypertensionPatientStatus(patientStatusDto, bundle, diabetesConditionReference, hypertensionConditionReference, isUpdate);
        Assertions.assertNotNull(patientStatusDto);
    }

    @Test
    void getConfirmDiagnosis() {
        //given
        String patientId = TestConstants.PATIENT_ID;
        String type = TestConstants.BLANK_STRING;
        Bundle bundle = TestDataProvider.getEncounterBundle();
        bundle.getEntry().getFirst().setResource(new Condition());

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        Condition response = commonConverter.getConfirmDiagnosis(patientId, type);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateNCDKnownStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        Bundle bundle = new Bundle();
        Condition condition = new Condition();
        condition.setId(TestConstants.ONE_STR);
        Boolean isUpdate = Boolean.TRUE;

        //when
        when(patientStatusConverter.createConfirmedNCDStatus(any(), any())).thenReturn(condition);

        //then
        commonConverter.updateNCDKnownStatus(patientStatusDto, bundle, condition, isUpdate);
        Assertions.assertNotNull(patientStatusDto);
    }

    @Test
    void createOrUpdateMentalHealthPatientStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        MentalHealthStatus mentalHealthStatus = new MentalHealthStatus();
        mentalHealthStatus.setStatus(TestConstants.BLANK_STRING);
        patientStatusDto.setMentalHealthStatus(mentalHealthStatus);
        patientStatusDto.setSubstanceUseStatus(mentalHealthStatus);
        Bundle bundle = new Bundle();
        Condition condition = new Condition();
        CodeableConcept verificationStatus = new CodeableConcept();
        verificationStatus.addCoding(new Coding()
                .setSystem("http://terminology.hl7.org/CodeSystem/condition-ver-status")
                .setCode("confirmed") // Example value
                .setDisplay("Confirmed"));

        condition.setVerificationStatus(verificationStatus);
        AtomicReference<Condition> mentalHealthCondition = new AtomicReference<>();
        mentalHealthCondition.set(condition);
        AtomicReference<Condition> substanceCondition = new AtomicReference<>();
        substanceCondition.set(condition);

        //then
        commonConverter.createOrUpdateMentalHealthPatientStatus(patientStatusDto, bundle, mentalHealthCondition, substanceCondition);
        Assertions.assertNotNull(patientStatusDto);
    }

    @Test
    void updateMentalHealthKnownStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        Bundle bundle = new Bundle();
        Condition condition = new Condition();
        Boolean isUpdate = Boolean.TRUE;

        //when
        when(patientStatusConverter.createConfirmedMentalHealthStatus(any(), any())).thenReturn(new Condition());

        //then
        commonConverter.updateMentalHealthKnownStatus(patientStatusDto, bundle, condition, isUpdate);
        Assertions.assertNotNull(patientStatusDto);
    }
}
