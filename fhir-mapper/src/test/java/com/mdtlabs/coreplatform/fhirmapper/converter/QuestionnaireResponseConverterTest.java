package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.StringType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class QuestionnaireResponseConverterTest {

    @Mock
    FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private CommonConverter commonConverter;

    @InjectMocks
    QuestionnaireResponseConverter questionnaireResponseConverter;

    @Test
    void createPhq4Test() {
        MentalHealthDTO phq4DTO = TestDataProvider.getMentalHealthRequest();
        QuestionnaireResponse questionnaireResponse = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());

        // response
        questionnaireResponse = questionnaireResponseConverter
                .createMentalHealthQuestionnaireResponse(phq4DTO, new Date(), Constants.PHQ4);
        Assertions.assertNotNull(questionnaireResponse);
    }

    @Test
    void createPhq4TestWithDateAsNull() {
        MentalHealthDTO phq4DTO = TestDataProvider.getMentalHealthRequest();
        QuestionnaireResponse questionnaireResponse = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());

        // response
        questionnaireResponse = questionnaireResponseConverter
                .createMentalHealthQuestionnaireResponse(phq4DTO, null, Constants.PHQ4);
        Assertions.assertNotNull(questionnaireResponse);
    }

    @Test
    void testSetReference_PatientWithIdPart() {
        // Mock the Patient and QuestionnaireResponse
        RelatedPerson relatedPerson = Mockito.mock(RelatedPerson.class);
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();

        // Define behavior for relatedPerson.getIdPart()
        when(relatedPerson.getIdPart()).thenReturn("123");

        // Call the method under test
        questionnaireResponseConverter.setReference(questionnaireResponse, null, relatedPerson);

        // Verify the source is set correctly
        assertEquals("RelatedPerson/123", questionnaireResponse.getSource().getReference());
    }

    @Test
    void testSetReference_PatientWithoutIdPart() {
        // Mock the Patient and QuestionnaireResponse
        RelatedPerson relatedPerson = Mockito.mock(RelatedPerson.class);
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();

        // Define behavior for relatedPerson.getIdPart()
        when(relatedPerson.getIdPart()).thenReturn(null);

        // Call the method under test
        questionnaireResponseConverter.setReference(questionnaireResponse, null, relatedPerson);

        // Verify the source is set correctly
        assertEquals(FhirConstants.RELATED_PERSON_IDENTIFIER_URL, questionnaireResponse.getSource().getReference());
    }

    @Test
    void testSetReference_RelatedPersonWithIdPart() {
        // Mock the RelatedPerson and QuestionnaireResponse
        RelatedPerson relatedPerson = Mockito.mock(RelatedPerson.class);
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();

        // Define behavior for relatedPerson.getIdPart()
        when(relatedPerson.getIdPart()).thenReturn("456");

        // Call the method under test
        questionnaireResponseConverter.setReference(questionnaireResponse, null, relatedPerson);

        // Verify the source is set correctly
        assertEquals("RelatedPerson/456", questionnaireResponse.getSource().getReference());
    }

    @Test
    void testSetReference_NullPatientAndRelatedPerson() {
        // Mock the QuestionnaireResponse
        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();

        // Call the method under test with null patient and relatedPerson
        questionnaireResponseConverter.
                setReference(questionnaireResponse, null, null);

        // Verify the source is not set
        assertEquals(null, questionnaireResponse.getSource().getReference());
    }

    @Test
    void processMentalHealthDetails() {
        //given
        MentalHealthObservationDTO mentalHealthObservationDTO = new MentalHealthObservationDTO();
        mentalHealthObservationDTO.setRelatedPersonId(TestConstants.TWO_STR);
        Map<String, QuestionnaireResponse> questionnaireResponses = new HashMap<>();
        QuestionnaireResponse healthSurveyResponse = new QuestionnaireResponse();
        healthSurveyResponse.setId("general-health-survey-001");
        healthSurveyResponse.setStatus(QuestionnaireResponse.QuestionnaireResponseStatus.INPROGRESS);
        QuestionnaireResponse.QuestionnaireResponseItemComponent smokingStatusItem = new QuestionnaireResponse.QuestionnaireResponseItemComponent();
        smokingStatusItem.setLinkId("smokingStatus");
        smokingStatusItem.setText("Do you smoke?");
        smokingStatusItem.addAnswer().setValue(new StringType("No"));
        healthSurveyResponse.addItem(smokingStatusItem);
        questionnaireResponses.put("PHQ4", healthSurveyResponse);
        mentalHealthObservationDTO.setQuestionnaireResponses(questionnaireResponses);
        Map<String, String> mentalRiskDetails = new HashMap<>();
        mentalRiskDetails.put("anxietyLevel", "Moderate");
        mentalRiskDetails.put("depressionSeverity", "Mild");
        mentalRiskDetails.put("stressLevel", "High");
        mentalRiskDetails.put("suicidalIdeation", "None");
        mentalRiskDetails.put("socialIsolation", "Occasional");
        mentalRiskDetails.put("substanceAbuseRisk", "Low");
        Bundle bundle = new Bundle();
        bundle.setType(Bundle.BundleType.COLLECTION);
        QuestionnaireResponse phq4Response = new QuestionnaireResponse();
        phq4Response.setIdentifier(new Identifier().setValue("PHQ4"));
        Bundle.BundleEntryComponent phq4Entry = new Bundle.BundleEntryComponent();
        phq4Entry.setResource(phq4Response);
        bundle.addEntry(phq4Entry);
        QuestionnaireResponse phq9Response = new QuestionnaireResponse();
        phq9Response.setIdentifier(new Identifier().setValue("PHQ9"));
        Bundle.BundleEntryComponent phq9Entry = new Bundle.BundleEntryComponent();
        phq9Entry.setResource(phq9Response);
        bundle.addEntry(phq9Entry);
        QuestionnaireResponse gad7Response = new QuestionnaireResponse();
        gad7Response.setIdentifier(new Identifier().setValue("GAD7"));
        Bundle.BundleEntryComponent gad7Entry = new Bundle.BundleEntryComponent();
        gad7Entry.setResource(gad7Response);
        bundle.addEntry(gad7Entry);
        mentalHealthObservationDTO.setMentalRiskDetails(mentalRiskDetails);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        Observation response = questionnaireResponseConverter.processMentalHealthDetails(mentalHealthObservationDTO, null, null);
        Assertions.assertNotNull(response);
    }

    @Test
    void createBasicQuestionnaireResponse() {
        //given
        QuestionnaireResponse questionnaireResponse = TestDataProvider.getQuestionnaireResponse();
        Date date = new Date();
        String type = TestConstants.BLANK_STRING;

        //then
        QuestionnaireResponse response = questionnaireResponseConverter.createBasicQuestionnaireResponse(questionnaireResponse, date, type);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMentalHealthRiskDetails() {
        //given
        Observation observation = new Observation();
        observation.setComponent(TestDataProvider.getObservstionComponentList());

        //then
        Map<String, String> response = questionnaireResponseConverter.getMentalHealthRiskDetails(observation);
        Assertions.assertNotNull(response);
    }

    @Test
    void createHIVQuestionnaireResponse() {
        //given
        Map<String, String> request = TestDataProvider.getHiv();
        request.put("Not being able to stop or control worrying?", "notBeingAbleToStopOrControlWorrying");
        request.put("Feeling down, depressed or hopeless?", "feelingDownDepressedOrHopeless");

        QuestionnaireResponse questionnaireResponse = new QuestionnaireResponse();

        //when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());

        //then
        QuestionnaireResponse response = questionnaireResponseConverter.createHIVQuestionnaireResponse(request, questionnaireResponse);
        Assertions.assertNotNull(response);
    }
}
