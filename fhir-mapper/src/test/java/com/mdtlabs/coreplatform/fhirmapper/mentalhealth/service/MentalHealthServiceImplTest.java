package com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.QuestionnaireResponse;
import org.hl7.fhir.r4.model.Reference;
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
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.QuestionnaireResponseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.impl.MentalHealthServiceImpl;

/**
 * <p>
 * MentalHealthServiceImplTest class used to test all possible positive
 * and negative cases for all methods and conditions used in
 * MentalHealthServiceImpl class.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Feb 9, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MentalHealthServiceImplTest {

    @InjectMocks
    MentalHealthServiceImpl mentalHealthService;

    @Mock
    RestApiUtil restApiUtil;

    @Mock
    EncounterConverter encounterConverter;

    @Mock
    QuestionnaireResponseConverter questionnaireResponseConverter;

    @Mock
    VitalSignsConverter vitalSignsConverter;
    
    @Mock
    CommonConverter commonConverter;

    @Mock
    FhirUtils fhirUtils;

    @Mock
    AssessmentService assessmentService;

    @Test
    void createMentalHealth() {
        AssessmentDTO request = TestDataProvider.getAssessmentDTO();
        request.setPhq9(TestDataProvider.getMentalHealthRequest());
        request.setMemberReference(TestConstants.ONE.toString());
        request.setUserId(1L);
        request.setSiteId(2L);
        request.setPhq4(TestDataProvider.getMentalHealthRequest());
        request.setGad7(TestDataProvider.getMentalHealthRequest());
        request.setEncounter(TestDataProvider.getEncounterDTO());
        request.getEncounter().setId(TestDataProvider.getEncounterDTO().getId());
        request.getEncounter().setProvenance(TestDataProvider.getProvenance());
        request.getEncounter().getProvenance().setOrganizationId(TestConstants.UNIQUE_ID);
        request.getEncounter().setPatientReference(TestConstants.PATIENT_REFERENCE);
        request.setPatientId(TestConstants.PATIENT_ID);
        request.setId(TestConstants.ENCOUNTER_REFERENCE);
        request.setAssessmentTakenOn(new Date());
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Encounter encounter = TestDataProvider.getEncounter();
        Location location = TestDataProvider.getLocation();
        Observation observation = TestDataProvider.getMentalHealthObservation();
        observation.setDerivedFrom(List.of((new Reference(
                String.format(FhirConstants.QUESTIONNAIRE_RESPONSE_ID,TestConstants.ONE_STR)))));
        QuestionnaireResponse questionnaireResponse = TestDataProvider.getQuestionnaireResponse();
        ResponseEntity<FhirResponseDTO> response = new ResponseEntity<>(new FhirResponseDTO(), HttpStatus.OK);
        Bundle bundle = new Bundle();
        String bundleDto = "";
        String url = String.format(Constants.GET_MEMBER_ID, request.getMemberReference());
        HttpHeaders headers = new HttpHeaders();
        bundle.addEntry()
                .setFullUrl(request.getMemberReference())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        ReflectionTestUtils.setField(mentalHealthService, TestConstants.FHIR_SERVER_URL, TestConstants.URL);
        Bundle relatedBundle = TestDataProvider.getRelatedPersonBundle();
        relatedBundle.addEntry()
                        .setResource(relatedPerson);
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();

        //when
        when(restApiUtil.getPatientById(any())).thenReturn(TestDataProvider.getPatient());
        when(restApiUtil.getBatchRequest(url)).thenReturn(bundle);
        when(encounterConverter.createEncounter(null, relatedPerson, location,
                null, new Date())).thenReturn(encounter);
        when(questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(request.getPhq9(), request.getAssessmentTakenOn() , Constants.PHQ9)).thenReturn(questionnaireResponse);
        when(questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(request.getPhq9(), request.getAssessmentTakenOn() , Constants.PHQ4)).thenReturn(questionnaireResponse);
        when(questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(request.getPhq9(), request.getAssessmentTakenOn() , Constants.GAD7)).thenReturn(questionnaireResponse);

        when(vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO,bundle)).thenReturn(observation);
        when(restApiUtil.constructRequestEntity(bundle)).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest(TestConstants.URL, new HttpEntity<>(bundleDto, headers))).thenReturn(
                response);

        mentalHealthService.createMentalHealth(request);
        verify(questionnaireResponseConverter, atLeastOnce()).createMentalHealthQuestionnaireResponse(request.getPhq9(), request.getAssessmentTakenOn() , Constants.PHQ9);
    }


    @Test
    void createQuestionnaireResponse(){
        AssessmentDTO request = TestDataProvider.getAssessmentDTO();
        request.setPhq4(TestDataProvider.getMentalHealthRequest());
        request.setPhq9(TestDataProvider.getMentalHealthRequest());
        request.setGad7(TestDataProvider.getMentalHealthRequest());
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Encounter encounter = TestDataProvider.getEncounter();
        Bundle bundle = new Bundle();
        ProvenanceDTO provenanceDTO = TestDataProvider.getProvenance();
        request.getEncounter().getProvenance().setOrganizationId(TestConstants.UNIQUE_ID);
              QuestionnaireResponse questionnaireResponse = TestDataProvider.getQuestionnaireResponse();
        questionnaireResponse.setAuthor(TestDataProvider.getQuestionnaireResponse().getAuthor());
        Map<String, QuestionnaireResponse> questionnaireResponses = new HashMap<>();
        questionnaireResponses.put(Constants.PHQ4, questionnaireResponse);

        //when
        when(questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(
                request.getPhq4(), request.getAssessmentTakenOn(), Constants.PHQ4)).thenReturn(questionnaireResponse);
        when(questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(
                request.getPhq9(), request.getAssessmentTakenOn(), Constants.PHQ9)).thenReturn(questionnaireResponse);
        when(questionnaireResponseConverter.createMentalHealthQuestionnaireResponse(
                request.getGad7(), request.getAssessmentTakenOn(), Constants.GAD7)).thenReturn(questionnaireResponse);
        Map<String, QuestionnaireResponse> result = mentalHealthService.createQuestionnaireResponse(request,patient,relatedPerson,encounter,bundle,provenanceDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void getMentalHealthDetails() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        MentalHealthDTO mentalHealthDTO = new MentalHealthDTO();
        QuestionnaireResponse response = TestDataProvider.getQuestionnaireResponse();
        request.setMemberReference(TestConstants.STRING_VALUE);
        mentalHealthDTO.setMentalHealthDetails(List.of(TestDataProvider.getMentalHealthDetailsDTO()));
        mentalHealthDTO.setEncounterId(fhirUtils.getIdFromReference(response.getEncounter().getReference()));
        mentalHealthDTO.setQuestionnaireId(TestConstants.UNIQUE_ID);
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(request.getMemberReference())
                .setResource(response)
                .getRequest()
                .setUrl(String.format(Constants.QUESTIONNAIRE_RESPONSE_BY_MEMBER_ID, request.getMemberReference(), request.getType()))
                .setMethod(Bundle.HTTPVerb.GET);
        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        MentalHealthDTO result = mentalHealthService.getMentalHealthDetails(request);
        Assertions.assertNotNull(result);

    }

    @Test
    void createMentalHealthCondition() {
        AssessmentDTO mentalHealth = TestDataProvider.getAssessmentDTO();
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();

        bundle.addEntry()
                .setResource(relatedPerson);
        mentalHealth.setEncounter(TestDataProvider.getEncounterDTO());
        Observation substanceAbuse = TestDataProvider.getSubstanceAbuseObservation();
        Observation suicideScreener = TestDataProvider.getSuicideScreenerObservation();
        Encounter encounter = TestDataProvider.getEncounter();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(restApiUtil.getEncounterById(mentalHealth.getEncounter().getId())).thenReturn(encounter);
        when(assessmentService.createSubstanceAbuseObservation(mentalHealth, encounter)).thenReturn(substanceAbuse);
        when(assessmentService.createSuicideObservation(mentalHealth, encounter)).thenReturn(suicideScreener);
        mentalHealthService.createMentalHealthCondition(mentalHealth);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(any());
    }

    @Test
    void getMentalHealthCondition() {
        AssessmentDTO request = TestDataProvider.getAssessmentDTO();
        request.setMemberReference(TestConstants.STRING_VALUE);
        Bundle observationBundle = TestDataProvider.getObservationBundle();
        request.setType(MetaCodeConstants.SUICIDE_SCREENER_KEY);
        request.setType(MetaCodeConstants.SUBSTANCE_ABUSE_KEY);
        Observation response = new Observation();
        response.setComponent(TestDataProvider.getObservstionComponentList());

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(observationBundle);
        AssessmentDTO result = mentalHealthService.getMentalHealthCondition(request);
        Assertions.assertNotNull(result);
    }

}
