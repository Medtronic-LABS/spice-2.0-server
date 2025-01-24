package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.RelationshipConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.APGARScoreDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabourDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RelationshipAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = org.mockito.quality.Strictness.LENIENT)
class LabourServiceImplTest {

    @InjectMocks
    private LabourServiceImpl labourService;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private HouseholdService householdService;

    @Mock
    private RelationshipAlgorithm relationshipAlgoritham;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private PatientService patientService;

    @Test
    void createLabourMotherAndNeonateMedicalReview() {
        //given
        LabourDTO labourDTO = new LabourDTO();
        labourDTO.setDateAndTimeOfDelivery(new Date());
        labourDTO.setNoOfNeoNates(TestConstants.INT_THREE);
        labourDTO.setNeonatePatientId(TestConstants.STRING_THREE);
        labourDTO.setDeliveryByOther(TestConstants.OTHER);
        labourDTO.setDateAndTimeOfLabourOnset(new Date());

        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDetailsData();
        encounterDetailsDTO.setHouseholdId(TestConstants.STRING_THREE);
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setHouseholdId(TestConstants.STRING_THREE);
        householdMemberDTO.setProvenance(TestDataProvider.getProvenance());
        householdMemberDTO.setPatientId(TestConstants.STRING_THREE);
        MotherDTO motherDTO = new MotherDTO();
        motherDTO.setEncounter(encounterDetailsDTO);
        motherDTO.setLabourDTO(labourDTO);

        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setId(TestConstants.STRING_THREE);
        householdDTO.setProvenance(TestDataProvider.getProvenance());
        householdDTO.setNoOfPeople(TestConstants.INT_THREE);

        NeonateDTO neonateDTO = new NeonateDTO();
        neonateDTO.setId(TestConstants.STRING_THREE);
        neonateDTO.setEncounter(encounterDetailsDTO);
        neonateDTO.setGender(TestConstants.MALE);

        MotherNeonateDTO motherNeonateDTO = TestDataProvider.getMotherNeonateDTO();
        motherNeonateDTO.setChild(householdMemberDTO);
        motherNeonateDTO.setMotherDTO(motherDTO);
        motherNeonateDTO.setNeonateDTO(neonateDTO);

        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

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

        List<String> ids = new ArrayList<>();
        ids.add(motherDTO.getId());

        String bundleDto = "";
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson), TestDataProvider.getHouseholdData());
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        ReflectionTestUtils.setField(labourService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(fhirUtils.initiateCodesMap()).thenReturn(new HashMap<>());
        when(relationshipAlgoritham.getChildRelationshipFromMother(householdMemberDTO.getHouseholdHeadRelationship())).thenReturn(RelationshipConstants.SON_DAUGHTER);
        when(householdService.getHouseholdById(householdMemberDTO.getHouseholdId())).thenReturn(householdDTO);
        when(householdService.getRelatedPersonCountByHouseholdId(householdDTO.getId())).thenReturn(TestConstants.INT_THREE);
        when(householdService.updateHousehold(householdDTO)).thenReturn(householdDTO);
        when(householdService.createHouseholdMember(householdMemberDTO)).thenReturn(householdMemberDTO);

        doNothing().when(patientService).setPatientReferenceInEncounterDetails(motherDTO.getEncounter(), bundle);
        when(patientService.createOrUpdateMedicalReviewEncounter(motherDTO.getId(), motherDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW, null, bundle)).thenReturn(TestConstants.STRING_THREE);
        when(fhirAssessmentMapper.setObservation(motherDTO.getEncounter(),
                Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW, Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW)).thenReturn(observation);
        when(fhirAssessmentMapper.createSignsObservation(motherDTO.getSigns(), motherDTO.getEncounter(),
                Constants.SIGNS, null)).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                motherDTO.getEncounter().getProvenance())).thenReturn(TestConstants.STRING_THREE);
        doNothing().when(patientService).closeAncDetails(bundle, new RequestDTO());
        doNothing().when(patientService).closePncDetails(bundle, new RequestDTO());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(motherDTO.getGeneralConditions(),
                Constants.GENERAL_CONDITION_OF_MOTHER, observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(motherDTO.getStateOfPerineum(),
                Constants.STATE_OF_THE_PERINEUM, observation.getComponent());
        when(fhirAssessmentMapper.createSignsObservation(motherDTO.getRiskFactors(),
                motherDTO.getEncounter(), Constants.RISK_FACTORS, null)).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle, motherDTO.getEncounter().getProvenance())).thenReturn(Constants.RISK_FACTORS);
        when(fhirAssessmentMapper.createSignsObservation(motherDTO.getStatus(), motherDTO.getEncounter(), Constants.STATUS, null)).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle, motherDTO.getEncounter().getProvenance())).thenReturn(Constants.RISK_FACTORS);
        doNothing().when(fhirAssessmentMapper).createObservationComponent(motherDTO.getTear(), Constants.TEAR, observation.getComponent());
        doNothing().when(fhirUtils).setBundle(TestConstants.URL, StringUtil.concatString(Constants.FHIR_BASE_URL), Bundle.HTTPVerb.POST,
                observation, bundle, motherDTO.getEncounter().getProvenance());

        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(fhirResponse)).thenReturn(new HashMap<>());

        when(fhirAssessmentMapper.setObservation(motherDTO.getEncounter(), Constants.LABOUR_DETAILS, Constants.LABOUR_DETAILS))
                .thenReturn(observation);
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDateAndTimeOfDelivery(),
                Constants.DATE_AND_TIME_OF_DELIVERY, observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, motherDTO.getEncounter(), Constants.PNC_VISIT_NUMBER,
                Constants.ONE, motherDTO.getEncounter().getPatientReference());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, motherDTO.getEncounter(), Constants.DATE_OF_DELIVERY,
                labourDTO.getDateAndTimeOfDelivery(), motherDTO.getEncounter().getPatientReference());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, motherDTO.getEncounter(), Constants.NO_OF_NEONATES,
                labourDTO.getNoOfNeoNates(), motherDTO.getEncounter().getPatientReference());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, motherDTO.getEncounter(), Constants.NEONATE_PATIENT_ID,
                labourDTO.getNeonatePatientId(), motherDTO.getEncounter().getPatientReference());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDeliveryByOther(), Constants.DELIVERY_BY_OTHER,
                observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDateAndTimeOfLabourOnset(),
                Constants.DATE_AND_TIME_OF_LABOUR_ONSET, observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDeliveryType(), Constants.DELIVERY_TYPE,
                observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDeliveryBy(), Constants.DELIVERY_BY,
                observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDeliveryAt(), Constants.DELIVERY_AT,
                observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getDeliveryStatus(),
                Constants.DELIVERY_STATUS, observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(labourDTO.getNoOfNeoNates(),
                Constants.NO_OF_NEONATES, observation.getComponent());
        doNothing().when(patientService).updatePatientStatus(bundle, Boolean.FALSE, motherDTO.getEncounter().getProvenance(),
                motherDTO.getEncounter().getPatientId(), null);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                motherDTO.getEncounter().getProvenance())).thenReturn(TestConstants.STRING_THREE);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle, motherDTO.getEncounter().getProvenance()))
                .thenReturn(TestConstants.OTHER);
        when(fhirAssessmentMapper.createSignsObservation(motherDTO.getStatus(),
                motherDTO.getEncounter(), Constants.STATUS, null)).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle, motherDTO.getEncounter().getProvenance()))
                .thenReturn(TestConstants.STRING_THREE);

        doNothing().when(patientService).setPatientReferenceInEncounterDetails(neonateDTO.getEncounter(), bundle);
        when(patientService.createOrUpdateMedicalReviewEncounter(neonateDTO.getId(), neonateDTO.getEncounter(),
                Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW, null, bundle)).thenReturn(TestConstants.STRING_THREE);
        when(fhirAssessmentMapper.setObservation(neonateDTO.getEncounter(),
                Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW, Constants.PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW)).thenReturn(observation);
        when(fhirAssessmentMapper.createSignsObservation(neonateDTO.getSigns(), neonateDTO.getEncounter(),
                Constants.SIGNS, null)).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(observation, bundle,
                neonateDTO.getEncounter().getProvenance())).thenReturn(TestConstants.STRING_THREE);
        doNothing().when(fhirAssessmentMapper).createObservationComponent(neonateDTO.getNeonateOutcome(),
                Constants.NEONATE_OUTCOME, observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(neonateDTO.getBirthWeight(), Constants.BIRTHWEIGHT,
                observation.getComponent(), Constants.KG);
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, neonateDTO.getEncounter(), Constants.WEIGHT,
                neonateDTO.getBirthWeight(), neonateDTO.getEncounter().getPatientReference());
        doNothing().when(fhirAssessmentMapper).createBirthHistory(neonateDTO.getSigns(), neonateDTO.getBirthWeight(),
                neonateDTO.getGestationalAge(), neonateDTO.getEncounter(), bundle);
        doNothing().when(fhirAssessmentMapper).createObservationComponent(neonateDTO.getGender().toLowerCase(), Constants.GENDER,
                observation.getComponent());
        doNothing().when(fhirAssessmentMapper).createObservationComponent(neonateDTO.getGestationalAge(),
                Constants.GESTATIONAL_AGE, observation.getComponent());

        //then
        Map<String, String> response = labourService.createLabourMotherAndNeonateMedicalReview(motherNeonateDTO);
        Assertions.assertNotNull(response);
        Assertions.assertNotNull(ids);
    }

    @Test
    void getBirthHistory() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.GESTATIONAL_AGE_CATEGORY);
        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));

        List<Observation.ObservationComponentComponent> theComponent = new ArrayList<>();

        Observation.ObservationComponentComponent observationComponentComponent = new Observation.ObservationComponentComponent();
        observationComponentComponent.setCode(codeableConcept);
        theComponent.add(observationComponentComponent);
        observation.setComponent(theComponent);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        bundle.setTotal(TestConstants.INT_ONE);
        List<String> types = new ArrayList<>();
        types.add(Constants.BIRTH_HISTORY);

        //when
        when(fhirAssessmentMapper.getPatientBasicDetails(TestConstants.STRING_THREE, types)).thenReturn(bundle);
        //then
        BirthHistoryDTO response = labourService.getBirthHistory(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.BIRTHWEIGHT_CATEGORY);
        //then
        response = labourService.getBirthHistory(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);

        //given
        codeableConcept.setText(Constants.HAVE_BREATHING_PROBLEMS);
        //then
        response = labourService.getBirthHistory(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);

        //then
        response = labourService.getBirthHistory(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void setNeonateDetails() {
        //given
        APGARScoreDTO apgarScoreDTO = new APGARScoreDTO();
        apgarScoreDTO.setOneMinuteTotalScore(TestConstants.INT_ONE);
        apgarScoreDTO.setFiveMinuteTotalScore(TestConstants.INT_ONE);
        apgarScoreDTO.setTenMinuteTotalScore(TestConstants.INT_ONE);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        NeonateDTO neonateDTO = new NeonateDTO();
        neonateDTO.setEncounter(TestDataProvider.getEncounterDetailsData());
        neonateDTO.setApgarScoreOneMinuteDTO(apgarScoreDTO);
        neonateDTO.setApgarScoreFiveMinuteDTO(apgarScoreDTO);
        neonateDTO.setApgarScoreTenMinuteDTO(apgarScoreDTO);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.RESPIRATION);

        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));


        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL);
        identifier.setValue(Constants.APGAR_SCORE_TEN_MINUTE);

        observation.setIdentifier(List.of(identifier));

        List<Observation.ObservationComponentComponent> theComponent = new ArrayList<>();

        Observation.ObservationComponentComponent observationComponentComponent = new Observation.ObservationComponentComponent();
        observationComponentComponent.setCode(codeableConcept);
        observationComponentComponent.getValueQuantity().setValue(TestConstants.INT_ONE);
        theComponent.add(observationComponentComponent);
        observation.setComponent(theComponent);
        observation.getValueQuantity().setValue(TestConstants.INT_ONE);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //then
        labourService.setNeonateDetails(bundle, neonateDTO);
        Assertions.assertNotNull(bundle);
    }
}