package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Encounter;
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
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.SpiceServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabourDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncChildMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMotherMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RelationshipAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MedicalReviewPregnancyPNCServiceImplTest {

    @InjectMocks
    MedicalReviewPregnancyPNCServiceImpl medicalReviewPregnancyPNCService;

    @Mock
    FhirUtils fhirUtils;

    @Mock
    FhirMapper fhirMapper;

    @Mock
    FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    PatientService patientService;

    @Mock
    RestApiUtil restApiUtil;

    @Mock
    private LabourServiceImpl labourService;

    @Mock
    private PrescriptionRequestService prescriptionRequestService;

    @Mock
    private InvestigationService investigationService;

    @Mock
    private RelationshipAlgorithm relationshipAlgoritham;

    @Mock
    private HouseholdService householdService;

    @Mock
    private SpiceServiceApiInterface spiceServiceApiInterface;

    @Test
    void savePncMedicalReview() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Reference reference = new Reference();
        reference.setId(TestConstants.TWO_STR);

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.setText(Constants.SYSTEMIC_EXAMINATIONS);
        Observation observation = new Observation();
        observation.setCode(codeableConcept);
        observation.setPerformer(List.of(reference));

        List<Observation.ObservationComponentComponent> theComponent = new ArrayList<>();
        observation.setComponent(theComponent);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(observation);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDetailsData();
        encounterDetailsDTO.setPatientId(TestConstants.PATIENT_ID);

        PncMedicalReviewDTO pncMedicalReviewDTO = new PncMedicalReviewDTO();
        PncMotherMedicalReviewDTO pncMotherMedicalReviewDTO = new PncMotherMedicalReviewDTO();
        pncMotherMedicalReviewDTO.setEncounter(TestDataProvider.getEncounterDetailsData());
        pncMotherMedicalReviewDTO.setNeonateOutcome(TestConstants.TWO_STR);
        pncMotherMedicalReviewDTO.setLabourDTO(new LabourDTO());
        pncMotherMedicalReviewDTO.setClinicalNotes(TestConstants.TWO_STR);
        PncChildMedicalReviewDTO pncChildMedicalReviewDTO = new PncChildMedicalReviewDTO();
        pncChildMedicalReviewDTO.setIsChildAlive(Boolean.FALSE);
        pncChildMedicalReviewDTO.setEncounter(encounterDetailsDTO);

        pncMedicalReviewDTO.setPncMother(pncMotherMedicalReviewDTO);
        pncMedicalReviewDTO.setPncChild(pncChildMedicalReviewDTO);

        RequestDTO request = new RequestDTO();
        request.setMemberId(TestConstants.STRING_VALUE);

        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setAncVisitNo(TestConstants.INT_THREE);
        pregnancyInfo.setAncMedicalReviewVisitNo(TestConstants.INT_THREE);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                pncMotherMedicalReviewDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(medicalReviewPregnancyPNCService, "fhirServerUrl", "fhirServerUrl");

        //when
        doNothing().when(patientService).setPatientReferenceInEncounterDetails(pncMedicalReviewDTO
                .getPncMother().getEncounter(), bundle);
        when(patientService.createOrUpdateMedicalReviewEncounter(pncMedicalReviewDTO.getPncMother().getId(),
                pncMedicalReviewDTO.getPncMother().getEncounter(), Constants.PNC_MOTHER_MEDICAL_REVIEW,
                null, bundle)).thenReturn(TestConstants.STRING_VALUE);
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, pncMedicalReviewDTO.getPncMother().getEncounter(),
                Constants.PNC_MOTHER_MEDICAL_REVIEW, pncMedicalReviewDTO.getPncMother().getEncounter().getVisitNumber(),
                pncMedicalReviewDTO.getPncMother().getEncounter().getPatientReference());
        when(fhirAssessmentMapper.setObservation(pncMedicalReviewDTO.getPncMother().getEncounter(),
                Constants.MOTHER_ALIVE, Constants.PNC_MOTHER_MEDICAL_REVIEW)).thenReturn(observation);
        doNothing().when(fhirAssessmentMapper).setValuesToObservation(observation,
                null, pncMedicalReviewDTO.getPncMother().getIsMotherAlive(), null);
        when(patientService.getPatientVitals(request)).thenReturn(pregnancyInfo);
        doNothing().when(patientService).closeAncDetails(bundle, new RequestDTO());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);
        when(fhirAssessmentMapper.setObservation(any(), anyString(), anyString())).thenReturn(observation);
        when(fhirAssessmentMapper.createSignsObservation(any(), any(), any(), any())).thenReturn(observation);

        //then
        Map<String, String> response = medicalReviewPregnancyPNCService.savePncMedicalReview(pncMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPNCMedicalReviewDetails() {
        //given
        String motherId = TestConstants.TWO_STR;
        String childId = TestConstants.ONE_STR;
        String motherPatientReference = TestConstants.PATIENT_REFERENCE;
        Bundle bundle = new Bundle();
        Encounter encounter = TestDataProvider.getEncounter();
        List<Identifier> theIdentifier = new ArrayList<>();
        theIdentifier.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue(TestConstants.TWO_STR));
        theIdentifier.add(new Identifier().setSystem(FhirIdentifierConstants.VISIT_NUMBER_SYSTEM_URL).setValue(TestConstants.TWO_STR));
        encounter.setIdentifier(theIdentifier);
        Reference reference = new Reference();
        reference.setReference(TestConstants.BLANK_STRING);
        bundle.addEntry().setResource(encounter);

        //when
        when(fhirAssessmentMapper.getEncounterDetails(motherId, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getEncounterDetails(childId, Boolean.TRUE)).thenReturn(bundle);

        //then
        PncMedicalReviewDTO result = medicalReviewPregnancyPNCService.getPNCMedicalReviewDetails(motherId, childId, motherPatientReference);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPNCMedicalReviewDetailsForCondition() {
        //given
        String motherId = TestConstants.TWO_STR;
        String childId = TestConstants.ONE_STR;
        String motherPatientReference = TestConstants.PATIENT_REFERENCE;
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getCondition());

        //when
        when(fhirAssessmentMapper.getEncounterDetails(motherId, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getEncounterDetails(childId, Boolean.TRUE)).thenReturn(bundle);

        //then
        PncMedicalReviewDTO result = medicalReviewPregnancyPNCService.getPNCMedicalReviewDetails(motherId, childId, motherPatientReference);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPNCMedicalReviewDetailsForPatient() {
        //given
        String motherId = TestConstants.TWO_STR;
        String childId = TestConstants.ONE_STR;
        String motherPatientReference = TestConstants.PATIENT_REFERENCE;
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getPatient());

        //when
        when(fhirAssessmentMapper.getEncounterDetails(motherId, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getEncounterDetails(childId, Boolean.TRUE)).thenReturn(bundle);

        //then
        PncMedicalReviewDTO result = medicalReviewPregnancyPNCService.getPNCMedicalReviewDetails(motherId, childId, motherPatientReference);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPNCMedicalReviewDetailsForObservation() {
        //given
        String motherId = TestConstants.TWO_STR;
        String childId = TestConstants.ONE_STR;
        String motherPatientReference = TestConstants.PATIENT_REFERENCE;
        Bundle bundle = new Bundle();
        Observation observation = new Observation();
        List<Identifier> theIdentifier = new ArrayList<>();
        for (String name : List.of("presentingComplaints", "systemicExaminations", "PNC_MOTHER_REVIEW", "clinicalNotes", "physicalExaminations", "PNC_CHILD_REVIEW", "pncMotherPhysicalExaminations", "pncMotherSystemicExamination")) {
            theIdentifier.add(new Identifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue(name));
        }
        List<Observation.ObservationComponentComponent> theComponents = new ArrayList<>();
        for (String name : List.of("cordExamination", "congenitalDetect", "breastFeeding", "exclusiveBreastFeeding", "breastCondition", "breastConditionCondition", "involutionsOfTheUterus", "involutionsOfTheUterusCondition")) {
            Observation.ObservationComponentComponent component = new Observation.ObservationComponentComponent();
            component.setCode(new CodeableConcept().setText(name));
            theComponents.add(component);
        }
        observation.setComponent(theComponents);
        observation.setIdentifier(theIdentifier);
        Reference reference = new Reference();
        reference.setReference(TestConstants.BLANK_STRING);
        bundle.addEntry().setResource(observation);

        //when
        when(fhirAssessmentMapper.getEncounterDetails(motherId, Boolean.TRUE)).thenReturn(bundle);
        when(fhirAssessmentMapper.getEncounterDetails(childId, Boolean.TRUE)).thenReturn(bundle);

        //then
        PncMedicalReviewDTO result = medicalReviewPregnancyPNCService.getPNCMedicalReviewDetails(motherId, childId, motherPatientReference);
        Assertions.assertNotNull(result);
    }

    @Test
    void createPncChild() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        //then
        Assertions.assertThrows(SpiceValidation.class, () -> medicalReviewPregnancyPNCService.createPncChild(householdMemberDTO));

        //given
        householdMemberDTO.setMotherPatientId(TestConstants.ONE_STR);
        householdMemberDTO.setMemberId(TestConstants.ONE_TWO_THREE);
        householdMemberDTO.setDateOfBirth(new Date());
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                householdMemberDTO);

        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(medicalReviewPregnancyPNCService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(labourService.createChild(householdMemberDTO.getHouseholdId(),
                householdMemberDTO)).thenReturn(householdMemberDTO);
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, encounterDetailsDTO, Constants.NEONATE_PATIENT_ID,
                householdMemberDTO.getPatientId(), householdMemberDTO.getMotherPatientId());
        doNothing().when(fhirAssessmentMapper).createVitalObservation(bundle, encounterDetailsDTO, Constants.DATE_OF_DELIVERY,
                householdMemberDTO.getDateOfBirth(),null);
        when(restApiUtil.constructRequestEntity(bundle)).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        medicalReviewPregnancyPNCService.createPncChild(householdMemberDTO);
        verify(labourService).createChild(householdMemberDTO.getHouseholdId(),
                householdMemberDTO);
    }

    @Test
    void createChildWithHouseHoldIsNull() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setHouseholdId(null);
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        TestDataProvider.init();

        //when
        when(householdService.getHouseholdById(any())).thenReturn(householdDTO);
        when(householdService.getRelatedPersonCountByHouseholdId(householdDTO.getId())).thenReturn(TestConstants.TEN);
        when(householdService.createHouseholdMember(any())).thenReturn(householdMemberDTO);

        //then
        HouseholdMemberDTO result = medicalReviewPregnancyPNCService.createChild(householdMemberDTO);
        Assertions.assertNotNull(result);
        TestDataProvider.cleanUp();
    }

    @Test
    void createChild() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();

        //when
        when(householdService.getHouseholdById(any())).thenReturn(householdDTO);
        when(householdService.getRelatedPersonCountByHouseholdId(householdDTO.getId())).thenReturn(TestConstants.TEN);
        when(householdService.createHouseholdMember(any())).thenReturn(householdMemberDTO);

        //then
        HouseholdMemberDTO result = medicalReviewPregnancyPNCService.createChild(householdMemberDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void createLabourMedicalReview() {
        //given
        PncMotherMedicalReviewDTO motherDTO = TestDataProvider.getPncMotherMedicalReviewData();
        LabourDTO labourDTO = new LabourDTO();
        labourDTO.setDateAndTimeOfLabourOnset(new Date());
        labourDTO.setDateAndTimeOfDelivery(new Date());
        labourDTO.setNoOfNeoNates(TestConstants.INT_ONE);
        labourDTO.setNeonatePatientId(TestConstants.PATIENT_ID);
        labourDTO.setDeliveryByOther(TestConstants.OTHER);
        motherDTO.setLabourDTO(labourDTO);
        Bundle bundle = new Bundle();
        Observation observation = TestDataProvider.getHeightObservation();

        //when
        when(fhirAssessmentMapper.setObservation(any(), anyString(), anyString())).thenReturn(observation);
        when(fhirAssessmentMapper.addObservationToBundle(any(), any(), any())).thenReturn(TestConstants.BLANK_STRING);

        //then
        String result = medicalReviewPregnancyPNCService.createLabourMedicalReview(motherDTO, bundle);
        Assertions.assertNotNull(result);
    }
}