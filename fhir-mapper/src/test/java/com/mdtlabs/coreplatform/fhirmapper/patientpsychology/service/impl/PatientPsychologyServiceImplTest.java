package com.mdtlabs.coreplatform.fhirmapper.patientpsychology.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Reference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpServerErrorException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NoteAssessment;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientPsychologyServiceImplTest {
    @InjectMocks
    private PatientPsychologyServiceImpl psychologyService;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Test
    void removePsychologyDataByIdThrowExceptionWhenIdIsNull() {
        PsychologyDTO request = new PsychologyDTO();
        assertThrows(Validation.class, () -> psychologyService.removePsychologyDataById(request));
    }

    @Test
    void removePsychologyDataByIdWhenRequestIsValid() {
        PsychologyDTO request = new PsychologyDTO();
        request.setId(1L);

        Bundle bundle = new Bundle();
        Observation observation = new Observation();
        observation.setId("observationId");
        bundle.addEntry().setResource(observation);
        when(restApiUtil.getBatchRequest(any(String.class))).thenReturn(bundle);
        when(restApiUtil.getObservationById(any())).thenReturn(observation);
        PsychologyDTO actualResponse = psychologyService.removePsychologyDataById(request);
        assertEquals(request, actualResponse);
    }

    @Test
    void removePsychologyDataByIdWhenNoObservationFound() {
        PsychologyDTO request = new PsychologyDTO();
        request.setId(1L);
        Bundle bundle = new Bundle();
        when(restApiUtil.getBatchRequest(any(String.class))).thenReturn(bundle);
        Observation observation = new Observation();
        observation.setId("observation123");
        when(restApiUtil.getObservationById(any())).thenReturn(observation);
        PsychologyDTO actualResponse = psychologyService.removePsychologyDataById(request);
        assertEquals(request, actualResponse);
    }

    @Test
    void getPatientPsychologyByRelatedPersonIdThrowsValidationExceptionWhenRequestIsNotValid() {
        PsychologyDTO request = new PsychologyDTO();
        request.setMemberReference(null);
        Validation exception = assertThrows(Validation.class, () -> {
            psychologyService.getPatientPsychologyByRelatedPersonId(request);
        });
        assertEquals(1033, exception.getCode());
    }

    @Test
    void getPatientPsychologyByRelatedPersonIdApiErrorThrowsDataNotFoundException() {
        PsychologyDTO request = new PsychologyDTO();
        request.setMemberReference(TestConstants.ONE);
        ProvenanceDTO provenance = new ProvenanceDTO();
        provenance.setUserId("validUserId");
        request.setProvenance(provenance);

        when(restApiUtil.getBatchRequest(any(String.class)))
                .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));

        DataNotFoundException exception = assertThrows(DataNotFoundException.class, () -> {
            psychologyService.getPatientPsychologyByRelatedPersonId(request);
        });
        assertEquals(1035, exception.getCode());
    }

    @Test
    void getPatientPsychologyByRelatedPersonIdWhenRequestIsValid() {
        PsychologyDTO request = new PsychologyDTO();
        request.setMemberReference(123L);
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId("provenanceUser123");
        request.setProvenance(provenanceDTO);
        Bundle bundle = getSampleBundle();

        when(restApiUtil.getBatchRequest(any(String.class))).thenReturn(bundle);

        List<PsychologyDTO> result = psychologyService.getPatientPsychologyByRelatedPersonId(request);
        assertNotNull(result);
    }

    private Bundle getSampleBundle() {

        // Create a Bundle
        Bundle bundle = new Bundle();

        // Create Practitioner resources
        Practitioner practitioner = new Practitioner();
        practitioner.setId("practitioner1");
        HumanName name = new HumanName().setFamily("Doe").addGiven("John");
        practitioner.addName(name);
        practitioner.setGender(Enumerations.AdministrativeGender.MALE);

        // Add Practitioner to the Bundle
        Bundle.BundleEntryComponent practitionerEntry = new Bundle.BundleEntryComponent();
        practitionerEntry.setResource(practitioner);
        bundle.addEntry(practitionerEntry);

        // Create Observation resources
        Observation noteObservation = new Observation();
        noteObservation.setId(String.valueOf(123));
        noteObservation.setCode(new CodeableConcept().setText("Psychology Note"));
        noteObservation.addIdentifier().setValue("psychologyNotes");
        noteObservation.addPerformer().setReference("Practitioner/practitioner1");

        Annotation note = new Annotation();
        note.setText("Patient has anxiety.");

        List<Annotation> observationNotes = new ArrayList<>();
        observationNotes.add(note);
        noteObservation.setNote(observationNotes);

        Observation assessmentObservation = new Observation();
        assessmentObservation.setId(String.valueOf(12345));
        assessmentObservation.setCode(new CodeableConcept().setText("Psychology Assessment"));
        assessmentObservation.addIdentifier().setValue("psychologyAssessment");
        assessmentObservation.addPerformer().setReference("Practitioner/practitioner1");
        List<Annotation> assessmentNotes = new ArrayList<>();
        assessmentNotes.add(new Annotation().setText("psychology assessment"));
        assessmentObservation.setNote(assessmentNotes);

        // Add Observations to the Bundle
        Bundle.BundleEntryComponent noteEntry = new Bundle.BundleEntryComponent();
        noteEntry.setResource(noteObservation);
        bundle.addEntry(noteEntry);

        Bundle.BundleEntryComponent assessmentEntry = new Bundle.BundleEntryComponent();
        assessmentEntry.setResource(assessmentObservation);
        bundle.addEntry(assessmentEntry);

        return bundle;

    }


    @Test
    void savePatientPsychologyThrowExceptionWhenEncounterNotFound() {
        PsychologyDTO request = new PsychologyDTO();
        request.setPatientVisitId(1L);
        when(restApiUtil.getEncounterById(anyString())).thenReturn(null);
        BadRequestException exception = assertThrows(BadRequestException.class, () -> {
            psychologyService.savePatientPsychology(request);
        });
        assertEquals(1032, exception.getCode());
    }

    @Test
    void savePatientPsychologyWhenClinicianNotesOrAssessmentsNotPresent() {
        PsychologyDTO request = new PsychologyDTO();
        request.setPatientVisitId(1L);
        Encounter encounter = new Encounter();
        when(restApiUtil.getEncounterById("1")).thenReturn(encounter);

        Validation thrown = assertThrows(Validation.class, () -> {
            psychologyService.savePatientPsychology(request);
        });
        assertEquals(1037, thrown.getCode());
    }

    @Test
    void savePatientPsychologyWhenResponseIsNull() {
        PsychologyDTO request = new PsychologyDTO();
        request.setPatientVisitId(1L);

        Encounter encounter = new Encounter();
        when(restApiUtil.getEncounterById("1")).thenReturn(encounter);

        when(fhirAssessmentMapper.createNoteObservation(any(), any(), any(), any(), any(), anyBoolean(), any()))
                .thenReturn(new Observation());

        ResponseEntity<FhirResponseDTO> responseEntity = null;
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        assertThrows(Validation.class, () -> psychologyService.savePatientPsychology(request));
    }


    @Test
    void savePatientPsychology_Success() {
        // Arrange
        PsychologyDTO request = new PsychologyDTO();
        request.setPatientVisitId(1L);
        request.setClinicianNotes(Collections.singletonList("Note 1"));

        Reference subject = new Reference();
        subject.setId(String.valueOf(123));
        subject.setReference("subject-reference");

        Reference individual = new Reference();
        individual.setId(String.valueOf(1234));
        individual.setReference("individual-reference");

        Encounter.EncounterParticipantComponent component = new Encounter.EncounterParticipantComponent();
        component.setIndividual(individual);

        List<Encounter.EncounterParticipantComponent> participants = new ArrayList<>();
        participants.add(component);

        Encounter encounter = new Encounter();
        encounter.setId(String.valueOf(123));
        encounter.setSubject(subject);
        encounter.setParticipant(participants);

        when(restApiUtil.getEncounterById("1")).thenReturn(encounter);
        when(fhirAssessmentMapper.createNoteObservation(any(), any(), any(), any(), any(), anyBoolean(), any()))
                .thenReturn(new Observation());
        when(fhirAssessmentMapper.addObservationToBundle(any(), any(), any())).thenReturn(null); // Assuming void method

        List<String> fhirIds = new ArrayList<>();
        fhirIds.add("sda");
        fhirIds.add("sdas");
        // Instantiate FhirResponseDTO with the list of IDs
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId(String.valueOf(123));
        // Create a ResponseEntity containing the FHIR response DTO
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        // Mocking the postBatchRequest method to return the valid ResponseEntity
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        // Act
        PsychologyDTO responseDTO = psychologyService.savePatientPsychology(request);

        // Assert
        assertNotNull(responseDTO);
        assertEquals(request.getPatientVisitId(), responseDTO.getPatientVisitId());
    }

    @Test
    void savePatientPsychologySuccessWithCounselorAssessments() {
        List<NoteAssessment> noteAssessments = new ArrayList<>();
        NoteAssessment noteAssessment = new NoteAssessment();
        noteAssessment.setCounselorAssessment("asa");
        noteAssessment.setId(123L);
        noteAssessments.add(noteAssessment);

        PsychologyDTO request = new PsychologyDTO();
        request.setPatientVisitId(1L);
        request.setCounselorAssessments(noteAssessments);

        Reference subject = new Reference();
        subject.setId(String.valueOf(123));
        subject.setReference("subject-reference");

        Reference individual = new Reference();
        individual.setId(String.valueOf(1234));
        individual.setReference("individual-reference");

        Encounter.EncounterParticipantComponent component = new Encounter.EncounterParticipantComponent();
        component.setIndividual(individual);

        List<Encounter.EncounterParticipantComponent> participants = new ArrayList<>();
        participants.add(component);

        Encounter encounter = new Encounter();
        encounter.setId(String.valueOf(123));
        encounter.setSubject(subject);
        encounter.setParticipant(participants);

        List<String> fhirIds = new ArrayList<>();
        fhirIds.add("sda");
        fhirIds.add("sdas");
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId(String.valueOf(123));
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        when(restApiUtil.getEncounterById("1")).thenReturn(encounter);
        when(fhirAssessmentMapper.createNoteObservation(any(), any(), any(), any(), any(), anyBoolean(), any()))
                .thenReturn(new Observation());
        when(fhirAssessmentMapper.addObservationToBundle(any(), any(), any())).thenReturn(null); // Assuming void method
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        PsychologyDTO responseDTO = psychologyService.savePatientPsychology(request);

        assertNotNull(responseDTO);
        assertEquals(request.getPatientVisitId(), responseDTO.getPatientVisitId());
    }

    @Test
    void savePatientPsychologySuccessWithCounselorAssementString() {
        PsychologyDTO request = new PsychologyDTO();
        request.setPatientVisitId(1L);
        request.setCounselorAssessment("counsellor assessment");

        Reference subject = new Reference();
        subject.setId(String.valueOf(123));
        subject.setReference("subject-reference");

        Reference individual = new Reference();
        individual.setId(String.valueOf(1234));
        individual.setReference("individual-reference");

        Encounter.EncounterParticipantComponent component = new Encounter.EncounterParticipantComponent();
        component.setIndividual(individual);

        List<Encounter.EncounterParticipantComponent> participants = new ArrayList<>();
        participants.add(component);

        Encounter encounter = new Encounter();
        encounter.setId(String.valueOf(123));
        encounter.setSubject(subject);
        encounter.setParticipant(participants);

        List<String> fhirIds = new ArrayList<>();
        fhirIds.add("sda");
        fhirIds.add("sdas");

        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId(String.valueOf(123));
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        when(restApiUtil.getEncounterById("1")).thenReturn(encounter);
        when(fhirAssessmentMapper.createNoteObservation(any(), any(), any(), any(), any(), anyBoolean(), any()))
                .thenReturn(new Observation());
        when(fhirAssessmentMapper.addObservationToBundle(any(), any(), any())).thenReturn(null); // Assuming void method

        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        PsychologyDTO responseDTO = psychologyService.savePatientPsychology(request);

        assertNotNull(responseDTO);
        assertEquals(request.getPatientVisitId(), responseDTO.getPatientVisitId());
        // Verify interactions
        verify(restApiUtil).getEncounterById("1");
        verify(fhirAssessmentMapper).createNoteObservation(any(), any(), any(), any(), any(), anyBoolean(), any());
        verify(fhirAssessmentMapper).addObservationToBundle(any(), any(), any());
        verify(restApiUtil).postBatchRequest(any(), any());
    }
}
