package com.mdtlabs.coreplatform.fhirmapper.patientpsychology.controller;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.fhirmapper.patientpsychology.service.PatientPsychologyService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientPsychologyControllerTest {

    @InjectMocks
    private PatientPsychologyController patientPsychologyController;

    @Mock
    private PatientPsychologyService psychologyService;

    @Test
    void savePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        request.setId(1L);
        PsychologyDTO expectedResponse = new PsychologyDTO();
        expectedResponse.setId(1L);
        when(psychologyService.savePatientPsychology(any(PsychologyDTO.class))).thenReturn(expectedResponse);
        PsychologyDTO actualResponse = patientPsychologyController.savePsychologyNotes(request);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void updatePsychology() {
        PsychologyDTO request = new PsychologyDTO();
        request.setId(1L);
        PsychologyDTO expectedResponse = new PsychologyDTO();
        expectedResponse.setId(1L);
        when(psychologyService.savePatientPsychology(any(PsychologyDTO.class))).thenReturn(expectedResponse);
        PsychologyDTO actualResponse = patientPsychologyController.updatePsychology(request);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void getPatientPsychology() {
        PsychologyDTO request = new PsychologyDTO();
        request.setId(1L);
        List<PsychologyDTO> expectedList = Collections.singletonList(new PsychologyDTO());
        when(psychologyService.getPatientPsychologyByRelatedPersonId(any(PsychologyDTO.class))).thenReturn(expectedList);
        List<PsychologyDTO> actualList = patientPsychologyController.getPatientPsychology(request);
        assertEquals(expectedList, actualList);
    }

    @Test
    void removePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        request.setId(1L);
        PsychologyDTO expectedResponse = new PsychologyDTO();
        expectedResponse.setId(1L);
        when(psychologyService.removePsychologyDataById(any(PsychologyDTO.class))).thenReturn(expectedResponse);
        PsychologyDTO actualResponse = patientPsychologyController.removePsychologyNotes(request);
        assertEquals(expectedResponse, actualResponse);
    }
}
