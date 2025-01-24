package com.mdtlabs.coreplatform.spiceservice.patientpsychology.controller;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.patientpsychology.service.PatientPsychologyService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class PatientPsychologyControllerTest {
    @InjectMocks
    private PatientPsychologyController patientPsychologyController;
    @Mock
    private PatientPsychologyService patientPsychologyService;

    @Test
    void getPsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        List<PsychologyDTO> psychologyDTOList = new ArrayList<>();
        psychologyDTOList.add(request);

        when(patientPsychologyService.getPsychologyDataByUserIdAndRelatedPersonId(request)).thenReturn(psychologyDTOList);
        patientPsychologyController.getPsychologyNotes(request);
    }

    @Test
    void savePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        List<PsychologyDTO> psychologyDTOList = new ArrayList<>();
        psychologyDTOList.add(request);
        when(patientPsychologyService.savePsychologyData(request)).thenReturn(request);
        patientPsychologyController.savePsychologyNotes(request);
    }

    @Test
    void updatePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        when(patientPsychologyService.updatePsychologyData(request)).thenReturn(request);
        patientPsychologyController.updatePsychologyNotes(request);
    }

    @Test
    void removePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();

        when(patientPsychologyService.removePsychologyDataById(request)).thenReturn(request);
        patientPsychologyController.removePsychologyNotes(request);
    }
}
