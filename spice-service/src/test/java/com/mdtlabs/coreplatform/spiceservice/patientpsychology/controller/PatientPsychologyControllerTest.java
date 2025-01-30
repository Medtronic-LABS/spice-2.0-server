package com.mdtlabs.coreplatform.spiceservice.patientpsychology.controller;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patientpsychology.service.PatientPsychologyService;

import org.junit.jupiter.api.Assertions;
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
class PatientPsychologyControllerTest {
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
        SuccessResponse<PsychologyDTO> result = patientPsychologyController.getPsychologyNotes(request);
        Assertions.assertNotNull(result);
    }

    @Test
    void savePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        when(patientPsychologyService.savePsychologyData(request)).thenReturn(request);
        SuccessResponse<PsychologyDTO> response = patientPsychologyController.savePsychologyNotes(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();
        when(patientPsychologyService.updatePsychologyData(request)).thenReturn(request);
        SuccessResponse<PsychologyDTO> response = patientPsychologyController.updatePsychologyNotes(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void removePsychologyNotes() {
        PsychologyDTO request = new PsychologyDTO();

        when(patientPsychologyService.removePsychologyDataById(request)).thenReturn(request);
        SuccessResponse<PsychologyDTO> response = patientPsychologyController.removePsychologyNotes(request);
        Assertions.assertNotNull(response);
    }
}
