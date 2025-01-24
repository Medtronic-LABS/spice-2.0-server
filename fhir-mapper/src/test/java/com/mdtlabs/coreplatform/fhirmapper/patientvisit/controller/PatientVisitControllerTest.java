package com.mdtlabs.coreplatform.fhirmapper.patientvisit.controller;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientVisitControllerTest {

    @InjectMocks
    private PatientVisitController patientVisitController;

    @Mock
    private PatientVisitService patientVisitService;

    @Test
    void createOrUpdatePatientVisit() {
        //given
        PatientVisitDTO patientVisitDTO = new PatientVisitDTO();

        //when
        when(patientVisitService.createPatientVisit(patientVisitDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, Object> response = patientVisitController.createOrUpdatePatientVisit(patientVisitDTO);
        Assertions.assertNotNull(response);
    }
}