package com.mdtlabs.coreplatform.spiceservice.patientvisit.controller;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.spiceservice.patientvisit.service.PatientVisitService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class PatientVisitControllerTest {

    @Mock
    private PatientVisitService patientVisitService;

    @InjectMocks
    private PatientVisitController patientVisitController;

    @Test
    void testAddPatientVisit() {
        patientVisitController.addPatientVisit(new PatientVisitDTO());
        verify(patientVisitService, times(1)).createPatientVisit(new PatientVisitDTO());
    }
}