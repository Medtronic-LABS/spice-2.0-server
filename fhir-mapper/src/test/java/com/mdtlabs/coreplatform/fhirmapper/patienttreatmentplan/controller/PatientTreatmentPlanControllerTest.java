package com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientTreatmentPlanControllerTest {

    @InjectMocks
    private PatientTreatmentPlanController patientTreatmentPlanController;

    @Mock
    private PatientTreatmentPlanService patientTreatmentPlanService;

    @Test
    void getPatientTreatmentPlan() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientTreatmentPlanService.getPatientTreatmentPlanDetails(requestDTO)).thenReturn(new TreatmentPlanResponseDTO());

        //then
        TreatmentPlanResponseDTO response = patientTreatmentPlanController.getPatientTreatmentPlan(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateTreatmentPlanData() {
        //given
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();

        //when
        doNothing().when(patientTreatmentPlanService).updateTreatmentPlanData(treatmentPlanDTO);

        //then
        patientTreatmentPlanController.updateTreatmentPlanData(treatmentPlanDTO);
        verify(patientTreatmentPlanService).updateTreatmentPlanData(treatmentPlanDTO);
    }
}