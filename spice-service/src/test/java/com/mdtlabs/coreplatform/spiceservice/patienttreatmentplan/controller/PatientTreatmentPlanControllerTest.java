package com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.controller;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;

import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.PatientTreatmentPlanService;

@ExtendWith(MockitoExtension.class)
class PatientTreatmentPlanControllerTest {

    @Mock
    private PatientTreatmentPlanService treatmentPlanService;

    @InjectMocks
    private PatientTreatmentPlanController patientTreatmentPlanController;


    @Test
    void testGetPatientTreatmentPlan() {
        RequestDTO request = new RequestDTO();
        patientTreatmentPlanController.getPatientTreatmentPlan(request);
        verify(treatmentPlanService, atLeastOnce()).getPatientTreatmentPlanDetails(request);
    }

    @Test
    void testUpdateTreatmentPlanData() {
        TreatmentPlanDTO request = new TreatmentPlanDTO();
        patientTreatmentPlanController.updateTreatmentPlanData(request);
        verify(treatmentPlanService, atLeastOnce()).updateTreatmentPlanData(request);
    }

}