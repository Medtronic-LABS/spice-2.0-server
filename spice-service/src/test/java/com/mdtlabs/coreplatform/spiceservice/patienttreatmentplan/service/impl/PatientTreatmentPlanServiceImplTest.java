package com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class PatientTreatmentPlanServiceImplTest {

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @InjectMocks
    private PatientTreatmentPlanServiceImpl patientTreatmentPlanService;

    @BeforeEach
    public void setup() {
        TestDataProvider.init();
    }

    @AfterEach
    public void close() {
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetPatientTreatmentPlanDetails() {
        TestDataProvider.getStaticMock();
        RequestDTO request = new RequestDTO();
        patientTreatmentPlanService.getPatientTreatmentPlanDetails(request);
        verify(fhirServiceApiInterface, times(1)).getPatientTreatmentPlan(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    @Test
    void testUpdateTreatmentPlanData() {
        TestDataProvider.getStaticMock();
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();
        patientTreatmentPlanService.updateTreatmentPlanData(treatmentPlanDTO);
        verify(fhirServiceApiInterface, times(1)).updateTreatmentPlanData(CommonUtil.getAuthToken(), CommonUtil.getClient(), treatmentPlanDTO);
    }
}