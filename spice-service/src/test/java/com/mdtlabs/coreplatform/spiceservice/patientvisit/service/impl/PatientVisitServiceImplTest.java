package com.mdtlabs.coreplatform.spiceservice.patientvisit.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientVisitDTO;
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
class PatientVisitServiceImplTest {

    @Mock
    FhirServiceApiInterface fhirServiceApiInterface;

    @InjectMocks
    PatientVisitServiceImpl patientVisitService;

    @BeforeEach
    public void setup() {
        TestDataProvider.init();
    }

    @AfterEach
    public void close() {
        TestDataProvider.cleanUp();
    }

    @Test
    void testCreatePatientVisit() {
        TestDataProvider.getStaticMock();
        PatientVisitDTO patientVisitDTO = new PatientVisitDTO();
        patientVisitService.createPatientVisit(patientVisitDTO);
        verify(fhirServiceApiInterface, times(1)).createPatientVisit(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientVisitDTO);
    }
}