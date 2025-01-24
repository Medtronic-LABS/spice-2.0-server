package com.mdtlabs.coreplatform.fhirmapper.glucoselog.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.glucoselog.service.GlucoseLogService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class GlucoseLogControllerTest {

    @InjectMocks
    private GlucoseLogController glucoseLogController;

    @Mock
    private GlucoseLogService glucoseLogService;

    @Test
    void getBpLogsByPatientTrackId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(glucoseLogService.getPatientGlucoseLogsWithSymptoms(requestDTO)).thenReturn(new PatientGlucoseLogDTO());

        //then
        PatientGlucoseLogDTO response = glucoseLogController.getBpLogsByPatientTrackId(requestDTO);
        Assertions.assertNotNull(response);
    }
}