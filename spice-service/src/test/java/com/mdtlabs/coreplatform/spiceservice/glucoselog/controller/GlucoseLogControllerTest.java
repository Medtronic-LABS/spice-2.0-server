package com.mdtlabs.coreplatform.spiceservice.glucoselog.controller;

import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.glucoselog.service.GlucoseLogService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class GlucoseLogControllerTest {

    @Mock
    private GlucoseLogService glucoseLogService;

    @InjectMocks
    private GlucoseLogController glucoseLogController;

    @Test
    void testAddGlucoseLog() {
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        glucoseLogDTO.setType(TestConstants.STRING_ONE);
        glucoseLogController.addGlucoseLog(glucoseLogDTO);
        verify(glucoseLogService, times(1)).addGlucoseLog(glucoseLogDTO);
    }

    @Test
    void testGetBpLogsByPatientTrackId() {
        RequestDTO requestDTO = new RequestDTO();
        glucoseLogController.getBpLogsByPatientTrackId(requestDTO);
        verify(glucoseLogService, times(1)).getPatientGlucoseLogsWithSymptoms(requestDTO);
    }

}