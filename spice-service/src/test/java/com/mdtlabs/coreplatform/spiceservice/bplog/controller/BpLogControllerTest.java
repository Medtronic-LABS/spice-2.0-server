package com.mdtlabs.coreplatform.spiceservice.bplog.controller;

import com.mdtlabs.coreplatform.spiceservice.bplog.service.BpLogService;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class BpLogControllerTest {

    @Mock
    private BpLogService bpLogService;

    @InjectMocks
    private BpLogController bpLogController;

    @Test
    void testAddBpLog() {
        BpLogDTO bpLogDTO = new BpLogDTO();
        bpLogDTO.setType(TestConstants.STRING_ONE);
        bpLogController.addBpLog(bpLogDTO);
        verify(bpLogService, atLeastOnce()).addBpLog(bpLogDTO);
    }

    @Test
    void testGetBpLogsByPatientTrackId() {
        RequestDTO requestDTO = new RequestDTO();
        bpLogController.getBpLogsByPatientTrackId(requestDTO);
        verify(bpLogService, atLeastOnce()).getPatientBpLogsWithSymptoms(requestDTO);
    }


}