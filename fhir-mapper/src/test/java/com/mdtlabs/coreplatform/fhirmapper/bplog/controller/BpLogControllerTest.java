package com.mdtlabs.coreplatform.fhirmapper.bplog.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.bplog.service.impl.BpLogServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class BpLogControllerTest {

    @InjectMocks
    private BpLogController bpLogController;

    @Mock
    private BpLogServiceImpl bpLogService;

    @Test
    void getBpLogsByPatientTrackId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(bpLogService.getPatientBpLogsWithSymptoms(requestDTO)).thenReturn(new PatientBpLogsDTO());

        //then
        PatientBpLogsDTO response = bpLogController.getBpLogsByPatientTrackId(requestDTO);
        Assertions.assertNotNull(response);
    }
}