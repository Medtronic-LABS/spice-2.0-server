package com.mdtlabs.coreplatform.fhirmapper.patienttransfer.controller;

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

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.patienttransfer.service.PatientTransferService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientTransferControllerTest {

    @InjectMocks
    private PatientTransferController patientTransferController;

    @Mock
    private PatientTransferService patientTransferService;

    @Test
    void validatePatientTransfer() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(patientTransferService.validatePatientTransfer(requestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, String> response = patientTransferController.validatePatientTransfer(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientRecords() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        doNothing().when(patientTransferService).updatePatientRecords(requestDTO);

        //then
        patientTransferController.updatePatientRecords(requestDTO);
        verify(patientTransferService).updatePatientRecords(requestDTO);
    }
}