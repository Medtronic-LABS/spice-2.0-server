package com.mdtlabs.coreplatform.spiceservice.patienttransfer.controller;

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

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferUpdateRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.service.PatientTransferService;

/**
 * <p>
 * PatientTransferControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in PatientTransferController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 25 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientTransferControllerTest {

    @InjectMocks
    PatientTransferController patientTransferController;

    @Mock
    PatientTransferService patientTransferService;

    @Test
    void createPatientTransfer() {
        //given
        PatientTransferRequestDTO patientTransferRequestDTO = TestDataProvider.getPatientTransferRequestDTO();

        //then
        SuccessResponse<PatientTransfer> response = patientTransferController.createPatientTransfer(patientTransferRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void validatePatientTransfer() {
        //when
        when(patientTransferService.validatePatientTransfer(new RequestDTO())).thenReturn(new HashMap<>());

        //then
        SuccessResponse<Map<String, String>> response = patientTransferController.validatePatientTransfer(new RequestDTO());
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientTransfer() {
        //given
        PatientTransferUpdateRequestDTO request = new PatientTransferUpdateRequestDTO();
        request.setTransferStatus(PatientTransferStatus.ACCEPTED);

        //then
        SuccessResponse<PatientTransfer> response = patientTransferController.updatePatientTransfer(request);
        Assertions.assertNotNull(response);
    }


    @Test
    void getPatientTransferCount() {
        //when
        when(patientTransferService.getPatientTransferCount(new RequestDTO())).thenReturn(new HashMap<>());

        //then
        SuccessResponse<PatientTransfer> response = patientTransferController.getPatientTransferCount(new RequestDTO());
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientTransferList() {
        //when
        when(patientTransferService.getPatientTransferList(new RequestDTO())).thenReturn(new HashMap<>());

        //then
        SuccessResponse<Map<String, Object>> response = patientTransferController.getPatientTransferList(new RequestDTO());
        Assertions.assertNotNull(response);
    }

}
