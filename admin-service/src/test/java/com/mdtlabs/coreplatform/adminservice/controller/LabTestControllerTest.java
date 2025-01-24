package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;
import com.mdtlabs.coreplatform.adminservice.service.LabTestService;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * LabTestControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in LabTestController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class LabTestControllerTest {

    @InjectMocks
    LabTestController labTestController;

    @Mock
    LabTestService labTestService;

    @Test
    void addLabTest() {
        //given
        LabTestDTO request = new LabTestDTO();

        //when
        when(labTestService.createLabTest(request)).thenReturn(new LabTest());

        //then
        SuccessResponse<LabTest> response = labTestController.addLabTest(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getAllLabTests() {
        //given
        SearchRequestDTO request = new SearchRequestDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        ResponseListDTO<LabTestDTO> responseListDTO = new ResponseListDTO<>(List.of(labTestDTO));

        //when
        when(labTestService.getAllLabTests(request)).thenReturn(responseListDTO);

        //then
        SuccessResponse<LabTestDTO> response = labTestController.getAllLabTests(request);
        Assertions.assertNotNull(response);
        responseListDTO.setTotalCount(TestConstants.ONE);
        response = labTestController.getAllLabTests(request);
        Assertions.assertNotNull(response);
        responseListDTO.setTotalCount(0l);
        response = labTestController.getAllLabTests(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void removeLabTest() {
        //given
        SearchRequestDTO request = new SearchRequestDTO();

        //when
        when(labTestService.removeLabTest(request)).thenReturn(Boolean.TRUE);

        //then
        SuccessResponse<Boolean> response = labTestController.removeLabTest(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateLabTest() {
        //given
        LabTestDTO request = new LabTestDTO();

        //when
        when(labTestService.updateLabTest(request)).thenReturn(new LabTest());

        //then
        SuccessResponse<LabTest> response = labTestController.updateLabTest(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getLabTestById() {
        //given
        SearchRequestDTO request = new SearchRequestDTO();

        //when
        when(labTestService.getLabTestById(request)).thenReturn(new LabTest());

        //then
        SuccessResponse<LabTest> response = labTestController.getLabTestById(request);
        Assertions.assertNotNull(response);
    }
}
