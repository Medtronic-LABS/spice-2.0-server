package com.mdtlabs.coreplatform.fhirmapper.labtest.controller;

import java.util.HashMap;
import java.util.List;
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

import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.IntensificationAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InvestigationControllerTest {

    @InjectMocks
    private InvestigationController investigationController;

    @Mock
    private InvestigationService investigationService;

    @Mock
    private IntensificationAlgorithm intensificationAlgorithm;

    @Test
    void createOrUpdateInvestigation() {
        //given
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();

        //when
        when(investigationService.createOrUpdateInvestigation(labTestRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, String> response = investigationController.createOrUpdateInvestigation(labTestRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateInvestigationRequestFormNull() {
        //given
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();
        labTestRequestDTO.setRequestFrom(null);

        //when
        when(investigationService.createOrUpdateInvestigation(labTestRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, String> response = investigationController.createOrUpdateInvestigation(labTestRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getListOfInvestigatedDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(investigationService.getListOfInvestigatedDetails(requestDTO)).thenReturn(List.of(new LabTestDTO()));

        //then
        List<LabTestDTO> response = investigationController.getListOfInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void removeInvestigatedDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setId(TestConstants.TWO_STR);
        requestDTO.setProvenance(TestDataProvider.getProvenance());

        //when
        when(investigationService.removeInvestigation(requestDTO.getId(), requestDTO.getProvenance())).thenReturn(new HashMap<>());

        //then
        Map<String, String> response = investigationController.removeInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getInvestigatedDetails() {
        //given
        SelectedAppTypeContextHolder.set(Constants.NON_COMMUNITY);
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(investigationService.getInvestigatedDetails(requestDTO)).thenReturn(new LabTestHistoryDTO());

        //then
        LabTestHistoryDTO response = investigationController.getInvestigatedDetails(requestDTO);
        Assertions.assertNull(response);
    }

    @Test
    void getInvestigatedDetailsNull() {
        //given
        SelectedAppTypeContextHolder.set(Constants.NON_COMMUNITY);
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(investigationService.getNcdInvestigatedDetails(requestDTO)).thenReturn(new LabTestHistoryDTO());

        //then
        LabTestHistoryDTO response = investigationController.getInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateInvestigationResult() {
        //given
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();

        //when
        doNothing().when(investigationService).updateInvestigationResult(labTestRequestDTO);

        //then
        investigationController.createOrUpdateInvestigationResult(labTestRequestDTO);
        verify(investigationService).updateInvestigationResult(labTestRequestDTO);
    }

    @Test
    void reviewInvestigation() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        doNothing().when(investigationService).reviewInvestigation(requestDTO);

        //then
        investigationController.reviewInvestigation(requestDTO);
        verify(investigationService).reviewInvestigation(requestDTO);
    }

    @Test
    void getIntensificationDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(intensificationAlgorithm.getIntensificationDetails(requestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, List<LabTestDTO>> response = investigationController.getIntensificationDetails(requestDTO);
        Assertions.assertNotNull(response);
    }
}