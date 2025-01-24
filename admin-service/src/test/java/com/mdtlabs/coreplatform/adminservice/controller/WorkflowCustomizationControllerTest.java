package com.mdtlabs.coreplatform.adminservice.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.WorkflowCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.service.WorkflowCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class WorkflowCustomizationControllerTest {

    @Mock
    private WorkflowCustomizationService workflowCustomizationService;

    @InjectMocks
    private WorkflowCustomizationController workflowCustomizationController;


    @Test
    void testAddCustomization_Success() {
        WorkflowCustomizationDTO dto = new WorkflowCustomizationDTO();
        WorkflowCustomization entity = new WorkflowCustomization();

        when(workflowCustomizationService.createWorkflowCustomization(any(WorkflowCustomization.class)))
                .thenReturn(entity);

        workflowCustomizationController.addCustomization(dto);
        verify(workflowCustomizationService, times(1))
                .createWorkflowCustomization(any(WorkflowCustomization.class));
    }

    @Test
    void testGetCustomization_Success() {
        CustomizationRequestDTO requestDTO = new CustomizationRequestDTO();
        WorkflowCustomization workflowCustomization = new WorkflowCustomization();

        when(workflowCustomizationService.getCustomization(requestDTO))
                .thenReturn(workflowCustomization);

        workflowCustomizationController.getCustomization(requestDTO);

        verify(workflowCustomizationService, times(1))
                .getCustomization(any(CustomizationRequestDTO.class));
    }

    @Test
    void testUpdateCustomization_Success() {
        WorkflowCustomizationDTO dto = new WorkflowCustomizationDTO();
        WorkflowCustomization entity = new WorkflowCustomization();

        when(workflowCustomizationService.updateCustomization(any(WorkflowCustomization.class)))
                .thenReturn(entity);

        workflowCustomizationController.updateCustomization(dto);

        verify(workflowCustomizationService, times(1))
                .updateCustomization(any(WorkflowCustomization.class));
    }

    @Test
    void testRemoveCustomization_Success() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();

        when(workflowCustomizationService.removeCustomization(requestDTO))
                .thenReturn(true);

        workflowCustomizationController.removeCustomization(requestDTO);

        verify(workflowCustomizationService, times(1))
                .removeCustomization(any(SearchRequestDTO.class));
    }

    @Test
    void testGetWorkflowCustomizations_Success() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        List<WorkflowCustomization> workflowCustomizationList = Collections.singletonList(new WorkflowCustomization());

        when(workflowCustomizationService.getWorkflowCustomizations(requestDTO))
                .thenReturn(workflowCustomizationList);

        workflowCustomizationController.getWorkflowCustomizations(requestDTO);

        verify(workflowCustomizationService, times(1))
                .getWorkflowCustomizations(any(SearchRequestDTO.class));
    }
}