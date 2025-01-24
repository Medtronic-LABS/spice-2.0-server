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
import com.mdtlabs.coreplatform.adminservice.service.ClinicalWorkflowService;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;

/**
 * <p>
 * ClinicalWorkflowControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in ClinicalWorkflowController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ClinicalWorkflowControllerTest {

    @InjectMocks
    ClinicalWorkflowController clinicalWorkflowController;

    @Mock
    ClinicalWorkflowService workflowService;

    @Test
    void addWorkflow() {
        //given
        ClinicalWorkflow request = new ClinicalWorkflow();

        //when
        when(workflowService.createWorkflow(request)).thenReturn(request);

        //then
        SuccessResponse<ClinicalWorkflow> response = clinicalWorkflowController.addWorkflow(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getWorkflows() {
        //given
        SearchRequestDTO request = new SearchRequestDTO();

        //when
        when(workflowService.getWorkflows(request)).thenReturn(new ResponseListDTO<ClinicalWorkflow>());

        //then
        SuccessResponse<ClinicalWorkflow> response = clinicalWorkflowController.getWorkflows(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateWorkflow() {
        //given
        ClinicalWorkflow request = new ClinicalWorkflow();

        //when
        when(workflowService.updateWorkflow(request)).thenReturn(new ClinicalWorkflow());

        //then
        SuccessResponse<ClinicalWorkflow> response = clinicalWorkflowController.updateWorkflow(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void deleteWorkflowById() {
        //given
        SearchRequestDTO request = new SearchRequestDTO();
        request.setId(TestConstants.ONE);

        //when
        when(workflowService.removeWorkflowById(request.getId())).thenReturn(Boolean.TRUE);

        //then
        SuccessResponse<ClinicalWorkflow> response = clinicalWorkflowController.deleteWorkflowById(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getAllWorkflows() {
        //when
        when(workflowService.getAllWorkflows(List.of(TestConstants.ONE))).thenReturn(List.of(new ClinicalWorkflow()));

        //then
        List<ClinicalWorkflow> response = clinicalWorkflowController.getAllWorkFlows(List.of(TestConstants.ONE));
        Assertions.assertNotNull(response);
    }

    @Test
    void getAllWorkFlowsByIds() {
        //when
        when(workflowService.getWorkflowsByIds(List.of(TestConstants.ONE))).thenReturn(List.of(new ClinicalWorkflow()));

        //then
        List<ClinicalWorkflow> response = clinicalWorkflowController.getAllWorkFlowsByIds(List.of(TestConstants.ONE));
        Assertions.assertNotNull(response);
    }
}
