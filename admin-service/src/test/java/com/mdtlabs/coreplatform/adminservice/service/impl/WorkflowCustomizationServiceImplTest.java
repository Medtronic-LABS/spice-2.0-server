package com.mdtlabs.coreplatform.adminservice.service.impl;


import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.WorkflowCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class WorkflowCustomizationServiceImplTest {

    @Mock
    private WorkflowCustomizationRepository workflowCustomizationRepository;

    @InjectMocks
    private WorkflowCustomizationServiceImpl workflowCustomizationService;

    @BeforeEach
    public void setUp() {
        TestDataProvider.init();
    }

    @AfterEach
    public void wrap() {
        TestDataProvider.cleanUp();
    }

    @Test
    void testCreateWorkflowCustomization_Success() {
        WorkflowCustomization customization = new WorkflowCustomization();
        when(workflowCustomizationRepository.save(any(WorkflowCustomization.class)))
                .thenReturn(customization);

        WorkflowCustomization result = workflowCustomizationService.createWorkflowCustomization(customization);

        assertNotNull(result);
        verify(workflowCustomizationRepository, times(1)).save(customization);
    }

    @Test
    void testCreateWorkflowCustomization_NullRequest_ThrowsBadRequestException() {
        assertThrows(BadRequestException.class, () -> {
            workflowCustomizationService.createWorkflowCustomization(null);
        });
    }

    @Test
    void testGetCustomization_Success() {
        CustomizationRequestDTO requestDTO = getMinimalCustomizationRequestDTO();
        List<WorkflowCustomization> customizations = new ArrayList<>();
        WorkflowCustomization customization = new WorkflowCustomization();
        customizations.add(customization);

        when(workflowCustomizationRepository.getWorkflowCustomization(requestDTO.getCountryId(),
                requestDTO.getCategory(), requestDTO.getType(),
                requestDTO.getClinicalWorkflowId(),
                requestDTO.getTenantId(), requestDTO.getDistrictId()))
                .thenReturn(customizations);

        WorkflowCustomization result = workflowCustomizationService.getCustomization(requestDTO);

        assertNotNull(result);
        verify(workflowCustomizationRepository, times(1))
                .getWorkflowCustomization(requestDTO.getCountryId(),
                        requestDTO.getCategory(), requestDTO.getType(),
                        requestDTO.getClinicalWorkflowId(),
                        requestDTO.getTenantId(), requestDTO.getDistrictId());
    }

    @Test
    void testGetCustomizationThrowsDataNotAcceptableException() {
        assertThrows(DataNotAcceptableException.class, () -> workflowCustomizationService.getCustomization(null));
    }

    @Test
    void testRemoveCustomizationThrowsBadRequestException() {
        assertThrows(BadRequestException.class, () -> workflowCustomizationService.removeCustomization(new SearchRequestDTO()));
    }

    @Test
    void testUpdateCustomizationThrowsBadRequestException() {
        assertThrows(BadRequestException.class, () -> workflowCustomizationService.updateCustomization(null));
    }

    private static CustomizationRequestDTO getMinimalCustomizationRequestDTO() {
        CustomizationRequestDTO requestDTO = new CustomizationRequestDTO();
        requestDTO.setType("sample");
        requestDTO.setCategory("test");
        requestDTO.setClinicalWorkflowId(1L);
        requestDTO.setTenantId(1L);
        requestDTO.setCountryId(1L);
        requestDTO.setDistrictId(1L);
        return requestDTO;
    }

    @Test
    void testGetCustomization_EmptyResult_ReturnsNull() {
        CustomizationRequestDTO requestDTO = getMinimalCustomizationRequestDTO();
        List<WorkflowCustomization> customizations = new ArrayList<>();

        when(workflowCustomizationRepository.getWorkflowCustomization(requestDTO.getCountryId(),
                requestDTO.getCategory(), requestDTO.getType(),
                requestDTO.getClinicalWorkflowId(),
                requestDTO.getTenantId(), requestDTO.getDistrictId()))
                .thenReturn(customizations);

        WorkflowCustomization result = workflowCustomizationService.getCustomization(requestDTO);

        assertNull(result);
    }

    @Test
    void testUpdateWorkflowCustomization_Success() {
        WorkflowCustomization workflowCustomization = new WorkflowCustomization();
        workflowCustomization.setId(1L);
        workflowCustomization.setTenantId(1L);
        WorkflowCustomization existingCustomization = new WorkflowCustomization();

        when(workflowCustomizationRepository.findByIdAndIsDeletedAndTenantId(workflowCustomization.getId(), Constants.BOOLEAN_FALSE,
                workflowCustomization.getTenantId()))
                .thenReturn(existingCustomization);
        when(workflowCustomizationRepository.save(any(WorkflowCustomization.class)))
                .thenReturn(existingCustomization);

        WorkflowCustomization result = workflowCustomizationService.updateCustomization(workflowCustomization);

        assertNotNull(result);
        verify(workflowCustomizationRepository, times(1))
                .findByIdAndIsDeletedAndTenantId(workflowCustomization.getId(), Constants.BOOLEAN_FALSE,
                        workflowCustomization.getTenantId());
        verify(workflowCustomizationRepository, times(1)).save(existingCustomization);
    }

    @Test
    void testUpdateWorkflowCustomization_NotFound_ThrowsDataNotFoundException() {
        WorkflowCustomization workflowCustomization = new WorkflowCustomization();
        workflowCustomization.setTenantId(1L);
        workflowCustomization.setId(1L);

        when(workflowCustomizationRepository.findByIdAndIsDeletedAndTenantId(workflowCustomization.getId(), Constants.BOOLEAN_FALSE,
                workflowCustomization.getTenantId()))
                .thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> {
            workflowCustomizationService.updateCustomization(workflowCustomization);
        });
    }

    @Test
    void testRemoveCustomization_Success() {
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setId(1L);
        requestData.setTenantId(1L);
        WorkflowCustomization customization = new WorkflowCustomization();

        when(workflowCustomizationRepository.findByIdAndIsDeletedAndTenantId(requestData.getId(), false, requestData.getTenantId()))
                .thenReturn(customization);
        when(workflowCustomizationRepository.save(any(WorkflowCustomization.class)))
                .thenReturn(customization);

        boolean result = workflowCustomizationService.removeCustomization(requestData);

        assertTrue(result);
        verify(workflowCustomizationRepository, times(1)).findByIdAndIsDeletedAndTenantId(anyLong(), anyBoolean(), anyLong());
        verify(workflowCustomizationRepository, times(1)).save(customization);
    }

    @Test
    void testRemoveCustomization_NotFound_ThrowsDataNotFoundException() {
        SearchRequestDTO requestData = new SearchRequestDTO();
        requestData.setId(1L);

        when(workflowCustomizationRepository.findByIdAndIsDeletedAndTenantId(requestData.getId(), false, requestData.getTenantId()))
                .thenReturn(null);

        Exception exception = assertThrows(DataNotFoundException.class, () -> {
            workflowCustomizationService.removeCustomization(requestData);
        });

    }

    @Test
    void testGetWorkflowCustomizations_Success() {
        SearchRequestDTO request = new SearchRequestDTO();
        List<WorkflowCustomization> customizations = new ArrayList<>();
        customizations.add(new WorkflowCustomization());

        when(workflowCustomizationRepository.geWorkflowCustomizations(request.getWorkflowIds(), request.getDistrictId()))
                .thenReturn(customizations);

        List<WorkflowCustomization> result = workflowCustomizationService.getWorkflowCustomizations(request);

        assertNotNull(result);
        assertFalse(result.isEmpty());
        verify(workflowCustomizationRepository, times(1)).geWorkflowCustomizations(request.getWorkflowIds(), request.getDistrictId());
    }
}