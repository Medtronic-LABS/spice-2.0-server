package com.mdtlabs.coreplatform.adminservice.service;

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.mdtlabs.coreplatform.adminservice.repository.ClinicalWorkflowRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.ClinicalWorkflowServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;

@ExtendWith(MockitoExtension.class)
class ClinicalWorkflowServiceTest {

    @InjectMocks
    private ClinicalWorkflowServiceImpl clinicalWorkflowService;

    @Mock
    private ClinicalWorkflowRepository clinicalWorkflowRepository;

    @Test
    void testCreateClinicalWorkflow() {
        ClinicalWorkflow clinicalWorkflow = TestDataProvider.getClinicalWorkflow();
        clinicalWorkflow.setCountryId(TestConstants.ONE);
        when(clinicalWorkflowRepository
                .existsByNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(clinicalWorkflow.getName(),
                clinicalWorkflow.getCountryId())).thenReturn(Boolean.FALSE);
        when(clinicalWorkflowRepository.save(clinicalWorkflow)).thenReturn(clinicalWorkflow);
        ClinicalWorkflow actualClinicalWorkflow = clinicalWorkflowService.createWorkflow(clinicalWorkflow);
        assertEquals(clinicalWorkflow.getName(), actualClinicalWorkflow.getName());

        when(clinicalWorkflowRepository
                .existsByNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(clinicalWorkflow.getName(),
                clinicalWorkflow.getCountryId())).thenReturn(true);
        assertThrows(DataConflictException.class, () -> clinicalWorkflowService.createWorkflow(clinicalWorkflow));
    }

    @Test
    void testGetDistrictsInvalidSearch() {
        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setTenantId(TestConstants.ONE);
        searchRequestDTO.setCountryId(TestConstants.ONE);

        //when
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //then
        ResponseListDTO actualDistrictWorkflows = clinicalWorkflowService.getWorkflows(searchRequestDTO);
        TestDataProvider.cleanUp();
        assertEquals(Constants.ZERO, actualDistrictWorkflows.getTotalCount());
        assertNotNull(actualDistrictWorkflows);
        assertNull(actualDistrictWorkflows.getData());
    }

    @Test
    void testUpdateDistrictWorkflow() {
        //given
        ClinicalWorkflow clinicalWorkflow = TestDataProvider.getClinicalWorkflow();
        clinicalWorkflow.setId(TestConstants.ONE);

        //when
        when(clinicalWorkflowRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(clinicalWorkflow.getId())).thenReturn(clinicalWorkflow);
        when(clinicalWorkflowRepository.save(clinicalWorkflow)).thenReturn(clinicalWorkflow);
        clinicalWorkflow.setViewScreens(Collections.emptyList());

        //then
        ClinicalWorkflow actualDistrictWorkflow = clinicalWorkflowService.updateWorkflow(clinicalWorkflow);
        assertEquals(clinicalWorkflow.getId(), actualDistrictWorkflow.getId());
    }

    @Test
    void testRemoveDistrictWorkflowById() {
        //given
        long id = TestConstants.ONE;
        ClinicalWorkflow accountWorkflow = TestDataProvider.getClinicalWorkflow();

        when(clinicalWorkflowRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> clinicalWorkflowService.removeWorkflowById(id));

        //when
        when(clinicalWorkflowRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(accountWorkflow);
        when(clinicalWorkflowRepository.save(accountWorkflow)).thenReturn(accountWorkflow);

        //then
        boolean isRemoved = clinicalWorkflowService.removeWorkflowById(id);
        assertTrue(isRemoved);

    }

    @Test
    void testGetAllDistrictWorkFlows() {
        //given
        List<Long> ids = List.of(1l);
        List<ClinicalWorkflow> clinicalWorkflows = List.of(TestDataProvider.getClinicalWorkflow());
        
        clinicalWorkflows.get(0).setId(1l);

        //when
        when(clinicalWorkflowRepository.findByModuleTypeAndIdNotIn(Constants.CLINICAL, ids)).thenReturn(clinicalWorkflows);

        //then
        List<ClinicalWorkflow> actualClinicalWorkflows = clinicalWorkflowService.getAllWorkflows(ids);
        assertNotNull(actualClinicalWorkflows);
        assertEquals(clinicalWorkflows.size(), actualClinicalWorkflows.size());
        assertEquals(TestConstants.ONE, actualClinicalWorkflows.get(0).getId());
    }

    @Test
    void throwDataNotAcceptableException() {
        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setCountryId(0l);
        ClinicalWorkflow clinicalWorkflow = null;
        Long id = 0L;
        assertThrows(BadRequestException.class, () -> clinicalWorkflowService.updateWorkflow(clinicalWorkflow));
        assertThrows(DataNotAcceptableException.class, () -> clinicalWorkflowService.removeWorkflowById(id));
        assertThrows(DataNotAcceptableException.class, () -> clinicalWorkflowService.getWorkflows(searchRequestDTO));
    }

    @Test
    void getWorkflowsByIds() {
        List<Long> workflowIds = List.of(1l);
        List<ClinicalWorkflow> clinicalWorkflows = List.of(TestDataProvider.getClinicalWorkflow());

        when(clinicalWorkflowRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(workflowIds)).thenReturn(clinicalWorkflows);

        List<ClinicalWorkflow> response = clinicalWorkflowService.getWorkflowsByIds(workflowIds);
        assertNotNull(response);
     }

    @Test
    void getAllWorkflows() {
        ClinicalWorkflow clinicalWorkflow = TestDataProvider.getClinicalWorkflow();
        List<ClinicalWorkflow> clinicalWorkflows = List.of(clinicalWorkflow);
        when(clinicalWorkflowRepository.findByModuleTypeAndIsDeletedFalseAndIsActiveTrue(Constants.CLINICAL)).thenReturn(clinicalWorkflows);
        List<ClinicalWorkflow> actualWorkflows = clinicalWorkflowService.getAllWorkflows();
        assertNotNull(actualWorkflows);
        assertEquals(1, actualWorkflows.size());
        assertEquals(clinicalWorkflows, actualWorkflows);
    }

}
