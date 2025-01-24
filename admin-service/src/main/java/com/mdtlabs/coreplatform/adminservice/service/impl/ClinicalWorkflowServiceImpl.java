package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.repository.ClinicalWorkflowRepository;
import com.mdtlabs.coreplatform.adminservice.service.ClinicalWorkflowService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * This service class contain all the business logic for ClinicalWorkflowService and
 * perform all the Clinical workflow operation here.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
@Service
public class ClinicalWorkflowServiceImpl implements ClinicalWorkflowService {

    private final ClinicalWorkflowRepository workflowRepository;

    public ClinicalWorkflowServiceImpl(ClinicalWorkflowRepository workflowRepository) {
        this.workflowRepository = workflowRepository;
    }

    /**
     * {@inheritDoc}
     */
    public ClinicalWorkflow createWorkflow(ClinicalWorkflow workflow) {
        if (workflowRepository.existsByNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(workflow.getName(),
                workflow.getCountryId())) {
            throw new DataConflictException(2201, workflow.getName());
        }
        workflow
                .setWorkflowName(workflow.getName().toLowerCase().replaceAll(Constants.SPACE, Constants.UNDER_SCORE));
        workflow.setModuleType(FieldConstants.CUSTOMIZED);
        return workflowRepository.save(workflow);
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<ClinicalWorkflow> getWorkflows(SearchRequestDTO searchRequestDto) {
        if (Objects.isNull(searchRequestDto.getCountryId()) || 0 == searchRequestDto.getCountryId()) {
            throw new DataNotAcceptableException(1001);
        }
        Pageable pageable = null;
        if (searchRequestDto.getLimit() > 0) {
            pageable = Pagination.setPagination(searchRequestDto.getSkip(), searchRequestDto.getLimit());
        }
        String searchTerm = searchRequestDto.getSearchTerm();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.REGEX_SEARCH_PATTERN)) {
            return new ResponseListDTO<>(null, Constants.LONG_ZERO);
        }
        Page<ClinicalWorkflow> workflows = workflowRepository
            .getDistrictWorkflows(searchRequestDto.getCountryId(), searchTerm, pageable);
        return new ResponseListDTO<>(workflows.toList(), workflows.getTotalElements());
    }

    /**
     * {@inheritDoc}
     */
    public ClinicalWorkflow updateWorkflow(ClinicalWorkflow workflow) {
        if (Objects.isNull(workflow)) {
            throw new BadRequestException(1003);
        }
        boolean isNullOrEmpty = workflow.getViewScreens().stream()
            .anyMatch(screen -> (Objects.isNull(screen) || screen.isBlank()));
        if (isNullOrEmpty) {
            throw new BadRequestException(2202, workflow.getName());
        }
        ClinicalWorkflow existingWorkflow = workflowRepository
                .findByIdAndIsDeletedFalseAndIsActiveTrue(workflow.getId());
        if (Objects.isNull(existingWorkflow)) {
            throw new DataNotFoundException(2203, String.valueOf(workflow.getId()));
        }
        existingWorkflow.setViewScreens(workflow.getViewScreens());
        workflow = workflowRepository.save(existingWorkflow);
        return workflow;
    }

    /**
     * {@inheritDoc}
     */
    public boolean removeWorkflowById(long id) {
        if (Constants.ZERO == id) {
            throw new DataNotAcceptableException(1003);
        }
        List<HealthFacility> healthFacilities = workflowRepository.getHealthFacilityByCustomizedWorkflowIds(id);
        if (!Objects.isNull(healthFacilities) && !healthFacilities.isEmpty()) {
            throw new BadRequestException(2204);
        }

        ClinicalWorkflow existingWorkflow = workflowRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id);
        if (Objects.isNull(existingWorkflow)) {
            throw new DataNotFoundException(2203, String.valueOf(id));
        }
        existingWorkflow.setDeleted(true);
        workflowRepository.save(existingWorkflow);

        return true;
    }

    /**
     * {@inheritDoc}
     */
    public List<ClinicalWorkflow> getAllWorkflows() {
        return workflowRepository.findByModuleTypeAndIsDeletedFalseAndIsActiveTrue(Constants.CLINICAL);
    }

    /**
     * {@inheritDoc}
     */
    public List<ClinicalWorkflow> getAllWorkflows(List<Long> ids) {
        return workflowRepository.findByModuleTypeAndIdNotIn(Constants.CLINICAL, ids);
    }
    
    /**
     * {@inheritDoc}
     */
    public List<ClinicalWorkflow> getWorkflowsByIds(List<Long> workflowIds) {
        return workflowRepository.findByIdInAndIsDeletedFalseAndIsActiveTrue(workflowIds);
    }
}
