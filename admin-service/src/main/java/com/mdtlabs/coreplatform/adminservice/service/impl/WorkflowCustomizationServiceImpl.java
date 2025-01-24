package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.WorkflowCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.service.WorkflowCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;


/**
 * <p>
 *   This service maintains the CRUD operations for workflow customization.
 * </p>
 *
 * @author Karthick M created on Feb 08, 2023
 */
@Service
public class WorkflowCustomizationServiceImpl implements WorkflowCustomizationService {

    private final WorkflowCustomizationRepository workflowCustomizationRepository;
    
    private ModelMapper modelMapper = new ModelMapper();

    @Autowired
    public WorkflowCustomizationServiceImpl(WorkflowCustomizationRepository workflowCustomizationRepository) {
        this.workflowCustomizationRepository = workflowCustomizationRepository;
    }
    
    /**
     * {@inheritDoc}
     */
    public WorkflowCustomization createWorkflowCustomization(WorkflowCustomization workflowCustomization) {
        if (Objects.isNull(workflowCustomization)) {
            Logger.logError(ErrorConstants.REQUEST_NOT_EMPTY);
            throw new BadRequestException(1003);
        }
        return workflowCustomizationRepository.save(workflowCustomization);
    }

    /**
     * {@inheritDoc}
     */
    public WorkflowCustomization getCustomization(CustomizationRequestDTO customizationRequestDto) {
        if (Objects.isNull(customizationRequestDto)) {
            Logger.logError(ErrorConstants.REQUEST_NOT_EMPTY);
            throw new DataNotAcceptableException(1003);
        }
        List<WorkflowCustomization> workflowCustomizations = 
        workflowCustomizationRepository.getWorkflowCustomization(
            customizationRequestDto.getCountryId(), 
            customizationRequestDto.getCategory(), customizationRequestDto.getType(),
            customizationRequestDto.getClinicalWorkflowId(),
            customizationRequestDto.getTenantId(), customizationRequestDto.getDistrictId());
        return Objects.isNull(workflowCustomizations) || workflowCustomizations.isEmpty() ? null
            : workflowCustomizations.get(Constants.ZERO);
    }

    /**
     * {@inheritDoc}
     */
    public WorkflowCustomization updateCustomization(WorkflowCustomization workflowCustomization) {
        if (Objects.isNull(workflowCustomization)) {
            Logger.logError(ErrorConstants.REQUEST_NOT_EMPTY);
            throw new BadRequestException(1003);
        }
        WorkflowCustomization existingWorkflowCustomization = workflowCustomizationRepository
                .findByIdAndIsDeletedAndTenantId(workflowCustomization.getId(), Constants.BOOLEAN_FALSE,
                        workflowCustomization.getTenantId());
        if (Objects.isNull(existingWorkflowCustomization)) {
            Logger.logError("No Workflow Customization forms found for the request.");
            throw new DataNotFoundException(2151);
        }
        modelMapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        modelMapper.map(workflowCustomization, existingWorkflowCustomization);
        return workflowCustomizationRepository.save(existingWorkflowCustomization);
    }

    /**
     * {@inheritDoc}
     */
    public boolean removeCustomization(SearchRequestDTO requestData) {
        if (Objects.isNull(requestData.getId())) {
            Logger.logError(ErrorConstants.REQUEST_NOT_EMPTY);
            throw new BadRequestException(1003);
        }
        WorkflowCustomization workflowCustomization = workflowCustomizationRepository.findByIdAndIsDeletedAndTenantId(requestData.getId(), false, requestData.getTenantId());
        if (Objects.isNull(workflowCustomization)) {
            Logger.logError("No Workflow Customization forms found for the request.");
            throw new DataNotFoundException(2151);
        }
        workflowCustomization.setDeleted(true);
        return workflowCustomizationRepository.save(workflowCustomization).isDeleted();
    }

    /**
     * {@inheritDoc}
     */
    public List<WorkflowCustomization> getWorkflowCustomizations(SearchRequestDTO request) {   
        return workflowCustomizationRepository.geWorkflowCustomizations(request.getWorkflowIds(), request.getDistrictId());
    }
}
