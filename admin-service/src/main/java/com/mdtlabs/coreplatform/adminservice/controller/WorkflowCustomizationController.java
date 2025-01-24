package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;
import java.util.Map;


import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.WorkflowCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.service.WorkflowCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;

import jakarta.validation.Valid;



/**
 * <p>
 *   This controller class maintains CRUD operation for workflow customization
 *   data.
 * </p>
 *
 * @author Karthick M created on Feb 08, 2023
 */
@RestController
@RequestMapping(value = "/workflow-customization")
public class WorkflowCustomizationController {

    private final ModelMapper modelMapper = new ModelMapper();
    private final WorkflowCustomizationService workflowCustomizationService;

    @Autowired
    public WorkflowCustomizationController(WorkflowCustomizationService workflowCustomizationService) {
        this.workflowCustomizationService = workflowCustomizationService;
    }
    /**
     * <p>
     *   This method is used to add a workflow customization form data.
     * </p>
     *
     * @param workflowCustomizationDto {@link WorkflowCustomization} workflow customization entity
     * @return {@link SuccessResponse} workflow customization entity
     */
    @UserTenantValidation
    @PostMapping("/create")
    public SuccessResponse<WorkflowCustomization> addCustomization(
            @Valid @RequestBody WorkflowCustomizationDTO workflowCustomizationDTO) {
        Logger.logInfo("In Workflow Customization controller, Adding an workflow customization");
        workflowCustomizationService
                .createWorkflowCustomization(modelMapper.map(workflowCustomizationDTO, WorkflowCustomization.class));
        return new SuccessResponse<>(SuccessCode.WORKFLOW_CUSTOMIZATION_SAVE, HttpStatus.CREATED);
    }

    /**
     * <p>
     *   Get the workflow customization data details such as screening, enrollment and
     *   consent forms based on workflow id and country id.
     * </p>
     *
     * @param customizationRequestDto {@link CustomizationRequestDTO} Customization request dto
     * @return {@link SuccessResponse} WorkflowCustomization entity
     */
    @UserTenantValidation
    @PostMapping("/details")
    public SuccessResponse<WorkflowCustomization> getCustomization(
        @RequestBody CustomizationRequestDTO customizationRequestDto) {
        Logger.logInfo("In Workflow Customization controller, fetching an workflow customization");
        return new SuccessResponse<>(SuccessCode.GET_WORKFLOW_CUSTOMIZATION,
            workflowCustomizationService.getCustomization(customizationRequestDto), HttpStatus.OK);
    }

    /**
     * <p>
     *   Update workflow customization data like screening, enrollment forms and
     *   consent data based on id.
     * </p>
     *
     * @param workflowCustomizationDTO {@link WorkflowCustomization} WorkflowCustomization entity
     * @return {@link SuccessResponse} WorkflowCustomization entity
     */
    @UserTenantValidation
    @PutMapping("/update")
    public SuccessResponse<WorkflowCustomization> updateCustomization(
            @Valid @RequestBody WorkflowCustomizationDTO workflowCustomizationDTO) {
        Logger.logInfo("In Workflow Customization controller, updating an workflow customization");
        workflowCustomizationService
                .updateCustomization(modelMapper.map(workflowCustomizationDTO, WorkflowCustomization.class));
        return new SuccessResponse<>(SuccessCode.WORKFLOW_CUSTOMIZATION_UPDATE, HttpStatus.OK);
    }

    /**
     * <p>
     *   To remove workflow customization by updating is_deleted column.
     * </p>
     *
     * @param requestData {@link Map} Requested data
     * @return {@link SuccessResponse} Boolean True or false
     */
    @PutMapping("/remove")
    public SuccessResponse<String> removeCustomization(@RequestBody SearchRequestDTO requestData) {
        Logger.logInfo("In Workflow Customization controller, removing an workflow customization");
        workflowCustomizationService.removeCustomization(requestData);
        return new SuccessResponse<>(SuccessCode.WORKFLOW_CUSTOMIZATION_DELETE, HttpStatus.OK);
    }

    /**
     * <p>
     *   Get the list workflow customization data details such as screening, enrollment and
     *   consent forms based on category id, type and country id.
     * </p>
     *
     * @param requestData  {@link Map} Requested data
     * @return {@link List} List of WorkflowCustomization entity
     */
    @PostMapping("/static-data/get-list")
    public List<WorkflowCustomization> getWorkflowCustomizations(@RequestBody SearchRequestDTO request) {
        Logger.logInfo("In Workflow Customization controller, fetching an workflow customization list");
        return workflowCustomizationService.getWorkflowCustomizations(request);
    }
}
