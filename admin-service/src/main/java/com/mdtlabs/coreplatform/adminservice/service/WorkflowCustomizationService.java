package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;


/**
 * <p>
 *   This interface maintains the CRUD operations for workflow customization.
 * </p>
 *
 * @author Karthick M created on Feb 08, 2023
 *
 */
public interface WorkflowCustomizationService {

    /**
	 * <p>
	 *   To add a new workflow customization data.
	 * </p>
	 *
	 * @param workflowCustomization {@link WorkflowCustomization} WorkflowCustomization Entity
	 * @return {@link WorkflowCustomization} WorkflowCustomization Entity
	 */
    public WorkflowCustomization createWorkflowCustomization(WorkflowCustomization workflowCustomization);

    /**
     * <p>
     * To get workflow customization data like screening, enrollment forms and
     * consent data based on conditions such as district id, country id etc.
     * </p>
     *
     * @param customizationRequestDto {@link CustomizationRequestDTO} Customization
     *                                request dto
     * @return {@link WorkflowCustomization} WorkflowCustomization entity
     */
    public WorkflowCustomization getCustomization(CustomizationRequestDTO customizationRequestDto);

    /**
     * <p>
     * Update workflow customization data like screening, enrollment forms and
     * consent data based on district id and country customization id.
     * </p>
     *
     * @param workflowCustomization {@link WorkflowCustomization} Workflow
     *                             customization entity
     * @return {@link WorkflowCustomization} WorkflowCustomization entity
     */
    public WorkflowCustomization updateCustomization(WorkflowCustomization workflowCustomization);

    /**
     * <p>
     * To remove the workflow customization by updating is_deleted field based on id.
     * </p>
     *
     * @param requestData {@link Map} Requested data map
     * @return boolean True or False
     */
    public boolean removeCustomization(SearchRequestDTO requestData);

    /**
     * <p>
     * To get workflow customization list.
     * </p>
     *
     * @param requestData {@link Map} Request data
     * @return {@link List} List of WorkflowCustomization
     */
    public List<WorkflowCustomization> getWorkflowCustomizations(SearchRequestDTO request);
}
