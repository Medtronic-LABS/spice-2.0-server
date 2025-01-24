package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.ClinicalWorkflowService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;

/**
 * <p>
 * The ClinicalWorkflowController class is a REST controller that handles requests related to ClinicalWorkflow.
 * </p>
 *
 * @author Karthick M
 */
@RestController
@RequestMapping("/clinical-workflow")
public class ClinicalWorkflowController {

    private final ClinicalWorkflowService workflowService;

    public ClinicalWorkflowController(ClinicalWorkflowService workflowService) {
        this.workflowService = workflowService;
    }

    /**
     * Creates a new ClinicalWorkflow entity.
     * <p>
     * This method handles a POST request to create a new ClinicalWorkflow entity based on the provided {@link ClinicalWorkflow} object.
     * It utilizes the {@link ClinicalWorkflowService#createWorkflow(ClinicalWorkflow)} method to persist the workflow entity.
     * Upon successful creation, it returns a {@link SuccessResponse} with the status CREATED, including the name of the created workflow.
     * </p>
     *
     * @param workflow The {@link ClinicalWorkflow} object containing the workflow details to be created.
     * @return A {@link SuccessResponse<ClinicalWorkflow>} object containing the name of the created workflow and the HTTP status code CREATED.
     */
    @PostMapping("/create")
    public SuccessResponse<ClinicalWorkflow> addWorkflow(@RequestBody ClinicalWorkflow workflow) {
        return new SuccessResponse<>(SuccessCode.WORKFLOW_SAVE, HttpStatus.CREATED,
                workflowService.createWorkflow(workflow).getName());
    }

    /**
     * Retrieves a ClinicalWorkflow details based on search criteria.
     * <p>
     * This method handles a POST request to retrieve ClinicalWorkflow entities that match the given search criteria encapsulated in a {@link SearchRequestDTO}.
     * It leverages the {@link ClinicalWorkflowService#getWorkflows(SearchRequestDTO)} method to fetch the workflow.
     * </p>
     *
     * @param searchRequestDto The {@link SearchRequestDTO} containing the search criteria.
     * @return A {@link SuccessResponse<ClinicalWorkflow>} object containing the list of matching ClinicalWorkflow entities, the total count, and the HTTP status code OK.
     */
    @PostMapping("/list")
    public SuccessResponse<ClinicalWorkflow> getWorkflows(@RequestBody SearchRequestDTO searchRequestDto) {
        ResponseListDTO<ClinicalWorkflow> accountWorkFlowDetails = workflowService
                .getWorkflows(searchRequestDto);
        return new SuccessResponse<>(SuccessCode.GET_WORKFLOW, accountWorkFlowDetails.getData(),
                accountWorkFlowDetails.getTotalCount(), HttpStatus.OK);
    }

    /**
     * Updates an existing ClinicalWorkflow entity.
     * <p>
     * This method handles a PUT request to update a ClinicalWorkflow entity based on the provided {@link ClinicalWorkflow} object.
     * It utilizes the {@link ClinicalWorkflowService#updateWorkflow(ClinicalWorkflow)} method to update the workflow entity.
     * Upon successful update, it returns a {@link SuccessResponse} with an OK status, including the name of the updated workflow.
     * </p>
     *
     * @param workflow The {@link ClinicalWorkflow} object containing the workflow details to be updated.
     * @return A {@link SuccessResponse<ClinicalWorkflow>} object containing the name of the updated workflow and the HTTP status code OK.
     */
    @PutMapping("/update")
    public SuccessResponse<ClinicalWorkflow> updateWorkflow(@RequestBody ClinicalWorkflow workflow) {
        return new SuccessResponse<>(SuccessCode.WORKFLOW_UPDATE, HttpStatus.OK,
                workflowService.updateWorkflow(workflow).getName());
    }

    /**
     * Deletes an existing ClinicalWorkflow entity by its ID.
     * <p>
     * This method handles a PUT request to delete a ClinicalWorkflow entity identified by the ID provided in a {@link SearchRequestDTO}.
     * Upon successful deletion, it returns a {@link SuccessResponse} with an OK status, indicating the workflow has been successfully removed.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the ID of the workflow to be deleted.
     * @return A {@link SuccessResponse<ClinicalWorkflow>} object indicating the successful deletion of the workflow and the HTTP status code OK.
     */
    @PutMapping("/remove")
    public SuccessResponse<ClinicalWorkflow> deleteWorkflowById(@RequestBody SearchRequestDTO request) {
        workflowService.removeWorkflowById(request.getId());
        return new SuccessResponse<>(SuccessCode.WORKFLOW_DELETE, HttpStatus.OK);
    }

    /**
     * Retrieves all ClinicalWorkflow entities.
     * <p>
     * This method handles a POST request to fetch all ClinicalWorkflow entities without any filtering criteria.
     * It leverages the {@link ClinicalWorkflowService#getAllWorkflows()} method to obtain the list of all workflows.
     * </p>
     *
     * @return A {@link List<ClinicalWorkflow>} containing all ClinicalWorkflow entities.
     */
    @PostMapping("/get-all-workflows")
    public List<ClinicalWorkflow> getAllWorkFlows(@RequestBody List<Long> ids) {
        return workflowService.getAllWorkflows(ids);
    }

    /**
     * Retrieves ClinicalWorkflow entities by their IDs.
     * <p>
     * This method handles a POST request to fetch ClinicalWorkflow entities based on a list of provided IDs.
     * The IDs are provided in the request body. It utilizes the {@link ClinicalWorkflowService#getWorkflowsByIds(List<Long>)} method
     * to fetch the specified workflows.
     * </p>
     *
     * @param ids The list of IDs for which ClinicalWorkflow entities are to be retrieved.
     * @return A {@link List<ClinicalWorkflow>} containing the ClinicalWorkflow entities that match the provided IDs.
     */
    @PostMapping("/get-workflows")
    public List<ClinicalWorkflow> getAllWorkFlowsByIds(@RequestBody List<Long> ids) {
        return workflowService.getWorkflowsByIds(ids);
    }
}
