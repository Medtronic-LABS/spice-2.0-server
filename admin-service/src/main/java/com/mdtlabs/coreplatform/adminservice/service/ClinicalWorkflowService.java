package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;

/**
 * <p>
 * This an interface class for ClinicalWorkflowService you can implement this
 * class in any class.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
public interface ClinicalWorkflowService {

    /**
     * Creates a new clinical workflow in the system.
     * <p>
     * This method is responsible for adding a new clinical workflow entity into the system. It takes a {@link ClinicalWorkflow} object
     * containing the necessary information to create a new workflow and persists it. The method returns the persisted workflow entity,
     * which includes any modifications made during the save process (e.g., generated ID).
     * </p>
     *
     * @param workflow The {@link ClinicalWorkflow} entity to be added.
     * @return The saved {@link ClinicalWorkflow} entity with any modifications made during the save process.
     */
    public ClinicalWorkflow createWorkflow(ClinicalWorkflow workflow);

    /**
     * Retrieves a list of clinical workflows based on search criteria.
     * <p>
     * This method fetches a list of clinical workflows matching the given search criteria encapsulated within the {@link SearchRequestDTO}.
     * The search criteria can include various parameters such as country ID, name, etc., to filter the workflows. The method returns a
     * {@link ResponseListDTO} containing the list of matching {@link ClinicalWorkflow} entities along with pagination information.
     * </p>
     *
     * @param searchRequestDto The {@link SearchRequestDTO} containing the search criteria.
     * @return A {@link ResponseListDTO} containing the list of matching {@link ClinicalWorkflow} entities.
     */
    public ResponseListDTO<ClinicalWorkflow> getWorkflows(SearchRequestDTO searchRequestDto);

    /**
     * Updates an existing clinical workflow.
     * <p>
     * This method updates an existing clinical workflow with new information provided in the {@link ClinicalWorkflow} object.
     * It can be used to update various aspects of a workflow, such as its view screen. The method returns the updated {@link ClinicalWorkflow}
     * entity, reflecting the changes made.
     * </p>
     *
     * @param workflow The {@link ClinicalWorkflow} entity with updated information.
     * @return The updated {@link ClinicalWorkflow} entity.
     */
    public ClinicalWorkflow updateWorkflow(ClinicalWorkflow workflow);

    /**
     * Deletes a clinical workflow by its ID.
     * <p>
     * This method marks a clinical workflow as deleted based on the provided ID. The actual deletion mechanism
     * may vary, typically involving setting a flag in the database rather than physically removing the record.
     * This approach allows for recovery of deleted workflows if necessary.
     * </p>
     *
     * @param id The ID of the clinical workflow to be marked as deleted.
     * @return boolean True if the workflow was successfully marked as deleted, false otherwise.
     */
    public boolean removeWorkflowById(long id);


    /**
     * Retrieves all clinical workflows.
     * <p>
     * This method fetches a list of all clinical workflows currently available in the system.
     * It can be used to display a comprehensive list of workflows, regardless of their state or association.
     * </p>
     *
     * @return {@link List<ClinicalWorkflow>} A list of all clinical workflows.
     */
    public List<ClinicalWorkflow> getAllWorkflows(List<Long> ids);

    /**
     * Retrieves a list of clinical workflows based on specified workflow IDs.
     * <p>
     * This method fetches clinical workflows that match the provided list of workflow IDs. It is useful for retrieving specific workflows
     * when their IDs are known, allowing for targeted data retrieval. This can be particularly useful in scenarios where only a subset of
     * workflows needs to be processed or displayed.
     * </p>
     *
     * @param workflowIds A {@link List} of workflow IDs for which to retrieve the workflows.
     * @return A {@link List<ClinicalWorkflow>} containing the workflows that match the given IDs.
     */
    public List<ClinicalWorkflow> getWorkflowsByIds(List<Long> workflowIds);

}
