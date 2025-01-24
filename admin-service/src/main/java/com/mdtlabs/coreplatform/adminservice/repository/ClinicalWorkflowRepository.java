package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the ClinicalWorkflow module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 */
@Repository
public interface ClinicalWorkflowRepository extends JpaRepository<ClinicalWorkflow, Long> {

    String WORKFLOW_LIST = "FROM ClinicalWorkflow"
            + " AS workflow WHERE (workflow.countryId = :countryId or workflow.countryId is null)"
            + " AND workflow.isDeleted=false AND workflow.isActive=true AND workflow.moduleType is not null "
            + " AND (COALESCE(:searchTerm) IS NULL OR lower(workflow.name) "
            + " LIKE CONCAT('%',lower(CAST(:searchTerm AS text)),'%')) order by updatedAt";

    public static String GET_BY_CUSTOMIZATION_ID = "SELECT healthFacility FROM HealthFacility AS healthFacility JOIN "
            + "healthFacility.customizedWorkflows as workflow where workflow.id=:id and healthFacility.isDeleted=false";

    /**
     * Checks if a ClinicalWorkflow exists by name (ignoring case), country ID, and not marked as deleted.
     * <p>
     * This method is used to verify the existence of a ClinicalWorkflow based on its name (case-insensitive),
     * associated country ID, and ensuring it has not been marked as deleted. This is particularly useful
     * for avoiding duplicate entries under the same conditions.
     * </p>
     *
     * @param name      The name of the ClinicalWorkflow to check.
     * @param countryId The ID of the country associated with the ClinicalWorkflow.
     * @return true if a matching ClinicalWorkflow exists, false otherwise.
     */
    boolean existsByNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(String name, Long countryId);

    /**
     * Finds a ClinicalWorkflow by its ID if it's not marked as deleted.
     * <p>
     * Retrieves a ClinicalWorkflow entity based on its unique identifier, provided it has not been marked
     * as deleted. This ensures that only active workflows are fetched for processing or display.
     * </p>
     *
     * @param id The unique identifier of the ClinicalWorkflow to retrieve.
     * @return The ClinicalWorkflow entity if found and not deleted, null otherwise.
     */
    ClinicalWorkflow findByIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     * Retrieves a list of ClinicalWorkflows based on the module type.
     * <p>
     * This method fetches all ClinicalWorkflow entities that are associated with a specific module type.
     * It allows for filtering ClinicalWorkflows that are relevant to a particular functionality or module
     * within the system.
     * </p>
     *
     * @param moduleType The module type to filter the ClinicalWorkflows by.
     * @return A list of ClinicalWorkflow entities matching the specified module type.
     */
    List<ClinicalWorkflow> findByModuleTypeAndIdNotIn(String moduleType, List<Long> ids);

    /**
     * Retrieves a list of ClinicalWorkflows based on the module type.
     * <p>
     * This method fetches all ClinicalWorkflow entities that are associated with a specific module type.
     * It allows for filtering ClinicalWorkflows that are relevant to a particular functionality or module
     * within the system.
     * </p>
     *
     * @param moduleType The module type to filter the ClinicalWorkflows by.
     * @return A list of ClinicalWorkflow entities matching the specified module type.
     */
    List<ClinicalWorkflow> findByModuleTypeAndIsDeletedFalseAndIsActiveTrue(String moduleType);
    
    /**
     * Retrieves a list of ClinicalWorkflow entities by their IDs, excluding those marked as deleted.
     * <p>
     * This method is designed to fetch multiple ClinicalWorkflow entities based on a list of identifiers,
     * ensuring that only those not marked as deleted are returned. It is useful for batch retrieval operations
     * where the deletion status needs to be considered to maintain data integrity and relevance.
     * </p>
     *
     * @param workflowIds A list of ClinicalWorkflow entity IDs to retrieve.
     * @return A list of ClinicalWorkflow entities matching the provided IDs and not marked as deleted.
     */
    List<ClinicalWorkflow> findByIdInAndIsDeletedFalseAndIsActiveTrue(List<Long> workflowIds);

    /**
     * Retrieves a paginated list of ClinicalWorkflow entities for a specific account based on country ID and optional search term.
     * <p>
     * This method utilizes a custom query defined by the {@code WORKFLOW_LIST} constant to fetch ClinicalWorkflow entities associated with a given country ID,
     * optionally filtered by a search term. The results are paginated, supporting large datasets. This method is particularly useful for accounts needing to filter workflows
     * by country and search criteria, ensuring efficient data retrieval and display.
     * </p>
     *
     * @param countryId  The ID of the country associated with the ClinicalWorkflow entities to retrieve.
     * @param searchTerm An optional search term for filtering the ClinicalWorkflow entities by name.
     * @param pageable   A {@link Pageable} object to determine the pagination parameters.
     * @return A {@link Page} of ClinicalWorkflow entities matching the criteria.
     */
    @Query(value = WORKFLOW_LIST)
    Page<ClinicalWorkflow> getDistrictWorkflows(@Param("countryId") long countryId,
        @Param("searchTerm") String searchTerm, Pageable pageable);

    /**
     * <p>
     * Finds account with give customized workflow ID
     * </p>
     *
     * @param id - customized workflow ID
     * @return List - List of account entity
     */
    @Query(value = GET_BY_CUSTOMIZATION_ID)
    List<HealthFacility> getHealthFacilityByCustomizedWorkflowIds(@Param(Constants.ID) long id);


}
