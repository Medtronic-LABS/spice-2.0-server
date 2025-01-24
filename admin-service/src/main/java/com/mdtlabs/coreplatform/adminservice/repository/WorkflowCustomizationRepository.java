package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;

/**
 * <p>
 *   This repository interface has the needed customized functions for workflow
 *   customization.
 * </p>
 *
 * @author Karthick M created on Feb 08, 2023
 *
 */
@Repository
public interface WorkflowCustomizationRepository extends JpaRepository<WorkflowCustomization, Long> {

    public static final String GET_WORKFLOW_CUSTOMIZATION = "SELECT workflowCustomization FROM "
        + " WorkflowCustomization as workflowCustomization WHERE (workflowCustomization.countryId = :countryId) "
        + " AND (:category IS NULL OR workflowCustomization.category = :category)"
        + " AND (:type IS NULL OR upper(workflowCustomization.type) = upper(:type))"
        + " AND (:clinicalWorkflowId IS NULL OR workflowCustomization.clinicalWorkflowId = :clinicalWorkflowId) "
            + " AND (:districtId IS NULL OR workflowCustomization.districtId = :districtId) "
            + " AND workflowCustomization.isDeleted= false AND workflowCustomization.tenantId=:tenantId";


    public static final String GET_FORMS_BY_WORKFLOW  = "SELECT workflowCustomization FROM "
        + " WorkflowCustomization as workflowCustomization WHERE ((COALESCE(:clinicalWorkflowIds) is null OR (workflowCustomization.clinicalWorkflowId in (:clinicalWorkflowIds)))"
        + "  OR workflowCustomization.districtId = :districtId) AND  workflowCustomization.isDeleted=false AND workflowCustomization.isActive=true";

    /**
     * <p>
     *   Gets a Workflow customization by Id And Is Deleted.
     * </p>
     *
     * @param id {@link Long} Workflow customization id
     * @param isDeleted {@link Boolean} True or False
     * @return {@link WorkflowCustomization}  WorkflowCustomization entity
     */
    public WorkflowCustomization findByIdAndIsDeleted(Long id, Boolean isDeleted);
    
    /**
     * <p>
     *   Gets a Workflow customization by Id And Is Deleted.
     * </p>
     *
     * @param id {@link Long} Workflow customization id
     * @param isDeleted {@link Boolean} True or False
     * @param tenantId {@link Long} Tenant id
     * @return {@link WorkflowCustomization}  WorkflowCustomization entity
     */
    public WorkflowCustomization findByIdAndIsDeletedAndTenantId(Long id, Boolean isDeleted, Long tenantId);

    /**
     * <p>
     *   To get a Workflow customization details with conditions.
     * </p>
     *
     * @param countryId {@link Long} Country id
     * @param category {@link String} Category
     * @param type {@link String} Type
     * @param clinicalWorkflowId {@link Long} Clinical workflow id
     * @param isDeleted True or false
     * @return {@link WorkflowCustomization} WorkflowCustomization entity
     */
    @Query(value = GET_WORKFLOW_CUSTOMIZATION)
    public List<WorkflowCustomization> getWorkflowCustomization(@Param(Constants.COUNTRY_ID) Long countryId,
        @Param(FieldConstants.CATEGORY) String category,
        @Param(Constants.TYPE) String type, @Param(Constants.CLINICAL_WORKFLOW_ID) Long clinicalWorkflowId,
        @Param(Constants.TENANT_PARAMETER_NAME) Long tenantId, @Param(Constants.DISTRICT_ID) Long districtId);

    /**
     * <p>
     *   Gets list by countryID, category list and type list.
     * </p>
     *
     * @param countryId  {@link Long} Country id
     * @param categories {@link List} List of categories
     * @param screenTypes {@link List} List of types
     * @return {@link List} List of WorkflowCustomization entities
     */
    public List<WorkflowCustomization> findByCountryIdAndCategoryInAndTypeInAndIsDeletedFalse(Long countryId, List<String> categories,
        List<String> screenTypes);

    /**
     * Gets workflow customization by clinical workflow ids or district id.
     * 
     * @param clinicalWorkflowIds
     * @param districtId
     * @return List<WorkflowCustomization>
     */
    @Query(value = GET_FORMS_BY_WORKFLOW)
    public List<WorkflowCustomization> geWorkflowCustomizations(@Param("clinicalWorkflowIds") List<Long> clinicalWorkflowIds, @Param("districtId") Long districtId);
}
