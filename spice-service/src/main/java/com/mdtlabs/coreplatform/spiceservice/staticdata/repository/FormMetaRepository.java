package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.FormMeta;

/**
 * This interface is responsible for performing database operations between
 * server and FormMeta entity.
 */
@Repository
public interface FormMetaRepository extends JpaRepository<FormMeta, Long> {

    // Get query for form meta based on form type
    static final String GET_QUERY_FOR_FORM_META_BY_TYPE = "FROM FormMeta AS form left join form.clinicalWorkflow as workflow  WHERE "
            + " form.isDeleted = false AND form.isActive = true ";

    // Get query for form meta based on form type and clinical workflow
    static final String GET_QUERY_FOR_FORM_META_BY_TYPE_AND_CLINICAL_WORKFLOW = GET_QUERY_FOR_FORM_META_BY_TYPE
            + " AND form.clinicalWorkflow.workflowName = (:workflowName) OR form.formType = (:formType)";

    // Get query for form meta based on form type and clinical workflow
    static final String GET_QUERY_FOR_FORM_META_BY_CLINICAL_WORKFLOW = "SELECT f.* "
            + "FROM form_meta f LEFT JOIN clinical_workflow cw ON f.clinical_workflow_id = cw.id "
            + "WHERE f.is_deleted = false AND f.is_active = true "
            + "AND (cw.id IN (:workflowIds) OR f.form_type IN (:formTypes)) "
            + "AND f.app_types && CAST(:appTypes AS VARCHAR[])";

    /**
     * Retrieves a {@link FormMeta} entity based on a specified form type.
     * <p>
     * This method performs a query to fetch a {@link FormMeta} entity that matches the specified form type.
     * It is designed to support filtering form metadata based on the form type, allowing for targeted retrieval
     * of form metadata. This can be particularly useful in scenarios where there's a need to fetch forms relevant
     * to a specific type.
     * </p>
     *
     * @param formType The type of form used to retrieve the form meta.
     * @return A {@link FormMeta} object that matches the specified form type.
     */
    @Query(value = GET_QUERY_FOR_FORM_META_BY_TYPE)
    FormMeta getFormMetaByFormType(String formType);

    /**
     * Retrieves a {@link FormMeta} entity based on a specified form type and clinical workflow name.
     * <p>
     * This method performs a query to fetch a {@link FormMeta} entity that matches both the specified form type
     * and clinical workflow name. It supports filtering form metadata based on both criteria, enabling more
     * precise retrieval of form metadata. This method is useful when forms are associated with specific clinical
     * workflows and there's a need to fetch forms relevant to certain workflows and types.
     * </p>
     *
     * @param workflowName The name of the clinical workflow used for filtering the form metadata.
     * @param formType     The type of form used to retrieve the form meta.
     * @return A {@link FormMeta} object that matches the specified criteria.
     */
    @Query(value = GET_QUERY_FOR_FORM_META_BY_TYPE_AND_CLINICAL_WORKFLOW)
    FormMeta getFormMetaByFormTypeAndClinicalWorkflowName(String workflowName, String formType);

    /**
     * Retrieves a list of {@link FormMeta} entities based on specified clinical workflow IDs and form types.
     * <p>
     * This method performs a query to fetch {@link FormMeta} entities that match a list of clinical workflow IDs and form types.
     * It is designed to support filtering form metadata based on clinical workflows and form types, allowing for more
     * granular retrieval of form metadata. This can be particularly useful in scenarios where forms are associated with
     * specific clinical workflows and there's a need to fetch forms relevant to certain workflows or types.
     * </p>
     *
     * @param workflowNames A list of clinical workflow IDs used for filtering the form metadata.
     * @param formTypes     A list of form types used for filtering the form metadata.
     * @return A list of {@link FormMeta} entities that match the specified criteria.
     */
    @Query(value = GET_QUERY_FOR_FORM_META_BY_CLINICAL_WORKFLOW, nativeQuery = true)
    List<FormMeta> getFormMetaByClinicalWorkflowIds(@Param("workflowIds") List<Long> workflowNames,
            @Param("formTypes") List<String> formTypes, @Param("appTypes") String[]  appTypes);

}
