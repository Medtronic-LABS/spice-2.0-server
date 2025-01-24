package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;


/**
 * <p>
 * This class contains the necessary field for WorkflowCustomization operations.
 * </p>
 * 
 * @author Jeyaharini T A created on Feb 08, 2023
 *
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_WORKFLOW_CUSTOMIZATION)
public class WorkflowCustomization extends TenantBaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.CATEGORY)
    private String category; // module, screening, enrollment

    @Column(name = FieldConstants.FORM_INPUT)
    private String formInput;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.CLINICAL_WORFKLOW_ID)
    private Long clinicalWorkflowId;

    @Column(name = FieldConstants.DISTRICT_ID)
    private Long districtId;
}
