package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

/**
 * This class is an Data transfer object for workflow customization Entity.
 * 
 * @author Karthick created on 17 Feb 2023
 *
 */
@Data
public class WorkflowCustomizationDTO {
    
    private Long id;
    
    private Long tenantId;
    
    private String type;

    private String category;

    private String formInput;

    private Long countryId;

    private Long clinicalWorkflowId;

    private Long districtId;

    private String workflow;
    
}
