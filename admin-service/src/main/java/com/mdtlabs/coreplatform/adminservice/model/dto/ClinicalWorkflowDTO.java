package com.mdtlabs.coreplatform.adminservice.model.dto;

import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * This class is a Data transfer object for clinical workflow Entity.
 *
 * @author Divya created on 7 Aug 2024
 */
@Data
public class ClinicalWorkflowDTO {

    private Long id;

    private String name;

    private String moduleType;

    private String groupName;

    private String workflowName;

    private Long countryId;

    private Long displayOrder;

    private List<Map<String, Object>> conditions;

    private List<String> viewScreens;

    private List<String> appTypes;

    private boolean isDefault;

}
