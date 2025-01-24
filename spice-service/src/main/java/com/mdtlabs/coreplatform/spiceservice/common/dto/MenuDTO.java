package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * This class is the Data Transfer Object for Menu entity.
 * </p>
 *
 * @author Divya S created on Aug 15, 2024
 */
@Data
public class MenuDTO {

    private Long id;

    private String roleName;

    private List<Map<String, Object>> menus;

    private List<String> meta;

    private List<String> metaForms;

    private Map<String, Object> jsonDisplayValues;

    private Long formDataId;

    private Long tenantId;

    private String formName;
}
