package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for DiseaseCondition.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
public class DiseaseConditionDTO implements Serializable {
	
    private Long id;
    
    private Long diseaseId;
    
    private String name;
    
    private Integer displayOrder;

    private String value;

}
