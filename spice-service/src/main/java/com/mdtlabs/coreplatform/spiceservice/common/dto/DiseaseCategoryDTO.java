package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for DiseaseCategory.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
public class DiseaseCategoryDTO {

    private Long id;

    private String name;

    private Integer displayOrder;

    private String value;

    private String type;

    private List<DiseaseConditionDTO> diseaseCondition = new ArrayList<>();

}
