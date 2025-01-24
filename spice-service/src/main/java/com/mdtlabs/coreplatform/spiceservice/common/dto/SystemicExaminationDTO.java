package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for SystemicExamination.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
public class SystemicExaminationDTO {

    private Long id;

    private String name;

    private String type;

    private Integer displayOrder;

    private String value;

}
