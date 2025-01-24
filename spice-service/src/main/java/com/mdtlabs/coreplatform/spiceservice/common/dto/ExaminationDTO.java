package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Examination.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
public class ExaminationDTO {

    private Long id;

    private String type;

    private String name;

    private Integer displayOrder;

    private List<ExaminationQADTO> examinationQA = new ArrayList<>();
}
