package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for ExaminationQA.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
public class ExaminationQADTO {

    private Long id;

    private Long examinationId;

    private String question;

    private String answer;

    private Integer displayOrder;

    private Integer answerDisplayOrder;

}
