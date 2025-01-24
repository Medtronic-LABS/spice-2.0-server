package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This is a entity class for Examination QA.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Entity
@Data
@Table(name = TableConstants.EXAMINATION_QA)
public class ExaminationQA extends BaseEntity {

    @Column(name = FieldConstants.EXAMINATION_ID)
    private Long examinationId;

    @Column(name = FieldConstants.QUESTION)
    private String question;

    @Column(name = FieldConstants.ANSWER)
    private String answer;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Integer displayOrder;

    @Column(name = FieldConstants.ANSWER_DISPLAY_ORDER)
    private Integer answerDisplayOrder;

}
