package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This class is an entity used to represent answers for questions form the application.
 * </p>
 *
 * @author Niraimathi S created on Jun 30, 2022
 *
 */
@Data
@Entity
@Table(name = TableConstants.MODEL_ANSWERS)
public class ModelAnswers extends MetaBaseEntity {

    private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Integer displayOrder;

    @Column(name = FieldConstants.IS_DEFAULT)
    private boolean isDefault;
    
    @Column(name = FieldConstants.VALUE)
    private Integer value;
    
}