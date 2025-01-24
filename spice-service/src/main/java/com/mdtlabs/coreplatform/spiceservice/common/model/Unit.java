package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This is entity class for unit.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_UNIT)
public class Unit extends MetaBaseEntity {
    
    private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.DESCRIPTION)
    private String description;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;
}
