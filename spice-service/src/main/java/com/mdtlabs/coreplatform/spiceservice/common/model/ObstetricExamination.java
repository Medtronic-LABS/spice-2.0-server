package com.mdtlabs.coreplatform.spiceservice.common.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.Data;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

/**
 * <p>
 * This is a entity class for Obstetric Examinations.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 20, 2024.
 */
@Entity
@Data
@Table(name = TableConstants.OBSTETRIC_EXAMINATION)
public class ObstetricExamination extends MetaBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Integer displayOrder;

    @Column(name = FieldConstants.VALUE)
    private String value;
}
