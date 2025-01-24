package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

import java.util.List;

/**
 * <p>
 * This is a entity class for  Meta Details.
 * </p>
 *
 * @author Nandhakumar Created on Apr 05, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_META)
public class Meta extends MetaBaseEntity {

    private static final long serialVersionUID = 4174505913611242103L;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Integer displayOrder;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.VALUE)
    private String value;

    @Column(name = FieldConstants.CATEGORY)
    private String category;

    @Column(name = FieldConstants.DESCRIPTION)
    private String description;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;

}
