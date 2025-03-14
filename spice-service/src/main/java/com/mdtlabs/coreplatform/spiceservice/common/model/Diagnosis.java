package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;
import lombok.Data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import java.util.List;

/**
 * This is entity class for diagnosis.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_DIAGNOSIS)
public class Diagnosis extends MetaBaseEntity {
    
    private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DESCRIPTION)
    private String description;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.GENDER)
    private String gender;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;
    
    @Column(name = FieldConstants.VALUE)
    private String value;
}