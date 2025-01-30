package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;
import java.util.Map;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.Data;
import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

/**
 * This is entity class for symptom.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_SYMPTOMS)
public class Symptom extends MetaBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Long displayOrder;

    @Column(name = FieldConstants.CATEGORY)
    private String category;

    @Column(name = FieldConstants.DISEASE_TYPE)
    private String diseaseType;

    @Column(name = FieldConstants.VALUE)
	private String value;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.CATEGORIES, columnDefinition = "jsonb")
    private Map<String, Boolean> categories;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;

}
