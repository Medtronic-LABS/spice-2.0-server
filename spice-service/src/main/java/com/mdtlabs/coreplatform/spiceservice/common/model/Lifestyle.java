package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;
import java.util.Map;

import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This is entity class for lifestyle.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_LIFESTYLE)
public class Lifestyle extends MetaBaseEntity {

    private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.ANSWERS, columnDefinition = "jsonb")
    private List<Map<String, Object>> answers;

    @Column(name = FieldConstants.IS_ANSWER_DEPENDENT)
    private boolean isAnswerDependent;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.JSON_DISPLAY_VALUES)
    private Map<String, Object> jsonDisplayValues;

    @Column(name = FieldConstants.VALUE)
    private String value;

}
