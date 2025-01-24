package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;
import java.util.Map;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.Data;
import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

/**
 *
 * This FormMetaUi Class is used to get FormMetaUi Entity.
 *
 * @author Tamilarasi Shanmugasundaram created on Aug 8, 2022
 *
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_FORM_META_UI)
public class FormMetaUi extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.FORMNAME)
    private String formName;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.COMPONENTS, columnDefinition = "jsonb")
    private List<Map<String, Object>> components;

}

