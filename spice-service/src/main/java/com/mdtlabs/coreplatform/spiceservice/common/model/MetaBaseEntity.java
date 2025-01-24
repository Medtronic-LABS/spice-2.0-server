package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.Map;

import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import lombok.Data;

/*
 * THis is mapped super class entity for meta entites
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@MappedSuperclass
public class MetaBaseEntity extends BaseEntity {

    @Column(name = FieldConstants.DISPLAY_VALUES) 
    @Type(value = JsonBinaryType.class)
    private Map<String, String> displayValues;
    
}
