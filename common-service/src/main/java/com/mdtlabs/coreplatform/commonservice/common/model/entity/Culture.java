package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

import java.util.List;

/**
 * This is a entity class for culture.
 * 
 * @author Karthick M created at Apr 25
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_CULTURE)
public class Culture extends BaseEntity {
    
    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.CODE)
    private String code;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;
}
