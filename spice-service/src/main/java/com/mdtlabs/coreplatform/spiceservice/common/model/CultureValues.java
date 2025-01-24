package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import lombok.Data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * <p>
 * CultureValues is a Java class representing a table called
 * "CultureValues" with various columns including a JSON column.
 * </p>
 *
 * @author Gopinath R created on Aug 21, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_CULTURE_VALUES)
public class CultureValues extends BaseEntity{
    
    @Column(name = FieldConstants.FORM_DATA_ID)
    private long formDataId;
    
    @Column(name = FieldConstants.FORM_NAME)
    private String formName;
    
    @Column(name = FieldConstants.CULTURE_ID)
    private long cultureId;
    
    @Column(name = FieldConstants.CULTURE_VALUE)
    private String cultureValue;
    
    @Column(name = FieldConstants.JSON_CULTURE_VALUE, columnDefinition = "jsonb")
    private Object jsonCultureValue;
    
}
