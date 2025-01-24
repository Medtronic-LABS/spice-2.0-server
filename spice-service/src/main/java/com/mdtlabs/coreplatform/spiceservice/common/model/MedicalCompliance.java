package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

import java.util.List;

/**
 * This is entity class for medical compliance.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_MEDICAL_COMPLIANCE)
public class MedicalCompliance extends MetaBaseEntity {
    
    private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.NAME)
    private String name;
    
    @Column(name = FieldConstants.STATUS)
    private boolean status;
    
    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;
    
    @Column(name = FieldConstants.IS_CHILD_EXISTS)
    private boolean isChildExists;
    
    @Column(name = FieldConstants.PARENT_COMPLIANCE_ID)
    private Long parentComplianceId;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;
    
    @Column(name = FieldConstants.VALUE)
    private String value;
    
}
