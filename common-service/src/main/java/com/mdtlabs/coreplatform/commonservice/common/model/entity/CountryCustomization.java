package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This is entity class for country customization.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_COUNTRY_CUSTOMIZATION)
public class CountryCustomization extends TenantBaseEntity {

    private static final long serialVersionUID = -8642271760727722638L;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.CATEGORY)
    private String category;

    @Column(name = FieldConstants.FORM_INPUT)
    private String formInput;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;
    
    @Column(name = FieldConstants.IS_DEFAULT)
    private boolean isDefault;

    @Column(name = FieldConstants.CULTURE_ID)
    private Long cultureId;

    @Column(name = FieldConstants.CLINICAL_WORFKLOW_ID)
    private Long clinicalWorkflowId;
}
