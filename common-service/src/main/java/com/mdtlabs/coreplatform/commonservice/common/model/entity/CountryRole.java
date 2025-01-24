package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This is entity class for country role.
 * 
 * @author Karthick M created on May 29 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_COUNTRY_ROLE)
public class CountryRole extends BaseEntity {

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.ROLE_ID)
    private Long roleId;

    @Column(name = FieldConstants.DISPLAY_NAME)
    private String displayName;
 
}
