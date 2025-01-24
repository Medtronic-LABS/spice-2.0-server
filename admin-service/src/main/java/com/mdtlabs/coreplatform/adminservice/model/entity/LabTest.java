package com.mdtlabs.coreplatform.adminservice.model.entity;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.adminservice.constants.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This class is an Entity represent LabTest fields.
 * </p>
 *
 * @author VigneshKumar created on Jun 30, 2022
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_LAB_TEST)
public class LabTest extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.UNIT)
    private String unit;

    @Column(name = FieldConstants.GENDER_VALUE)
    private String genderValue;

    @Column(name = FieldConstants.MIN_VALUE)
    private Integer minValue;

    @Column(name = FieldConstants.MAX_VALUE)
    private Integer maxValue;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

}
