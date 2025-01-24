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
 * This class is an Entity represent Brand fields.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_REGION_CUSTOMIZATION)
public class RegionCustomization extends TenantBaseEntity {

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
}
