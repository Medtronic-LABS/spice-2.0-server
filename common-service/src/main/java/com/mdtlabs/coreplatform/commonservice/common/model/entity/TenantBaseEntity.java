package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import lombok.Data;
import org.hibernate.annotations.Filter;
import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.ParamDef;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.listeners.TenantEntityListener;

import jakarta.persistence.Column;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.MappedSuperclass;



/**
 * <p>
 * This class is for tenant entity
 * </p>
 * 
 * @author Vignesh created on July 30 2022
 */
@Data
@MappedSuperclass
@FilterDef(name = Constants.TENANT_FILTER_NAME,
        parameters = @ParamDef(name = Constants.TENANT_PARAMETER_NAME, type = Long.class),
        //defaultCondition = Constants.TENANT_COLUMN_NAME + " in (:" + Constants.TENANT_PARAMETER_NAME + ")")
        defaultCondition = Constants.TENANT_COLUMN_NAME + " = :" + Constants.TENANT_PARAMETER_NAME)
@Filter(name = Constants.TENANT_FILTER_NAME)
@EntityListeners(TenantEntityListener.class)
public class TenantBaseEntity extends BaseEntity{


    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.TENANT_ID)
	protected Long tenantId;

    public TenantBaseEntity(Long id) {
        super(id);
    }

    public TenantBaseEntity() {
    }
}
