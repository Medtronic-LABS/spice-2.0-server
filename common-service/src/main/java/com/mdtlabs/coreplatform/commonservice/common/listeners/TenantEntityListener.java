package com.mdtlabs.coreplatform.commonservice.common.listeners;

import java.util.Objects;

import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreRemove;
import jakarta.persistence.PreUpdate;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;

/**
 * <p>
 * This class is for tenant entity listener
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public class TenantEntityListener {

    /**
     * <p>
     * This function sets the tenant ID for a given object before it is persisted or updated in the
     * database.
     * </p>
     *
     * @param object The "object" parameter is an instance of an entity class that is being persisted or
     * updated in the database is given
     */
    @PrePersist
    @PreUpdate
    public void prePersistAndUpdate(Object object) {
        if(object instanceof TenantBaseEntity tenantBaseEntity){
            Long tenantId = Objects.isNull(tenantBaseEntity.getTenantId()) ? UserSelectedTenantContextHolder.get() : tenantBaseEntity.getTenantId();
            tenantBaseEntity.setTenantId(Constants.LONG_ZERO.equals(tenantId) ? null : tenantId);
        }
    }

    /**
     * <p>
     * This is a pre-remove hook in Java that checks if the entity being removed belongs to the current
     * tenant, and throws an exception if it doesn't.
     * </p>
     *
     * @param object The object being removed from the database.
     */
    @PreRemove
    public void preRemove(Object object) {
        if (object instanceof TenantBaseEntity tenantBaseEntity &&
                (!Objects.equals(tenantBaseEntity.getTenantId(), UserSelectedTenantContextHolder.get()))) {
            throw new EntityNotFoundException();
        }
    }
}