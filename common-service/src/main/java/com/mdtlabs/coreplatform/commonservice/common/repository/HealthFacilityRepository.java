package com.mdtlabs.coreplatform.commonservice.common.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;


/**
 * This interface represents a repository for managing {@link HealthFacility} entities.
 * It extends {@link JpaRepository} to provide CRUD operations and custom queries.
 *
 * @author Vishwaeaswaran M created at 3 Oct 2024.
 */
@Repository(value = "healthFacilityRepo")
public interface HealthFacilityRepository extends JpaRepository<HealthFacility, Long> {

    /**
     * Finds a single {@link HealthFacility} entity by tenantId, ensuring it is not deleted and is active.
     *
     * @param tenantId The unique identifier of the tenant.
     * @return The {@link HealthFacility} entity that matches the given criteria, or {@code null} if no such entity
     * exists.
     */
    HealthFacility findByTenantIdAndIsDeletedFalseAndIsActiveTrue(Long tenantId);
}
