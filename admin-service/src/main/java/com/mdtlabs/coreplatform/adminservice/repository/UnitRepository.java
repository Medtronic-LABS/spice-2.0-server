package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.Unit;

/**
 * <p>
 * UnitRepository provides methods to interact with Unit entity.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jul 15, 2024
 */
@Repository
public interface UnitRepository extends JpaRepository<Unit, Long> {

    /**
     * Fetches all units by type.
     *
     * @param type - type of the unit.
     * @return a list of all units.
     */
    List<Unit> findByTypeAndIsActiveTrueAndIsDeletedFalse(String type);
}
