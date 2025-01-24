package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.entity.Unit;

/**
 * <p>
 * UnitService interface provides methods to interact with Unit entity.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jul 15, 2024
 */
public interface UnitService {

    /**
     * Fetches all units.
     *
     * @return a list of all units.
     */
    List<Unit> getUnitsByType(String type);
}
