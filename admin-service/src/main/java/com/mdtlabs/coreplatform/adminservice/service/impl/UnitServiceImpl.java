package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;

import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.entity.Unit;
import com.mdtlabs.coreplatform.adminservice.repository.UnitRepository;
import com.mdtlabs.coreplatform.adminservice.service.UnitService;

/**
 * <p>
 * UnitServiceImpl class provides methods to interact with Unit entity.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jul 15, 2024
 */
@Service
public class UnitServiceImpl implements UnitService {

    private final UnitRepository unitRepository;

    public UnitServiceImpl(UnitRepository unitRepository) {
        this.unitRepository = unitRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Unit> getUnitsByType(String type) {
        return unitRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(type.toUpperCase());
    }
}
