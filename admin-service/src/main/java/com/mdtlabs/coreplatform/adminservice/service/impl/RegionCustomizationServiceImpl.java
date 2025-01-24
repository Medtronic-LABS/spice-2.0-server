package com.mdtlabs.coreplatform.adminservice.service.impl;

import com.mdtlabs.coreplatform.adminservice.model.entity.RegionCustomization;
import com.mdtlabs.coreplatform.adminservice.repository.RegionCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.service.RegionCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * This service class maintains the CRUD operations for region customization.
 * </p>
 *
 * @author Praveen created on Aug 26, 2024
 */
@Service
public class RegionCustomizationServiceImpl implements RegionCustomizationService {

    private final RegionCustomizationRepository regionCustomizationRepository;

    public RegionCustomizationServiceImpl(RegionCustomizationRepository regionCustomizationRepository) {
        this.regionCustomizationRepository = regionCustomizationRepository;
    }

    /**
     * {@inheritDoc}
     */
    public List<RegionCustomization> getRegionCustomizationsByCategory(String category) {
        Long countryId = UserContextHolder.getUserDto().getCountry().getId();
        return regionCustomizationRepository.findByCountryIdAndCategoryAndIsActiveTrueAndIsDeletedFalse(countryId, category);
    }

}
