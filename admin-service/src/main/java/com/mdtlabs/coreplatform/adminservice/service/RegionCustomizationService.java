package com.mdtlabs.coreplatform.adminservice.service;

import com.mdtlabs.coreplatform.adminservice.model.entity.RegionCustomization;

import java.util.List;

/**
 * <p>
 * This an interface class for Country module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Praveen created on Aug 26, 2024
 */
public interface RegionCustomizationService {

    /**
     * <p>
     * To get list of region customized data.
     * </p>
     *
     * @param category category of the region customization
     * @return {@link List<RegionCustomization>} List of RegionCustomization Entity.
     */
    List<RegionCustomization> getRegionCustomizationsByCategory(String category);
}
