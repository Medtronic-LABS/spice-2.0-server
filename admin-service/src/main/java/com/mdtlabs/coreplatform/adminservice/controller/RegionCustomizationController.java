package com.mdtlabs.coreplatform.adminservice.controller;

import com.mdtlabs.coreplatform.adminservice.model.entity.RegionCustomization;
import com.mdtlabs.coreplatform.adminservice.service.RegionCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * <p>
 * This controller class maintains CRUD operation for region customization data.
 * </p>
 *
 * @author Praveen created on Aug 26, 2024
 */
@RestController
@RequestMapping("/region-customization")
public class RegionCustomizationController {

    private final RegionCustomizationService regionCustomizationService;

    public RegionCustomizationController(RegionCustomizationService regionCustomizationService) {
        this.regionCustomizationService = regionCustomizationService;
    }

    /**
     * <p>
     * Gets list of customizations.
     * </p>
     *
     * @param category category of the region customization
     * @return {@link List} List of RegionCustomizations
     */
    @PostMapping("/static-data/get-list/{category}")
    public List<RegionCustomization> getRegionCustomizationsByCategory(@PathVariable(Constants.CATEGORY) String category) {
        return regionCustomizationService.getRegionCustomizationsByCategory(category);
    }
}
