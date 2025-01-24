package com.mdtlabs.coreplatform.adminservice.repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.RegionCustomization;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * This Repository class contains the needed customized functions for region
 * customization.
 * </p>
 *
 * @author Praveen created on Aug 26, 2024
 */
@Repository
public interface RegionCustomizationRepository extends JpaRepository<RegionCustomization, Long> {
    /**
     * <p>
     * Gets list of RegionCustomization by countryId and cultureId.
     * </p>
     *
     * @param countryId country id
     * @param category category of the region customization
     * @return RegionCustomization list
     */
    public List<RegionCustomization> findByCountryIdAndCategoryAndIsActiveTrueAndIsDeletedFalse(Long countryId, String category);

}
