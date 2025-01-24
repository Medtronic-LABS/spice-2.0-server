package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;

/**
 * <p>
 * This Repository class contains the needed customized functions for terms and condition
 * customization.
 * </p>
 *
 * @author Divya created on Oct 22, 2024
 */
@Repository
public interface UserTermsAndConditionsRepository extends JpaRepository<UserTermsAndConditions, Long> {

    /**
     * <p>
     * Gets list of Terms and condition by country id.
     * </p>
     *
     * @param countryId {@link Long} The country id for which the user terms and conditions is returned
     * @return {@link UserTermsAndConditions} UserTermsAndCondition entity of given country id is returned.
     */
    List<UserTermsAndConditions> findByCountryIdAndIsDeletedFalseAndIsActiveTrue(Long countryId);
}
