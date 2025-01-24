package com.mdtlabs.coreplatform.adminservice.service;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;

/**
 * <p>
 * This service class maintains the CRUD operations for Terms and condition customization.
 * </p>
 *
 * @author Divya created on Oct 22, 2024
 */
public interface UserTermsAndConditionsService {

    /**
     * <p>
     * Get the Terms and Condition data based on country ID.
     * </p>
     *
     * @param searchRequestDTO {@link SearchRequestDTO} The request containing county id is given
     * @return {@link UserTermsAndConditions} UserTermsAndCondition entity of given country id is returned.
     */
    UserTermsAndConditions getTermsAndConditionsValue(SearchRequestDTO searchRequestDTO);
}
