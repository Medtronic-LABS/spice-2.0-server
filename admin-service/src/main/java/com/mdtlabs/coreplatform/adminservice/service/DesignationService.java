package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;

/**
 * <p>
 * This an interface class for Designation module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Divya created on Oct 20, 2024
 */
public interface DesignationService {

    /**
     * <p>
     * To Fetch all Designation for given countryId.
     * </p>
     *
     * @param countryId ID of the country to which the designation list to be retrieved is given
     * @return {@link List} the list of designation is retrieved
     */
    List<Designation> getAllDesignation(Long countryId);
}
