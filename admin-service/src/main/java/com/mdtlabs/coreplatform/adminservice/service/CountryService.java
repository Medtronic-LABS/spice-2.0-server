package com.mdtlabs.coreplatform.adminservice.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;

/**
 * <p>
 * This an interface class for Country module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
public interface CountryService {

    /**
     * Creates a new country.
     * <p>
     * This method is responsible for creating a new country entity based on the provided {@link CountryRequestDTO}.
     * It encapsulates the country creation logic, including validation and persistence in the database.
     * </p>
     *
     * @param request The {@link CountryRequestDTO} containing the necessary information to create a new country.
     * @return The created {@link Country} entity.
     */
    Country createCountry(CountryRequestDTO request);

    /**
     * Adds an administrator to a region.
     * <p>
     * This method assigns an administrator role to a user for a specific region. The assignment details,
     * including the target region and the user to be assigned, are provided through the {@link UserRequestDTO}.
     * This can include setting specific permissions or roles to the user within the region's context.
     * </p>
     *
     * @param request The {@link UserRequestDTO} containing the details of the user and the region.
     * @return A {@link UserResponseDTO} containing the updated information of the user after the assignment.
     */
    UserResponseDTO addRegionAdmin(UserRequestDTO request);

    /**
     * Updates the administrator details for a region.
     * <p>
     * This method updates the details of an existing administrator for a region. It can involve changing the
     * user's roles, permissions, or associated regions. The updated details are provided through the
     * {@link UserRequestDTO}.
     * </p>
     *
     * @param request The {@link UserRequestDTO} containing the updated details of the administrator.
     * @return A {@link UserResponseDTO} reflecting the updated administrator details.
     */
    UserResponseDTO updateRegionAdmin(UserRequestDTO request);

    /**
     * Removes an administrator from a region.
     * <p>
     * This method is responsible for removing an administrator role from a user for a specific region. The details of the
     * administrator to be removed, including the target region, are provided through the {@link SearchRequestDTO}.
     * This operation may involve updating the user's roles or permissions within the context of the region.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the details of the administrator to be removed.
     * @return A {@link UserResponseDTO} containing the updated information of the user after the removal.
     */
    UserResponseDTO removeAdmin(SearchRequestDTO request);

    /**
     * Retrieves details of a country.
     * <p>
     * This method fetches the details of a country based on the criteria provided in the {@link SearchRequestDTO}.
     * The search criteria can include various parameters such as country ID, name, etc., to identify the country.
     * The method returns a {@link CountryDTO} containing the details of the country.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the search criteria.
     * @return A {@link CountryDTO} containing the details of the country.
     */
    CountryDTO getCountryDetails(SearchRequestDTO request);

    /**
     * Updates a country's details.
     * <p>
     * This method updates the details of an existing country with new information provided in the {@link CountryRequestDTO}.
     * It can be used to update various aspects of a country, such as its name, code, or other attributes. The method returns
     * the updated {@link Country} entity, reflecting the changes made.
     * </p>
     *
     * @param request The {@link CountryRequestDTO} containing the updated information of the country.
     * @return The updated {@link Country} entity.
     */
    Country updateCountry(CountryRequestDTO request);

    /**
     * <p>
     *     Gets the page of countries based on the given pagination
     * </p>
     *
     * @param searchTerm {@link String} The search term to fetch the page of countries is given
     * @param pageable {@link Pageable} The pageable to fetch the page of countries is given
     * @return {@link Page} The page of countries for the given search term and pageable is retrieved
     */
    Page<Country> getCountries(String searchTerm, Pageable pageable);
}
