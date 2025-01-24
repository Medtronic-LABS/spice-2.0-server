package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Country module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Dec 26, 2023
 */
@Repository
public interface CountryRepository extends JpaRepository<Country, Long> {

    String GET_COUNTRIES_BY_NAME = "select country from Country as "
            + "country where lower(country.name) LIKE CONCAT('%',lower(:searchTerm),'%') "
            + "AND country.isDeleted=false AND country.isActive = true order by country.updatedBy";

    String GET_COUNTRY_BY_ID = "select country from Country as "
            + "country where country.id=:id AND (:tenantId is null OR country.tenantId=:tenantId) "
            + "AND country.isDeleted=:isDeleted AND country.isActive=:isActive";

    /**
     * Finds countries by their phone number code or name, ignoring case.
     * <p>
     * This method searches for countries either by their phone number code or by their name, disregarding case sensitivity.
     * It allows for flexible searches that can match on either attribute, providing a broader search capability.
     * </p>
     *
     * @param countryCode The phone number code of the country to find.
     * @param strip       The name of the country to find, case-insensitive.
     * @return A list of {@link Country} entities that match the given phone number code or name.
     */
    List<Country> findByPhoneNumberCodeOrNameIgnoreCaseAndIsActiveTrueAndIsDeletedFalse(String countryCode, String strip);

    /**
     * Finds a country by its ID, tenant ID, deletion, and activation status.
     * <p>
     * This method retrieves a single country based on a combination of its unique identifier, associated tenant ID,
     * deletion status, and activation status. It is particularly useful for multi-tenant applications where it's
     * necessary to filter countries not only by their existence but also by their relevance to a specific tenant
     * and their current state (deleted or active).
     * </p>
     *
     * @param id       The unique identifier of the country.
     * @param tenantId The tenant ID associated with the country.
     * @param b        The deletion status of the country (true if deleted).
     * @param c        The activation status of the country (true if active).
     * @return The {@link Country} entity if found, null otherwise.
     */
    @Query(value = GET_COUNTRY_BY_ID)
    Country getCountryById(Long id, Long tenantId, boolean isDeleted, boolean isActive);

    /**
     * Finds a country by its ID, provided it has not been marked as deleted.
     * <p>
     * This method retrieves a single Country entity based on its unique identifier,
     * ensuring that the country has not been marked as deleted. This is particularly
     * useful for fetching active country records for processing or display.
     * </p>
     *
     * @param id The unique identifier of the country to retrieve.
     * @return The Country entity if found and not deleted, null otherwise.
     */
    Country findByIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     * Retrieves a list of countries that are not marked as deleted and are active.
     * <p>
     * This method fetches all Country entities that are both active and not marked
     * as deleted. It is useful for operations that require a list of currently active
     * and relevant countries, such as filtering options in user interfaces.
     * </p>
     *
     * @return A list of Country entities that are active and not marked as deleted.
     */
    List<Country> findByIsDeletedFalseAndIsActiveTrue();

    /**
     * <p>
     *     Gets the page of countries based on the given pagination
     * </p>
     *
     * @param searchTerm {@link String} The search term to fetch the page of countries is given
     * @param pageable {@link Pageable} The pageable to fetch the page of countries is given
     * @return {@link Page} The page of countries for the given search term and pageable is retrieved
     */
    @Query(value = GET_COUNTRIES_BY_NAME)
    Page<Country> searchCountries(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm, Pageable pageable);
}
