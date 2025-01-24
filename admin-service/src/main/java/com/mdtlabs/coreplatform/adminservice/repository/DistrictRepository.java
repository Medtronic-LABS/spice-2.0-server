package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the role module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Divya S
 */
@Repository
public interface DistrictRepository extends JpaRepository<District, Long> {

    public static final String GET_DISTRICT_LIST = "SELECT district FROM District as district"
            + " WHERE district.countryId=:countryId "
            + " AND district.isDeleted=false and district.isActive=true "
            + " AND (:searchTerm is null or lower(district.name) LIKE CONCAT('%',lower(:searchTerm),'%'))";

    public static final String GET_DEACTIVATED_DISTRICTS = "SELECT district FROM District AS district WHERE "
            + " (:searchTerm IS null OR lower(district.name) LIKE CONCAT('%',lower(:searchTerm),'%')) "
            + " AND (:countryId IS NULL or district.countryId=:countryId) AND district.isActive=false AND "
            + " district.isDeleted = false";

    String COUNT_BY_COUNTRY_IDS = "SELECT district.countryId as countryId, COUNT(district.id) AS count "
            + "FROM District as district WHERE countryId in :ids AND district.isDeleted=false AND "
            + "district.isActive=:isActive GROUP BY countryId";

    /**
     * Finds district by country.
     * 
     * @param countryId
     * @return List<District>
     */
	List<District> findByCountryIdAndIsDeletedFalse(Long countryId);

    /**
     * <p>
     * This method is used to get the page of districts for the given search term with provided pagination.
     * </p>
     *
     * @param searchTerm {@link String} The term or keyword that is being searched for in the query to
     *                   filter the results is given
     * @param countryId  {@link Long} The ID of the country associated with the districts that
     *                   are being searched is given
     * @param pageable   {@link Pageable} The pagination information that contains information such as the page number,
     *                   page size, sorting criteria, and more is given
     * @return {@link Page<District>} A Page of non-deleted and active districts that match the search criteria with applied
     * pagination is retrieved and returned from the database
     */
    @Query(value = GET_DISTRICT_LIST)
    public Page<District> findDistrictList(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                                       @Param(Constants.COUNTRY_ID) Long countryId, Pageable pageable);

    /**
     * Finds districts by country ID, excluding those marked as deleted.
     * <p>
     * This method retrieves a list of District entities based on the provided country ID,
     * ensuring that only those not marked as deleted are included. It is particularly useful
     * for operations requiring the retrieval of active districts within a specific country.
     * </p>
     *
     * @param countryId The ID of the country for which districts are to be retrieved.
     * @return A list of District entities that belong to the specified country and are not marked as deleted.
     */
    List<District> findByCountryIdAndIsDeletedFalseAndIsActiveTrue(Long countryId);

    /**
     * <p>
     * This method is used to get an district by tenant ID, active status and deletion status.
     * </p>
     *
     * @param id        {@link Long} The ID of the district that need to be retrieved is given
     * @param tenantId  {@link Long} The tenant ID of the district that need to be retrieved is given
     * @param isActive  The boolean value that is used to filter the results of the query based
     *                  on whether the district has been marked as active or not is given
     * @param isDeleted The boolean value that is used to filter the results of the query based
     *                  on whether the district has been marked as deleted or not is given
     * @return {@link District} The district for the given tenant ID, active status and deletion status is
     * retrieved from the database and returned
     */
    public District findByIdAndTenantIdAndIsActiveAndIsDeleted(Long id, Long tenantId, boolean isActive,
                                                              boolean isDeleted);

    /**
     * <p>
     * This method is used to get a district for the given ID and deletion status.
     * </p>
     *
     * @param id        The ID for which the district is being searched is given
     * @param isDeleted The boolean value that is used to filter the results of the query based
     *                  on whether the district has been marked as deleted or not is given
     * @return {@link District} The district is returned for the given search criteria like ID and isDeleted status
     */
    public District findByIdAndIsDeleted(long id, boolean isDeleted);

    /**
     * <p>
     * This method is used to check the availability of the district by its name and ID which
     * has not been deleted.
     * </p>
     *
     * @param name {@link String} The name of the district to be searched is given
     * @param id   {@link Long} The ID for which the district is being searched is given
     * @return True is returned if the district for given name and ID is already available
     * in the database otherwise false
     */
    public boolean existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(String name, Long id);

    /**
     * <p>
     * This method is used to get list of district for the provided tenant ID and active status.
     * </p>
     *
     * @param tenantId {@link Long} The tenant ID of the district that need to be retrieved is given
     * @param isActive The boolean value that is used to filter the results of the query based
     *                 on whether the district has been marked as active or not is given
     * @return {@link District} The non-deleted district which is either active or inactive for the given tenant ID is
     * retrieved from the database and returned
     */
    public District findByTenantIdAndIsDeletedFalseAndIsActive(Long tenantId, boolean isActive);

    /**
     * <p>
     * This method is used to get the page of  deactivated districts for the given search term with provided pagination.
     * </p>
     *
     * @param searchTerm {@link String} The term or keyword that is being searched for in the query to
     *                   filter the results is given
     * @param countryId  {@link Long} The ID of the country associated with the districts that
     *                   are being searched is given
     * @param pageable   {@link Pageable} The pagination information that contains information such as the page number,
     *                   page size, sorting criteria, and more is given
     * @return {@link Page<District>} A Page of non-deleted and active districts that match the search criteria with applied
     * pagination is retrieved and returned from the database
     */
    @Query(value = GET_DEACTIVATED_DISTRICTS)
    public Page<District> getDeactivatedDistricts(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                                                @Param(Constants.COUNTRY_ID) Long countryId, Pageable pageable);

    /**
     * <p>
     * Gets District count by country ids and its active status
     * </p>
     *
     * @param ids      {@link List}     - Country ids for which the district count to be retrieved is given
     * @param isActive {@link Boolean} - Active status of the districts that need to be counted is given
     * @return List {@link List} - The district count based on country id is retrieved
     */
    @Query(value = COUNT_BY_COUNTRY_IDS)
    List<Map<String, Object>> getDistrictCountByCountryIds(List<Long> ids, Boolean isActive);
}
