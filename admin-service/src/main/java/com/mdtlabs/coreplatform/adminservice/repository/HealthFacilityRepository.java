package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the HealthFacility module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Dec 26, 2023
 */
@Repository
public interface HealthFacilityRepository extends JpaRepository<HealthFacility, Long> {

    String GET_HEALTH_FACILITIES = "SELECT hf FROM HealthFacility hf " +
            "WHERE hf.country.id = :countryId " +
            "AND (COALESCE(:tenantIds) is null OR (hf.tenantId IN (:tenantIds))) " +
            "AND (COALESCE(:searchTerm) IS NULL OR (LOWER(hf.name) LIKE CONCAT('%', LOWER(CAST(:searchTerm AS text)), '%'))) " +
            "AND hf.isDeleted = false " +
            "AND hf.isActive = true";

    public static String COUNT_BY_DISTRICT_IDS = "select healthFacility.district.id as districtId, count(healthFacility.id) "
            + "as count from HealthFacility as healthFacility where healthFacility.district.id in (:districtIds) "
            + " AND healthFacility.isDeleted=false AND healthFacility.isActive=true group by healthFacility.district.id";

    public static String GET_HEALTH_FACILITY = "select healthFacility from HealthFacility as healthFacility where (:countryId is null or "
            + "healthFacility.country.id=:countryId) AND (:districtId is null or healthFacility.district.id=:districtId) AND (:chiefdomId "
            + "is null or healthFacility.chiefdom.id =:chiefdomId) AND healthFacility.isDeleted=false AND healthFacility.isActive=:isActive";

    String COUNT_BY_COUNTRY_ID = "SELECT hf.country_id as countryId, COUNT(hf.id) AS count " +
            "FROM health_facility as hf WHERE hf.country_id in (:countryIds ) " +
            "AND hf.is_deleted=false AND hf.is_active=:isActive GROUP BY hf.country_id";


    public static String COUNT_BY_CHIEFDOM_IDS = "SELECT healthFacility.chiefdom.id as chiefdomId, COUNT(healthFacility.id) AS count "
            + "FROM HealthFacility as healthFacility WHERE healthFacility.chiefdom.id in :chiefdomIds AND healthFacility.isDeleted=false AND "
            + "healthFacility.isActive=:isActive GROUP BY healthFacility.chiefdom.id";

    String FETCH_DISTRICT_FACILITY_BY_TENANT_ID = "SELECT hf.* FROM health_facility hf WHERE hf.district_id IN " +
            " (SELECT sub_hf.district_id FROM health_facility sub_hf WHERE sub_hf.tenant_id = :tenantId) ";

    public static String GET_HEALTH_FACILITIES_BY_COUNTRY_TENANT = "select hf FROM HealthFacility hf "
            + " where lower(hf.name)"
            + " LIKE CONCAT('%',lower(:searchTerm),'%') AND hf.isDeleted=false AND hf.isActive=true AND"
            + " hf.tenantId!=:tenantId AND (:countryId is null or hf.country.id=:countryId)";

    /**
     * Finds a specific HealthFacility entity based on its ID, tenant ID, deletion, and activation status.
     * <p>
     * This method is designed to retrieve a HealthFacility entity that matches a unique identifier and tenant ID,
     * while also considering its deletion and activation status. It is particularly useful in multi-tenant applications
     * where health facilities need to be queried based on their operational status and association with a specific tenant.
     * </p>
     *
     * @param id        The unique identifier of the HealthFacility entity.
     * @param tenantId  The tenant ID associated with the HealthFacility entity.
     * @param isDeleted The deletion status of the HealthFacility entity (true if deleted).
     * @param isActive  The activation status of the HealthFacility entity (true if active).
     * @return The HealthFacility entity if found, null otherwise.
     */
    HealthFacility findByIdAndTenantIdAndIsDeletedAndIsActive(Long id, Long tenantId, boolean isDeleted, boolean isActive);

    /**
     * Retrieves a paginated list of HealthFacility entities based on search criteria, country ID, and tenant IDs.
     * <p>
     * This method fetches HealthFacility entities that match the given search term (if any), belong to a specified country,
     * and are associated with any of the provided tenant IDs. The results are paginated to support large datasets efficiently.
     * This method is particularly useful for applications that need to filter health facilities by multiple criteria,
     * including geographical location and tenant association, while also supporting search functionality.
     * </p>
     *
     * @param searchTerm An optional search term for filtering the HealthFacility entities by name.
     * @param countryId  The ID of the country associated with the HealthFacility entities to retrieve.
     * @param tenantIds  A list of tenant IDs to further filter the HealthFacility entities.
     * @param pageable   A {@link Pageable} object to determine the pagination parameters.
     * @return A {@link Page} of HealthFacility entities matching the criteria.
     */
    @Query(value = GET_HEALTH_FACILITIES)
    Page<HealthFacility> getHealthFacilities(@Param("searchTerm") String searchTerm, @Param("countryId") Long countryId, 
                @Param("tenantIds") Set<Long> tenantIds,
                                    Pageable pageable);

    /**
     * Retrieves a HealthFacility entity by its ID, ensuring it has not been marked as deleted.
     * <p>
     * This method is used to fetch a single HealthFacility entity based on its unique identifier,
     * provided that the entity has not been marked as deleted. It is particularly useful for operations
     * that require fetching active health facility records.
     * </p>
     *
     * @param id The unique identifier of the HealthFacility entity to retrieve.
     * @return The HealthFacility entity if found and not deleted, null otherwise.
     */
    HealthFacility findByIdAndIsDeletedFalse(Long id);

    /**
     * Retrieves a list of HealthFacility entities based on the country ID, filtered by deletion status.
     * <p>
     * This method fetches all HealthFacility entities associated with a specific country, allowing
     * for the inclusion or exclusion of entities based on their deletion status. It enables targeted
     * retrieval of health facilities within a country, considering their operational status.
     * </p>
     *
     * @param countryId The ID of the country associated with the HealthFacility entities to retrieve.
     * @param isDeleted The deletion status to filter the HealthFacility entities by (true for deleted).
     * @return A list of HealthFacility entities that match the specified country ID and deletion status.
     */
    List<HealthFacility> findByCountryIdAndIsDeleted(Long countryId, boolean isDeleted);

    /**
     * Retrieves a list of HealthFacility entities based on tenant IDs and deletion status.
     * <p>
     * This method fetches HealthFacility entities that are associated with any of the provided tenant IDs
     * and matches the specified deletion status. It is useful for filtering health facilities by tenant
     * ownership and whether they have been marked as deleted or not.
     * </p>
     *
     * @param tenantIds A list of tenant IDs to filter the HealthFacility entities by.
     * @param isDeleted The deletion status to filter the HealthFacility entities by (true for deleted).
     * @return A list of HealthFacility entities that match the specified tenant IDs and deletion status.
     */
    List<HealthFacility> findByTenantIdInAndIsDeleted(List<Long> tenantIds, boolean isDeleted);

    /**
     * Retrieves a list of HealthFacility entities based on chiefdom ID and deletion status.
     * <p>
     * This method is designed to fetch HealthFacility entities associated with a specific chiefdom,
     * filtered by their deletion status. It enables targeted retrieval of health facilities within
     * a chiefdom, considering their operational status.
     * </p>
     *
     * @param chiefdomId The ID of the chiefdom associated with the HealthFacility entities to retrieve.
     * @param isDeleted  The deletion status to filter the HealthFacility entities by (true for deleted).
     * @return A list of HealthFacility entities that match the specified chiefdom ID and deletion status.
     */
    List<HealthFacility> findByChiefdomIdAndIsDeleted(Long chiefdomId, boolean isDeleted);

    /**
     * Retrieves all HealthFacility entities that are active and not marked as deleted.
     * <p>
     * This method is used to fetch all HealthFacility entities that are currently active (isActive = true)
     * and have not been marked as deleted (isDeleted = false). It is particularly useful for operations
     * that require a comprehensive list of operational health facilities without the deleted records.
     * </p>
     *
     * @return A list of HealthFacility entities that are active and not deleted.
     */
    List<HealthFacility> findAllByIsDeletedFalseAndIsActiveTrue();

    /**
     * Retrieves all HealthFacility entities by district ID, filtered by active and non-deleted status.
     * <p>
     * This method fetches all HealthFacility entities associated with a specific district, ensuring
     * that only those which are active (isActive = true) and not marked as deleted (isDeleted = false)
     * are included. It enables targeted retrieval of health facilities within a district, considering
     * their operational status.
     * </p>
     *
     * @param districtId The ID of the district associated with the HealthFacility entities to retrieve.
     * @return A list of HealthFacility entities that match the specified district ID and are active and not deleted.
     */
    List<HealthFacility> findAllByDistrictIdAndIsDeletedFalseAndIsActiveTrue(Long districtId);

    /**
     * Finds health facility by fhirId and isDeleted and isActive.
     *
     * @param fhirId
     * @return HealthFacility
     */
    HealthFacility findByFhirIdAndIsDeletedFalseAndIsActiveTrue(String fhirId);

    /**
     * Retrieves a HealthFacility entity by its TenantID, ensuring it has not been marked as deleted.
     * <p>
     * This method is used to fetch a single HealthFacility entity based on its unique tenant identifier,
     * provided that the entity has not been marked as deleted. It is particularly useful for operations
     * that require fetching active health facility records.
     * </p>
     *
     * @param tenantId {@link Long} The unique identifier of the HealthFacility entity to retrieve.
     * @return {@link HealthFacility} The HealthFacility entity if found and not deleted, null otherwise.
     */
    HealthFacility findByTenantIdAndIsDeletedFalse(Long tenantId);

    /**
     * <p>
     * This method is used to get a list of map containing operating unit IDs with corresponding count of sites that
     * is searched using the given district IDs.
     * </p>
     *
     * @param districtIds {@link List<Long>} The list of district IDs associated with the sites that need to counted
     *                   is given
     * @return {@link List<Map>} A list of map containing key as district IDs and value as count of sites
     * for the corresponding district IDs provided is retrieved and returned from the database
     */
    @Query(value = COUNT_BY_DISTRICT_IDS)
    public List<Map<String, Object>> getCountByDistrictIds(@Param(Constants.DISTRICT_IDS) List<Long> districtIds);

    /**
     * <p>
     * This method is used to get active health facilities for the given search criteria.
     * </p>
     *
     * @param districtId     {@link Long} The district ID for which the site is being searched is given
     * @param chiefdomId {@link Long} The operating unit ID for which the site is being searched is given
     * @param countryId     {@link Long} The ID of the country associated with the sites that
     *                      are being searched is given
     * @param isActive      The boolean value that is used to filter the results of the query based
     *                      on whether the sites have been marked as active or not is given
     * @return {@link List} The list of sites those are not deleted is returned for the
     * given search criteria like country ID, district ID, operating unit ID and active status
     */
    @Query(value = GET_HEALTH_FACILITY)
    public List<HealthFacility> findHealthFacilityDistrict(@Param(Constants.COUNTRY_ID) Long countryId, 
                                                         @Param(Constants.DISTRICT_ID) Long districtId,
                                                         @Param("chiefdomId") Long chiefdomId, 
                                                         @Param(Constants.IS_ACTIVE) boolean isActive);

    /**
     * <p>
     * Gets health facility count by country ids and its active status
     * </p>
     *
     * @param countryIds {@link List}     - Country ids for which the health facility count to be retrieved is given
     * @param isActive   {@link Boolean} - Active status of the health facilities that need to be counted is given
     * @return List {@link List} - The health facility count based on country id is retrieved
     */
    @Query(value = COUNT_BY_COUNTRY_ID, nativeQuery = true)
    List<Map<String, Object>> getHealthFacilityCountByCountryIds(List<Long> countryIds, Boolean isActive);

    /**
     * <p>
     * This method is used to get a list of map containing chiefdom IDs with corresponding count of health facility that
     * is searched using the given chiefdom IDs.
     * </p>
     *
     * @param chiefdomIds {@link List<Long>} The list of chiefdom IDs associated with the health facility
     *                         that need to counted is given
     * @return {@link List<Map>} A list of map containing key as chiefdom IDs and value as count of health facility
     * for the corresponding chiefdom IDs provided is retrieved and returned from the database
     */
    @Query(value = COUNT_BY_CHIEFDOM_IDS)
    public List<Map<String, Object>> getHealthFacilityCountByChiefdomIds(
            @Param(Constants.CHIEFDOM_IDS) List<Long> chiefdomIds, @Param(Constants.IS_ACTIVE) boolean isActive);
     

    /**
     * Finds health facility by list of ids and isDeleted and isActive.
     * 
     * @param ids
     * @param isDeleted
     * @param isActive
     * @return Set<HealthFacility>
     */
    Set<HealthFacility> findByIdInAndIsDeletedAndIsActive(Set<Long> ids, boolean isDeleted, boolean isActive);

    /**
     * Finds health facility by tenantId and isDeleted false and isActive true
     * @param tenantId
     * @return HealthFacility
     */
    HealthFacility findByTenantIdAndIsDeletedFalseAndIsActiveTrue(Long tenantId);


    /**
     * Gets health facilities by given tenant id's district
     *
     * @param tenantId
     * @return list of HealthFacilityDTO
     */
    @Query(value = FETCH_DISTRICT_FACILITY_BY_TENANT_ID, nativeQuery = true)
    List<HealthFacility> findDistrictFacilitiesByTenantId(@Param(Constants.TENANT_IDS_CLAIM) Long tenantId);

    /**
     * Retrieves a list of health facilities based on the specified country ID, tenant ID, and an optional search term.
     *
     * @param searchTerm an optional search term to filter the health facilities.
     * @param tenantId the tenant ID to filter health facilities by.
     * @param countryId the country ID to filter health facilities by.
     * @return a list of health facilities matching the specified criteria.
     */
    @Query(value = GET_HEALTH_FACILITIES_BY_COUNTRY_TENANT)
    List<HealthFacility> getHealthFacilityByCountryAndTenant(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm, Long tenantId,
                                         @Param(Constants.COUNTRY_ID) Long countryId);
}
