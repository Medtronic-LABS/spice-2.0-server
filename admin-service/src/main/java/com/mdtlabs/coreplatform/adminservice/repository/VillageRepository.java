package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.jpa.repository.query.Procedure;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Village module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Dec 26, 2023
 */
@Repository
public interface VillageRepository extends JpaRepository<Village, Long> {

    String GET_VILLAGES = "FROM Village as village WHERE (:countryId is null OR village.countryId=:countryId)"
    + " AND (:districtId is null OR village.districtId=:districtId) AND (:chiefdomId is null OR village.chiefdomId=:chiefdomId)"
    + " AND village.isDeleted=false AND village.isActive=true"
    + " AND (COALESCE(:searchTerm) IS NULL OR (LOWER(village.name) LIKE CONCAT('%', LOWER(CAST(:searchTerm AS text)), '%'))) " ;

    String GET_VILLAGES_BY_COUNTRY = "FROM Village as village WHERE (:countryId is null OR village.countryId=:countryId)"
            + " AND village.isDeleted=false AND village.isActive=true";

    String SELECTION_PARAMS = "SELECT country.id AS countryId, country.name AS countryName, " 
    + "chiefdom.id AS chiefdomId, chiefdom.name AS chiefdomName, "
    + "district.id AS districtId, district.name AS districtName, "
    + "village.id AS villageId, village.name AS villageName, village.TYPE AS villageType, "
    + "country.region_code as countryCode, district.code AS districtCode, chiefdom.code AS chiefdomCode , village.code AS villageCode, "
    + "country.tenant_id AS countryTenantId, district.tenant_id AS districtTenantId, chiefdom.tenant_id AS chiefdomTenantId";

    String SELECTION_PARAMS_FOR_VILLAGE = "SELECT country.id AS countryId, country.name AS countryName, " 
    + "chiefdom.id AS chiefdomId, chiefdom.name AS chiefdomName, "
    + "district.id AS districtId, district.name AS districtName, "
    + "village.id AS id, village.name AS name, village.TYPE AS villageType, "
    + "country.region_code as countryCode, district.code AS districtCode, chiefdom.code AS chiefdomCode , village.code AS villageCode ";

    String GET_REGION_DETAILS_QUERY_LEFT_JOIN = " FROM country AS country "
            + " LEFT JOIN district AS district ON country.id = district.country_id AND district.is_deleted = false AND district.is_active = true "
            + " LEFT JOIN chiefdom AS chiefdom ON district.id = chiefdom.district_id AND chiefdom.is_deleted = false AND chiefdom.is_active = true "
            + " LEFT JOIN village AS village ON chiefdom.id = village.chiefdom_id AND village.is_deleted = false AND village.is_active = true ";

    String GET_REGION_DETAILS_QUERY_JOIN = " FROM country AS country "
            + " JOIN district AS district ON country.id = district.country_id AND district.is_deleted = false AND district.is_active = true "
            + " JOIN chiefdom AS chiefdom ON district.id = chiefdom.district_id AND chiefdom.is_deleted = false AND chiefdom.is_active = true "
            + " JOIN village AS village ON chiefdom.id = village.chiefdom_id AND village.is_deleted = false AND village.is_active = true ";

    String GET_REGION_DETAILS = SELECTION_PARAMS + GET_REGION_DETAILS_QUERY_LEFT_JOIN;

    String GET_REGION_DETAILS_BY_COUNTRY = SELECTION_PARAMS + GET_REGION_DETAILS_QUERY_LEFT_JOIN + "WHERE country.id=:countryId";

    String GET_VILLAGES_BY_IDS = SELECTION_PARAMS_FOR_VILLAGE + GET_REGION_DETAILS_QUERY_JOIN + "WHERE village.id in (:ids)";

    String GET_VILLAGES_BY_COUNTRY_ID = SELECTION_PARAMS_FOR_VILLAGE + GET_REGION_DETAILS_QUERY_JOIN + "WHERE country.id=:countryId";

    String GET_REGION_DETAILS_BY_SEARCH = SELECTION_PARAMS + GET_REGION_DETAILS_QUERY_LEFT_JOIN + "WHERE country.id=:countryId"
            + " AND (:searchTerm is null OR (lower(country.name) LIKE CONCAT('%',lower(:searchTerm),'%') OR "
            + " lower(chiefdom.name) LIKE CONCAT('%',lower(:searchTerm),'%') OR "
            + "lower(district.name) LIKE CONCAT('%',lower(:searchTerm),'%') OR "
            + "lower(village.name) LIKE CONCAT('%',lower(:searchTerm),'%')))";

    String GET_UNLINKED_VILLAGES = "SELECT * FROM village AS v LEFT JOIN health_facility_village hfv ON v.id ="
            + " hfv.village_id WHERE (:countryId IS NULL OR v.country_id=:countryId) AND "
            + " (:districtId IS NULL OR v.district_id=:districtId) AND (:chiefdomId IS NULL OR v.chiefdom_id=:chiefdomId)"
            + " AND (hfv.health_facility_id IS NULL OR hfv.health_facility_id = :healthFacilityId)"
            + " AND v.is_deleted=false AND v.is_active=true AND (COALESCE(:searchTerm) IS NULL OR (LOWER(v.name) "
            + " LIKE CONCAT('%', LOWER(CAST(:searchTerm AS text)), '%')))";

    String FIND_UNLINKED_VILLAGES_WITHOUT_USERS = "SELECT DISTINCT v.id AS id , v.name AS name, v.code AS villagecode,"
            + " v.country_id AS countryId, v.district_id AS districtId,v.chiefdom_id AS chiefdomId "
            + " FROM village AS v LEFT JOIN health_facility_village hfv ON hfv.village_id = v.id "
            + " LEFT JOIN user_village uv ON uv.village_id = v.id WHERE "
            + " hfv.health_facility_id IN (SELECT hf.id FROM health_facility hf WHERE tenant_id IN (:tenantIds)) "
            + " AND (uv.user_id IS NULL OR uv.user_id = :userId) AND v.is_deleted=false AND v.is_active=true";

    String GET_USER_LINKED_VILLAGES = "SELECT * FROM village v JOIN user_village uv ON uv.village_id = v.id WHERE v.id "
            + "in (:tenantIds) AND v.is_active=true AND v.is_deleted=false";

    String GET_OTHER_VILLAGE = "select village from Village as village where village.name = :name";
    
    /**
     * Finds villages by a list of IDs, deletion status, and activity status.
     * <p>
     * This method retrieves a list of {@link Village} entities based on their IDs, ensuring they match the specified
     * deletion and activity status. It is useful for filtering villages that are active and not deleted, based on a
     * predefined list of village IDs.
     * </p>
     *
     * @param linkedVillageIds A list of village IDs to filter the villages.
     * @param isDeleted        The deletion status to filter the villages. If true, only deleted villages are retrieved.
     * @param isActive         The activity status to filter the villages. If true, only active villages are retrieved.
     * @return A list of {@link Village} entities that match the criteria.
     */
    List<Village> findByIdInAndIsDeletedAndIsActive(List<Long> linkedVillageIds, boolean isDeleted, boolean isActive);

    /**
     * Retrieves a list of villages based on country ID, district ID, and chiefdom ID, including an optional search term.
     * <p>
     * This method fetches a list of {@link Village} entities that belong to a specified country, district, and chiefdom.
     * It supports an optional search term to further filter the villages by name. This is particularly useful for
     * applications needing to display villages based on geographical administrative divisions while also providing
     * a search functionality to narrow down the results.
     * </p>
     *
     * @param countryId  The ID of the country to which the villages must belong.
     * @param districtId The ID of the district to which the villages must belong.
     * @param chiefdomId The ID of the chiefdom to which the villages must belong.
     * @param searchTerm An optional search term for filtering the villages by name. Supports partial matches.
     * @return A list of {@link Village} entities matching the specified criteria.
     */
    @Query(value = GET_VILLAGES)
    List<Village> getVillages(@Param("countryId") Long countryId, @Param("districtId") Long districtId, @Param("chiefdomId") Long chiefdomId, @Param("searchTerm") String searchTerm);

    /**
     * Retrieves village that are active and not marked as deleted.
     * <p>
     * This method is used to fetch a list of Village entities that are currently active and have not been marked as deleted.
     * It is particularly useful for operations that require a list of all active villages without any additional filtering criteria.
     * </p>
     *
     * @return A list of {@link Village} entities that are active and not deleted.
     */
    Village findByIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     * Retrieves a specific village by its ID, ensuring it is active and not marked as deleted.
     * <p>
     * This method fetches a single Village entity based on its unique identifier, provided that the village is active and has not been marked as deleted.
     * It is particularly useful for operations that require fetching a specific village's details while ensuring the village is currently active.
     * </p>
     *
     * @param id The unique identifier of the Village entity to retrieve.
     * @return The Village memberSequence if found and active, null otherwise.
     */
    @Procedure(name = "getMemberSequenceByVillageId")
    Long getMemberSequenceByVillageId(@Param ("villageId") Long id);

    /**
     * Retrieves a specific village by its ID, ensuring it is active and not marked as deleted.
     * <p>
     * This method fetches a single Village entity based on its unique identifier, provided that the village is active and has not been marked as deleted.
     * It is particularly useful for operations that require fetching a specific village's details while ensuring the village is currently active.
     * </p>
     *
     * @param id The unique identifier of the Village entity to retrieve.
     * @return The Village household sequence if found and active, null otherwise.
     */
    @Procedure(name = "getHouseholdSequenceByVillageId")
    Long getHouseholdSequenceByVillageId(@Param ("villageId") Long id);


    /**
     * Retrieves detailed information about regions based on a country ID and an optional search term.
     * <p>
     * This method performs a native query to fetch detailed information about regions (countries, districts, chiefdoms, and villages)
     * based on the provided country ID and an optional search term. The search term is applied to the names of countries, districts,
     * chiefdoms, and villages to filter the results. This method supports pagination and is particularly useful for applications
     * that require detailed geographical information with flexible search capabilities.
     * </p>
     *
     * @param countryId  The ID of the country for which to retrieve region details.
     * @param searchTerm An optional search term to filter the results by matching it with the names of countries, districts, chiefdoms, and villages.
     * @param pageable   A {@link Pageable} object to determine the pagination parameters.
     * @return A {@link Page} of {@link Map} objects, each representing detailed information about a region.
     */
    @Query(value = GET_REGION_DETAILS_BY_SEARCH, nativeQuery = true)
    Page<Map<String, Object>> getRegionDetailsByCountry(@Param("countryId") Long countryId, @Param("searchTerm") String searchTerm, Pageable pageable);

    /**
     * Retrieves detailed information about all regions.
     * <p>
     * This method performs a native query to fetch detailed information about all regions (countries, districts, chiefdoms, and villages).
     * It does not apply any filtering criteria and is useful for applications that require a comprehensive list of geographical information
     * without any restrictions. The method returns a list of {@link Map} objects, each representing detailed information about a region.
     * </p>
     *
     * @return A {@link List} of {@link Map} objects, each representing detailed information about a region.
     */
    @Query(value = GET_REGION_DETAILS, nativeQuery = true)
    List<Map<String, Object>> getRegionDetails();

    /**
     * Retrieves detailed information about regions based on a specific country ID.
     * <p>
     * This method performs a native query to fetch detailed information about regions (countries, districts, chiefdoms, and villages)
     * based on the provided country ID. It is particularly useful for applications that require detailed geographical information
     * within a specific country without applying any additional filtering criteria. The method returns a list of {@link Map} objects,
     * each representing detailed information about a region within the specified country.
     * </p>
     *
     * @param countryId The ID of the country for which to retrieve region details.
     * @return A {@link List} of {@link Map} objects, each representing detailed information about a region within the specified country.
     */
    @Query(value = GET_REGION_DETAILS_BY_COUNTRY, nativeQuery = true)
    List<Map<String, Object>> getRegionDetails(@Param("countryId") Long countryId);

    /**
     * Retrieves detailed information about villages based on a list of village IDs.
     * <p>
     * This method performs a native query to fetch detailed information about villages specified by a list of village IDs.
     * The information includes the village's name, type, and code, as well as details of the corresponding country, district, and chiefdom.
     * It is useful for applications that need to retrieve detailed information about specific villages.
     * </p>
     *
     * @param countryId A list of village IDs for which to retrieve detailed information.
     * @return A list of {@link Map} objects, each representing detailed information about a village.
     */
    @Query(value = GET_VILLAGES_BY_IDS, nativeQuery = true)
    List<Map<String, Object>> getVillagesByIds(@Param("ids") List<Long> countryId);

    /**
     * <p>
     * Gets Villages that are not linked to any Health facilities based on countryId and districtId and chiefdomId
     * </p>
     *
     * @param countryId  {@link Long} - country Id
     * @param districtId {@link Long} - District ID
     * @param chiefdomId {@link Long} - Cheifdom ID
     * @param searchTerm {@link Long} - search term
     * @return {@link List<Village>}  - List of village entity
     */
    @Query(value = GET_UNLINKED_VILLAGES, nativeQuery = true)
    List<Village> getUnlinkedVillages(@Param(Constants.COUNTRY_ID) Long countryId, @Param(Constants.DISTRICT_ID) Long districtId,
        @Param(Constants.CHIEFDOM_ID) Long chiefdomId, @Param(Constants.HEALTHFACILITY_ID) Long healthFacilityId,
        @Param(Constants.SEARCH_TERM_FIELD) String searchTerm);

    /**
     * <p>
     * Used to get Villages that are not linked to any user in tha given Health Facilities.
     * </p>
     *
     * @param tenantIds {@link List<Long>} - List of Health Facility tenant IDs.
     * @param userId    {@link Long} - User ID
     * @return {@link List<Map>} -  List of Unlinked Villages
     */
    @Query(value = FIND_UNLINKED_VILLAGES_WITHOUT_USERS, nativeQuery = true)
    List<Map<String, Object>> findUnlinkedVillagesByUserId(@Param(Constants.TENANT_IDS) List<Long> tenantIds,
        @Param(Constants.USER_ID_PARAM) Long userId);

    /**
     * <p>
     * Gets the villages with users linked to it.
     * </p>
     *
     * @param villageIds {@link Long}- village IDs
     * @return {@link List<Village>} - List of Villages
     */
    @Query(value = GET_USER_LINKED_VILLAGES, nativeQuery = true)
    List<Village> getUserLinkedVillage(@Param(Constants.TENANT_IDS) List<Long> villageIds);

    /**
     * <p>
     * Get the villages list buy country id
     * </p>
     *
     * @param countryId {@link Long} - country id
     * @return {@link List<Village>} - List of Villages
     */
    @Query(value = GET_VILLAGES_BY_COUNTRY_ID, nativeQuery = true)
    List<Map<String, Object>> findByCountryIdAndIsDeletedFalse(Long countryId);

    @Query(value = GET_OTHER_VILLAGE)
    List<Village> getOther(@Param(Constants.NAME) String name);
}