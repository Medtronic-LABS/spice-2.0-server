package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;


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
public interface ChiefdomRepository extends JpaRepository<Chiefdom, Long> {

    String GET_CHIEFDOM_BY_DISTRICT = "FROM Chiefdom as chiefdom WHERE chiefdom.district.id=:id AND chiefdom.isDeleted=false AND chiefdom.isActive=true";

    public static String COUNT_ON_DISTRICT_ID = "SELECT chiefdom.districtId as districtId, COUNT(chiefdom.id) "
            + "AS count FROM Chiefdom as chiefdom WHERE chiefdom.districtId in :districtIds AND "
            + "chiefdom.isDeleted=false AND chiefdom.isActive=:isActive GROUP BY chiefdom.districtId";

    public static String GET_BY_DISTRICT_ID_AND_COUNTRY_ID = "select chiefdom from Chiefdom as "
            + "chiefdom where chiefdom.isDeleted=false AND "
            + " (:countryId is null or chiefdom.countryId=:countryId) AND (:districtId "
            + "is null or chiefdom.districtId=:districtId) AND chiefdom.isActive=:isActive";

    String COUNT_ON_COUNTRY_ID = "SELECT chiefdom.countryId as countryId,COUNT(chiefdom.id) "
            + "AS count FROM Chiefdom as chiefdom WHERE chiefdom.countryId in :countryIds AND "
            + "chiefdom.isDeleted=false AND chiefdom.isActive=:isActive GROUP BY chiefdom.countryId";

    public static String GET_ALL_CHIEFDOMS_BY_TENANTS = "select chiefdom.name as name, chiefdom.tenantId as tenantId,"
            + "chiefdom.countryId as countryId, chiefdom.id as id, district.name as districtName from Chiefdom "
            + "as chiefdom left join District district  on chiefdom.districtId = district.id "
            + "where (:searchTerm is null or lower(chiefdom.name) "
            + " LIKE CONCAT('%',lower(:searchTerm),'%')) AND chiefdom.isDeleted=false AND "
            + "(:countryId is null or chiefdom.countryId=:countryId) AND (:districtId is null or "
            + "chiefdom.districtId=:districtId) AND chiefdom.isActive=true";

    public static String GET_ALL_CHIEFDOMS = "select chiefdom from Chiefdom "
            + "as chiefdom where lower(chiefdom.name) "
            + " LIKE CONCAT('%',lower(:searchTerm),'%') AND chiefdom.isDeleted=false AND"
            + "(:countryId is null or chiefdom.countryId=:countryId) AND (:districtId is null or "
            + "chiefdom.districtId=:districtId) AND chiefdom.isActive=true ";

    public static String GET_CHIEFDOM_DETAILS = "select chiefdom.name as name, chiefdom.tenantId as tenantId,"
            + "chiefdom.countryId as countryId, chiefdom.id as id, district.name as districtName, district.id as districtId,"
            + " district.tenantId AS districtTenantId from Chiefdom "
            + "as chiefdom left join District district  on chiefdom.districtId = district.id "
            + "where chiefdom.isDeleted=false AND chiefdom.isActive=true AND chiefdom.id = :id";


    /**
     * Fins chiefdom by counry and district
     * 
     * @param countryId
     * @param districtId
     * @return List<Chiefdom>
     */
    List<Chiefdom> findByCountryIdAndDistrictIdAndIsDeletedFalse(Long countryId, Long districtId);

    /**
     * <p>
     * This method is used to get a list of map containing district IDs with corresponding count of chiefdoms that
     * is searched using the given district IDs.
     * </p>
     *
     * @param districtIds {@link List} The list of district IDs associated with the chiefdoms that
     *                   are being searched is given
     * @param isActive   {@link Boolean} The boolean value that is used to filter the results of the query based
     *                   on whether the chiefdom has been marked as active or not is given
     * @return {@link List<Map>} A list of map containing key as district IDs and value as count of chiefdoms
     * for the corresponding district IDs provided is retrieved and returned from the database
     */
    @Query(value = COUNT_ON_DISTRICT_ID)
    List<Map<String, Object>> getChiefdomCountByDistrictIds(@Param(Constants.DISTRICT_IDS) List<Long> districtIds,
                                                                @Param(Constants.IS_ACTIVE) Boolean isActive);

    /**
     * <p>
     * This method is used to find a list of chiefdoms based on country Id or district Id.
     * </p>
     *
     * @param countryId {@link Long} The ID of the country for which the chiefdoms
     *                  are being searched is given
     * @param districtId {@link Long} The ID of the country for which the chiefdoms
     *                  are being searched is given
     * @param isActive  The boolean value that is used to filter the results of the query based
     *                  on whether the chiefdoms have been marked as active or not is given
     * @return {@link List} The list of chiefdoms those are not deleted is returned for the
     * given search criteria like country ID, district ID and active status
     */

    @Query(value = GET_BY_DISTRICT_ID_AND_COUNTRY_ID)
    List<Chiefdom> findByCountryIdAndDistrictIdAndIsActive(@Param(Constants.COUNTRY_ID) Long countryId,
                                                                @Param("districtId") Long districtId,
                                                                @Param(Constants.IS_ACTIVE) boolean isActive);


    /**
     * <p>
     * Gets Chiefdoms count by country ids and its active status
     * </p>
     *
     * @param countryIds {@link List}     - Country ids for which the chiefdom count to be retrieved is given
     * @param isActive {@link Boolean} - Active status of the chiefdoms that need to be counted is given
     * @return List {@link List} - The chiefdom count based on country id is retrieved
     */
    @Query(value = COUNT_ON_COUNTRY_ID)
    List<Map<String, Object>> getChiefdomCountByCountryIds(@Param(Constants.COUNTRY_IDS) List<Long> countryIds,
                                                            @Param(Constants.IS_ACTIVE) Boolean isActive);

    /**
     * <p>
     * This method is used to get a list of chiefdoms based on given name.
     * </p>
     *
     * @param searchTerm {@link String} The term or keyword that is being searched for in the query to
     *                   filter the results is given
     * @param pageable   {@link Pageable} The pagination information that contains information such as the page number,
     *                   page size, sorting criteria, and more is given
     * @return {@link Page<Chiefdom>} A Page of chiefdoms that match the search criteria with applied
     * pagination is retrieved and returned from the database
     */
    @Query(value = GET_ALL_CHIEFDOMS_BY_TENANTS)
    Page<Map<String, Object>> getChiefdoms(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                                      @Param(Constants.COUNTRY_ID) Long countryId,
                                      @Param(Constants.DISTRICT_ID) Long districtId, Pageable pageable);

    /**
     * <p>
     * This method is used to get a list of chiefdoms based on given name.
     * </p>
     *
     * @param searchTerm {@link String} The term or keyword that is being searched for in the query to
     *                   filter the results is given
     * @param pageable   {@link Pageable} The pagination information that contains information such as the page number,
     *                   page size, sorting criteria, and more is given
     * @return {@link Page<Chiefdom>} A Page of chiefdoms that match the search criteria with applied
     * pagination is retrieved and returned from the database
     */
    @Query(value = GET_ALL_CHIEFDOMS)
    Page<Chiefdom> findChiefdoms(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                                    @Param(Constants.COUNTRY_ID) Long countryId,
                                    @Param(Constants.DISTRICT_ID) Long accountId, Pageable pageable);

    /**
     * <p>
     * This method is used to find an non-deleted chiefdom by given Id and active status.
     * </p>
     *
     * @param id       {@link Long} The ID for which the chiefdom is being searched is given
     * @param isActive {@link Boolean} The boolean value that is used to filter the results of the query based
     *                 on whether the chiefdom has been marked as active or not is given
     * @return {@link Chiefdom} The non-deleted chiefdom for the given ID with
     * specified active status is retrieved from the database
     */
    Chiefdom findByIdAndIsDeletedFalseAndIsActive(long id, boolean isActive);

    /**
     * <p>
     * This method is used to check the availability of the chiefdom by its name and ID which
     * has not been deleted.
     * </p>
     *
     * @param name {@link String} The name of the chiefdom to be searched is given
     * @param id   {@link Long} The ID for which the chiefdom is being searched is given
     * @return True is returned if the chiefdom for given name and ID is already available
     * in the database otherwise false
     */
    boolean existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(String name, Long id);

    /**
     * <p>
     * This method is used to get chiefdom by its name that has not been deleted.
     * </p>
     *
     * @param name {@link String} The name of the chiefdom to be searched is given
     * @return {@link Chiefdom} The non-deleted chiefdom for the given name (ignoring case) with
     * specified active status is retrieved from the database
     */
    Chiefdom findByNameIgnoreCaseAndIsDeletedFalse(String name);

    /**
     * <p>
     * This method is used to get an non-deleted chiefdom based on its Id,tenantId and isActive status.
     * </p>
     *
     * @param id        {@link Long} The ID for which the chiefdom is being searched is given
     * @return {@link Map<>} The non-deleted chiefdom for the given ID and tenant ID with
     * specified active status is retrieved from the database
     */
    @Query(value = GET_CHIEFDOM_DETAILS)
    Map<String, Object> getChiefdomDetails(@Param(Constants.ID) Long id);

    /**
     * Finds Chiefdom by countryId and isDeleted false.
     * 
     * @param countryId
     * @return List<Chiefdom>
     */
    List<Chiefdom> findByCountryIdAndDistrictIdAndIsDeletedFalseAndIsActiveTrue(Long countryId, Long districtId);

    List<Chiefdom> findByCountryIdAndIsDeletedFalse(Long countryId);
}
