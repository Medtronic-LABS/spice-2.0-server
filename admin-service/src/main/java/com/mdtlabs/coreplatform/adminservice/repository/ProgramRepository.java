package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;



/**
 * <p>
 * This class is a repository class to establish communication between database
 * and server side.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2022
 *
 */
@Repository
public interface ProgramRepository extends JpaRepository<Program, Long> {

    public static final String GET_ALL_PROGRAMS = "select program from Program as program where (:countryId is null or program.country.id=:countryId) "
        + "AND  (:tenantId is null or program.tenantId=:tenantId) "
        + " and program.isDeleted=false and (:searchTerm is null or lower(program.name)"
        + " LIKE CONCAT('%',lower(:searchTerm),'%')) order by program.updatedAt DESC";
        
    public static final String GET_PROGRAM_BY_HEALTH_FACILITY_IDS = "select program from Program as program"
        + " join program.healthFacilities as hf where hf.id in (:healthFacilityIds) and program.isActive = true and program.isDeleted = false";
    public static final String COUNT_BY_COUNTRY_AND_TENANT_ID = "select count(program.id) from Program as program "
        + "where program.isDeleted=false AND (:countryId is null or program.country.id=:countryId) AND "
        + "program.tenantId=:tenantId";
    
    /**
     * <p>
     * Finds the program based on its name and isDeleted.
     * </p>
     *
     * @param name - program name
     * @param isDeleted - true or false
     * @return Program Entity
     */
    public Program findByNameAndTenantIdAndIsDeleted(String name, long tenantId, boolean isDeleted);

    /**
     * <p>
     * Finds the program based on its id and isDeleted.
     * </p>
     *
     * @param id - program id
     * @param isDeleted - true or false
     * @return Program Entity
     */
    public Program findByIdAndIsDeleted(long id, boolean isDeleted);

    /**
     * <p>
     * Finds the program based on searchTerm and its countryId and operatingUnitId and
     * accountId and siteId.
     * </p>
     *
     * @param searchTerm - search term
     * @param countryId - country id
     * @param pageable - pageable
     * @return Program Entity
     */
    @Query(value = GET_ALL_PROGRAMS)
    public Page<Program> getAllProgram(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
        @Param(Constants.COUNTRY_ID) Long countryId, @Param(Constants.TENANT_PARAMETER_NAME) Long tenantId,
        Pageable pageable);

    /**
     * <p>
     * Gets list of programs using list of site Ids.
     * </p>
     *
     * @param siteIds List of siteIds
     * @return List of Program Entities
     */
    @Query(value = GET_PROGRAM_BY_HEALTH_FACILITY_IDS)
    public List<Program> findProgramsByHealthFacilityIds(@Param("healthFacilityIds") List<Long> healthFacilityIds);

    /**
     * <p>
     * Find count of programs by its countryID and TenantId.
     * </p>
     *
     * @param countryId - countryId
     * @param tenantId - tenantId
     * @return Integer - count of Programs
     */
    @Query(value = COUNT_BY_COUNTRY_AND_TENANT_ID)
    public Integer countByCountryIdAndTenantId(Long countryId, Long tenantId);

    /**
     * <p>
     * Finds the program based on its id, tenantId and isDeleted.
     * </p>
     *
     * @param id - program id
     * @param tenantId - tenantId
     * @param isDeleted - true or false
     * @return Program Entity
     */
    public Program findByIdAndIsDeletedAndTenantId(Long id, boolean isDeleted, Long tenantId);
}
