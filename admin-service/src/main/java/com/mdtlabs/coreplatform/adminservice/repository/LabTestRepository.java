package com.mdtlabs.coreplatform.adminservice.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the LabTest module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Dec 26, 2023
 */
@Repository
public interface LabTestRepository extends JpaRepository<LabTest, Long> {

    String GET_ALL_LABTESTS = "select labtest from LabTest as labtest where "
            + "(:countryId is null or labtest.countryId=:countryId) AND labtest.isDeleted=false"
            + " and labtest.tenantId=:tenantId and (COALESCE(:searchTerm) is null or "
            + "lower(labtest.name) LIKE CONCAT('%',lower(CAST(:searchTerm AS text)),'%')) ORDER BY labtest.updatedAt desc ";

    /**
     * Retrieves a LabTest entity based on country ID, test name, deletion status, and tenant ID.
     * <p>
     * This method is designed to find a specific LabTest entity that matches the given country ID, test name,
     * deletion status, and tenant ID. It is particularly useful for applications that need to query lab tests
     * based on these criteria to ensure accurate retrieval within a multi-tenant architecture.
     * </p>
     *
     * @param countryId The ID of the country associated with the LabTest entity.
     * @param name      The name of the LabTest entity.
     * @param b         The deletion status of the LabTest entity (true if deleted).
     * @param tenantId  The tenant ID associated with the LabTest entity.
     * @return The LabTest entity if found, null otherwise.
     */
    LabTest findByCountryIdAndNameAndIsDeletedAndTenantId(Long countryId, String name, boolean b, Long tenantId);

    /**
     * Retrieves a paginated list of LabTest entities based on search criteria, country ID, and tenant ID.
     * <p>
     * This method fetches LabTest entities that match the given search term (if any), belong to a specified country,
     * and are associated with a specific tenant ID. The results are paginated to support large datasets efficiently.
     * This method is particularly useful for applications that need to filter lab tests by multiple criteria,
     * including geographical location and tenant association, while also supporting search functionality.
     * </p>
     *
     * @param searchTerm An optional search term for filtering the LabTest entities by name.
     * @param countryId  The ID of the country associated with the LabTest entities to retrieve.
     * @param tenantId   The tenant ID to further filter the LabTest entities.
     * @param pageable   A {@link Pageable} object to determine the pagination parameters.
     * @return A {@link Page} of LabTest entities matching the criteria.
     */
    @Query(value = GET_ALL_LABTESTS)
    public Page<LabTest> getAllLabTests(@Param("searchTerm") String searchTerm,
                                        @Param("countryId") Long countryId, @Param("tenantId") Long tenantId,
                                        Pageable pageable);

    /**
     * Retrieves a LabTest entity by its ID, ensuring it is not marked as deleted and belongs to a specific tenant.
     * <p>
     * This method is used to fetch a single LabTest entity based on its unique identifier,
     * provided that the entity has not been marked as deleted and belongs to the specified tenant ID.
     * It is particularly useful for operations that require fetching specific lab test records
     * within a multi-tenant architecture.
     * </p>
     *
     * @param id           The unique identifier of the LabTest entity to retrieve.
     * @param booleanFalse The deletion status of the LabTest entity (false to indicate not deleted).
     * @param tenantId     The tenant ID associated with the LabTest entity.
     * @return The LabTest entity if found, null otherwise.
     */
    LabTest findByIdAndIsDeletedAndTenantId(Long id, Boolean booleanFalse, Long tenantId);

}
