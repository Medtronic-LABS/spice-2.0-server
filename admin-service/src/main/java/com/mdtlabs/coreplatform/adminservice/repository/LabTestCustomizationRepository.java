package com.mdtlabs.coreplatform.adminservice.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.LabTestCustomization;

/**
 * <p>
 * This is the repository class of LabTestCustomization for communicate link between server side and database.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jun 20, 2024
 */
@Repository
public interface LabTestCustomizationRepository extends JpaRepository<LabTestCustomization, Long> {
    String GET_LAB_TEST_CUSTOMIZATION = "FROM LabTestCustomization as labTestCustomization" +
            " where (COALESCE(:searchTerm) is null OR (lower(labTestCustomization.testName)" +
            " LIKE CONCAT('%',lower(cast(:searchTerm as text)),'%')))" +
            " and labTestCustomization.countryId = (:countryId)" +
            " and labTestCustomization.isDeleted=false" +
            " and labTestCustomization.isActive=true";

    /**
     * Get the list of LabTestCustomization based on the search term and country id.
     *
     * @param searchTerm The search term used to filter LabTestCustomization by name. It can be null.
     * @param countryId  The id of the country for which LabTestCustomization are to be retrieved.
     * @param pageable   The pagination information (page number, size, sort order).
     * @return A page of LabTestCustomization that match the search term and country id.
     */
    @Query(value = GET_LAB_TEST_CUSTOMIZATION)
    Page<LabTestCustomization> findAllByName(@Param("searchTerm") String searchTerm,
                                             @Param("countryId") Long countryId,
                                             Pageable pageable);

    /**
     * Find a LabTestCustomization by its id, only if it is active and not deleted.
     *
     * @param id The id of the LabTestCustomization to be retrieved.
     * @return The LabTestCustomization if found, null otherwise.
     */
    LabTestCustomization findByIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     *
     * Get LabTest Customization details by using uniqueName and country Id.
     *
     * @param uniqueName unique name of the test.
     * @param countryId countryId of the test.
     * @return labTestCustomization details.
     */
    LabTestCustomization findByUniqueNameAndCountryIdAndIsDeletedFalseAndIsActiveTrue(String uniqueName, Long countryId);

    /**
     *
     * Get LabTestCustomization details by using TestName and uniqueName and Country Id.
     *
     * @param testName name of the test.
     * @param uniqueName Unique name of the test.
     * @param countryId Country Id of the test.
     * @return LabTestCustomization details.
     */
    LabTestCustomization findByTestNameIgnoreCaseAndUniqueNameAndCountryIdAndIsDeletedFalseAndIsActiveTrue(String testName, String uniqueName, Long countryId);

    /**
     *
     * Get LabTestCustomization details by using name and CountryId
     *
     * @param testName name of the test
     * @param countryId country id of the test
     * @return LabTestCustomization details
     */
    LabTestCustomization findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(String testName, Long countryId);

    /**
     * To get the labTestCustomization with name and country and other then the given id.
     *
     * @param testName name of the test
     * @param id id of the test
     * @param countryId countryId of the test
     * @return labTestCustomization details
     */
    LabTestCustomization findByTestNameIgnoreCaseAndIdNotAndCountryIdAndIsDeletedFalseAndIsActiveTrue(String testName, Long id, Long countryId);
}
