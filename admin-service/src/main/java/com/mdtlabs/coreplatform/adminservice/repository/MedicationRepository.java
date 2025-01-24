package com.mdtlabs.coreplatform.adminservice.repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;

import java.util.List;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Medication module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Dec 26, 2023
 */
@Repository
public interface MedicationRepository extends JpaRepository<Medication, Long> {

    String GET_MEDICATION_BY_MEDICATION_NAME = "select medication from Medication as medication"
            + " WHERE (COALESCE(:searchTerm) is null or lower(medication.name) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%'))"
            + " AND medication.countryId=:countryId AND medication.isDeleted=false AND medication.isActive=true"
            + " ORDER BY medication.name ASC,medication.updatedAt DESC";


    String MEDICATION_BY_MANDATORY_FIELDS = "select medication from Medication "
            + "as medication where (:countryId is null or medication.countryId=:countryId)"
            + " AND (COALESCE(:name) is null or lower(medication.name) LIKE"
            + " lower(CAST(:name AS text))) "
            + " AND (:classificationId is null or medication.classificationId=:classificationId)"
            + " AND (:brandId is null or medication.brandId=:brandId) AND (:dosageFormId is null or "
            + " medication.dosageFormId=:dosageFormId) AND medication.isDeleted=false AND medication.isActive=true";


    String MEDICATION_BY_COUNTRY = "select medication from Medication"
            + " AS medication where (:countryId is null or medication.countryId=:countryId)"
            + " AND medication.isDeleted=false AND medication.isActive=true"
            + " AND (COALESCE(:searchTerm) is null or lower(medication.name) LIKE"
            + " CONCAT('%',lower(CAST(:searchTerm AS text)),'%'))";


    public static final String GET_OTHER_MEDICATION = "FROM Medication AS medication"
            + " WHERE medication.countryId = :countryId AND medication.brandName = :brandName AND"
            + " medication.name = :name AND medication.classificationName = :classificationName"
            + " AND medication.dosageFormName = :dosageFormName AND medication.isDeleted=false AND"
            + " medication.isActive=true";

    public static final String GET_ALL_MEDICATIONS_BY_IDS = "SELECT medication FROM Medication as medication WHERE medication.id "
            + "IN :ids AND medication.isDeleted = false AND medication.isActive = true";

    /**
     * Retrieves a single Medication entity based on mandatory fields including classification, brand, dosage form, country ID, and name.
     * <p>
     * This method is designed to fetch a Medication entity that strictly matches the given criteria. It is particularly useful
     * for operations requiring precise retrieval of medication records based on a combination of mandatory attributes.
     * The query filters out any deleted or inactive medication records, ensuring only relevant, active medications are returned.
     * </p>
     *
     * @param classification The classification ID of the Medication.
     * @param brand          The brand ID of the Medication.
     * @param dosageForm     The dosage form ID of the Medication.
     * @param country        The country ID associated with the Medication.
     * @param name           The name of the Medication.
     * @return A Medication entity that matches the specified criteria, or null if no match is found.
     */
    @Query(value = MEDICATION_BY_MANDATORY_FIELDS)
    Medication getMedicationByMandatoryFields(@Param("classificationId") Long classification,
                                              @Param("brandId") Long brand, @Param("dosageFormId") Long dosageForm,
                                              @Param("countryId") Long country,
                                              @Param("name") String name);

    /**
     * Retrieves a Medication entity by its ID, ensuring it has not been marked as deleted.
     * <p>
     * This method is used to fetch a single Medication entity based on its unique identifier,
     * provided that the entity has not been marked as deleted. It is particularly useful for operations
     * that require fetching active medication records.
     * </p>
     *
     * @param id The unique identifier of the Medication entity to retrieve.
     * @return The Medication entity if found and not deleted, null otherwise.
     */
    Medication findByIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     * Retrieves a Medication entity by its name, ensuring it has not been marked as deleted.
     * <p>
     * This method is used to fetch a single Medication entity based on its name,
     * provided that the entity has not been marked as deleted. It is particularly useful for operations
     * that require fetching active medication records by name.
     * </p>
     *
     * @param name The name of the Medication entity to retrieve.
     * @return The Medication entity if found and not deleted, null otherwise.
     */
    Medication findByNameAndIsDeletedFalseAndIsActiveTrue(String name);

    /**
     * Retrieves a paginated list of Medication entities based on search criteria and country ID.
     * <p>
     * This method fetches Medication entities that match the given search term (if any) and belong to a specified country.
     * The results are paginated to support large datasets efficiently. This method is particularly useful for applications
     * that need to filter medications by name and geographical location, while also supporting search functionality.
     * </p>
     *
     * @param searchTerm An optional search term for filtering the Medication entities by name.
     * @param countryId  The ID of the country associated with the Medication entities to retrieve.
     * @param pageable   A {@link Pageable} object to determine the pagination parameters.
     * @return A {@link Page} of Medication entities matching the criteria.
     */
    @Query(value = MEDICATION_BY_COUNTRY)
    Page<Medication> getAllMedications(@Param("searchTerm") String searchTerm, @Param("countryId") Long countryId, Pageable pageable);

    /**
     * Retrieves a specific Medication entity based on a combination of country ID, medication name, brand name,
     * classification name, and dosage form name, ensuring it is active and not deleted.
     * <p>
     * This method is particularly useful for fetching a Medication entity that exactly matches a set of criteria
     * including the country ID, medication name, brand name, classification name, and dosage form name. It ensures
     * that the retrieved medication is active and has not been marked as deleted, making it suitable for operations
     * that require precise and current medication data.
     * </p>
     *
     * @param countryId          The ID of the country associated with the Medication entity.
     * @param medicationName     The name of the Medication entity.
     * @param brandName          The brand name associated with the Medication entity.
     * @param classificationName The classification name associated with the Medication entity.
     * @param dosageFormName     The dosage form name associated with the Medication entity.
     * @return The Medication entity if found, null otherwise.
     */
    @Query(value = GET_OTHER_MEDICATION)
    Medication getOtherMedication(@Param("countryId") long countryId,
                                  @Param("name") String medicationName, @Param("brandName") String brandName,
                                  @Param("classificationName") String classificationName, @Param("dosageFormName") String dosageFormName);

    /**
     * Searches for Medication entities based on a search term and country ID.
     * <p>
     * This method performs a search operation in the Medication entities, filtering them by a provided search term
     * and country ID. The search term is applied to the name of the medication, allowing for partial matches.
     * This is particularly useful for applications that need to offer search functionality across medications
     * within a specific country. The search is case-insensitive and supports pagination for efficient data retrieval.
     * </p>
     *
     * @param searchTerm The search term used to filter Medication entities by name. It supports partial matches.
     * @param countryId  The ID of the country to which the Medication entities must belong.
     * @return A list of Medication entities that match the search criteria.
     */
    @Query(value = GET_MEDICATION_BY_MEDICATION_NAME)
    public List<Medication> searchMedications(@Param(Constants.SEARCH_TERM_FIELD) String searchTerm,
                                              @Param(Constants.COUNTRY_ID) Long countryId);

    /**
     * Retrieves a All Medication entity by its ID, ensuring it has not been marked as deleted.
     * <p>
     * This method is used to fetch a single Medication entity based on its unique identifier,
     * provided that the entity has not been marked as deleted. It is particularly useful for operations
     * that require fetching active medication records.
     * </p>
     *
     * @param ids The unique identifier of the Medication entity to retrieve.
     * @return The Medication entity if found and not deleted, null otherwise.
     */
    @Query(value = GET_ALL_MEDICATIONS_BY_IDS)
    List<Medication> getAllMedicationsByIds(@Param("ids") List<Long> ids);
}
