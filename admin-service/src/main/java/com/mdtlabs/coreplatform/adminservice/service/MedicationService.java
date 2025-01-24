package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.dto.ClassificationDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.MedicationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.DosageForm;
import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * This an interface class for Medication module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
public interface MedicationService {

    /**
     * Creates a list of new medications.
     * <p>
     * This method is responsible for creating new medication records based on the provided list of MedicationDTOs.
     * Each MedicationDTO contains all necessary information required to create a medication record, such as its name,
     * classification, dosage form, and other relevant details.
     * </p>
     *
     * @param request A list of MedicationDTO containing the details for the new medications.
     * @return A list of Medication entities representing the created medication records.
     */

    public List<Medication> createMedication(List<MedicationDTO> request);

    /**
     * Updates the details of an existing medication.
     * <p>
     * This method updates the details of a medication based on the provided MedicationDTO. The DTO should contain
     * the updated information such as the medication's name, classification, dosage form, and other relevant details.
     * The update is applied to the medication entity that matches the ID provided in the DTO.
     * </p>
     *
     * @param medication A MedicationDTO containing the updated details of the medication.
     * @return The updated Medication entity.
     */
    public Medication updateMedication(MedicationDTO medication);

    /**
     * Retrieves the details of a medication by its ID.
     * <p>
     * This method fetches the details of a medication based on the ID provided in the SearchRequestDTO.
     * The ID should uniquely identify the medication record in the system. The method returns the Medication
     * entity that matches the provided ID.
     * </p>
     *
     * @param request A SearchRequestDTO containing the ID of the medication to retrieve.
     * @return The Medication entity corresponding to the provided ID.
     */
    public Medication getMedicationById(SearchRequestDTO request);

    /**
     * Retrieves the details of  medication by its IDs.
     * <p>
     * This method fetches the details of a medication based on the ID provided in the SearchRequestDTO.
     * The ID should uniquely identify the medication record in the system. The method returns the Medication
     * entity that matches the provided ID.
     * </p>
     *
     * @param ids A SearchRequestDTO containing the list ID of the medication to retrieve.
     * @return The Medication entity corresponding to the provided ID.
     */
    public List<MedicationDTO> getMedicationsByIds(List<Long> ids);

    /**
     * Retrieves a single medication's details by its name.
     * <p>
     * This method fetches the details of a medication based on the name provided in the {@link SearchRequestDTO}.
     * The name should uniquely identify the medication record in the system. The method returns the {@link Medication}
     * entity that matches the provided name.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the name of the medication to retrieve.
     * @return The {@link Medication} entity corresponding to the provided name.
     */

    public Medication getMedicationByName(SearchRequestDTO request);

    /**
     * Retrieves all medications' details.
     * <p>This method fetches details of all medications stored in the system. It can be used to retrieve a comprehensive
     * list of medications, including their names, classifications, dosage forms, and other relevant details. The method
     * utilizes the {@link SearchRequestDTO} for specifying any filtering criteria, such as searching by name or classification,
     * to refine the results.</p>
     *
     * @param request The {@link SearchRequestDTO} used for specifying any filtering criteria.
     * @return A {@link ResponseListDTO} of {@link MedicationDTO} representing the details of all medications.
     */

    public ResponseListDTO<MedicationDTO> getAllMedications(SearchRequestDTO request);

    /**
     * Soft deletes a medication by its ID.
     * <p>
     * This method marks a medication as deleted within the system based on the provided ID in the {@link SearchRequestDTO}.
     * Soft deletion is preferred over hard deletion to allow for data recovery if necessary. The method updates the medication's
     * status to indicate it is deleted without actually removing its record from the database.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the ID of the medication to be soft deleted.
     * @return {@code Boolean} indicating whether the soft deletion was successful. Returns {@code true} if the operation was successful,
     * {@code false} otherwise.
     */
    public Boolean deleteMedicationById(SearchRequestDTO request);

    /**
     * Validates the provided medication details.
     * <p>
     * This method checks the validity of the medication details provided in the {@link MedicationDTO}.
     * It ensures that all required fields are present and meet the system's validation criteria, such as
     * name uniqueness, valid classification, and dosage form. This method is crucial for maintaining data integrity
     * and preventing invalid data from being entered into the system.
     * </p>
     *
     * @param medication The {@link MedicationDTO} containing the medication details to be validated.
     * @return {@code Boolean} indicating whether the medication details are valid ({@code true}) or not ({@code false}).
     */
    public Boolean validateMedication(MedicationDTO medication);

    /**
     * Retrieves medication details for a specific country.
     * <p>
     * This method is used to fetch medication details that are specific to a given country, identified by the countryId.
     * It can be useful for applications that need to display or process medication information based on the user's location
     * or the regulatory requirements of a particular country.
     * </p>
     *
     * @param countryId The unique identifier of the country for which medication details are to be retrieved.
     * @return A {@link Medication} entity containing the medication details for the specified country.
     */
    public Medication getOtherMedication(long countryId);

    /**
     * Retrieves a list of classifications for a specific country.
     * <p>
     * This method fetches a list of medication classifications that are applicable or available in a given country,
     * identified by the {@code countryId}. It is useful for filtering medications or medication-related data based on
     * the regulatory classifications that vary from country to country.
     * </p>
     *
     * @param countryId The unique identifier of the country for which classifications are to be retrieved.
     * @return A list of {@link ClassificationDTO} entities representing the medication classifications for the specified country.
     */
    List<ClassificationDTO> getClassifications(Long countryId);

    /**
     * Retrieves a list of all available dosage forms.
     * <p>
     * This method fetches a comprehensive list of dosage forms available in the system. Dosage forms are distinct ways
     * in which a medication can be produced and administered to patients (e.g., tablet, capsule, liquid). This information
     * is crucial for categorizing medications and ensuring accurate prescription and administration.
     * </p>
     *
     * @return A list of {@link DosageForm} entities representing all available dosage forms.
     */
    public List<DosageForm> getDosageForms();

    /**
     * Searches for medications by name.
     * <p>
     * This method allows for searching medications based on their name. It utilizes the {@link SearchRequestDTO} to encapsulate
     * the search criteria, primarily the name of the medication. The search is designed to be flexible and can support various
     * search patterns (e.g., partial names, case-insensitive matching). The result is a list of {@link MedicationDTO} that match
     * the search criteria, encapsulated in a {@link ResponseListDTO} to provide additional metadata about the search results, such
     * as total count.
     * </p>
     *
     * @param requestDTO The {@link SearchRequestDTO} containing the name of the medication to search for.
     * @return A {@link ResponseListDTO} of {@link MedicationDTO} representing the medications that match the search criteria.
     */
    ResponseListDTO<MedicationDTO> searchMedicationByName(SearchRequestDTO requestDTO);
}
