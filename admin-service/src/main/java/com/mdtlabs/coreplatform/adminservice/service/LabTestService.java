package com.mdtlabs.coreplatform.adminservice.service;

import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * This an interface class for LabTest module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
public interface LabTestService {

    /**
     * Creates a new LabTest entity based on the provided LabTestDTO.
     * <p>
     * This method is responsible for creating a new LabTest entity in the system. The LabTestDTO contains all the necessary
     * information required for creating the LabTest, such as its name, type, and other relevant details. The method ensures
     * that the LabTest is properly created and persisted in the database.
     * </p>
     *
     * @param labTest The LabTestDTO containing the information for the new LabTest.
     * @return The newly created LabTest entity.
     */
    public LabTest createLabTest(LabTestDTO labTest);

    /**
     * Retrieves a list of all LabTests available in a specific country.
     * <p>
     * This method fetches a comprehensive list of LabTests based on the search criteria provided in the SearchRequestDTO.
     * The criteria can include filters such as country ID, allowing for the retrieval of LabTests specific to a country.
     * The method returns a ResponseListDTO containing the list of LabTestDTOs that match the criteria.
     * </p>
     *
     * @param requestDto The SearchRequestDTO containing the search criteria.
     * @return A ResponseListDTO of LabTestDTOs matching the search criteria.
     */
    public ResponseListDTO<LabTestDTO> getAllLabTests(SearchRequestDTO requestDto);

    /**
     * Soft deletes a LabTest based on the provided criteria.
     * <p>
     * This method updates the status of a LabTest to indicate it has been soft deleted. The actual deletion is not performed;
     * instead, the LabTest's status is updated to reflect it is no longer active. This allows for potential recovery of the
     * LabTest in the future. The method takes in a SearchRequestDTO containing the criteria for identifying the LabTest to be
     * soft deleted.
     * </p>
     *
     * @param requestDto The SearchRequestDTO containing the criteria for the LabTest to be soft deleted.
     * @return true if the LabTest was successfully soft deleted, false otherwise.
     */
    public boolean removeLabTest(SearchRequestDTO requestDto);

    /**
     * Updates the details of an existing LabTest entity.
     * <p>
     * This method is responsible for updating the details of a LabTest entity based on the provided LabTestDTO.
     * The LabTestDTO contains the updated information for the LabTest, such as its name, type, and other relevant details.
     * This ensures that the LabTest entity is updated accordingly in the database.
     * </p>
     *
     * @param labTest The LabTestDTO containing the updated information for the LabTest.
     * @return The updated LabTest entity.
     */
    public LabTest updateLabTest(LabTestDTO labTest);

    /**
     * Retrieves the details of a specific LabTest by its ID.
     * <p>
     * This method fetches the details of a LabTest based on the ID provided in the SearchRequestDTO. It is useful for retrieving
     * specific LabTest details when the ID is known. This can be particularly useful in scenarios where detailed information
     * of a specific LabTest is required.
     * </p>
     *
     * @param requestDto The SearchRequestDTO containing the ID of the LabTest to retrieve.
     * @return The LabTest entity with the specified ID.
     */
    public LabTest getLabTestById(SearchRequestDTO requestDto);

    /**
     * Validates the details of a LabTest entity.
     * <p>
     * This method performs validation on the provided LabTestDTO to ensure that all necessary information is correct and
     * complete. This includes checking the validity of the LabTest's name, type, and other relevant details. The method
     * returns true if the LabTest details are valid, false otherwise.
     * </p>
     *
     * @param labTest The LabTestDTO to validate.
     * @return true if the LabTest details are valid, false otherwise.
     */
    public Boolean validateLabTest(LabTestDTO labTest);
}
