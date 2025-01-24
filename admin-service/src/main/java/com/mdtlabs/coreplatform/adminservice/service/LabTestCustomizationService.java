package com.mdtlabs.coreplatform.adminservice.service;

import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * * This interface defines the operations that can be performed on LabTestCustomization.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jun 20, 2024
 */
public interface LabTestCustomizationService {

    /**
     * Creates a new LabTestCustomization.
     *
     * @param labTestCustomizationDTO The DTO containing the details of the LabTestCustomization to be created.
     * @return The created LabTestCustomizationDTO.
     */
    LabTestCustomizationDTO createLabTestCustomization(LabTestCustomizationDTO labTestCustomizationDTO);

    /**
     * Retrieves a LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     * @return The retrieved LabTestCustomizationDTO.
     */
    LabTestCustomizationDTO getLabTestCustomization(SearchRequestDTO requestDTO);

    /**
     *
     * To retrieve a LabTestCustomization based on the provided search criteria name.
     *
     * @param requestDTO
     * @return labTestCustomizationDTO
     */
    LabTestCustomizationDTO getLabTestCustomizationByUniqueName(SearchRequestDTO requestDTO);

    /**
     * Retrieves a list of LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     * @return A ResponseListDTO containing the list of retrieved LabTestCustomizationDTOs.
     */
    ResponseListDTO<LabTestCustomizationDTO> listLabTestCustomization(SearchRequestDTO requestDTO);

    /**
     * Updates a LabTestCustomization.
     *
     * @param labTestCustomizationDTO The DTO containing the updated details of the LabTestCustomization.
     * @return The updated LabTestCustomizationDTO.
     */
    LabTestCustomizationDTO updateLabTestCustomization(LabTestCustomizationDTO labTestCustomizationDTO);

    /**
     * Deletes a LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     */
    void deleteLabTestCustomization(SearchRequestDTO requestDTO);

    /**
     * to validate the name of the labTest.
     * @param requestDTO request with name of the labTest
     * @return boolean
     */
    Boolean validateLabTestCustomization(SearchRequestDTO requestDTO);
}
