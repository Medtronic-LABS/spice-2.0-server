package com.mdtlabs.coreplatform.adminservice.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.service.LabTestCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * Controller class for LabTestCustomization.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jun 20, 2024
 */
@RestController
@RequestMapping("/lab-test-customization")
public class LabTestCustomizationController {

    private final LabTestCustomizationService labTestCustomizationService;

    public LabTestCustomizationController(LabTestCustomizationService labTestCustomizationService) {
        this.labTestCustomizationService = labTestCustomizationService;
    }

    /**
     * Create a new LabTestCustomization.
     *
     * @param labTestCustomizationDTO The DTO containing the details of the LabTestCustomization to be created.
     * @return The created LabTestCustomizationDTO wrapped in a SuccessResponse.
     */
    @PostMapping("/create")
    public SuccessResponse<LabTestCustomizationDTO> createLabTestCustomization(@RequestBody LabTestCustomizationDTO labTestCustomizationDTO) {
        return new SuccessResponse<>(SuccessCode.LABTEST_SAVE,
                labTestCustomizationService.createLabTestCustomization(labTestCustomizationDTO),
                HttpStatus.CREATED);
    }

    /**
     * Retrieve a LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     * @return The retrieved LabTestCustomizationDTO.
     */
    @PostMapping("/search")
    public LabTestCustomizationDTO getLabTestCustomization(@RequestBody SearchRequestDTO requestDTO) {
        return labTestCustomizationService.getLabTestCustomization(requestDTO);
    }

    @PostMapping("/get-by-unique-name")
    public SuccessResponse<LabTestCustomizationDTO> getLabTestCustomizationByUniqueName(@RequestBody SearchRequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GET_LABTEST,
                labTestCustomizationService.getLabTestCustomizationByUniqueName(requestDTO),
                HttpStatus.OK);
    }

    /**
     * Retrieve a list of LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     * @return A list of retrieved LabTestCustomizationDTOs wrapped in a SuccessResponse.
     */
    @PostMapping("/list")
    public SuccessResponse<LabTestCustomizationDTO> listLabTestCustomization(@RequestBody SearchRequestDTO requestDTO) {
        ResponseListDTO<LabTestCustomizationDTO> labTestCustomizationDTOs =
                labTestCustomizationService.listLabTestCustomization(requestDTO);
        return new SuccessResponse<>(SuccessCode.GET_LABTESTS,
                labTestCustomizationDTOs.getData(),
                labTestCustomizationDTOs.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * Update a LabTestCustomization.
     *
     * @param labTestCustomizationDTO The DTO containing the updated details of the LabTestCustomization.
     * @return The updated LabTestCustomizationDTO wrapped in a SuccessResponse.
     */
    @PostMapping("/update")
    public SuccessResponse<LabTestCustomizationDTO> updateLabTestCustomization(@RequestBody LabTestCustomizationDTO labTestCustomizationDTO) {
        return new SuccessResponse<>(SuccessCode.LABTEST_UPDATE,
                labTestCustomizationService.updateLabTestCustomization(labTestCustomizationDTO),
                HttpStatus.ACCEPTED);
    }

    /**
     * Delete a LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     * @return A SuccessResponse indicating the deletion was successful.
     */
    @PostMapping("/delete")
    public SuccessResponse<Boolean> deleteLabTestCustomization(@RequestBody SearchRequestDTO requestDTO) {
        labTestCustomizationService.deleteLabTestCustomization(requestDTO);
        return new SuccessResponse<>(SuccessCode.LABTEST_DELETE, Boolean.TRUE, HttpStatus.OK);
    }

    /**
     * <p>
     *      To validate the labTest name to avoid duplicates.
     * </p>
     *
     * @param requestDTO request with name and countryId of labTest
     * @return boolean wrapped with SuccessResponse
     */
    @PostMapping("/validate")
    public SuccessResponse<Boolean> validateLabTestCustomization(@RequestBody SearchRequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.LABTEST_VALIDATE,
                labTestCustomizationService.validateLabTestCustomization(requestDTO),
                HttpStatus.OK);
    }
}
