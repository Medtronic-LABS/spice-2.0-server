package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.Objects;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;
import com.mdtlabs.coreplatform.adminservice.service.LabTestService;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * The LabTestController class is a REST controller that handles requests related to LabTest.
 * </p>
 *
 * @author Karthick M
 */
@RestController
@RequestMapping("/labtest")
public class LabTestController {

    private final LabTestService labTestService;

    public LabTestController(LabTestService labTestService) {
        this.labTestService = labTestService;
    }

    /**
     * Adds a new LabTest.
     * <p>
     * This method handles a POST request to create a new LabTest entity based on the provided LabTestDTO.
     * It logs the creation process, calls the LabTestService to create the LabTest, and returns a SuccessResponse
     * with the status code CREATED. The SuccessResponse includes the SuccessCode indicating the operation's success.
     * </p>
     *
     * @param labTestDto The LabTestDTO containing the details of the LabTest to be created.
     * @return A SuccessResponse containing the created LabTest entity and the HTTP status code CREATED.
     */
    @PostMapping("/create")
    public SuccessResponse<LabTest> addLabTest(@RequestBody LabTestDTO labTestDto) {
        Logger.logInfo("In Labtest Controller, creating a new labtest");
        labTestService.createLabTest(labTestDto);
        return new SuccessResponse<>(SuccessCode.LABTEST_SAVE, HttpStatus.CREATED);
    }

    /**
     * Retrieves a list of LabTests based on a given criteria.
     * <p>
     * This method processes a POST request to fetch a list of LabTests under a specific country. It logs the retrieval
     * process, invokes the LabTestService to get all LabTests based on the provided SearchRequestDTO, and returns a
     * SuccessResponse. The SuccessResponse includes the list of LabTestDTOs, the total count of LabTests (if available),
     * and the HTTP status code OK. If no LabTests are found, the total count is set to null.
     * </p>
     *
     * @param requestObject The SearchRequestDTO containing the criteria for fetching the LabTests.
     * @return A SuccessResponse containing the list of LabTestDTOs, the total count (if available), and the HTTP status code OK.
     */
    @PostMapping("/list")
    public SuccessResponse<LabTestDTO> getAllLabTests(@RequestBody SearchRequestDTO requestObject) {
        Logger.logInfo("In Labtest Controller, getting labtest list");
        ResponseListDTO<LabTestDTO> response = labTestService.getAllLabTests(requestObject);
        if ((Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount())) {
            return new SuccessResponse<>(SuccessCode.GET_LABTESTS, response.getData(), null, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_LABTESTS, response.getData(), response.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * Soft deletes a lab test by its ID.
     * <p>
     * This method handles a POST request to mark a lab test as deleted in the system based on its ID.
     * It logs the removal process, calls the {@link LabTestService#removeLabTest(SearchRequestDTO)} method to perform the deletion,
     * and returns a {@link SuccessResponse<Boolean>} with a status of OK, indicating the successful deletion.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the ID of the lab test to be deleted.
     * @return A {@link SuccessResponse<Boolean>} object with the status OK, indicating successful deletion.
     */
    @PostMapping("/remove")
    public SuccessResponse<Boolean> removeLabTest(@RequestBody SearchRequestDTO requestDto) {
        Logger.logInfo("In Labtest Controller, removing a labtest");
        labTestService.removeLabTest(requestDto);
        return new SuccessResponse<>(SuccessCode.LABTEST_DELETE, HttpStatus.OK);
    }

    /**
     * Updates a lab test's details.
     * <p>
     * This method handles a POST request to update the details of a lab test, such as its name, based on the provided {@link LabTestDTO}.
     * It logs the update process, calls the {@link LabTestService#updateLabTest(LabTestDTO)} method to perform the update,
     * and returns a {@link SuccessResponse<LabTest>} with a status of OK, indicating the successful update.
     * </p>
     *
     * @param labTestDto A {@link LabTestDTO} object containing the updated details of the lab test.
     * @return A {@link SuccessResponse<LabTest>} object with the status OK, indicating successful update.
     */
    @PostMapping("/update")
    public SuccessResponse<LabTest> updateLabTest(@RequestBody LabTestDTO labTestDto) {
        Logger.logInfo("In Labtest Controller, updating labtest list");
        labTestService.updateLabTest(labTestDto);
        return new SuccessResponse<>(SuccessCode.LABTEST_UPDATE, HttpStatus.OK);
    }

    /**
     * Retrieves the details of a specific lab test by its ID.
     * <p>
     * This method handles a POST request to fetch the details of a lab test identified by its ID. It logs the retrieval process,
     * calls the {@link LabTestService#getLabTestById(SearchRequestDTO)} method to perform the search, and returns a
     * {@link SuccessResponse<LabTest>} with a status of OK, indicating the successful retrieval of lab test details.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the ID of the lab test to be retrieved.
     * @return A {@link SuccessResponse<LabTest>} object with the status OK, containing the details of the requested lab test.
     */
    @PostMapping("/details")
    public SuccessResponse<LabTest> getLabTestById(@RequestBody SearchRequestDTO requestDto) {
        Logger.logInfo("In Labtest Controller, getting a labtest details");
        return new SuccessResponse<>(SuccessCode.GET_LABTEST, labTestService.getLabTestById(requestDto), HttpStatus.OK);
    }
}
