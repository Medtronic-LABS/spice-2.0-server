package com.mdtlabs.coreplatform.spiceservice.screening.controller;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.screening.service.ScreeningService;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 *     Controller class handling screening-related endpoints.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@RestController
@RequestMapping("/screening")
public class ScreeningController {
    private final ScreeningService screeningService;

    @Autowired
    public ScreeningController(ScreeningService screeningService) {
        this.screeningService = screeningService;
    }

    /**
     * <p>
     *     Endpoint for processing the screening log details and saved in FHIR Server
     * </p>
     *
     * @param screeningRequest The request of the patient to be saved is given.
     * @param signatureFile The file to be saved is given
     *
     * @return A String containing the created screening log fhir details.
     */
    @PostMapping("/create")
    public SuccessResponse<Boolean> processScreeningLog(@RequestParam(Constants.SCREENING_REQUEST) String screeningRequest,
                                                        @RequestParam(required = false, name = Constants.SIGNATURE_FILE) MultipartFile signatureFile) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            ScreeningLogRequestDTO request = mapper.readValue(screeningRequest,
                    ScreeningLogRequestDTO.class);
            Logger.logInfo("In ScreeningLogController, adding screening information");
            return new SuccessResponse<>(SuccessCode.SCREENING_LOG_CREATED, screeningService.processScreeningLog(request, signatureFile), HttpStatus.OK);
        } catch (JsonProcessingException processingException) {
            Logger.logError(processingException);
            throw new SpiceValidation(1511, processingException.getMessage());
        }
    }

    /**
     * <p>
     * Method used to get the user's patient count details for given fhir id.
     * <p/>
     *
     * @param request Patient request DTO
     * @return Patient dashboard count information.
     */
    @PostMapping("/dashboard-count")
    public SuccessResponse<DashboardDetails> getCountOfUser(@RequestBody DashboardDetailsRequestDTO request) throws DataNotFoundException {
        return new SuccessResponse<>(SuccessCode.PATIENTS_COUNT_FETCHED, screeningService.getPatientCountOfUsers(request), HttpStatus.OK);
    }
}
