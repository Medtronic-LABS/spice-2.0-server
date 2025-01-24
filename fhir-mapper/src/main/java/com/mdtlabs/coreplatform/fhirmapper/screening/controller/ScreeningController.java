package com.mdtlabs.coreplatform.fhirmapper.screening.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MessageConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.screening.service.ScreeningService;

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
     * @param request The screening log request DTO received in the request body.
     *
     * @return A String containing the created screening log fhir details.
     */
    @PostMapping("/create")
    public BioDataDTO processScreeningLog(@RequestBody ScreeningLogRequestDTO request) {
        BioDataDTO response = screeningService.processScreeningLog(request);
        Logger.logInfo(MessageConstants.CREATED_SCREENED_LOG + response.toString());
        return response;
    }

    /**
     * <p>
     * Method used to get the screening log details for given fhir id.
     * <p/>
     *
     * @param request Patient request DTO
     * @return {@link ScreeningLog} Patient screening log information.
     */
    @PostMapping("/get-by-person-id")
    public ScreeningLog getScreeningLog(@RequestBody ScreeningLogRequestDTO request) {
        return screeningService.getScreeningLog(request);
    }

    /**
     * <p>
     * Method used to get the user's patient count details for given fhir id.
     * <p/>
     *
     * @param request Patient request DTO
     * @return {@link DashboardDetails} Patient screening log information.
     */
    @PostMapping("/dashboard-count")
    public DashboardDetails getPatientCountDetails(@RequestBody DashboardDetailsRequestDTO request) {
        return screeningService.getPatientCountOfUsers(request);
    }
}
