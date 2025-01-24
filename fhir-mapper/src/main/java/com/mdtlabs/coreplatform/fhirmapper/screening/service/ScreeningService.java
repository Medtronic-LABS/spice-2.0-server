package com.mdtlabs.coreplatform.fhirmapper.screening.service;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;

/**
 * <p>
 *   Service for handling screening related operations.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
public interface ScreeningService {

    /**
     * <p>
     * Endpoint for processing the screening log details and saved in FHIR Server
     * </p>
     *
     * @param request The screening log request DTO received in the request body.
     * @return A String containing the created screening log fhir details.
     */
    BioDataDTO processScreeningLog(ScreeningLogRequestDTO request);

    /**
     * <p>
     * Method used to get the screening log details for given fhir id.
     * <p/>
     *
     * @param requestDTO Patient request DTO
     * @return {@link ScreeningLog} Patient screening log information.
     */
    ScreeningLog getScreeningLog(ScreeningLogRequestDTO requestDTO);

    /**
     * <p>
     * Method used to get the patient count for user using fhir id.
     * <p/>
     *
     * @param requestDTO Patient request DTO
     * @return {@link DashboardDetails} Patient count like screened, enrolled and assessed information.
     */
    DashboardDetails getPatientCountOfUsers(DashboardDetailsRequestDTO requestDTO);
}
