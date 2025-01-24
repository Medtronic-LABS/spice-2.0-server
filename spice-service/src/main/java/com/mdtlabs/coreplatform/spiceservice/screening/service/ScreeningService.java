package com.mdtlabs.coreplatform.spiceservice.screening.service;

import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;

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
     *     Endpoint for processing the screening log details and saved in FHIR Server
     * </p>
     *
     * @param request The screening log request DTO received in the request body.
     * @param file The multipart file to be uploaded is given.
     *
     * @return A String containing the created screening log fhir details.
     */
    BioDataDTO processScreeningLog(ScreeningLogRequestDTO request, MultipartFile file) throws Exception;

    /**
     * <p>
     * Method used to get the patient count for user using fhir id.
     * <p/>
     *
     * @param request Patient request DTO
     * @return {@link DashboardDetails} Patient count like screened, enrolled and assessed information.
     */
    DashboardDetails getPatientCountOfUsers(DashboardDetailsRequestDTO request);
}
