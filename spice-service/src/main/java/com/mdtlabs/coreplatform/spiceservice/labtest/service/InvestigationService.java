package com.mdtlabs.coreplatform.spiceservice.labtest.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

public interface InvestigationService {

    /**
     *<p>
     *    To create the investigation details.
     *</p>
     *
     * @param requestDTO investigation details
     * @return response of investigation details
     */
    Map<String, String> createOrUpdateInvestigation(LabTestRequestDTO requestDTO);

    /**
     *  <p>
     *      This method to get the list of the investigatedDetails.
     *  </p>
     *
     * @param requestDTO request details
     * @return list of the investigatedDetails
     */
    LabTestDTO getInvestigatedDetails(RequestDTO requestDTO);

    /**
     *  <p>
     *      This method to get the list of the investigatedDetails by using encounter.
     *  </p>
     *
     * @param requestDTO request details
     * @return list of the investigatedDetails
     */
    List<LabTestDTO> getInvestigationsByEncounter(RequestDTO requestDTO);

    /**
     * <p>
     *  To remove or delete the investigation details by using investigation id.
     * </p>
     *
     * @param requestDTO investigation details
     * @return id of labTest.
     */
    Map<String,String> removeInvestigation(RequestDTO requestDTO);

    /**
     * <p>
     *     This method to get the list of the investigated details.
     * </p>
     * @param request request with patient reference and encounter details.
     * @return list of the investigatedDetails.
     */
    LabTestHistoryDTO getHistoryInvestigatedDetails(RequestDTO request);

    /**
     * Retrieves a LabTestCustomization based on the provided search criteria.
     *
     * @param requestDTO The DTO containing the search criteria.
     * @return The retrieved LabTestCustomizationDTO.
     */
    LabTestCustomizationDTO getLabTestCustomization(SearchRequestDTO requestDTO);

    /**
     * <p>
     *     This method used to create the lab test result.
     * <p/>
     *
     * @param labTestRequestDTO - Result create DTO with the lab test information.
     */
    void updateInvestigationResult(LabTestRequestDTO labTestRequestDTO);

    /**
     * <p>
     *     Review the lab test result entered by the lab technician.
     * <p/>
     *
     * @param request - Review comments and lab test id.
     */
    void reviewInvestigation(RequestDTO request);

    /**
     * <p>
     * Get patient lab test intensification details.
     * </p>
     * @param request - Contains patient tracker id
     * @return - Returns patient's lab test history details.
     */
    Map<String, List<LabTestDTO>> getIntensificationDetails(RequestDTO request);
}
