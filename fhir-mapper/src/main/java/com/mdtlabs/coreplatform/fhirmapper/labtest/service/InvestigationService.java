package com.mdtlabs.coreplatform.fhirmapper.labtest.service;

import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 *  This is an interface of InvestigationService.
 *  This class provides methods to handle investigation request-related operations
 * </p>
 *
 * @author Jaganathan R created on Jul 30, 2024.
 */
public interface InvestigationService {

    /**
     *<p>
     *    To create the investigation details.
     *</p>
     *
     * @param labTestRequestDTO investigation details
     * @return response of investigation details
     */
    Map<String, String> createOrUpdateInvestigation(LabTestRequestDTO labTestRequestDTO);

    /**
     *<p>
     *    To create the investigation details.
     *</p>
     *
     * @param labTestRequestDTO investigation details
     * @return response of investigation details
     */
    Map<String, String> createOrUpdateNcdInvestigation(LabTestRequestDTO labTestRequestDTO);

    /**
     *<p>
     *  To update the investigation information with result.
     *</p>
     *
     * @param labTestDTO investigation details
     * @param bundle bundle reference
     * @param encounterId encounter identifier
     * @param provenance provenance information
     * @param response response reference
     * @return response of the investigation details
     */
    Bundle updateInvestigationRequest(LabTestDTO labTestDTO, Bundle bundle, String encounterId,
                                                   ProvenanceDTO provenance, Map<String, String> response);

    /**
     * <p>
     *  To remove or delete the investigation details by using investigation id.
     * </p>
     *
     * @param id identifier of the investigation
     * @param provenance provenance details
     * @return id of labTest.
     */
    Map<String, String> removeInvestigation(String id, ProvenanceDTO provenance);

    /**
     *  <p>
     *      This method to get the list of the investigatedDetails.
     *  </p>
     *
     * @param requestDTO request details
     * @return list of the investigatedDetails
     */
    List<LabTestDTO> getListOfInvestigatedDetails(RequestDTO requestDTO);

    /**
     * <p>
     *     This method to get the list of the investigated details.
     * </p>
     * @param request request with patient reference and encounter details.
     * @return list of the investigatedDetails.
     */
    LabTestHistoryDTO getInvestigatedDetails(RequestDTO request);

    /**
     * <p>
     *     This method to get the list of the NCD investigated details.
     * </p>
     * @param request request with patient reference and encounter details.
     * @return list of the investigatedDetails.
     */
    LabTestHistoryDTO getNcdInvestigatedDetails(RequestDTO request);

    /**
     * <p>
     *      This method to get the list of the investigated details by using encounterId.
     * </p>
     *
     * @param encounterId encounter id
     * @param patientReference patient reference
     * @return list of the investigatedDetails
     */
    List<LabTestDTO> getInvestigationsByEncounter(String encounterId, String patientReference);

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
     * @param requestDTO - Contains patient tracker id
     * @param names - Contains the intensification lab test names.
     * @return - Returns patient's lab test history details.
     */
    List<LabTestDTO> getInvestigationByNames(RequestDTO requestDTO, List<String> names);

    /**
     * <p>
     * Get total number of active labtest of a patient.
     * </p>
     *
     * @param request {@link RequestDTO} - request object with patient details
     * @return Labtest count
     */
    int getLabtestCount(RequestDTO request);

}
