package com.mdtlabs.coreplatform.spiceservice.labtest.controller;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * <p>
 *     Controller class for investigation Details.
 * </p>
 *
 * @author Jaganathan R Jul 30, 2024
 */
@RestController
@RequestMapping("/investigation")
public class InvestigationController {

    private final InvestigationService investigationService;

    public InvestigationController(InvestigationService investigationService) {
        this.investigationService = investigationService;
    }

    /**
     * <p>
     *  This method to create or update the investigation details with and without the investigation results.
     * </p>
     *
     * @param requestDTO investigation details
     * @return response of the investigation ids wrapped in Map
     */
    @PostMapping("/create")
    public SuccessResponse<Map<String, String>> createInvestigation(@RequestBody LabTestRequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_CREATE_STATUS,
                investigationService.createOrUpdateInvestigation(requestDTO), HttpStatus.CREATED);
    }

    /**
     * <p>
     *     To get retrieve the investigation details by using id ana patientReference.
     * </p>
     *
     * @param requestDTO investigation details
     * @return investigation details wrapped in SuccessResponse
     */
    @PostMapping("/details")
    public SuccessResponse<LabTestDTO> getInvestigatedDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_HISTORY_DETAILS,
                investigationService.getInvestigatedDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     *   To get or retrieve the investigation details by using the encounter and patient information.
     * </p>
     *
     * @param requestDTO request details
     * @return list of investigation details
     */
    @PostMapping("/list")
    public SuccessResponse<LabTestDTO> getInvestigationByEncounter(@RequestBody RequestDTO requestDTO) {
        List<LabTestDTO> labTests = investigationService.getInvestigationsByEncounter(requestDTO);
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_LIST, labTests,
                Long.valueOf(labTests.size()), HttpStatus.OK);
    }

    /**
     * <p>
     *  To remove or delete the investigation details by using id.
     * </p>
     *
     * @param requestDTO request details
     * @return id
     */
    @PostMapping("/remove")
    public SuccessResponse<Map<String,String>> removeInvestigatedDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_REMOVE_STATUS,
                investigationService.removeInvestigation(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     *   To get history list of investigated details.
     * </p>
     *
     * @param requestDTO request with patientReference.
     * @return history details wrapped in SuccessResponse.
     */
    @PostMapping("/history-list")
    public SuccessResponse<LabTestHistoryDTO> getHistoryInvestigatedDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_HISTORY_DETAILS,
                investigationService.getHistoryInvestigatedDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     *     This method used to create the lab test result.
     * <p/>
     *
     * @param labTestRequestDTO - Result create DTO with the lab test information.
     */
    @PostMapping("/result/create")
    public SuccessResponse<String> createOrUpdateInvestigationResult(@RequestBody LabTestRequestDTO labTestRequestDTO) {
        investigationService.updateInvestigationResult(labTestRequestDTO);
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_CREATE_STATUS, null, HttpStatus.CREATED);
    }

    /**
     * <p>
     *     Review the lab test result entered by the lab technician.
     * <p/>
     *
     * @param requestDTO - Review comments and lab test id.
     */
    @PostMapping("/review")
    public SuccessResponse<String> reviewInvestigation(@RequestBody RequestDTO requestDTO) {
        investigationService.reviewInvestigation(requestDTO);
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_CREATE_STATUS, null, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Get patient lab test intensification details.
     * </p>
     * @param requestData - Contains patient tracker id
     * @return - Returns patient's lab test history details.
     */
    @PostMapping("/prediction")
    public SuccessResponse<Map<String, List<LabTestDTO>>> getIntensificationDetails(@RequestBody RequestDTO requestData) {
        return new SuccessResponse<>(SuccessCode.INVESTIGATION_LIST,
                investigationService.getIntensificationDetails(requestData), HttpStatus.OK);
    }
}
