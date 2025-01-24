package com.mdtlabs.coreplatform.fhirmapper.labtest.controller;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.IntensificationAlgorithm;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;

/**
 * <p>
 *     Controller class for InvestigationDetails.
 * </p>
 *
 * @author Jaganathan R Jul 30, 2024
 */
@RestController
@RequestMapping("/investigation")
public class InvestigationController {

    private final InvestigationService investigationService;

    private final IntensificationAlgorithm intensificationAlgorithm;

    public InvestigationController(InvestigationService labTestService, IntensificationAlgorithm intensificationAlgorithm) {
        this.investigationService = labTestService;
        this.intensificationAlgorithm = intensificationAlgorithm;
    }

    /**
     * <p>
     *  This method to create or update the investigation details with and without the investigation results.
     * </p>
     *
     * @param labTestRequestDTO investigation details
     * @return response of the investigation ids wrapped in Map
     */
    @ConfigureAppType
    @PostMapping("/create")
    public Map<String, String> createOrUpdateInvestigation(@RequestBody LabTestRequestDTO labTestRequestDTO) {
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            return investigationService.createOrUpdateNcdInvestigation(labTestRequestDTO);
        }
        return investigationService.createOrUpdateInvestigation(labTestRequestDTO);
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
    public List<LabTestDTO> getListOfInvestigatedDetails(@RequestBody RequestDTO requestDTO) {
        return investigationService.getListOfInvestigatedDetails(requestDTO);
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
    public Map<String, String> removeInvestigatedDetails(@RequestBody RequestDTO requestDTO) {
        return investigationService.removeInvestigation(requestDTO.getId(), requestDTO.getProvenance());
    }

    /**
     * <p>
     *   To get history list of investigated details.
     * </p>
     *
     * @param requestDTO request with patient reference
     * @return list of investigated details
     */
    @ConfigureAppType
    @PostMapping("/history-list")
    public LabTestHistoryDTO getInvestigatedDetails(@RequestBody RequestDTO requestDTO) {
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            return investigationService.getNcdInvestigatedDetails(requestDTO);
        } else {
            return investigationService.getInvestigatedDetails(requestDTO);
        }
    }

    /**
     * <p>
     *     This method used to create the lab test result.
     * <p/>
     *
     * @param labTestRequestDTO - Result create DTO with the lab test information.
     */
    @PostMapping("/result/create")
    public void createOrUpdateInvestigationResult(@RequestBody LabTestRequestDTO labTestRequestDTO) {
        investigationService.updateInvestigationResult(labTestRequestDTO);
    }

    /**
     * <p>
     *     Review the lab test result entered by the lab technician.
     * <p/>
     *
     * @param requestDTO - Review comments and lab test id.
     */
    @PostMapping("/review")
    public void reviewInvestigation(@RequestBody RequestDTO requestDTO) {
        investigationService.reviewInvestigation(requestDTO);
    }

    /**
     * <p>
     * Get patient lab test intensification details.
     * </p>
     * @param requestData - Contains patient tracker id
     * @return - Returns patient's lab test history details.
     */
    @PostMapping("/prediction")
    public Map<String, List<LabTestDTO>> getIntensificationDetails(@RequestBody RequestDTO requestData) {
        return intensificationAlgorithm.getIntensificationDetails(requestData);
    }
}
