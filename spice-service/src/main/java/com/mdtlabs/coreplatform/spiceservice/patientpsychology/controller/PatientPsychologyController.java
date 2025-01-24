package com.mdtlabs.coreplatform.spiceservice.patientpsychology.controller;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patientpsychology.service.PatientPsychologyService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * <p>
 *  Controller for handling Patient Psychology related operations.
 *  This controller provides endpoints for creating, listing, updating, and deleting
 *  Patient Psychology entries.
 * </p>
 *
 * @author Bala Ashwanth N
 * @since Nov 18, 2024
 */
@RestController
@RequestMapping("/medical-review/patient-psychology")
public class PatientPsychologyController {
    
    private final PatientPsychologyService psychologyService;

    public PatientPsychologyController(PatientPsychologyService psychologyService) {
        this.psychologyService = psychologyService;
    }

    /**
     * Used to get the clinical notes
     *
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PostMapping("/list")
    public SuccessResponse<PsychologyDTO> getPsychologyNotes(@RequestBody PsychologyDTO request) {
        List<PsychologyDTO> psychologyDTOList = psychologyService.getPsychologyDataByUserIdAndRelatedPersonId(request);
        return new SuccessResponse<>(SuccessCode.PATIENT_PSYCHOLOGY_RETRIEVED_SUCCESSFULLY, psychologyDTOList, Long.valueOf(psychologyDTOList.size()), HttpStatus.OK) ;
    }
    
    /**
     * Used to create or update the patient status
     *
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PostMapping("/create")
    public SuccessResponse<PsychologyDTO> savePsychologyNotes(@RequestBody PsychologyDTO request) {
        return new SuccessResponse<>(SuccessCode.PATIENT_PSYCHOLOGY_SAVED_SUCCESSFULLY, psychologyService.savePsychologyData(request), HttpStatus.OK) ;
    }

    /**
     * Used to update the clinical notes
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */

    @PutMapping("/update")
    public SuccessResponse<PsychologyDTO> updatePsychologyNotes(@RequestBody PsychologyDTO request) {
        return new SuccessResponse<>(SuccessCode.PATIENT_PSYCHOLOGY_UPDATED_SUCCESSFULLY, psychologyService.updatePsychologyData(request), HttpStatus.OK) ;
    }

    /**
     * Used to remove the clinical notes
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */

    @PostMapping("/remove")
    public SuccessResponse<PsychologyDTO> removePsychologyNotes(@RequestBody PsychologyDTO request) {
        PsychologyDTO psychologyDTO = psychologyService.removePsychologyDataById(request);
        return new SuccessResponse<>(SuccessCode.PATIENT_PSYCHOLOGY_REMOVED_SUCCESSFULLY, psychologyDTO,  HttpStatus.OK) ;
    }
}
