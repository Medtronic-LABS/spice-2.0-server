package com.mdtlabs.coreplatform.fhirmapper.patientpsychology.controller;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.fhirmapper.patientpsychology.service.PatientPsychologyService;
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
     * Used to create psychology notes
     * <p/>
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PostMapping("/create")
    public PsychologyDTO savePsychologyNotes(@RequestBody PsychologyDTO request) {
        return psychologyService.savePatientPsychology(request);
    }
    
    /**
     * Used to update the clinical notes
     *
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PutMapping("/update")
    PsychologyDTO updatePsychology(@RequestBody PsychologyDTO request) {
        return psychologyService.savePatientPsychology(request);
    }

    /**
     * Used to retrieve clinical notes
     * <p/>
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PostMapping("/list")
    List<PsychologyDTO> getPatientPsychology(@RequestBody PsychologyDTO request) {
        return psychologyService.getPatientPsychologyByRelatedPersonId(request);
    }

    /*
     * Used to remove the clinical notes
     *
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PostMapping("/remove")
    public PsychologyDTO removePsychologyNotes(@RequestBody PsychologyDTO request) {
        return psychologyService.removePsychologyDataById(request);
    }
}
