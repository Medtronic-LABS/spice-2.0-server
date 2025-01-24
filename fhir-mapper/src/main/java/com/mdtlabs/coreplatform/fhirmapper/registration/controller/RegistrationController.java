package com.mdtlabs.coreplatform.fhirmapper.registration.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.registration.service.RegistrationService;

/**
 * <p>
 * This class is a controller class to perform operation on Registration. 
 * </p>
 *
 * @author Karthick M created on Aug 05, 2024
 */
@RestController 
@RequestMapping("/patient")
public class RegistrationController {

    private final RegistrationService registrationService;

    @Autowired
    public RegistrationController(RegistrationService registrationService) {
        this.registrationService = registrationService;
    }
    
    /** 
     * Registers patient and member.
     * 
     * @param request
     * @return EnrollmentResponseDTO
     */
    @PostMapping("/register")
    public EnrollmentResponseDTO registerPatientAndMember(@RequestBody EnrollmentRequestDTO request) {
        return registrationService.registerPatientAndMember(request);
    }

    /**
     * <p>
     * Validates patient from national-id.
     * </p>
     *
     * @param request the {@link BioDataDTO} request object contains Patient ID and patient national ID.
     * @return A {@link PatientValidationResponseDTO} if the patient is valid.
     */
    @PostMapping("/validate")
    public PatientValidationResponseDTO validatePatient(@RequestBody BioDataDTO request) {
        return registrationService.validatePatient(request);
    }

    /**
     * <p>
     * updates patient signature obtained from screening and registration..
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains signature url.
     * @return A boolean value is returned .
     */
    @PostMapping("/update-signature")
    public boolean updateMemberSignature(@RequestBody RequestDTO request) {
        return registrationService.updateMemberSignature(request);
    }
}
