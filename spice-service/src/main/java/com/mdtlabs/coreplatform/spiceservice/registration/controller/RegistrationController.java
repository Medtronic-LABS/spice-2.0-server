package com.mdtlabs.coreplatform.spiceservice.registration.controller;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientValidationResponseDTO;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.registration.service.RegistrationService;

/**
 * <p>
 * This class is a controller class to perform operation on Registration. 
 * </p>
 *
 * @author Karthick M created on Aug 05, 2024
 */
@RestController
@RequestMapping(value = "/patient")
public class RegistrationController {

    private final RegistrationService registrationService;

    @Autowired 
    public RegistrationController(RegistrationService registrationService) {
        this.registrationService = registrationService;
    }

    /** 
     * Registers patient and member.
     * 
     * @param registrationRequest The request of the patient to be saved is given.
     * @param signatureFile The file to be saved is given
     * @return EnrollmentResponseDTO is returned
     */
    @PostMapping("/register")
    public SuccessResponse<EnrollmentResponseDTO> registerPatientAndMember(@RequestParam(Constants.REGISTRATION_REQUEST) String registrationRequest,
                                                                           @RequestParam(required = false, name = Constants.SIGNATURE_FILE) MultipartFile signatureFile) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            EnrollmentRequestDTO patient = objectMapper.readValue(registrationRequest,
                    EnrollmentRequestDTO.class);
            Logger.logInfo("In Patient controller, creating a new patient");
        return new SuccessResponse<>(SuccessCode.REGISTER_PATIENT, registrationService.registerPatientAndMember(patient, signatureFile), HttpStatus.CREATED);
        } catch (JsonProcessingException processingException) {
            Logger.logError(processingException);
            throw new SpiceValidation(1510, processingException.getMessage());
        }
    }

    /**
     * <p>
     * Validates patient using national-id.
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains Patient ID and patient national ID.
     * @return A {@link PatientValidationResponseDTO} if the patient is valid.
     */
    @PostMapping("/validate")
    public SuccessResponse<PatientValidationResponseDTO> validatePatient(@RequestBody BioDataDTO request) {
        PatientValidationResponseDTO response = registrationService.isPatientRegisteredInRelatedPerson(request);
        if(response.getPatientDetails() != null) {
            return new SuccessResponse<>(SuccessCode.PATIENT_EXIST, response, HttpStatus.CONFLICT);
        }
        return new SuccessResponse<>(SuccessCode.PATIENT_VALIDATED_SUCCESSFULLY, response, HttpStatus.OK);
    }
}
