package com.mdtlabs.coreplatform.fhirmapper.patienttransfer.controller;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.patienttransfer.service.PatientTransferService;

/**
 * This is the controller class for the patient transfer entity. It maintains the
 * request and response for the PatientTransfer Entity.
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 07, 2024
 */
@RestController
@RequestMapping(value = "/patient-transfer")
public class PatientTransferController {

    private final PatientTransferService patientTransferService;

    @Autowired
    public PatientTransferController(PatientTransferService patientTransferService) {
        this.patientTransferService = patientTransferService;
    }

    /**
     * <p>
     * Validate patient based on patient reference id.
     * </p>
     *
     * @param requestDTO Request data with patient reference
     * @return Map<String, String>
     */
    @PostMapping(value = "/validate")
    public Map<String, String> validatePatientTransfer(@RequestBody RequestDTO requestDTO) {
        return patientTransferService.validatePatientTransfer(requestDTO);
    }

    /**
     * <p>
     * This method is used to update patient organization for given patient reference.
     * </p>
     *
     * @param requestDTO {@link RequestDTO} The requestDTO contains necessary information
     *                   with patient reference and organization data
     */
    @PostMapping(value = "/update/patient-records")
    public void updatePatientRecords(@RequestBody RequestDTO requestDTO) {
        patientTransferService.updatePatientRecords(requestDTO);
    }
}
