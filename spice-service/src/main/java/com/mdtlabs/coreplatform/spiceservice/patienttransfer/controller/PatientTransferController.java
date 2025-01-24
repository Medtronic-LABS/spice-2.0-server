package com.mdtlabs.coreplatform.spiceservice.patienttransfer.controller;

import java.util.Map;

import jakarta.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferUpdateRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.service.PatientTransferService;
/**
 * This is the controller class for the patient transfer entity. It maintains the
 * request and response for the PatientTransfer Entity.
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 07, 2024
 */
@RestController
@RequestMapping(value = "/patient-transfer")
public class PatientTransferController {

    private PatientTransferService patientTransferService;

    @Autowired
    public PatientTransferController(PatientTransferService patientTransferService) {
        this.patientTransferService = patientTransferService;
    }

    /**
     * <p>
     * Creates patient transfer for a given patient
     * </p>
     *
     * @param patientTransferDto Request data with patient transfer Object to create
     * @return Success response
     */
    @PostMapping(value = "/create")
    public SuccessResponse<PatientTransfer> createPatientTransfer(
            @Valid @RequestBody PatientTransferRequestDTO patientTransferDto) {
        patientTransferService.createPatientTransfer(patientTransferDto);
        return new SuccessResponse<>(SuccessCode.PATIENT_TRANSFER_SAVE, HttpStatus.CREATED);
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
    @TenantValidation
    public SuccessResponse<Map<String, String>> validatePatientTransfer(
            @RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.PATIENT_TRANSFER_VALIDATE,
                patientTransferService.validatePatientTransfer(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to retrieve a list of districts based on a search request.
     * </p>
     *
     * @param patientTransferDto {@link PatientTransferUpdateRequestDTO} The RequestDTO  contains necessary information
     *                         to update the patient transfer
     * @return {@link PatientTransfer} The response PatientTransfer containing updated
     * patient tranfer object
     */
    @PostMapping(value = "/update")
    public SuccessResponse<PatientTransfer> updatePatientTransfer(
            @Valid @RequestBody PatientTransferUpdateRequestDTO patientTransferDto) {
        patientTransferService.updatePatientTransfer(patientTransferDto);
        return new SuccessResponse<>(SuccessCode.PATIENT_TRANSFER_UPDATE, HttpStatus.OK, patientTransferDto.getTransferStatus().toString().toLowerCase());
    }

    /**
     * <p>
     * Get transferred patient count for notification
     * </p>
     *
     * @param requestDTO {@link RequestDTO} The RequestDTO  contains necessary information
     *                         to get the count of the transferred patients
     * @return {@link PatientTransfer} The response PatientTransfer containing updated
     * patient tranfer object
     */
    @PostMapping(value = "/notification-count")
    @TenantValidation
    public SuccessResponse<PatientTransfer> getPatientTransferCount(
            @RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.PATIENT_TRANSFER_COUNT,
                patientTransferService.getPatientTransferCount(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     * Get transferred patients list
     * </p>
     *
     * @param requestDTO {@link RequestDTO} The RequestDTO  contains necessary information
     *                         to get the count of the transferred patients
     * @return {@link Map<String, Object>} The response containing the list
     * of transferred patients
     */
    @PostMapping(value = "/list")
    @TenantValidation
    public SuccessResponse<Map<String, Object>> getPatientTransferList(
            @RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.PATIENT_TRANSFER_LIST,
                patientTransferService.getPatientTransferList(requestDTO), HttpStatus.OK);
    }
}