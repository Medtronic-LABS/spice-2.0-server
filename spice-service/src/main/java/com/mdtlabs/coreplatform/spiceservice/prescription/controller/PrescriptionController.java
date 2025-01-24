package com.mdtlabs.coreplatform.spiceservice.prescription.controller;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import io.minio.errors.MinioException;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.spiceservice.prescription.service.PrescriptionService;

/**
 * Controller for handling prescription-related requests.
 * <p>
 * This controller provides endpoints for managing prescriptions, including listing, creating, removing,
 * and retrieving prescription details and history. It leverages the {@link PrescriptionService} for
 * the business logic associated with each operation.
 * </p>
 */
@RestController
@RequestMapping(value = "/prescription-request")
@Validated
public class PrescriptionController {

    private final PrescriptionService prescriptionRequestService;

    public PrescriptionController(PrescriptionService prescriptionRequestService) {
        this.prescriptionRequestService = prescriptionRequestService;
    }

    /**
     * Lists all prescriptions based on the provided criteria.
     * <p>
     * This endpoint accepts a POST request with a {@link RequestDTO} body containing the criteria for filtering
     * prescriptions. It returns a list of {@link PrescriptionDTO} objects that match the criteria.
     * </p>
     *
     * @param requestDTO The criteria for filtering prescriptions.
     * @return A {@link SuccessResponse} containing the list of matching {@link PrescriptionDTO} objects and the HTTP status.
     */
    @PostMapping("/list")
    public SuccessResponse<List<PrescriptionDTO>> getPrescriptions(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.PRESCRIPTION_LIST, prescriptionRequestService.getPrescriptions(requestDTO), HttpStatus.OK);
    }

    /**
     * Creates a new prescription with an optional signature.
     * <p>
     * This endpoint accepts a POST request with prescription details and an optional signature file. It processes
     * the request to create a new prescription entry and returns a map containing the status of the creation operation.
     * </p>
     *
     * @param prescriptionRequest The prescription details as a JSON string.
     * @param signatureFile       The optional signature file.
     * @return A {@link SuccessResponse} containing the creation status and the HTTP status.
     */
    @PostMapping("/create")
    public SuccessResponse<Map<String, String>> createPrescriptionRequest(@RequestParam(Constants.PRESCRIPTION_REQUEST) String prescriptionRequest,
                                                                          @RequestParam(Constants.SIGNATURE) MultipartFile signatureFile) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            PrescriptionRequestDTO prescriptionRequestDto = mapper.readValue(prescriptionRequest, PrescriptionRequestDTO.class);
            prescriptionRequestDto.setSignatureFile(signatureFile);
            return new SuccessResponse<>(SuccessCode.PRESCRIPTION_CREATE_STATUS, prescriptionRequestService.createMedicationRequest(prescriptionRequestDto), HttpStatus.OK);
        } catch (IOException | MinioException | GeneralSecurityException iOException) {
            Logger.logError(iOException);
            throw new SpiceValidation(1514, iOException.getMessage());
        }
    }

    /**
     * Removes a prescription by its ID.
     * <p>
     * This endpoint accepts a POST request with a {@link PrescriptionRequestDTO} body containing the prescription ID.
     * It invokes the service layer to remove the specified prescription from the system. A success response is returned
     * indicating the operation's outcome.
     * </p>
     *
     * @param prescriptionRequestDTO The prescription request DTO containing the ID of the prescription to be removed.
     * @return A {@link SuccessResponse} indicating the status of the removal operation.
     */
    @PostMapping("/remove")
    public SuccessResponse<Object> removePrescription(@RequestBody PrescriptionRequestDTO prescriptionRequestDTO) {
        prescriptionRequestService.removePrescription(prescriptionRequestDTO);
        return new SuccessResponse<>(SuccessCode.PRESCRIPTION_REMOVE_STATUS, HttpStatus.OK);
    }

    /**
     * Retrieves prescribed details for a patient.
     * <p>
     * This endpoint accepts a POST request with a {@link RequestDTO} body containing criteria to filter prescriptions.
     * It returns detailed prescription information for a patient, including medication names, dosages, and administration
     * instructions. This can be particularly useful for reviewing a patient's medication regimen.
     * </p>
     *
     * @param requestDTO The criteria for filtering prescribed details.
     * @return A {@link SuccessResponse} containing detailed prescription information.
     */
    @PostMapping("/prescribed-details")
    public SuccessResponse<PrescriptionHistoryDTO> getPrescribedDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.PRESCRIPTION_HISTORY_DETAILS, prescriptionRequestService.getPrescribedDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * Retrieves a list of prescription history for a patient.
     * <p>
     * This endpoint accepts a POST request with a {@link RequestDTO} body containing criteria to filter prescription history.
     * It returns a list of prescriptions that match the criteria, providing a historical view of a patient's prescriptions.
     * This can aid in tracking medication changes over time.
     * </p>
     *
     * @param requestDTO The criteria for filtering prescription history.
     * @return A {@link SuccessResponse} containing a list of prescription history records.
     */
    @PostMapping("/history-list")
    public SuccessResponse<PrescriptionDTO> getPrescriptionHistoryList(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.PRESCRIPTION_HISTORY_LIST, prescriptionRequestService.getPrescriptionHistory(requestDTO), HttpStatus.OK);
    }


    /**
     * <p>
     * To list the fill-prescription details.
     * </p>
     *
     * @param requestDTO - request dto
     * @return List of fill-prescription data's
     */
    @PostMapping("/fill-prescription/list")
    public SuccessResponse<PrescriptionDTO> listFillPrescription(
            @RequestBody RequestDTO requestDTO) {
        List<PrescriptionDTO> prescriptionList = prescriptionRequestService.getFillPrescriptions(requestDTO);
        if (!prescriptionList.isEmpty()) {
            return new SuccessResponse<>(SuccessCode.FILL_PRESCRIPTION_GET, prescriptionList, (long) prescriptionList.size(),
                    HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.FILL_PRESCRIPTION_GET, new ArrayList<>(), (long) Constants.ZERO, HttpStatus.OK);
    }

    /**
     * <p>
     * To update the fill-prescription data along with its associated tables like
     * fill-prescription history, prescription and prescription history.
     * </p>
     *
     * @param fillPrescriptionRequestDto
     * @return Success Message
     */
    @PostMapping("/fill-prescription/update")
    public SuccessResponse<Map<String, String >> updateFillPrescription(
            @RequestBody PrescriptionRequestDTO fillPrescriptionRequestDto) {
            return new SuccessResponse<>(SuccessCode.FILL_PRESCRIPTION_UPDATE,
                    prescriptionRequestService.updateFillPrescription(fillPrescriptionRequestDto), HttpStatus.OK);
    }

    /**
     * <p>
     * To list the last filled prescription history for the patient.
     * </p>
     *
     * @param requestDTO - search request dto
     * @return List of latest fill prescription history
     */
    @PostMapping(path = "/refill-prescription/history")
    public SuccessResponse<PrescriptionDTO> getReFillPrescriptionHistory(
            @RequestBody RequestDTO requestDTO) {
        List<PrescriptionDTO> prescriptionList = prescriptionRequestService.getRefillPrescriptionHistory(requestDTO);
        if (!prescriptionList.isEmpty()) {
            return new SuccessResponse<>(SuccessCode.REFILL_PRESCRIPTION_GET, prescriptionList,
                    (long) prescriptionList.size(), HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.REFILL_PRESCRIPTION_GET, new ArrayList<>(),
                (long) Constants.ZERO, HttpStatus.OK);
    }

    /**
     * <p>
     *  Get patient prescription prediction details using given request
     * </p>
     *
     * @param request - prediction request dto
     * @return PrescriptionPredictionDTO entity
     */
    @PostMapping("/prediction")
    public SuccessResponse<PrescriptionPredictionDTO> getPrescriptionPrediction(@RequestBody RequestDTO request) {
        return new SuccessResponse<>(SuccessCode.PRESCRIPTION_PREDICTION,
                prescriptionRequestService.getPrescriptionPrediction(request), HttpStatus.OK);
    }
}
