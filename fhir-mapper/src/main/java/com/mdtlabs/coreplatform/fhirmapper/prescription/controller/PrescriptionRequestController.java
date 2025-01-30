package com.mdtlabs.coreplatform.fhirmapper.prescription.controller;

import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.MedicationRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.IntensificationAlgorithm;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * Controller for managing prescription requests.
 * <p>
 * This controller handles operations related to managing prescriptions, including
 * listing, updating, removing prescriptions, and retrieving prescription details and history.
 * </p>
 *
 * @author Yogeshwaran Mohan created on Apr 17, 2024
 */
@RestController
@RequestMapping(value = "/prescription-request")
@Validated
public class PrescriptionRequestController {

    private final PrescriptionRequestService prescriptionRequestService;

    private final IntensificationAlgorithm intensificationAlgorithm;

    @Autowired
    public PrescriptionRequestController(PrescriptionRequestService prescriptionRequestService,
                                         IntensificationAlgorithm intensificationAlgorithm) {
        this.prescriptionRequestService = prescriptionRequestService;
        this.intensificationAlgorithm = intensificationAlgorithm;
    }

    /**
     * Retrieves a list of prescriptions based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing search criteria and returns
     * a list of {@link PrescriptionDTO} that match the criteria.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the search criteria.
     * @return A list of {@link PrescriptionDTO} matching the criteria.
     */
    @PostMapping("/list")
    public List<PrescriptionDTO> getPrescriptions(@RequestBody RequestDTO requestDTO) {
        return prescriptionRequestService.getPrescriptions(requestDTO);
    }

    /**
     * Retrieves a list of prescriptions based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing search criteria and returns
     * a list of {@link PrescriptionDTO} that match the criteria.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the search criteria.
     * @return A list of {@link PrescriptionDTO} matching the criteria.
     */
    @PostMapping("/ncd-list")
    public List<PrescriptionDTO> getNcdPrescriptions(@RequestBody RequestDTO requestDTO) {
        return prescriptionRequestService.getNcdPrescriptions(requestDTO);
    }

    /**
     * Creates or updates a prescription request.
     * <p>
     * This endpoint accepts a {@link PrescriptionRequestDTO} containing prescription details
     * and either creates a new prescription or updates an existing one based on the provided details.
     * </p>
     *
     * @param prescriptionRequestDTO The {@link PrescriptionRequestDTO} containing prescription details.
     * @return A {@link Map} indicating the result of the operation.
     */
    @PostMapping("/update")
    public Map<String, String> createOrUpdatePrescriptionRequest(@RequestBody PrescriptionRequestDTO prescriptionRequestDTO) {
        return prescriptionRequestService.createOrUpdateMedicationRequest(prescriptionRequestDTO);
    }

    /**
     * Creates or updates a prescription request.
     * <p>
     * This endpoint accepts a {@link PrescriptionRequestDTO} containing prescription details
     * and either creates a new prescription or updates an existing one based on the provided details.
     * </p>
     *
     * @param prescriptionRequestDTO The {@link PrescriptionRequestDTO} containing prescription details.
     * @return A {@link Map} indicating the result of the operation.
     */
    @PostMapping("/ncd-update")
    public Map<String, String> createOrUpdatePrescription(@RequestBody PrescriptionRequestDTO prescriptionRequestDTO) {
        return prescriptionRequestService.createOrUpdateNcdPrescription(prescriptionRequestDTO);
    }

    /**
     * Removes a prescription based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link PrescriptionRequestDTO} containing the ID of the prescription
     * to be removed and performs the removal operation.
     * </p>
     *
     * @param prescriptionRequestDTO The {@link PrescriptionRequestDTO} containing the ID of the prescription to be removed.
     */
    @ConfigureAppType
    @PostMapping("/remove")
    public void removePrescription(@RequestBody PrescriptionRequestDTO prescriptionRequestDTO) {
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            prescriptionRequestService.removePrescription(prescriptionRequestDTO,
                    MedicationRequest.MedicationRequestStatus.CANCELLED);
        } else {
            prescriptionRequestService.removePrescription(prescriptionRequestDTO,
                    MedicationRequest.MedicationRequestStatus.COMPLETED);
        }
    }

    /**
     * Retrieves prescribed details for a patient.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the ID of the patient and returns
     * a {@link PrescriptionHistoryDTO} containing the prescribed details for the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID.
     * @return A {@link PrescriptionHistoryDTO} containing the prescribed details for the patient.
     */
    @ConfigureAppType
    @PostMapping("/prescribed-details")
    public PrescriptionHistoryDTO getPrescribedDetails(@RequestBody RequestDTO requestDTO) {
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            return prescriptionRequestService.getNcdPrescribedDetails(requestDTO);
        } else {
            return prescriptionRequestService.getPrescribedDetails(requestDTO);
        }
    }

    /**
     * Retrieves the prescription history for a patient based on the prescription ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the prescription ID and returns
     * a list of {@link PrescriptionDTO} representing the prescription history for the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the prescription ID.
     * @return A list of {@link PrescriptionDTO} representing the prescription history.
     */
    @ConfigureAppType
    @PostMapping("/history-list")
    public List<PrescriptionDTO> getPrescriptionHistory(@RequestBody RequestDTO requestDTO) {
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            return prescriptionRequestService.getNcdPrescriptionHistory(requestDTO);
        } else {
            return prescriptionRequestService.getPrescriptionHistory(requestDTO);
        }
    }

    /**
     * Retrieves a list of fill-prescriptions based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing search criteria and returns
     * a list of {@link PrescriptionDTO} that match the criteria.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the search criteria.
     * @return A list of {@link PrescriptionDTO} matching the criteria.
     */
    @PostMapping("/dispense-prescription/list")
    public List<PrescriptionDTO> listDispensePrescription(@RequestBody RequestDTO requestDTO) {
        return prescriptionRequestService.listDispensePrescription(requestDTO);
    }

    /**
     * Update the fill prescription details
     * <p>
     * This endpoint accepts a {@link PrescriptionRequestDTO} containing prescription details
     * and update the fill prescription based on the provided details.
     * </p>
     *
     * @param prescriptionRequestDTO The {@link PrescriptionRequestDTO} containing prescription details.
     * @return A {@link Map} indicating the result of the operation.
     */
    @PostMapping("/dispense-prescription/update")
    public Map<String, String> updateDispensePrescription(@RequestBody PrescriptionRequestDTO prescriptionRequestDTO) {
        return prescriptionRequestService.updateDispensePrescription(prescriptionRequestDTO);
    }

    /**
     * Get dispense history details based on given details
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing prescription details
     * to get history of dispense prescription details.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing prescription details.
     * @return A list of {@link PrescriptionDTO} representing the dispense history.
     */
    @PostMapping("/dispense-prescription/history")
    public List<PrescriptionDTO> getDispenseHistory(@RequestBody RequestDTO requestDTO) {
        return prescriptionRequestService.getDispenseHistory(requestDTO);
    }

    /**
     * Get Patient prescription prediction details based on given details
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing prescription details
     * to get prescription prediction details.
     * </p>
     *
     * @param request The {@link RequestDTO} containing .
     * @return A list of {@link PrescriptionDTO} representing the prescription prediction details.
     */
    @PostMapping(path = "/prediction")
    public PrescriptionPredictionDTO getPrescriptionPrediction(@RequestBody RequestDTO request) {
        return intensificationAlgorithm.getMedicationSuggestion(request.getMemberId());
    }
}
