package com.mdtlabs.coreplatform.spiceservice.prescription.service;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;
import java.util.Map;

import io.minio.errors.MinioException;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * Interface defining the contract for prescription-related services.
 * <p>
 * This interface outlines methods for managing prescriptions, including creating medication requests,
 * removing prescriptions, retrieving prescriptions, and accessing prescription history. It serves as a
 * bridge between the controller layer and the data access layer, facilitating operations on prescription data.
 * </p>
 *
 * @author Yogeshwaran Mohan created on Apr 17, 2024
 */
public interface PrescriptionService {

    /**
     * Creates a medication request for a list of prescriptions.
     * <p>
     * This method takes a {@link PrescriptionRequestDTO} containing prescription details and uses it to create
     * a medication request resource. It returns a map containing the operation's result, typically including
     * identifiers or status information.
     * </p>
     *
     * @param prescriptionRequestDTO The prescription request DTO containing the data for adding medication-request reference.
     * @return A map containing the response data indicating the result of the operation.
     * @throws IOException If an input/output exception occurs during the operation.
     */
    Map<String, String> createMedicationRequest(PrescriptionRequestDTO prescriptionRequestDTO) throws IOException, MinioException, GeneralSecurityException;

    /**
     * Removes a prescription by its identifier.
     * <p>
     * This method accepts a {@link PrescriptionRequestDTO} containing the identifier of the prescription to be removed.
     * It performs the removal operation without returning a result.
     * </p>
     *
     * @param prescriptionRequestDTO The DTO containing the ID of the prescription to be removed.
     */
    void removePrescription(PrescriptionRequestDTO prescriptionRequestDTO);

    /**
     * Retrieves a list of prescriptions based on a given criteria.
     * <p>
     * This method takes a {@link RequestDTO} containing criteria for filtering prescriptions and returns a list
     * of {@link PrescriptionDTO} objects that match these criteria. It is used to fetch prescriptions based on
     * specific requirements.
     * </p>
     *
     * @param requestDTO The DTO containing the criteria for filtering prescriptions.
     * @return A list of {@link PrescriptionDTO} objects matching the criteria.
     */
    List<PrescriptionDTO> getPrescriptions(RequestDTO requestDTO);

    /**
     * Retrieves the prescription history for a given prescription ID.
     * <p>
     * This method accepts a {@link RequestDTO} containing the prescription ID and returns a list of
     * {@link PrescriptionDTO} objects representing the prescription's history. It is useful for tracking
     * changes or reviewing the prescription history of a patient.
     * </p>
     *
     * @param request The DTO containing the prescription ID for which history is requested.
     * @return A list of {@link PrescriptionDTO} objects representing the prescription's history.
     */
    List<PrescriptionDTO> getPrescriptionHistory(RequestDTO request);

    /**
     * Retrieves detailed prescription information for a patient.
     * <p>
     * This method takes a {@link RequestDTO} and returns a {@link PrescriptionHistoryDTO} containing detailed
     * information about a patient's prescriptions. It is used to access comprehensive prescription details,
     * including medication names, dosages, and administration instructions.
     * </p>
     *
     * @param request The DTO containing criteria for retrieving prescription details.
     * @return A {@link PrescriptionHistoryDTO} containing detailed prescription information.
     */
    PrescriptionHistoryDTO getPrescribedDetails(RequestDTO request);

    /**
     * Retrieves the fill prescription details for a given request.
     * <p>
     * This method accepts a {@link RequestDTO} containing the he patient ID and returns a list of
     * {@link PrescriptionDTO} objects representing the fill prescription's.
     * </p>
     *
     * @param request The DTO containing the patient ID for who fill prescription is requested.
     * @return A list of {@link PrescriptionDTO} objects representing the fill prescription's.
     */
    List<PrescriptionDTO> getFillPrescriptions(RequestDTO request);

    /**
     * Updates a medication request dispense details based on given details.
     * <p>
     * This method takes a {@link PrescriptionRequestDTO} containing prescription dispense
     * details to update medication request dispense details
     * </p>
     *
     * @param prescriptionRequestDTO The prescription request DTO containing the data for
     *                               update medication-request dispense details.
     * @return A map containing the response data indicating the result of the operation.
     */
    Map<String, String> updateFillPrescription(PrescriptionRequestDTO prescriptionRequestDTO);

    /**
     * Retrieves the fill prescription history for a given patient visit id.
     * <p>
     * This method accepts a {@link RequestDTO} containing the patient visit id and returns a list of
     * {@link PrescriptionDTO} objects representing the fill prescription's history. It is useful for tracking
     * changes or reviewing the fill prescription history of a patient.
     * </p>
     *
     * @param request The DTO containing the patient visit id for which history is requested.
     * @return A list of {@link PrescriptionDTO} objects representing the fill prescription's history.
     */
    List<PrescriptionDTO> getRefillPrescriptionHistory(RequestDTO request);

    /**
     * Retrieves patient prescription prediction details
     * <p>
     * This method accepts a {@link RequestDTO} containing the patient visit id and returns a list of
     * {@link PrescriptionPredictionDTO} objects representing the prescription prediction details.
     * It is useful for tracking changes or reviewing the prescription prediction of a patient.
     * </p>
     *
     * @param request The DTO containing the patient member id for which prescription prediction is requested.
     * @return A list of {@link PrescriptionPredictionDTO} objects representing the prescription prediction details.
     */
    PrescriptionPredictionDTO getPrescriptionPrediction(RequestDTO request);
}
