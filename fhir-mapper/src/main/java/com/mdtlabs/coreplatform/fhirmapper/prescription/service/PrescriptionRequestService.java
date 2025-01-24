package com.mdtlabs.coreplatform.fhirmapper.prescription.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

import org.hl7.fhir.r4.model.MedicationRequest;

/**
 * Implementation of the MedicationRequestService interface.
 * This class provides methods to handle medical request-related operations.
 *
 * @author Yogeshwaran Mohan created on Apr 17, 2024
 */
public interface PrescriptionRequestService {
    /**
     * returns list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param requestDTO The request review DTO containing the data for adding medication-request reference.
     * @return List<PrescriptionRequestDTO> which is the result of the operation.
     */
    List<PrescriptionDTO> getPrescriptions(RequestDTO requestDTO);

    /**
     * returns list of prescriptionDTO in medicationRequest resource
     * This method constructs an resource based on prescription
     *
     * @param requestDTO The request review DTO containing the data for adding medication-request reference.
     * @return List<PrescriptionRequestDTO> which is the result of the operation.
     */
    List<PrescriptionDTO> getNcdPrescriptions(RequestDTO requestDTO);

    /**
     * Method for creating and  updating Medication request
     * This method handles HTTP POST requests to Create MedicationRequest.
     *
     * @param prescriptionRequestDTO The list of prescription DTO containing Prescription details to be saved.
     */
    Map<String, String> createOrUpdateMedicationRequest(PrescriptionRequestDTO prescriptionRequestDTO);

    /**
     * Method for creating and  updating Medication request
     * This method handles HTTP POST requests to Create MedicationRequest.
     *
     * @param prescriptionRequestDTO The list of prescription DTO containing Prescription details to be saved.
     */
    Map<String, String> createOrUpdateNcdPrescription(PrescriptionRequestDTO prescriptionRequestDTO);

    /**
     * Remove prescription by id
     *
     * @param prescriptionRequestDTO - PrescriptionRequestDTO
     */
    void removePrescription(PrescriptionRequestDTO prescriptionRequestDTO, MedicationRequest.MedicationRequestStatus status);

    /**
     * Get prescription history by prescriptionId.
     *
     * @param request
     * @return List<PrescriptionDTO> list of PrescriptionDTO
     */
    List<PrescriptionDTO> getPrescriptionHistory(RequestDTO request);

    /**
     * Get prescription history by prescriptionId.
     *
     * @param request
     * @return List<PrescriptionDTO> list of PrescriptionDTO
     */
    List<PrescriptionDTO> getNcdPrescriptionHistory(RequestDTO request);

    /**
     * Get prescriptions by encounter
     *
     * @param encounterId
     * @param patientReference
     * @return List of PrescriptionDTO
     */
    List<PrescriptionDTO> getPrescriptionsByEncounter(String encounterId, String patientReference);

    /**
     * Get Patient's prescription data
     *
     * @param request
     * @return PrescriptionHistoryDTO
     */
    PrescriptionHistoryDTO getPrescribedDetails(RequestDTO request);

    /**
     * Get Patient's prescription data
     *
     * @param request
     * @return PrescriptionHistoryDTO
     */
    PrescriptionHistoryDTO getNcdPrescribedDetails(RequestDTO request);

    /**
     * This method used to get all dispense prescription details
     *
     * @param requestDTO get dispense requestDTO entity
     *
     * @return List<PrescriptionDTO> which is the result of the operation.
     */
    List<PrescriptionDTO> listDispensePrescription(RequestDTO requestDTO);

    /**
     * This method used to updates the dispense prescription details
     *
     * @param prescriptionRequestDTO The request DTO containing the data for
     *                               update prescription dispense details
     * @return List<PrescriptionDTO> which is the result of the operation.
     */
    Map<String, String> updateDispensePrescription(PrescriptionRequestDTO prescriptionRequestDTO);

    /**
     * Get prescription dispense history by prescriptionId.
     *
     * @param requestDTO it contains prescription request details
     *
     * @return List<PrescriptionDTO> list of PrescriptionDTO
     */
    List<PrescriptionDTO> getDispenseHistory(RequestDTO requestDTO);

    /**
     * <p>
     * Gets Prescription count of a patient.
     * </p>
     *
     * @param request {@link RequestDTO} - request data with patient ID
     * @return {@link Integer} count of Prescription.
     */
    Integer getPrescriptionCount(RequestDTO request);

    /**
     * Get prescription medication details using given prescriptions.
     * </p>
     *
     * @param prescriptionDTOList it contains list od prescriptions
     *
     * @return Map<Long, MedicationDTO> Map of medication details
     */
    Map<Long, MedicationDTO> getMedicationDetails(List<PrescriptionDTO> prescriptionDTOList);

    /**
     * <p>
     *  Get patient prescriptions using given member id
     * </p>
     *
     * @param memberId member id of the patient
     *
     * @return list of patient prescriptionDTO entity's
     *
     */
    List<PrescriptionDTO> getPrescriptionsByMemberId(String memberId);

    /**
     * <p>
     *  Get patient prescriptions history using given prescription id's
     * </p>
     *
     * @param prescriptionIds list of prescription id's
     *
     * @return Map of patient prescription history details
     *
     */
    Map<String, List<PrescriptionDTO>> getPrescriptionHistoryByPrescriptions(List<String> prescriptionIds);

    /**
     * <p>
     * This method used to set medication details to appropriate prescriptionDTO entity
     * </p>
     *
     * @param prescriptionDTOList The list of prescriptionDTO entity
     */
    void setMedicationDetails(List<PrescriptionDTO> prescriptionDTOList);

}
