package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service;

import java.util.Map;

import org.springframework.web.bind.annotation.RequestBody;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 * Service interface for managing medical reviews related to pregnancy ANC (Antenatal Care).
 * This interface defines methods to handle medical review operations specific to pregnancy ANC.
 * </p>
 *
 * @author Nanthinee sugumar created on Mar 27, 2024
 */
public interface MedicalReviewPregnancyANCService {

    /**
     * Creates a new value observation in the FHIR database.
     * This method constructs an observation resource with the provided observation DTO
     * and saves it to the FHIR server using a bundle transaction.
     *
     * @param observationDTO The observation DTO containing details of the observation to be created.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    ObservationDTO createValueObservation(@RequestBody ObservationDTO observationDTO);

    /**
     * Sets medical observations based on medical review data.
     * This method constructs observation resources for various medical review data
     * and saves them to the FHIR server using a bundle transaction.
     *
     * @param medicalReviewDTO The medical review DTO containing the data for setting observations.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    Map<String, String> createMedicalReview(@RequestBody MedicalReviewPregnancyDTO medicalReviewDTO);


    /**
     * Retrieves pregnancy summary details for a medical review based on the given identifier.
     * <p>
     * This method fetches and returns pregnancy summary details related to a specific medical review.
     * It takes an identifier as input to locate the desired medical review.
     *
     * @param id The unique identifier of the medical review.
     * @return MedicalReviewPregnancySummaryDetailsDTO containing pregnancy summary details.
     */
    MedicalReviewPregnancySummaryDetailsDTO getPregnancyMedicalReviewDetails(String id, String patientReference);

    /**
     * get Patient Latest Weight
     *
     * @param requestDTO Request Details
     * @return weight
     */
    Map<String, Double> getPatientWeight(RequestDTO requestDTO);

    /**
     * get Patient Latest Bp values
     *
     * @param requestDTO Request Details
     * @return Bp values
     */
    Map<String, Double> getPatientBp(RequestDTO requestDTO);

    /**
     * Gets Last encounter details.
     *
     * @param request
     * @return MedicalReviewPregnancySummaryDetailsDTO
     */
    MedicalReviewPregnancySummaryDetailsDTO getLatestEncounter(RequestDTO request);

}
