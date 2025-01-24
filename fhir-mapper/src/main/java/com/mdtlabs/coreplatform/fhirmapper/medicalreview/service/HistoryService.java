package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewHistoryDTO;

/**
 * <p>
 * Service class provides a contract for implementing classes to provide functionality related to medical review
 * history. It declares a method for retrieving the medical review history for a given patient or encounter.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 09, 2024
 */
public interface HistoryService {

    /**
     * Retrieves the medical review history for a given patient or encounter.
     * Implementing classes should provide the logic to retrieve the medical review history based on the provided request.
     *
     * @param requestDTO The request data transfer object containing the patientId and/or encounterId.
     * @return A MedicalReviewHistoryDTO object containing the medical review history of the patient or encounter.
     */
    MedicalReviewHistoryDTO getHistory(MedicalReviewRequestDTO requestDTO);

    /**
     * Retrieves the PNC medical review history for a given patient or encounter.
     * Implementing classes should provide the logic to retrieve the PNC medical review history based on the provided request.
     *
     * @param requestDTO The request data transfer object containing the patientId and/or encounterId.
     * @return A MedicalReviewHistoryDTO object containing the PNC medical review history of the patient or encounter.
     */
    MedicalReviewHistoryDTO getPncHistory(MedicalReviewRequestDTO requestDTO);

    /**
     * The function retrieves NCD medical review history based on the provided
     * `MedicalReviewRequestDTO`.
     *
     * @param requestDTO  {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     * history, and any other relevant data needed to retrieve the NCD history
     * @return {@link NCDMedicalReviewHistoryDTO} The history based on given request is retrieved.
     */
    NCDMedicalReviewHistoryDTO getNCDMedicalReviewHistory(MedicalReviewRequestDTO requestDTO);

    /**
     * The function  retrieves the NCD medical review summary history
     * based on the provided `MedicalReviewRequestDTO`.
     *
     * @param requestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     * history, and any other relevant data needed to retrieve the NCD history
     * @return {@link NCDMedicalReviewHistoryDTO} The history based on given request is retrieved.
     */
    NCDMedicalReviewHistoryDTO getNCDMedicalReviewSummaryHistory(MedicalReviewRequestDTO requestDTO);
}
