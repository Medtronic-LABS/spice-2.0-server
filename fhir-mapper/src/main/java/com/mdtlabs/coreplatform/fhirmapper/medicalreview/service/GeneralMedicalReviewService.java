package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service;

import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * Service interface for managing general medical reviews.
 * <p>
 * Defines operations for creating and retrieving general medical reviews and summaries,
 * including specialized summaries for mother and neonate. This interface abstracts the
 * underlying data access and business logic for general medical review management.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Mar 20, 2024
 */
public interface GeneralMedicalReviewService {

    /**
     * Creates a new general medical review.
     * <p>
     * This method accepts a {@link GeneralMedicalReviewDTO} object containing the details
     * of the medical review to be created. It returns a map containing the ID of the newly
     * created medical review.
     * </p>
     *
     * @param generalMedicalReviewDTO the DTO containing the details of the general medical review to be created.
     * @return a map containing the ID of the created general medical review.
     */
    Map<String, String> createGeneralMedicalReview(GeneralMedicalReviewDTO generalMedicalReviewDTO);

    /**
     * Retrieves the details of a general medical review.
     * <p>
     * Given an encounter ID and a patient reference, this method fetches the detailed summary
     * of the specified general medical review. It returns a {@link GeneralMedicalReviewSummaryDetailsDTO}
     * object containing the summary details.
     * </p>
     *
     * @param id               the encounter ID of the general medical review.
     * @param patientReference the patient reference associated with the medical review.
     * @return a {@link GeneralMedicalReviewSummaryDetailsDTO} containing the summary details of the medical review.
     */
    GeneralMedicalReviewSummaryDetailsDTO getGeneralMedicalReviewDetails(String id, String patientReference);

    /**
     * Creates a summary for a general medical review.
     * <p>
     * This method accepts a {@link GeneralMedicalReviewSummaryDTO} object containing the summary
     * details to be saved. It returns a {@link GeneralMedicalReviewSummaryDTO} object containing
     * the details of the created summary.
     * </p>
     *
     * @param summaryDTO the DTO containing the summary details to be saved.
     * @return a {@link GeneralMedicalReviewSummaryDTO} containing the details of the created summary.
     */
    GeneralMedicalReviewSummaryDTO saveSummaryDetails(GeneralMedicalReviewSummaryDTO summaryDTO);

    /**
     * <p>
     * Used to create or update the patient status
     * <p/>
     *
     * @param patientStatusDto - The patient status details.
     */
    PatientStatusDTO createPatientStatus(PatientStatusDTO patientStatusDto);

    /**
     * <p>
     * Used to get the patient status details based on the patient id.
     * <p/>
     *
     * @param patientStatusDto - Patient id to get the patient status.
     * @return {@link PatientStatusDTO} - Patient status details
     */
    PatientStatusDTO getPatientStatusDetails(PatientStatusDTO patientStatusDto);

    /**
     * <p>
     * This function creates and updates confirm diagnosis information in a FHIR server.
     * </p>
     *
     * @param confirmDiagnosisDTO {@link ConfirmDiagnosisDTO} It contains information related to confirming a diagnosis for a patient.
     */
    void updateConfirmDiagnosis(ConfirmDiagnosisDTO confirmDiagnosisDTO);

    /**
     * <p>
     * Gets medical review count.
     * </p>
     *
     * @param request {@link RequestDTO} - request dto
     * @return {@link Map} contains medical review related counts.
     */
    Map<String, Integer> getMedicalReviewCount(RequestDTO request);

    /**
     * <p>
     * Used to Update Patient Nutrition Lifestyle and Patient Psychology by its Patient Track id and Patient Visit id.
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains Patient reference and Menu name.
     * @return A {@link Boolean} object corresponds to the View update
     */
    Boolean updateViewCount(RequestDTO request);
}
