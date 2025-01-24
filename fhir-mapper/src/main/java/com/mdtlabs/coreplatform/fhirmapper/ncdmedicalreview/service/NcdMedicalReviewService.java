package com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 * This interface is a service to perform operation on NCD medical review
 * operations.
 * </p>
 *
 * @author Karthick M created on Sep 01, 2024
 */
public interface NcdMedicalReviewService {

    /**
     * Creates NCD medical review.
     * 
     * @param request
     * @return
     */
    Map<String, String> createNcdMedicalReview(NCDMedicalReviewDTO request);

    /**
     * Get medical review details.
     * 
     * @param request
     * @return
     */
    NcdMedicalReviewResponse ncdMedicalReviewSummary(MedicalReviewRequestDTO request);

    /**
     * <p>
     * Creates a summary for the given NCD medical review.
     * </p>
     *
     * @param ncdMedicalReviewDTO The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     */
    void addOrUpdateNextVisitDate(NCDMedicalReviewDTO ncdMedicalReviewDTO);

    /**
     * The function retrieves a list of encounter IDs based on a given encounter ID.
     *
     * @param encounterId It is used to retrieve encounters that are part of the specified encounter.
     * @return The encounter IDs extracted from a Bundle of Encounters retrieved using a REST API call.
     */
    List<String> getEncounterIdsByVisit(String encounterId);

    /**
     * Gets patient lifestyle details
     * 
     * @param requestDTO
     * @return List<LifestyleResponseDTO>
     */
    List<LifestyleResponseDTO> getPatientLifestyle(RequestDTO requestDTO);

}
