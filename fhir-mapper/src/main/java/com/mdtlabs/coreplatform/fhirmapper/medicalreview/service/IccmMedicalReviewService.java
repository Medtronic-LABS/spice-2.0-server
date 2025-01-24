package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service;

import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.UnderFiveIccmDTO;

/**
 * <p>
 * This interface is a service to perform operation on ICCM medical review
 * operations.
 * </p>
 *
 * @author Karthick M created on Mar 15, 2024
 */
public interface IccmMedicalReviewService {

    /**
     * Creates iccm under two months assessment data.
     *
     * @param request
     * @return Map<String, String>
     */
    Map<String, String> createMedicalReviewForUnder5years(UnderFiveIccmDTO request);

    /**
     * Summarize the List observations performed in medical review.
     *
     * @param request
     * @return IccmResponseDTO
     */
    IccmResponseDTO getMedicalReviewDetailsForUnderFive(MedicalReviewRequestDTO request);

}
