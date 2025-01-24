package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service;

import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;

/**
 * <p>
 * Service interface for managing medical reviews related to pregnancy PNC.
 * This interface defines methods to handle medical review operations specific to pregnancy ANC.
 * </p>
 *
 * @author Nandhakumar sugumar created on Mar 27, 2024
 */
public interface MedicalReviewPregnancyPNCService {

    /**
     * Save Pnc Medical Review Details
     *
     * @param pncMedicalReviewDTO Pnc Medical Review Details
     * @return Dto
     */
    Map<String, String> savePncMedicalReview(PncMedicalReviewDTO pncMedicalReviewDTO);

    /**
     * Get Medical Review Details
     *
     * @param motherId MotherId
     * @param childId  ChildId
     * @return PncMedicalReviewDTO
     */
    PncMedicalReviewDTO getPNCMedicalReviewDetails(String motherId, String childId, String patientReference);

    /**
     * Save Pnc Child Details
     *
     * @param householdMemberDTO Pnc Medical Review child Details
     */
    void createPncChild(HouseholdMemberDTO householdMemberDTO);

    /**
     * Create child in PNC direct flow
     *
     * @param householdMemberDTO household member Details
     */
    HouseholdMemberDTO createChild(HouseholdMemberDTO householdMemberDTO);
}
