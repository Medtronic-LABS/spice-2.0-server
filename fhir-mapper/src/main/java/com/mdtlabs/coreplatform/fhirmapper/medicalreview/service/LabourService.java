package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service;

import java.util.Map;

import org.springframework.web.bind.annotation.RequestBody;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * Service interface for handling labor and delivery related operations.
 *
 * @author Nanthinee sugumar created on April 25, 2024
 */
public interface LabourService {

    /**
     * Creates a medical review for a mother's labour and a neonate based on the provided data.
     *
     * @param requestDTO The request data containing information about the mother's labour and the neonate.
     * @return The created medical review for the mother's labour and the neonate.
     */
    Map<String, String> createLabourMotherAndNeonateMedicalReview(@RequestBody MotherNeonateDTO requestDTO);

    /**
     * Retrieves the details of a mother's labour and a neonate based on their IDs.
     *
     * @param requestDTO the request dto contains the mother's id neonate id.
     * @return The details of the mother's labour and the neonate associated with the provided IDs.
     */
    MotherNeonateDTO getLabourMotherAndNeonateDetails(RequestDTO requestDTO);


    /**
     * Get Birth-History Details
     *
     * @param memberId member id
     * @return BirthHistoryDTO Details
     */
    BirthHistoryDTO getBirthHistory(String memberId);
}
