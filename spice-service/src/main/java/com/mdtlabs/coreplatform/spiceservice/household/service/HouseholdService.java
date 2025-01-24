package com.mdtlabs.coreplatform.spiceservice.household.service;


import java.util.List;

import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * Interface for household service operations.
 * <p>
 * This interface defines the contract for services handling operations related to households and their members.
 * It includes methods for creating and updating households and household members, retrieving household details,
 * and fetching lists and sequences of households and household members based on various criteria.
 * </p>
 *
 * @author Nandhakumar Karthikeyan
 * Created on Jan 04, 2024.
 */
public interface HouseholdService {

    /**
     * Creates a new household.
     * <p>
     * This method is responsible for creating a new household based on the provided {@link HouseholdDTO} details.
     * It returns the created household.
     * </p>
     *
     * @param householdDTO The household details as a {@link HouseholdDTO} object.
     * @return The created {@link HouseholdDTO} object.
     */
    HouseholdDTO createHousehold(HouseholdDTO householdDTO);

    /**
     * Updates an existing household.
     * <p>
     * This method updates an existing household with the details provided in the {@link HouseholdDTO} object.
     * It returns the updated household.
     * </p>
     *
     * @param householdDTO The household details to be updated as a {@link HouseholdDTO} object.
     * @return The updated {@link HouseholdDTO} object.
     */
    HouseholdDTO updateHousehold(HouseholdDTO householdDTO);

    /**
     * Retrieves household details by ID.
     * <p>
     * This method fetches the details of a household based on the provided household ID.
     * It returns the {@link HouseholdDTO} object containing the household details.
     * </p>
     *
     * @param householdDTO The ID of the household to retrieve.
     * @return The {@link HouseholdDTO} object containing the household details.
     */
    HouseholdDTO getHousehold(String householdDTO);

    /**
     * Creates a new household member.
     * <p>
     * This method is responsible for creating a new household member based on the provided {@link HouseholdMemberDTO} details.
     * It returns the created household member.
     * </p>
     *
     * @param householdMemberDTO The household member details as a {@link HouseholdMemberDTO} object.
     * @return The created {@link HouseholdMemberDTO} object.
     */
    HouseholdMemberDTO createHouseholdMember(HouseholdMemberDTO householdMemberDTO);

    /**
     * Updates an existing household member.
     * <p>
     * This method updates an existing household member with the details provided in the {@link HouseholdMemberDTO} object.
     * It returns the updated household member.
     * </p>
     *
     * @param householdMemberDTO The household member details to be updated as a {@link HouseholdMemberDTO} object.
     * @return The updated {@link HouseholdMemberDTO} object.
     */
    HouseholdMemberDTO updateHouseholdMember(HouseholdMemberDTO householdMemberDTO);

    /**
     * Retrieves a list of households based on the provided criteria.
     * <p>
     * This method fetches a list of households that match the criteria specified in the {@link RequestDTO} object.
     * It returns a list of {@link HouseholdDTO} objects that match the criteria.
     * </p>
     *
     * @param request The criteria for retrieving households as a {@link RequestDTO} object.
     * @return A list of {@link HouseholdDTO} objects.
     */
    List<HouseholdDTO> getHouseholdList(RequestDTO request);

    /**
     * Retrieves a list of household members based on the provided criteria.
     * <p>This method fetches a list of household members that match the criteria specified in the {@link RequestDTO} object.
     * It returns a list of {@link HouseholdMemberDTO} objects that match the criteria.</p>
     *
     * @param request The criteria for retrieving household members as a {@link RequestDTO} object.
     * @return A list of {@link HouseholdMemberDTO} objects.
     */
    List<HouseholdMemberDTO> getHouseholdMemberList(RequestDTO request);

    /**
     * Update Household member signature
     *
     * @param request - RequestDTO Entity
     * @return HouseholdMemberDTO
     */
    HouseholdMemberDTO updateSignature(RequestDTO request);

}
