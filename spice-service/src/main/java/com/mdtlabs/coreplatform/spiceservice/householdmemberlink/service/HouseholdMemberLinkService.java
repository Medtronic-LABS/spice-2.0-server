package com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import org.springframework.http.ResponseEntity;

import java.util.List;

/**
 * Interface for householdLink service operations.
 * <p>
 * This interface defines the contract for services handling operations related to households members.
 * </p>
 *
 * @author Denisha
 * Created on Oct 03, 2024.
 */
public interface HouseholdMemberLinkService {

    /**
     * Creates a Member Link notification to map with household
     * <p>
     * This method is responsible for creating a  Member Link notification {@link HouseholdMemberDTO} details.
     * </p>
     *
     * @param householdMemberDTO The household member details as a {@link HouseholdMemberDTO} object.
     */
    void createHouseholdMemberLink(HouseholdMemberDTO householdMemberDTO);

    /**
     * Fetch List of household member details.
     * <p>
     * This method is responsible for fetching List of household member details. {@link RequestDTO} details.
     * It returns the list of HouseholdMemberLinkDTO.
     * </p>
     *
     * @param requestDTO  {@link RequestDTO} object.
     * @return The created list{@link HouseholdMemberLinkDTO} object.
     */
    List<HouseholdMemberLinkDTO> getHouseholdMemberLinkList(RequestDTO requestDTO);

    /**
     * Create Household member link notification
     * <p>
     * This method is responsible for Creating member link notification. {@link HouseholdMemberLinkDTO} details.
     * </p>
     *
     * @param householdMemberLinkDTO {@link HouseholdMemberLinkDTO} object.
     */
    void updateHouseholdMemberLink(HouseholdMemberLinkDTO householdMemberLinkDTO);

    /**
     * Update member with household details
     * <p>
     * This method is responsible for updating member link notification. {@link HouseholdMemberDTO} details.
     * </p>
     *
     * @param householdMemberDto {@link HouseholdMemberDTO} object.
     */
    void updateMemberLink(List<HouseholdMemberDTO> householdMemberDto);
}
