package com.mdtlabs.coreplatform.spiceservice.householdmemberlink.controller;

import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.HouseholdMemberLinkService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Controller for link households and their members.
 * <p>
 * Provides REST endpoints for creating, updating, and retrieving household members.
 * It interacts with the {@link HouseholdMemberLinkService} to perform these operations.
 * </p>
 *
 * @author Denisha
 * Created on Oct 03, 2024.
 */
@RestController
@RequestMapping(value = "/household-member-link")
public class HouseholdMemberLinkController {

    private final HouseholdMemberLinkService householdMemberlinkService;

    @Autowired
    public HouseholdMemberLinkController(HouseholdMemberLinkService householdMemberlinkService) {
        this.householdMemberlinkService = householdMemberlinkService;
    }


    /**
     * Creates a Member Link notification to map with household
     * <p>
     * This method is responsible for creating a  Member Link notification {@link HouseholdMemberDTO} details.
     * </p>
     *
     * @param householdMemberDTO The household member details as a {@link HouseholdMemberDTO} object.
     */
    @PostMapping("/create")
    public void createHouseholdMemberLink(@RequestBody HouseholdMemberDTO householdMemberDTO) {
        householdMemberlinkService.createHouseholdMemberLink(householdMemberDTO);
    }

    /**
     * Fetch List of unassigned member
     * <p>
     * Accepts a {@link RequestDTO} object as input and returns the list of members
     * </p>
     *
     * @param request RequestDto
     * @return {@link HouseholdMemberLinkDTO}.
     */
    @PostMapping("/list")
    public List<HouseholdMemberLinkDTO> getHouseholdMemberLinkList(@RequestBody RequestDTO request) {
        return householdMemberlinkService.getHouseholdMemberLinkList(request);
    }

    /**
     * Create Household member link Notification
     * <p>
     * Accepts a {@link HouseholdMemberLinkDTO} object as input
     * </p>
     *
     * @param householdMemberLinkDTO HouseholdMemberLinkDTO
     */
    @PostMapping("/update")
    public void updateHouseholdMemberLink(@RequestBody HouseholdMemberLinkDTO householdMemberLinkDTO) {
        householdMemberlinkService.updateHouseholdMemberLink(householdMemberLinkDTO);
    }

    /**
     * Update member with household details
     * <p>
     * Accepts a {@link HouseholdMemberDTO} object as input
     * </p>
     *
     * @param householdMemberDto HouseholdMemberDTO
     */
    @PostMapping("/update-member")
    public void updateMemberLink(@RequestBody List<HouseholdMemberDTO> householdMemberDto) {
        householdMemberlinkService.updateMemberLink(householdMemberDto);
    }

}
