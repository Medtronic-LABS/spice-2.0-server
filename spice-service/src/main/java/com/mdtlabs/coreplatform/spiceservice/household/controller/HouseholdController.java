package com.mdtlabs.coreplatform.spiceservice.household.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.household.service.HouseholdService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * Controller for managing households and their members.
 * <p>
 * Provides REST endpoints for creating, updating, and retrieving households and household members.
 * It interacts with the {@link HouseholdService} to perform these operations.
 * </p>
 *
 * @author Nandhakumar Karthikeyan
 * Created on Jan 04, 2024.
 */
@RestController
@RequestMapping(value = "/household")
@Validated
public class HouseholdController {

    private final HouseholdService householdService;

    public HouseholdController(HouseholdService householdService) {
        this.householdService = householdService;
    }

    /**
     * Creates a new household.
     * <p>
     * Accepts a {@link HouseholdDTO} object as input and returns the created household.
     * </p>
     *
     * @param householdDTO The household details.
     * @return ResponseEntity containing the created {@link HouseholdDTO}.
     */
    @PostMapping("/create")
    public ResponseEntity<HouseholdDTO> createHousehold(@RequestBody HouseholdDTO householdDTO) {
        return ResponseEntity.ok().body(householdService.createHousehold(householdDTO));
    }

    /**
     * Retrieves a household by its ID.
     * <p>
     * Fetches a household from the database based on the provided ID.
     * </p>
     *
     * @param householdId The ID of the household to retrieve.
     * @return SuccessResponse containing the retrieved {@link HouseholdDTO}.
     */
    @GetMapping("/{householdId}")
    public SuccessResponse<HouseholdDTO> getHouseHold(@PathVariable(value = "householdId") String householdId) {
        return new SuccessResponse<>(SuccessCode.GOT_HOUSEHOLD, householdService.getHousehold(householdId), HttpStatus.OK);
    }

    /**
     * Creates a new household member.
     * <p>
     * Accepts a {@link HouseholdMemberDTO} object as input and returns the created household member.
     * </p>
     *
     * @param householdMember The household member details.
     * @return ResponseEntity containing the created {@link HouseholdMemberDTO}.
     */
    @PostMapping("/create-member")
    public ResponseEntity<HouseholdMemberDTO> createHouseholdMember(@RequestBody HouseholdMemberDTO householdMember) {
        return ResponseEntity.ok().body(householdService.createHouseholdMember(householdMember));
    }

    /**
     * Updates an existing household.
     * <p>
     * Accepts a {@link HouseholdDTO} object as input and returns the updated household.
     * </p>
     *
     * @param householdDTO The updated household details.
     * @return ResponseEntity containing the updated {@link HouseholdDTO}.
     */
    @PostMapping("/update")
    public ResponseEntity<HouseholdDTO> updateHousehold(@RequestBody HouseholdDTO householdDTO) {
        return ResponseEntity.ok().body(householdService.updateHousehold(householdDTO));
    }

    /**
     * Updates an existing household member.
     * <p>
     * This endpoint accepts a {@link HouseholdMemberDTO} object as input and updates the corresponding household member's details in the database.
     * It returns a {@link ResponseEntity} containing the updated {@link HouseholdMemberDTO}.
     * </p>
     *
     * @param householdMember The household member details to be updated.
     * @return ResponseEntity containing the updated {@link HouseholdMemberDTO}.
     */
    @PostMapping("/update-member")
    public ResponseEntity<HouseholdMemberDTO> updateHouseholdMember(@RequestBody HouseholdMemberDTO householdMember) {
        return ResponseEntity.ok().body(householdService.updateHouseholdMember(householdMember));
    }

    /**
     * Retrieves a list of all households.
     * <p>
     * This endpoint accepts a {@link RequestDTO} object containing criteria for filtering the households and returns a list of {@link HouseholdDTO} objects that match the criteria.
     * </p>
     *
     * @param request The criteria for retrieving households.
     * @return A list of {@link HouseholdDTO} objects.
     */
    @PostMapping("/list")
    public List<HouseholdDTO> getHouseholdList(@RequestBody RequestDTO request) {
        return householdService.getHouseholdList(request);
    }

    /**
     * Retrieves a list of household members.
     * <p>
     * This endpoint accepts a {@link RequestDTO} object containing criteria for filtering household members and returns a list of {@link HouseholdMemberDTO} objects that match the criteria.
     * </p>
     *
     * @param request The criteria for retrieving household members.
     * @return A list of {@link HouseholdMemberDTO} objects.
     */
    @PostMapping("/member/list")
    public List<HouseholdMemberDTO> getHouseholdMemberList(@RequestBody RequestDTO request) {
        return householdService.getHouseholdMemberList(request);
    }

    /**
     * Update Household member signature
     *
     * @param request - RequestDTO Entity
     * @return HouseholdMemberDTO
     */
    @PostMapping("/member/update-signature")
    public HouseholdMemberDTO updateSignature(@RequestBody RequestDTO request) {
        return householdService.updateSignature(request);
    }
}
