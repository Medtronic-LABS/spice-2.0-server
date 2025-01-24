package com.mdtlabs.coreplatform.fhirmapper.household.controller;

import java.util.List;

import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;

/**
 * Controller for managing households and their members within the FHIR mapping system.
 * <p>
 * This controller provides endpoints for creating, updating, retrieving, and listing households and their members.
 * It interacts with the {@link HouseholdService} to perform these operations, ensuring that requests are validated
 * and properly formatted before passing them to the service layer.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Feb 05, 2024
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
     * Accepts a {@link HouseholdDTO} object as input and returns the created household object.
     * The household data is passed to the {@link HouseholdService} for processing and persistence.
     * </p>
     *
     * @param householdDTO The household data transfer object containing household information.
     * @return The created {@link HouseholdDTO} object.
     */
    @PostMapping("/create")
    public HouseholdDTO createHousehold(@RequestBody HouseholdDTO householdDTO) {
        return householdService.createHousehold(householdDTO);
    }

    /**
     * Retrieves a household by its ID.
     * <p>
     * Given a household ID, this method retrieves the corresponding household object.
     * It utilizes the {@link HouseholdService} to fetch the household data from the database.
     * </p>
     *
     * @param householdId The ID of the household to retrieve.
     * @return The {@link HouseholdDTO} object corresponding to the given ID.
     */
    @GetMapping("/{householdId}")
    public HouseholdDTO getHouseHold(@PathVariable(value = "householdId") String householdId) {
        return householdService.getHousehold(householdId);
    }

    /**
     * Creates a new household member.
     * <p>
     * Accepts a {@link HouseholdMemberDTO} object as input and returns the created household member object.
     * The household member data is passed to the {@link HouseholdService} for processing and persistence.
     * </p>
     *
     * @param householdMember The household member data transfer object.
     * @return The created {@link HouseholdMemberDTO} object.
     */
    @PostMapping("/create-member")
    public HouseholdMemberDTO createHouseholdMember(@RequestBody HouseholdMemberDTO householdMember) {
        return householdService.createHouseholdMember(householdMember);
    }

    /**
     * Updates household details.
     * <p>
     * Accepts a {@link HouseholdDTO} object as input and returns the updated household object.
     * The household data is passed to the {@link HouseholdService} for processing and updating in the database.
     * </p>
     *
     * @param householdDTO The household data transfer object containing updated household information.
     * @return The updated {@link HouseholdDTO} object.
     */
    @PostMapping("/update")
    public HouseholdDTO updateHousehold(@RequestBody HouseholdDTO householdDTO) {
        return householdService.updateHousehold(householdDTO);
    }

    /**
     * Updates a household member.
     * <p>
     * Accepts a {@link HouseholdMemberDTO} object as input and returns the updated household member object.
     * The household member data is passed to the {@link HouseholdService} for processing and updating in the database.
     * </p>
     *
     * @param householdMember The household member data transfer object containing updated member information.
     * @return The updated {@link HouseholdMemberDTO} object.
     */
    @PostMapping("/update-member")
    public HouseholdMemberDTO updateHouseholdMember(@RequestBody HouseholdMemberDTO householdMember) {
        return householdService.updateHouseholdMember(householdMember);
    }

    /**
     * Retrieves a list of households based on the given criteria.
     * <p>
     * Accepts a {@link RequestDTO} object as input and returns a list of {@link HouseholdDTO} objects that match the criteria.
     * The criteria for selection is processed by the {@link HouseholdService}.
     * </p>
     *
     * @param request The request data transfer object containing criteria for household selection.
     * @return A list of {@link HouseholdDTO} objects.
     */
    @PostMapping("/list")
    public List<HouseholdDTO> getHouseholdList(@RequestBody RequestDTO request) {
        return householdService.getHouseholdList(request);
    }

    /**
     * Retrieves a list of household members based on the given criteria.
     * <p>
     * Accepts a {@link RequestDTO} object as input and returns a list of {@link HouseholdMemberDTO} objects that match the criteria.
     * The criteria for selection is processed by the {@link HouseholdService}.
     * </p>
     *
     * @param request The request data transfer object containing criteria for household member selection.
     * @return A list of {@link HouseholdMemberDTO} objects.
     */
    @PostMapping("/member/list")
    public List<HouseholdMemberDTO> getHouseholdMemberList(@RequestBody RequestDTO request) {
        return householdService.getHouseholdMemberList(request);
    }

    /**
     * Retrieves a household member by their ID.
     * <p>
     * Given a household member ID, this method retrieves the corresponding household member object.
     * It utilizes the {@link HouseholdService} to fetch the household member data from the database.
     * </p>
     *
     * @param request The ID of the household member to retrieve.
     * @return The {@link HouseholdMemberDTO} object corresponding to the given ID.
     */
    @PostMapping("/member/id")
    public HouseholdMemberDTO getHouseholdMemberById(@RequestBody RequestDTO request) {
        return householdService.getHouseholdMemberById(request.getMemberId());
    }

    /**
     * Retrieves a household member by patient ID.
     * <p>
     * Given a household member patient ID, this method retrieves the corresponding household member object.
     * It utilizes the {@link HouseholdService} to fetch the household member data from the database.
     * </p>
     *
     * @param request The patient ID of the household member to retrieve.
     * @return The {@link HouseholdMemberDTO} object corresponding to the given ID.
     */
    @PostMapping("/member/patient-id")
    public HouseholdMemberDTO getHouseholdMemberByPatientId(@RequestBody RequestDTO request) {
        return householdService.getHouseholdMemberByPatientId(request.getPatientId());
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
