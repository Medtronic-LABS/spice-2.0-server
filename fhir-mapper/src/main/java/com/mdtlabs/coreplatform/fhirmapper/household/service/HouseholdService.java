package com.mdtlabs.coreplatform.fhirmapper.household.service;

import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.RelatedPerson;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 * This class is a service class to perform operation on Household
 * operations.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Feb 05, 2024
 */
public interface HouseholdService {

    /**
     * The function creates a household using a list of household DTOs and returns a ResponseEntity with a
     * string.
     *
     * @param householdDTO A list of HouseholdDTO objects.
     * @return The method is returning a ResponseEntity object with a generic type of String.
     */
    HouseholdDTO createHousehold(HouseholdDTO householdDTO);

    /**
     * The function updates a household using a list of household DTOs and returns a ResponseEntity with a
     * string.
     *
     * @param householdDTO A list of HouseholdDTO objects.
     * @return The method is returning a ResponseEntity object with a generic type of String.
     */
    HouseholdDTO updateHousehold(HouseholdDTO householdDTO);

    /**
     * To get household details
     *
     * @param householdDTO A HouseholdDTO objects.
     * @return The method is returning a HouseholdDTO object.
     */
    HouseholdDTO getHousehold(String householdDTO);

    /**
     * To get household member details
     *
     * @param patientId a household member PatientId.
     * @return The method is returning a HouseholdMemberDto object.
     */
    HouseholdMemberDTO getHouseholdMemberByPatientId(String patientId);

    /**
     * To get household member details
     *
     * @param id a household member PatientId.
     * @return The method is returning a HouseholdMemberDto object.
     */
    HouseholdMemberDTO getHouseholdMemberById(String id);

    /**
     * To create household member
     *
     * @param householdMemberDTO A HouseholdDTO Member.
     * @return The method is returning a HouseholdMemberDTO object.
     */
    HouseholdMemberDTO createHouseholdMember(HouseholdMemberDTO householdMemberDTO);

    /**
     * To create household member
     *
     * @param householdMemberDTO A HouseholdDTO Member.
     * @return The method is returning a HouseholdMemberDTO object.
     */
    HouseholdMemberDTO updateHouseholdMember(HouseholdMemberDTO householdMemberDTO);

    /**
     * Retrieve household list.
     *
     * @param request - RequestDTO Entity
     * @return HouseholdDTO List
     */
    List<HouseholdDTO> getHouseholdList(RequestDTO request);

    /**
     * Retrieve household member list.
     *
     * @param request - RequestDTO Entity
     * @return HouseholdMemberDTO List
     */
    List<HouseholdMemberDTO> getHouseholdMemberList(RequestDTO request);

    /**
     * get Household Details ny PatientId
     *
     * @param patientId patient Id
     * @param includes  List of include resources
     * @return Household Object
     */
    HouseholdDTO getHouseholdByMemberPatientId(String patientId, List<String> includes);

    /**
     * Get Household member by villages that has patient vitals
     *
     * @param villages        - List of villages
     * @param lastSyncTime    - Last Sync time
     * @param currentSyncTime - Current Sync time
     * @param projections     - List of elements to project
     * @param skip            - skip
     * @param limit           - limit
     * @return - List of member Ids
     */
    List<String> getHouseholdMemberByVillagesWithPatientVitals(List<String> villages, Date lastSyncTime,
                                                               Date currentSyncTime, List<String> projections, int skip, int limit);

    /**
     * Retrieves a RelatedPerson resource from the FHIR server using the patient's ID.
     * It uses the FHIR client to search for the RelatedPerson resource where the identifier matches the provided patient ID.
     *
     * @param patientId The ID of the patient for whom the RelatedPerson resource is to be fetched.
     * @return The RelatedPerson resource fetched from the FHIR server.
     */
    RelatedPerson getRelatedPersonByPatientId(String patientId);

    /**
     * Get Household by Id
     *
     * @param id - Household Id
     * @return - HouseholdDTO
     */
    HouseholdDTO getHouseholdById(String id);

    /**
     * Get RelatedPerson count by Household id
     *
     * @param householdId - Household id
     * @return - RelatedPerson count
     */
    int getRelatedPersonCountByHouseholdId(String householdId);

    /**
     * Update Household member signature
     *
     * @param request - RequestDTO Entity
     * @return HouseholdMemberDTO
     */
    HouseholdMemberDTO updateSignature(RequestDTO request);
}
