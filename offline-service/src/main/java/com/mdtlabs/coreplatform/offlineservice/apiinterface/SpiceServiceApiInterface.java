package com.mdtlabs.coreplatform.offlineservice.apiinterface;

import java.util.List;

import com.mdtlabs.coreplatform.offlineservice.common.dto.PatientDetailsDTO;
import feign.FeignException;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.offlineservice.FeignConfig;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpCriteria;
import com.mdtlabs.coreplatform.offlineservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.offlineservice.common.dto.RequestDTO;


/**
 * This API interface used to interact with FHIR mapper
 *
 * @author Nandhakumar
 */
@FeignClient(name = "spice-service", url = "${app.spice-service}", configuration = FeignConfig.class)
public interface SpiceServiceApiInterface {

    /**
     * Create new Household in FHIR DB
     * @param token  auth token
     * @param client auth client
     * @param householdDTO Request DTO
     * @return Household Object
     * @throws FeignException
     */
    @PostMapping("/household/create")
    public ResponseEntity<HouseholdDTO> createHouseHold(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdDTO householdDTO) throws FeignException;

    /**
     * Create new Household Member in FHIR DB
     * @param token  auth token
     * @param client auth client
     * @param householdMemberDTO Request DTO
     * @return HouseholdMemberDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/create-member")
    public ResponseEntity<HouseholdMemberDTO> createHouseHoldMember(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdMemberDTO householdMemberDTO) throws FeignException;

    /**
     * Update  Household in FHIR DB
     * @param token  auth token
     * @param client auth client
     * @param householdDTO Request DTO
     * @return HouseholdDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/update")
    public ResponseEntity<HouseholdDTO> updateHouseHold(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdDTO householdDTO) throws FeignException;

    /**
     * Update  Household Member in FHIR DB
     * @param token  auth token
     * @param client auth client
     * @param householdMemberDTO Request DTO
     * @return HouseholdMemberDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/update-member")
    public ResponseEntity<HouseholdMemberDTO> updateHouseHoldMember(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdMemberDTO householdMemberDTO) throws FeignException;

    /**
     * Create assessment in FHIR DB
     * @param token  auth token
     * @param client auth client
     * @param assessmentDTO Assessment DTO
     * @return Assessment Object
     * @throws FeignException
     */
    @PostMapping("/assessment/create")
    public ResponseEntity<AssessmentDTO> createAssessment(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody AssessmentDTO assessmentDTO) throws FeignException;

    /**
     * Creates a Patient resource in a FHIR Bundle by using patient id.
     *
     * @param token   auth token
     * @param client  auth client
     * @param request Request DTO
     * @return The ID of the created Patient resource.
     * @throws FeignException
     */
    @PostMapping("/patient/create")
    public ResponseEntity<String> createPatientByPatientId(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request) throws FeignException;

    /**
     * Retrieve household list
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return Household List
     * @throws FeignException
     */
    @PostMapping("/household/list")
    public List<HouseholdDTO> getHouseholdList(@RequestHeader("Authorization") String token,
                                               @RequestHeader("client") String client,
                                               @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Retrieve household member list
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return Household member List
     * @throws FeignException
     */
    @PostMapping("/household/member/list")
    public List<HouseholdMemberDTO> getHouseholdMemberList(@RequestHeader("Authorization") String token,
                                                           @RequestHeader("client") String client,
                                                           @RequestBody RequestDTO requestDTO) throws FeignException;


    /**
     * Update Household member signature
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return HouseholdMemberDTO
     * @throws FeignException
     */
    @PostMapping("/household/member/update-signature")
    public HouseholdMemberDTO updateSignature(@RequestHeader("Authorization") String token,
                                              @RequestHeader("client") String client,
                                              @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Create followup for household members
     *
     * @param token    auth token
     * @param client   auth client
     * @param followUp - FollowUpDTO Entity
     * @return FollowUpDTO
     * @throws FeignException
     */
    @PostMapping("/follow-up/create")
    public FollowUpDTO createFollowUp(@RequestHeader("Authorization") String token,
                                      @RequestHeader("client") String client,
                                      @RequestBody FollowUpDTO followUp) throws FeignException;

    /**
     * Update followup for household members
     *
     * @param token    auth token
     * @param client   auth client
     * @param followUp - FollowUpDTO Entity
     * @return FollowUpDTO
     * @throws FeignException
     */
    @PostMapping("/follow-up/update")
    FollowUpDTO updateFollowUp(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                               @RequestBody FollowUpDTO followUp) throws FeignException;

    /**
     * Retrieve followup list.
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return FollowUpDTO List
     * @throws FeignException
     */
    @PostMapping("/follow-up/list")
    List<FollowUpDTO> getFollowUpList(@RequestHeader("Authorization") String token,
                                      @RequestHeader("client") String client,
                                      @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get followup criteria.
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return FollowUpCriteria
     * @throws FeignException
     */
    @PostMapping("/follow-up/criteria")
    public FollowUpCriteria getFollowUpCriteria(@RequestHeader("Authorization") String token,
                                                @RequestHeader("client") String client,
                                                @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get followup criteria.
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return FollowUpCriteria
     * @throws FeignException
     */
    @PostMapping("/follow-up/offline/screening-followups")
    List<FollowUpDTO> getScreeningFollowUps(@RequestHeader("Authorization") String token,
                                            @RequestHeader("client") String client,
                                            @RequestBody PatientRequestDTO requestDTO) throws FeignException;

    /**
     * Get followup criteria.
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return FollowUpCriteria
     * @throws FeignException
     */
    @PostMapping("/follow-up/offline/assessment-followups")
    List<FollowUpDTO> getAssessmentFollowUps(@RequestHeader("Authorization") String token,
                                             @RequestHeader("client") String client,
                                             @RequestBody PatientRequestDTO requestDTO) throws FeignException;

    /**
     * Get followup criteria.
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return FollowUpCriteria
     * @throws FeignException
     */
    @PostMapping("/follow-up/offline/lost-to-followups")
    List<FollowUpDTO> getLostToFollowUps(@RequestHeader("Authorization") String token,
                                         @RequestHeader("client") String client,
                                         @RequestBody PatientRequestDTO requestDTO) throws FeignException;

    /**
     * Get followup criteria.
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return FollowUpCriteria
     * @throws FeignException
     */
    @PostMapping("/follow-up/offline/medical-review-followups")
    List<FollowUpDTO> getMedicalReviewFollowUps(@RequestHeader("Authorization") String token,
                                                @RequestHeader("client") String client,
                                                @RequestBody PatientRequestDTO requestDTO) throws FeignException;

    /**
     * Get patient pregnancy information by villages
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return PregnancyInfo List
     * @throws FeignException
     */
    @PostMapping("/patient/pregnancy/info")
    List<PregnancyInfo> getPregnancyInfoByVillages(@RequestHeader("Authorization") String token,
                                                   @RequestHeader("client") String client,
                                                   @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get MemberList to link with household
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return PregnancyInfo List
     * @throws FeignException
     */
    @PostMapping("/household-member-link/list")
    List<HouseholdMemberLinkDTO> getHouseholdMemberLinkList(@RequestHeader("Authorization") String token,
                                                            @RequestHeader("client") String client,
                                                            @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Create Household member Notification
     *
     * @param token                  auth token
     * @param client                 auth client
     * @param householdMemberLinkDTO Request DTO
     * @throws FeignException
     */
    @PostMapping("/household-member-link/update")
    public void updateHouseholdMemberLink(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdMemberLinkDTO householdMemberLinkDTO) throws FeignException;

    /**
     * Update member with household details
     *
     * @param token            auth token
     * @param client           auth client
     * @param householdMembers Request DTO
     * @throws FeignException
     */
    @PostMapping("/household-member-link/update-member")
    public void updateMemberLink(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody List<HouseholdMemberDTO> householdMembers) throws FeignException;


    /**
     * Get patient details by village ids
     *
     * @param request - RequestDTO Entity
     * @return List of PatientDetailsDTO entity contains patient details
     */
    @PostMapping("/patient/offline/list")
    List<PatientDetailsDTO> listPatientDetails(@RequestHeader("Authorization") String token,
                                               @RequestHeader("client") String client,
                                               @RequestBody RequestDTO request);

}