package com.mdtlabs.coreplatform.spiceservice.followup.controller;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDto;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpCriteria;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

/**
 * Controller for handling follow-up operations.
 * <p>
 * This controller provides endpoints for creating, updating, and retrieving follow-up records
 * for household members. It interacts with the {@link FollowUpService} to perform these operations.
 * </p>
 *
 * @author Maria Antony
 * Created on April 29, 2024.
 */
@RestController
@RequestMapping(value = "/follow-up")
@Validated
public class FollowUpController {

    private final FollowUpService followUpService;

    public FollowUpController(FollowUpService followUpService) {
        this.followUpService = followUpService;
    }

    /**
     * Creates a follow-up record for a household member.
     * <p>
     * This endpoint accepts a {@link FollowUpDTO} object as input, sets the first interaction mode to FOLLOW_UP,
     * and calls the follow-up service to create a new follow-up record.
     * </p>
     *
     * @param followUp The follow-up details as a {@link FollowUpDTO} object.
     * @return The created {@link FollowUpDTO} object.
     */
    @PostMapping("/create")
    public FollowUpDTO createFollowUp(@RequestBody FollowUpDTO followUp) {
        followUp.setFirstInteractionMode(InteractionMode.FOLLOW_UP);
        return followUpService.createFollowUp(followUp);
    }

    /**
     * Updates an existing follow-up record for a household member.
     * <p>
     * This endpoint accepts a {@link FollowUpDTO} object as input, sets the last interaction mode to FOLLOW_UP,
     * and calls the follow-up service to update the follow-up record.
     * </p>
     *
     * @param followUp The follow-up details to be updated as a {@link FollowUpDTO} object.
     * @return The updated {@link FollowUpDTO} object.
     */
    @PostMapping("/update")
    public FollowUpDTO updateFollowUp(@RequestBody FollowUpDTO followUp) {
        followUp.setLastInteractionMode(InteractionMode.FOLLOW_UP);
        return followUpService.updateFollowUp(followUp);
    }

    /**
     * Updates an existing follow-up record for a household member.
     * <p>
     * This endpoint accepts a {@link FollowUpDTO} object as input, sets the last interaction mode to FOLLOW_UP,
     * and calls the follow-up service to update the follow-up record.
     * </p>
     *
     * @param followUp The follow-up details to be updated as a {@link FollowUpDTO} object.
     * @return The updated {@link FollowUpDTO} object.
     */
    @PostMapping("/ncd/update")
    public SuccessResponse<FollowUpDTO> updateNcdFollowUp(@RequestBody FollowUpDTO followUp) {
        return new SuccessResponse<>(SuccessCode.CALL_REGISTER_UPDATE_MSG, followUpService.updateNcdFollowUp(followUp),
                HttpStatus.OK);
    }

    /**
     * Retrieves a list of follow-up records based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link RequestDTO} object containing the criteria for retrieving follow-up records,
     * and returns a list of {@link FollowUpDTO} objects that match the criteria.
     * </p>
     *
     * @param request The criteria for retrieving follow-up records as a {@link RequestDTO} object.
     * @return A list of {@link FollowUpDTO} objects.
     */
    @PostMapping("/list")
    public List<FollowUpDTO> getFollowUpList(@RequestBody RequestDTO request) {
        return followUpService.getFollowUpList(request);
    }

    /**
     * Retrieves the follow-up criteria based on the provided request.
     * <p>
     * This endpoint accepts a {@link RequestDTO} object and returns the {@link FollowUpCriteria} object
     * that contains the criteria for follow-up operations.
     * </p>
     *
     * @param request The request details as a {@link RequestDTO} object.
     * @return The {@link FollowUpCriteria} object.
     */
    @PostMapping("/criteria")
    public FollowUpCriteria getFollowUpCriteria(@RequestBody RequestDTO request) {
        return followUpService.getFollowUpCriteria(request);
    }

    /**
     * This method is used to get patient list for followup.
     *
     * @param patientRequestDTO - patient request dto
     * @return List - list of FollowUpListDTO
     */
    @PostMapping("/ncd/list")
    public SuccessResponse<FollowUpDTO> getFollowUpPatients(@RequestBody PatientRequestDTO patientRequestDTO) {
        ResponseListDTO<FollowUpDTO> response = followUpService.getFollowUpPatients(patientRequestDTO);
        return new SuccessResponse<>(SuccessCode.GET_PATIENT_LIST, response.getData(), response.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * <p> This method is responsible for retrieving pending call register based on logged in user </p>
     *
     * @return call register details.
     */
    @GetMapping
    public SuccessResponse<CallRegisterDto> getPendingCallRegister() {
        return new SuccessResponse<>(SuccessCode.GET_PATIENT_LIST, followUpService.getPendingCallRegister(),
                HttpStatus.OK);
    }

    /**
     * This method is used to get patient list for followup.
     *
     * @param patientRequestDTO - patient request dto.
     * @return List - list of FollowUpListDTO.
     */
    @PostMapping("/offline/screening-followups")
    public List<FollowUpDTO> getOfflineScreeningFollowUps(@RequestBody PatientRequestDTO patientRequestDTO) {
        List<FollowUpDTO> followUpList = new ArrayList<>();
        List<FollowUpDTO> screeningFollowUpList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            patientRequestDTO.setSkip(skip);
            patientRequestDTO.setLimit(limit);
            patientRequestDTO.setType(AppointmentType.SCREENED.name());
            patientRequestDTO.setLastSyncTime(patientRequestDTO.getLastSyncTime());
            patientRequestDTO.setCurrentSyncTime(patientRequestDTO.getCurrentSyncTime());
            screeningFollowUpList = followUpService.getAllCallRegistersByVillages(patientRequestDTO, Constants.SCREENED);
            followUpList.addAll(screeningFollowUpList);
            skip += limit;
        } while (!screeningFollowUpList.isEmpty());
        return followUpList;
    }

    /**
     * This method is used to get patient list for followup after assessment.
     *
     * @param patientRequestDTO - Dto
     * @return List - list of FollowUpListDTO
     */
    @PostMapping("/offline/assessment-followups")
    public List<FollowUpDTO> getOfflineAssessmentFollowUps(@RequestBody PatientRequestDTO patientRequestDTO) {
        List<FollowUpDTO> followUpList = new ArrayList<>();
        List<FollowUpDTO> assessmentFollowUpList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            patientRequestDTO.setSkip(skip);
            patientRequestDTO.setLimit(limit);
            patientRequestDTO.setType(AppointmentType.ASSESSMENT.name());
            patientRequestDTO.setCurrentSyncTime(patientRequestDTO.getCurrentSyncTime());
            patientRequestDTO.setLastSyncTime(patientRequestDTO.getLastSyncTime());
            assessmentFollowUpList = followUpService.getAllCallRegistersByVillages(patientRequestDTO, Constants.ASSESSMENT);
            followUpList.addAll(assessmentFollowUpList);
            skip += limit;
        } while (!assessmentFollowUpList.isEmpty());
        return followUpList;
    }

    /**
     * This method is used to get patient list for followup after medical review.
     *
     * @param patientRequestDTO
     * @return List - list of FollowUpListDTO
     */
    @PostMapping("/offline/lost-to-followups")
    public List<FollowUpDTO> getOfflineLostToFollowUps(@RequestBody PatientRequestDTO patientRequestDTO) {
        List<FollowUpDTO> followUpList = new ArrayList<>();
        List<FollowUpDTO> lostToFollowUpList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            patientRequestDTO.setSkip(skip);
            patientRequestDTO.setLimit(limit);
            patientRequestDTO.setType(AppointmentType.LOST_TO_FOLLOW_UP.name());
            patientRequestDTO.setCurrentSyncTime(patientRequestDTO.getCurrentSyncTime());
            patientRequestDTO.setLastSyncTime(patientRequestDTO.getLastSyncTime());
            lostToFollowUpList = followUpService.getAllCallRegistersByVillages(patientRequestDTO,
                    Constants.LOST_TO_FOLLOW_UP);
            followUpList.addAll(lostToFollowUpList);
            skip += limit;
        } while (!lostToFollowUpList.isEmpty());
        return followUpList;
    }

    /**
     * This method is used to get patient list for followup after medical review.
     *
     * @param patientRequestDTO
     * @return List - list of FollowUpListDTO
     */
    @PostMapping("/offline/medical-review-followups")
    public List<FollowUpDTO> getOfflineMedicalReviewFollowUps(@RequestBody PatientRequestDTO patientRequestDTO) {
        List<FollowUpDTO> followUpList = new ArrayList<>();
        List<FollowUpDTO> medicalReviewFollowUpList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            patientRequestDTO.setSkip(skip);
            patientRequestDTO.setLimit(limit);
            patientRequestDTO.setType(AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.name());
            patientRequestDTO.setCurrentSyncTime(patientRequestDTO.getCurrentSyncTime());
            patientRequestDTO.setLastSyncTime(patientRequestDTO.getLastSyncTime());
            medicalReviewFollowUpList = followUpService.getAllCallRegistersByVillages(patientRequestDTO,
                    Constants.MEDICAL_REVIEW);
            followUpList.addAll(medicalReviewFollowUpList);
            skip += limit;
        } while (!medicalReviewFollowUpList.isEmpty());
        return followUpList;
    }
}
