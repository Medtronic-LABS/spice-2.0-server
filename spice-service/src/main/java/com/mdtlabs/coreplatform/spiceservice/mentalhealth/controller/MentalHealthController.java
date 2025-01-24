package com.mdtlabs.coreplatform.spiceservice.mentalhealth.controller;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.MentalHealthService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * This class is a controller class to perform operation on mental health entity.
 *
 * @author Niraimathi S created on Sept 06, 2024
 * 
 */
@RestController
@RequestMapping("/mentalhealth")
public class MentalHealthController {

    private final MentalHealthService mentalHealthService;

    @Autowired
    public MentalHealthController(MentalHealthService mentalHealthService, ModelMapper mapper) {
        this.mentalHealthService = mentalHealthService;
    }

    /**
     * <p>
     * This method is used to add a new mental health for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     * @return {@link SuccessResponse} - SuccessResponse Object with success messages.
     */
    @PostMapping("/create")
    public SuccessResponse<String> createMentalHealth(@RequestBody AssessmentDTO request) {
        mentalHealthService.createMentalHealth(request);
        return new SuccessResponse<>(SuccessCode.MENTAL_HEALTH_SAVE, HttpStatus.CREATED);
    }

    /**
     * <p>
     * This method is used to get mental health details for a patient.
     * </p>
     *
     * @param request {@link RequestDTO} - Request Object with mental health questions, answers and Scores.
     * @return {@link MentalHealthDTO} - MentalHealthDTO Object with mental health details.
     */
    @PostMapping("/details")
    public SuccessResponse<MentalHealthDTO> getMentalHealthDetails(@RequestBody RequestDTO request) {
        MentalHealthDTO mentalHealthDTO = mentalHealthService.getMentalHealthDetails(request);
        return new SuccessResponse<>(SuccessCode.MENTAL_HEALTH_DETAILS, mentalHealthDTO, HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to add a new mental health for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     * @return {@link SuccessResponse} - SuccessResponse Object with success messages.
     */
    @PostMapping("/condition-create")
    public SuccessResponse<String> createMentalHealthCondition(@RequestBody AssessmentDTO request) {
        mentalHealthService.createMentalHealthCondition(request);
        return new SuccessResponse<>(SuccessCode.MENTAL_HEALTH_SAVE, HttpStatus.CREATED);
    }

    /**
     * <p>
     * This method is used to get mental health details for a patient.
     * </p>
     *
     * @param request {@link RequestDTO} - Request Object with mental health questions, answers and Scores.
     * @return {@link AssessmentDTO} - AssessmentDTO Object with mental health details.
     */
    @PostMapping("/condition-details")
    public SuccessResponse<AssessmentDTO> getMentalHealthCondition(@RequestBody AssessmentDTO request) {
        return new SuccessResponse<>(SuccessCode.MENTAL_HEALTH_DETAILS, mentalHealthService.getMentalHealthCondition(request), HttpStatus.OK);
    }
}
