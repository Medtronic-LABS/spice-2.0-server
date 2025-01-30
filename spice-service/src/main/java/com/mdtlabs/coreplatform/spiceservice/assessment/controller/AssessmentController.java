package com.mdtlabs.coreplatform.spiceservice.assessment.controller;


import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;

/**
 * Controller for handling assessment-related operations.
 * <p>
 * This controller provides endpoints for creating and managing assessments within the application.
 * It leverages the {@link AssessmentService} to perform the business logic associated with each operation.
 * </p>
 *
 * @author Jeyaharini T A created on Feb 10, 2024
 */
@RestController
@RequestMapping(value = "/assessment")
@Validated
public class AssessmentController {

    private AssessmentService assessmentService;

    @Autowired
    public AssessmentController(AssessmentService assessmentService) {
        this.assessmentService = assessmentService;
    }

    /**
     * Endpoint for creating a new assessment.
     * <p>
     * This method accepts an {@link AssessmentDTO} object as input, representing the details of the assessment
     * to be created. It uses the {@link AssessmentService} to create the assessment and returns the created
     * assessment details as a {@link ResponseEntity}.
     * </p>
     *
     * @param assessmentDTO The assessment details to be created.
     * @return A {@link ResponseEntity} containing the created {@link AssessmentDTO}.
     */
    @PostMapping("/create")
    public ResponseEntity<AssessmentDTO> createAssessment(@RequestBody AssessmentDTO assessmentDTO) {
        return ResponseEntity.ok().body(assessmentService.createAssessment(assessmentDTO));
    }

    /**
     * <p>
     * This method is used to add a new BpLog Assessment.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link SuccessResponse<String>} Success Message is returned
     */
    @PostMapping("/bp-log-create")
    public SuccessResponse<String> createBpLog(@RequestBody AssessmentDTO assessmentDTO) {
        assessmentService.createBpAssessment(assessmentDTO);
        return new SuccessResponse<>(SuccessCode.ASSESSMENT_BP_LOG_SAVE, HttpStatus.CREATED);
    }

    /**
     * <p>
     * This method is used to add a new Glucose Log Assessment.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link SuccessResponse<String>} success response is returned
     */
    @PostMapping("/glucose-log-create")
    public SuccessResponse<String> createGlucoseLog(@RequestBody AssessmentDTO assessmentDTO) {
        assessmentService.createGlucoseLog(assessmentDTO);
        return new SuccessResponse<>(SuccessCode.ASSESSMENT_BG_LOG_SAVE, HttpStatus.CREATED);
    }
}
