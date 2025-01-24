package com.mdtlabs.coreplatform.fhirmapper.assessment.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;

/**
 * Controller for handling assessment-related operations.
 * <p>
 * This controller provides endpoints for creating assessments and managing TB workflows.
 * It leverages the {@link AssessmentService} for the business logic associated with each operation.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Feb 12, 2024
 */
@RestController
@RequestMapping(value = "/assessment")
@Validated
public class AssessmentController {

    private final AssessmentService assessmentService;

    @Autowired
    public AssessmentController(AssessmentService assessmentService) {
        this.assessmentService = assessmentService;
    }

    /**
     * Endpoint for creating a new assessment for a patient.
     * <p>
     * Accepts assessment details in the form of {@link AssessmentDTO} and delegates to
     * {@link AssessmentService#createAssessment(AssessmentDTO)} for processing.
     * </p>
     *
     * @param assessmentDTO The assessment details.
     * @return The created {@link AssessmentDTO} with populated identifiers and status.
     */
    @PostMapping("/create")
    public AssessmentDTO createAssessment(@RequestBody AssessmentDTO assessmentDTO) {
        return assessmentService.createAssessment(assessmentDTO);
    }

    /**
     * Endpoint for saving TB workflow information.
     * <p>
     * Accepts TB assessment details and delegates to {@link AssessmentService#createTB(AssessmentDTO)}
     * for creation and management of TB-specific workflows.
     * </p>
     *
     * @param assessmentDTO The TB assessment details.
     * @return A {@link ResponseEntity} indicating the outcome of the operation.
     */
    @PostMapping("/tb/create")
    public ResponseEntity<FhirResponseDTO> createTB(@RequestBody AssessmentDTO assessmentDTO) {
        return assessmentService.createTB(assessmentDTO);
    }

    /**
     * <p>
     * This method is used to get a latest BpLog Assessment.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link BpLogDTO} Latest bp log details
     */
    @PostMapping("/bp-log/latest")
    public BpLogDTO getLatestBpLog(@RequestBody AssessmentDTO assessmentDTO) {
        return assessmentService.getLatestBpLog(assessmentDTO);
    }

    /**
     * <p>
     * This method is used to get a latest Glucose Log Assessment.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link GlucoseLogDTO} Latest glucose log details
     */
    @PostMapping("/glucose-log/latest")
    public GlucoseLogDTO getLatestGlucoseLog(@RequestBody AssessmentDTO assessmentDTO) {
        return assessmentService.getLatestGlucoseLog(assessmentDTO);
    }
}
