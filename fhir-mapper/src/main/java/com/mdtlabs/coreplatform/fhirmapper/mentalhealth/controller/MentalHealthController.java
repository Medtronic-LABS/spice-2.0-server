package com.mdtlabs.coreplatform.fhirmapper.mentalhealth.controller;

import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.MentalHealthService;

/**
 * This class is a controller class to perform operation on mental health entity.
 *
 * @author Niraimathi S created on Sept 06, 2024
 */
@RestController
@RequestMapping("/mentalhealth")
public class MentalHealthController {

    private final MentalHealthService mentalHealthService;

    @Autowired
    public MentalHealthController(MentalHealthService mentalHealthService) {
        this.mentalHealthService = mentalHealthService;
    }

    /**
     * <p>
     * This method is used to add a new mental health for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     * @return {@link ResponseEntity} - ResponseEntity Object with success messages.
     */
    @PostMapping("/create")
    public ResponseEntity<String> createMentalHealth(@RequestBody AssessmentDTO request) {
        request.setAssessmentTakenOn(new Date());
        mentalHealthService.createMentalHealth(request);
        return new ResponseEntity<>(HttpStatus.CREATED);
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
    public MentalHealthDTO getMentalHealthDetails(@RequestBody RequestDTO request) {
        return mentalHealthService.getMentalHealthDetails(request);
    }

    /**
     * <p>
     * This method is used to add a new mental health condition for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     * @return {@link ResponseEntity} - ResponseEntity Object with success messages.
     */
    @PostMapping("/condition-create")
    public ResponseEntity<String> createMentalHealthCondition(@RequestBody AssessmentDTO request) {
        request.setAssessmentTakenOn(new Date());
        mentalHealthService.createMentalHealthCondition(request);
        return new ResponseEntity<>(HttpStatus.CREATED);
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
    public AssessmentDTO getMentalHealthCondition(@RequestBody AssessmentDTO request) {
        return mentalHealthService.getMentalHealthCondition(request);
    }
}
