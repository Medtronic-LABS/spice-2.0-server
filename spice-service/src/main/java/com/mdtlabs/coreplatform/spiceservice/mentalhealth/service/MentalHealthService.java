package com.mdtlabs.coreplatform.spiceservice.mentalhealth.service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * This is an interface to perform any actions in mentalHealth related entities.
 *
 * @author Niraimathi S created on Sept 06 07, 2024
 */
public interface MentalHealthService {

    /**
     * <p>
     * This method is used to add a new mental health for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     */
    void createMentalHealth(AssessmentDTO request);

    /**
     * <p>
     * This method is used to gets mental health details for a patient.
     * </p>
     *
     * @param request {@link RequestDTO} - Request Object with mental health questions, answers and Scores.
     */
    MentalHealthDTO getMentalHealthDetails(RequestDTO request);

    /**
     * <p>
     * This method is used to add a new mental health for a patient.
     * </p>
     *
     * @param mentalHealth {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     */
    void createMentalHealthCondition(AssessmentDTO mentalHealth);

    /**
     * <p>
     * This method is used to gets mental health details for a patient.
     * </p>
     *
     * @param request {@link AssessmentDTO} - Request Object with mental health questions, answers and Scores.
     */
    AssessmentDTO getMentalHealthCondition(AssessmentDTO request);

}
