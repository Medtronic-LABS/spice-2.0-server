package com.mdtlabs.coreplatform.spiceservice.assessment.service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * Service interface for managing assessments.
 * <p>
 * This interface defines the contract for assessment-related operations, such as creating assessments.
 * It is intended to be implemented by classes that handle the business logic associated with assessments,
 * interacting with the data layer and other services as necessary.
 * </p>
 *
 * @author Jeyaharini T A created on Feb 10, 2024
 */
public interface AssessmentService {

    /**
     * Creates a new assessment.
     * <p>
     * This method is responsible for creating a new assessment based on the provided {@link AssessmentDTO}.
     * The implementation should ensure that the assessment is valid and persist it to the database or any other
     * storage mechanism in use. The method returns the created {@link AssessmentDTO}, which may include additional
     * information such as an ID assigned by the storage mechanism.
     * </p>
     *
     * @param assessmentDTO The details of the assessment to be created.
     * @return The created {@link AssessmentDTO} with any additional information populated.
     */
    AssessmentDTO createAssessment(AssessmentDTO assessmentDTO);

	/**
	 * Used to add red risk notification
	 * @param patientId   - Patient id saved in fhir database
	 * @param encounterId - Encounter id saved in fhir database
	 * @param memberId    - Member id saved in fhir database
	 */
	void addRedRiskNotification(String patientId, String encounterId, String memberId);

	/**
	 * <p>
	 * Creates a new assessment based on AssessmentDTO.
	 * </p>
	 *
	 * @param assessmentDTO {@link AssessmentDTO} Object with patient assessment data is given
	 * @return {@link AssessmentDTO} entity is returned
	 */
	AssessmentDTO createNcdAssessment(AssessmentDTO assessmentDTO);

	/**
	 * <p>
	 * This method is used to add a new BpLog Assessment.
	 * </p>
	 *
	 * @param assessmentDTO {@link AssessmentDTO} entity is given
	 */
	void createBpAssessment(AssessmentDTO assessmentDTO);

	/**
	 * <p>
	 * This method is used to add a new Glucose Log Assessment.
	 * </p>
	 *
	 * @param assessmentDTO {@link AssessmentDTO} entity is given
	 */
	void createGlucoseLog(AssessmentDTO assessmentDTO);


}
