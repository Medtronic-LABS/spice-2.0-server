package com.mdtlabs.coreplatform.fhirmapper.assessment.service;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;

/**
 * Service interface for managing assessments and TB workflows.
 * <p>
 * Provides functionality for creating assessments for patients and managing TB (Tuberculosis) specific workflows.
 * This includes operations to create new assessments and TB workflows based on the provided assessment details.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Feb 05, 2024
 */
public interface AssessmentService {

    /**
     * Creates a new assessment for a patient.
     * <p>
     * This method takes assessment details encapsulated in an {@link AssessmentDTO} and processes it to create
     * a new assessment. The process may involve validation, persistence, and other business logic specific to
     * the assessment creation.
     * </p>
     *
     * @param assessmentDTO The assessment details provided by the client.
     * @return The created {@link AssessmentDTO} with identifiers and status populated, indicating the result of the creation process.
     */
    AssessmentDTO createAssessment(@RequestBody AssessmentDTO assessmentDTO);

    /**
     * Creates a TB workflow based on assessment details.
     * <p>
     * This method is responsible for creating and managing TB-specific workflows. It takes an {@link AssessmentDTO}
     * containing assessment details relevant to TB management and processes it according to the defined TB workflow logic.
     * </p>
     *
     * @param assessmentDto The assessment details relevant to TB management.
     * @return A {@link ResponseEntity} containing a {@link FhirResponseDTO}, which encapsulates the outcome of the TB workflow creation process.
     */
    ResponseEntity<FhirResponseDTO> createTB(AssessmentDTO assessmentDto);

    /**
     * <p>
     * Creates a new assessment based on AssessmentDTO.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} Object with patient assessment data is given
     * @return {@link AssessmentDTO} entity is returned
     */
    AssessmentDTO createNcdAssessment(@RequestBody AssessmentDTO assessmentDTO);

    /**
     * Creates provisional treatment plan.
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     */
    TreatmentPlanResponseDTO createProvisionalTreatmentPlan(AssessmentDTO assessmentDTO);

    /**
     * <p>
     * This method is used to get a latest BpLog Assessment.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link BpLogDTO} Latest bp log details
     */
    BpLogDTO getLatestBpLog(@RequestBody AssessmentDTO assessmentDTO);

    /**
     * <p>
     * This method is used to get a latest Glucose Log Assessment.
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link GlucoseLogDTO} Latest glucose log details
     */
    GlucoseLogDTO getLatestGlucoseLog(@RequestBody AssessmentDTO assessmentDTO);

    /**
     * Process and create FHIR substance abuse observation based on given
     * assessment details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    Observation createSubstanceAbuseObservation(AssessmentDTO assessmentDTO,
                                                        Encounter encounter);

    /**
     * Process and create FHIR suicide screener observation based on given
     * screening log details
     *
     * @param assessmentDTO    The screening log details
     * @param encounter     The FHIR Encounter entity
     *
     * @return Converted FHIR Observation entity.
     */
    Observation createSuicideObservation(AssessmentDTO assessmentDTO,
                                                Encounter encounter);

    /**
     * Set observation details in FHIR bundle resource
     *
     * @param bundle          The FHIR bundle resource
     * @param observation     The FHIR Observation entity
     * @param identifierUrl   The Observation identifier url
     * @param provenanceDTO   The ProvenanceDTO entity to store performers information
     *
     */
    void setObservationDetails(Bundle bundle,
                                      Observation observation,
                                      String identifierUrl, AssessmentDTO assessmentDTO, Patient patient,
                                      RelatedPerson relatedPerson, ProvenanceDTO provenanceDTO);
}
