package com.mdtlabs.coreplatform.fhirmapper.registration.service;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 * This interface is a service to perform operation on Registration.
 * </p>
 *
 * @author Karthick M created on Aug 05, 2024
 */
public interface RegistrationService {

    /**
     * Register a Patient and member.
     * 
     * @param request
     * @return EnrollmentResponseDTO
     */
    EnrollmentResponseDTO registerPatientAndMember(EnrollmentRequestDTO request);

    /**
     * Validates Patient from national-id.
     * 
     * @param request
     * @return PatientValidationResponseDTO
     */
    PatientValidationResponseDTO isPatientPresentInRelatedPerson(BioDataDTO request);

    /**
     * <p>
     * Validates patient using national-id.
     * </p>
     *
     * @param request the {@link BioDataDTO} request object contains Patient ID and patient national ID.
     * @return A {@link PatientValidationResponseDTO} if the patient is valid.
     */
    PatientValidationResponseDTO validatePatient(BioDataDTO request);

    /**
     * <p>
     * updates patient signature obtained from screening and registration..
     * </p>
     *
     * @param requestDTO the {@link RequestDTO} request object contains signature url.
     * @return A boolean value is returned .
     */
    boolean updateMemberSignature(RequestDTO requestDTO);
}
