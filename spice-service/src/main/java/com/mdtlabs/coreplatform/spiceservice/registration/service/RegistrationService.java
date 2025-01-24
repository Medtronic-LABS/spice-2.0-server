package com.mdtlabs.coreplatform.spiceservice.registration.service;

import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

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
     * @param request The request of the patient to be saved is given.
     * @param file The file to be saved is given
     * @return Saved EnrollmentResponseDTO is returned
     */
    EnrollmentResponseDTO registerPatientAndMember(EnrollmentRequestDTO request, MultipartFile file);

    /**
     * <p>
     * Validates patient using national-id.
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains Patient ID and patient national ID.
     * @return A {@link PatientValidationResponseDTO} if the patient is valid.
     */
    PatientValidationResponseDTO isPatientRegisteredInRelatedPerson(BioDataDTO request);

}
