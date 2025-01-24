package com.mdtlabs.coreplatform.fhirmapper.patientpsychology.service;

import java.util.List;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PsychologyDTO;

/**
 * <p>
 *     This is a service interface for PatientPsychology entity.
 * </p>
 *
 * @author Bala Ashwanth N
 * @since Nov 18, 2024
 */
public interface PatientPsychologyService {

    /**
     * <p>
     * Used to save the patient clinical note details based on the patient.
     * <p/>
     *
     * @param request - Patient tracker id to get the patient status.
     * @return {@link PatientStatusDTO} - Patient details
     */
    PsychologyDTO savePatientPsychology(PsychologyDTO request);

    /**
     * <p>
     * Used to remove the patient clinical note details based on the request.
     *  <p>
     */
    PsychologyDTO removePsychologyDataById(PsychologyDTO request);

    /**
     * <p>
     * Used to get the patient clinical note details based on the patient.
     *  <p>
     */
    List<PsychologyDTO> getPatientPsychologyByRelatedPersonId(PsychologyDTO request);
}
