package com.mdtlabs.coreplatform.spiceservice.patientpsychology.service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;

import java.util.List;

/**
 * <p>
 *  This is a service interface for PatientPsychology entity.
 * </p>
 *
 * @author Bala Ashwanth N
 * @since Nov 18, 2024
 */
public interface PatientPsychologyService {

    /**
     * Get clinical notes details for patient.
     *
     * @param request - Patient Tracker id and visit id to get the patient clinical notes.
     * @return {@link PsychologyDTO}
     */
    List<PsychologyDTO> getPsychologyDataByUserIdAndRelatedPersonId(PsychologyDTO request);


    /**
     * Save clinical notes details for patient.
     *
     * @param request - Patient Tracker id and visit id to save the patient clinical notes.
     * @return {@link PsychologyDTO}
     */
    PsychologyDTO savePsychologyData(PsychologyDTO request);

    /**
     * remove clinical notes details for patient.
     *
     * @param request - Note id is to remove patient clinical notes.
     */
    PsychologyDTO removePsychologyDataById(PsychologyDTO request);

    /**
     * Update clinical notes details for patient.
     *
     * @param request - Patient Tracker id and visit id to update the patient clinical notes.
     * @return {@link PsychologyDTO}
     */
    PsychologyDTO updatePsychologyData(PsychologyDTO request);
}
