package com.mdtlabs.coreplatform.fhirmapper.patientvisit.service;

import java.util.Map;

import org.hl7.fhir.r4.model.Encounter;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientVisitDTO;

/**
 * <p>
 * This class is a service class to perform operation on Patient visit.
 * </p>
 *
 * @author Karthick M created on Aug 19, 2024
 */
public interface PatientVisitService {

    /**
     * Creats a patient visit with encounter resource.
     * 
     * @param patientVisit
     * @return Map<String, String>
     */
    Map<String, Object> createPatientVisit(PatientVisitDTO patientVisit);

    /**
     * Updates patient visit status
     * 
     * @param encounterId
     * @param isPrescription
     * @param isMedicalReview
     * @param isInvestication
     * @param patientReference
     * @return
     */
    Encounter updatePatientVisitStatus(String encounterId, boolean isPrescription, boolean isMedicalReview, boolean isInvestication, String patientReference);

    /**
     * Get patient medical review status
     *
     * @param memberReference
     * @return boolean
     */
    boolean getPatientMedicalReviewStatus(String memberReference);
    
}
