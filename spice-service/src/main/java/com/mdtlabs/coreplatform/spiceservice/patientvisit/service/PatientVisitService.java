package com.mdtlabs.coreplatform.spiceservice.patientvisit.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientVisitDTO;


/**
 * <p>
 * This an interface class for patient visit module you can implemented this
 * class in any class.
 * </p>
 *
 * @author VigneshKumar created on Jun 30, 2022
 */
public interface PatientVisitService {

    /**
     * <p>
     * This method is used to add patient visit.
     * </p>
     *
     * @param patientVisitDto - common request dto
     * @return Map - patient visit map
     */
    Map<String, Object> createPatientVisit(PatientVisitDTO patientVisitDto);

    

}
