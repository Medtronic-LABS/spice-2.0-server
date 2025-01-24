package com.mdtlabs.coreplatform.fhirmapper.bplog.service;


import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 * This is an interface to perform any actions in bplog related entities.
 * </p>
 *
 *
 */
public interface BpLogService {

    /**
     * Gets bp log information by patient id
     *
     * @param patientBpLogRequestData patient bp log request dto
     * @return PatientBpLogsDto
     */
    PatientBpLogsDTO getPatientBpLogsWithSymptoms(RequestDTO patientBpLogRequestData);
}
