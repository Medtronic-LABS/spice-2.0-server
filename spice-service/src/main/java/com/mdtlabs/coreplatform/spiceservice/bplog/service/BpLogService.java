package com.mdtlabs.coreplatform.spiceservice.bplog.service;


import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * <p>
 * This is an interface to perform any actions in bplog related entities.
 * </p>
 *
 * @author Tamilmani created on Aug 28, 2024
 *
 */
public interface BpLogService {

    /**
     * Creates new bp log information.
     *
     * @param bpLog - entity
     * @return BpLogDTO entity.
     */
    BpLogDTO addBpLog(BpLogDTO bpLog);

    /**
     * Get the patient bp log information
     *
     * @param patientBpLogsRequestDto patient bp log request information
     * @return PatientBpLog information.
     */
    PatientBpLogsDTO getPatientBpLogsWithSymptoms(RequestDTO patientBpLogsRequestDto);
}
