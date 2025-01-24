package com.mdtlabs.coreplatform.spiceservice.glucoselog.service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * This is an interface to perform any actions in glucose log related entities.
 *
 * @since  Aug 28, 2024
 * @author Tamilmani
 */
public interface GlucoseLogService {

	/**
	 * This method adds a new Glucose log.
	 *
	 * @param glucoseLog - GlucoseLogDTO
	 * @return GlucoseLogDTO
	 */
	GlucoseLogDTO addGlucoseLog(GlucoseLogDTO glucoseLog);

	/**
	 * This method is used to fetch bp logs using related person id.
	 *
	 * @param patientGlucoseLogsRequestDto PatientGlucoseLogsRequest object to fetch the list of patient's glucose logs.
	 * @return PatientGlucoseLogDTO entity.
	 */
	PatientGlucoseLogDTO getPatientGlucoseLogsWithSymptoms(RequestDTO patientGlucoseLogsRequestDto);
}
