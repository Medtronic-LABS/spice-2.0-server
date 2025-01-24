package com.mdtlabs.coreplatform.fhirmapper.glucoselog.service;


import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

import java.util.List;

/**
 * This is an interface to perform any actions in glucose log related entities.
 *
 * @since Sep 05, 2024
 * @author Tamilmani
 */
public interface GlucoseLogService {

	/**
	 * Gets patient glucose log dto with symptoms.
	 *
	 * @param requestData - request dto
	 * @return PatientGlucoseLogDto - patient glucose log dto
	 */
	PatientGlucoseLogDTO getPatientGlucoseLogsWithSymptoms(RequestDTO requestData);

	/**
	 * Gets patient glucose log dto with out symptoms.
	 *
	 * @param requestData - request dto
	 * @return list of glucoseLogDTO - patient list of glucose log dto
	 */
	List<GlucoseLogDTO> getPatientGlucoseLogs(RequestDTO requestData);
}
