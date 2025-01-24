package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * PatientGlucoseLogDTO is utilized for handling the blood glucose log information of patient.
 *
 */
@Data
public class PatientGlucoseLogDTO {
    
    private long total;

	private int limit;

	private int skip;

	private List<GlucoseLogDTO> glucoseLogList;

	private GlucoseLogDTO latestGlucoseLog;

	private List<Map<String, Object>> glucoseThreshold;
}
