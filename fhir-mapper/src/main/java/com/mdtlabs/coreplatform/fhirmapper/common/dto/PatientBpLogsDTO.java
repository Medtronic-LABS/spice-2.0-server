package com.mdtlabs.coreplatform.fhirmapper.common.dto;


import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * PatientBpLogsDTO is utilized for handling the blood pressure log information of patient.
 *
 */
@Data
public class PatientBpLogsDTO {
	
	private long total;

	private int limit;

	private int skip;

	private List<BpLogDTO> bpLogList;

	private BpLogDTO latestBpLog;

	private Map<String, Integer> bpThreshold;
    
}