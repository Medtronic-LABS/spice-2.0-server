package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

@Data
public class NcdPatientStatus {

    private String id;

    private String diabetesStatus;

    private String hypertensionStatus;

    private String diabetesYearOfDiagnosis;

    private String hypertensionYearOfDiagnosis;

    private String diabetesControlledType;

	private String diabetesDiagnosis;

}
