package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 *      This class used to get NCD patient status.
 * </p>
 *
 * @author - Gopinath
 */
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
