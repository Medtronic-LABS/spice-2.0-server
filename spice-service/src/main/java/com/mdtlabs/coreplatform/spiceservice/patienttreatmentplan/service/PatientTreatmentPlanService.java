package com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanResponseDTO;

/**
 * <p>
 * This interface is a service to perform operation on Patient treatmentplan.
 * </p>
 *
 * @author Karthick M created on Aug 14, 2024
 */
public interface PatientTreatmentPlanService {

    /**
	 * <p>
	 * This method is used to get patient treatment plan details.
	 * </p>
	 *
	 * @param id - id
	 * @return PatientTreatmentPlan - entity
	 */
	public TreatmentPlanResponseDTO getPatientTreatmentPlanDetails(RequestDTO request);

    /**
	 * <p>
	 * Updates the patient treatment plan data.
	 * </p>
	 *
	 * @param patientTreatmentPlan - entity
	 * @return boolean - true or false
	 */
	public void updateTreatmentPlanData(TreatmentPlanDTO treatmentPlan);
    
}
