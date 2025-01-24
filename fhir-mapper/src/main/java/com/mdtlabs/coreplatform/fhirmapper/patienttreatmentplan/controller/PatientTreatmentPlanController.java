package com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;

import jakarta.validation.Valid;

/**
 * <p>
 * This class is a controller class to perform operation on Patient treatmentplan. 
 * </p>
 *
 * @author Karthick M created on Aug 14, 2024
 */
@RestController
@RequestMapping("/patient-treatment-plan")
public class PatientTreatmentPlanController {

    private final PatientTreatmentPlanService treatmentPlanService;

	@Autowired
	public PatientTreatmentPlanController(PatientTreatmentPlanService treatmentPlanService) {
		this.treatmentPlanService = treatmentPlanService;
	}

    /**
     * <p>
     * This method retrieves a patient treatment plan.
     * </p>
     *
     * @param request - request dto
     * @return TreatmentPlanResponseDTO Entity
     */
	@PostMapping("/details")
	public TreatmentPlanResponseDTO getPatientTreatmentPlan(@RequestBody RequestDTO request) {
		return treatmentPlanService.getPatientTreatmentPlanDetails(request);
	}

    /**
    * <p>
    * Updates patient treatment plan data information.
	* </p>
	*
	* @param treatmentPlanDTO - entity
	*/
	@PostMapping("/update")
	public void updateTreatmentPlanData(@Valid @RequestBody
    			TreatmentPlanDTO treatmentPlanDTO) {
        treatmentPlanService
                .updateTreatmentPlanData(treatmentPlanDTO);
    }

    
}
