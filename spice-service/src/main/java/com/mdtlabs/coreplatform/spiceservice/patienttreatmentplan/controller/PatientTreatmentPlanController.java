package com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.controller;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.PatientTreatmentPlanService;

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
	private final ModelMapper modelMapper;

	@Autowired
	public PatientTreatmentPlanController(PatientTreatmentPlanService treatmentPlanService, ModelMapper modelMapper) {
		this.treatmentPlanService = treatmentPlanService;
		this.modelMapper = modelMapper;
	}

    /**
     * <p>
     * This method retrieves a patient treatment plan.
     * </p>
     *
     * @param request - request dto
     * @return PatientTreatmentPlan Entity
     */
	@PostMapping("/details")
	public SuccessResponse<TreatmentPlanResponseDTO> getPatientTreatmentPlan(@RequestBody RequestDTO request) {
	    Logger.logDebug("In PatientTreatmentPlanController, get patient treatment plan");
		return new SuccessResponse<>(SuccessCode.GET_TREATMENTPLAN,
				treatmentPlanService.getPatientTreatmentPlanDetails(request), HttpStatus.OK);
	}

    /**
    * <p>
    * Updates patient treatment plan data information.
	* </p>
	*
	* @param patientTreatmentPlan - entity
	* @return Boolean - true or false
	*/
	@PostMapping("/update")
	public SuccessResponse<Boolean> updateTreatmentPlanData(@Valid @RequestBody
    TreatmentPlanDTO treatmentPlanDTO) {
        Logger.logDebug("In PatientTreatmentPlanController, update patient treatment plan");
        treatmentPlanService
                .updateTreatmentPlanData(treatmentPlanDTO);
        return new SuccessResponse<>(SuccessCode.TREATMENTPLAN_UPDATE, HttpStatus.OK);
    }

    
}
