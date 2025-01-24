package com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.service.PatientNutritionLifestyleService;

/**
 * <p>
 *  Controller for handling Patient Nutrition Lifestyle related operations.
 *  This controller provides endpoints for creating, listing, updating, and deleting
 *  Patient Nutrition Lifestyle entries.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@RestController
@RequestMapping(value = "/patient-nutrition-lifestyle")
@Validated
public class PatientNutritionLifestyleController {

    private final PatientNutritionLifestyleService patientNutritionLifestyleService;

    @Autowired
    public PatientNutritionLifestyleController(PatientNutritionLifestyleService patientNutritionLifestyleService) {
        this.patientNutritionLifestyleService = patientNutritionLifestyleService;
    }

    /**
     * <p>
     * Endpoint to create a new Patient Nutrition Lifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyle the Patient Nutrition Lifestyle object to be created
     * @return the created Patient Nutrition Lifestyle object
     */
    @PostMapping("/create")
    public SuccessResponse<PatientNutritionLifestyle> addPatientNutritionLifestyle(
            @RequestBody PatientNutritionLifestyle patientNutritionLifestyle) {
        return new SuccessResponse<>(SuccessCode.PATIENT_NUTRITION_LIFESTYLE_SAVE,
                patientNutritionLifestyleService.addPatientNutritionLifestyle(patientNutritionLifestyle),
                HttpStatus.CREATED);
    }

    /**
     * <p>
     * Endpoint to retrieve a list of Patient Nutrition Lifestyle entries.
     * </p>
     *
     * @param request the RequestDTO containing the request parameters
     * @return a list of Patient Nutrition Lifestyle entries
     */
    @PostMapping("/list")
    public SuccessResponse<PatientNutritionLifestyle> getPatientNutritionLifeStyleList(@RequestBody RequestDTO request) {
        List<PatientNutritionLifestyle> patientNutritionLifestyles = patientNutritionLifestyleService.getPatientNutritionLifeStyleList(request);
        return new SuccessResponse<>(SuccessCode.PATIENT_NUTRITION_LIFESTYLE_LIST,
                patientNutritionLifestyles, Long.valueOf(patientNutritionLifestyles.size()),  HttpStatus.OK);
    }

    /**
     * Endpoint to update an existing Patient Nutrition Lifestyle entry.
     *
     * @param patientNutritionLifestyles the Patient Nutrition Lifestyle object to be updated
     * @return the updated Patient Nutrition Lifestyle object
     */
    @PutMapping("/update")
    public SuccessResponse<PatientNutritionLifestyleUpdateDTO> updatePatientNutritionLifestyle(
            @RequestBody PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles) {
        return new SuccessResponse<>(SuccessCode.PATIENT_NUTRITION_LIFESTYLE_UPDATE, patientNutritionLifestyleService
                .updatePatientNutritionLifestyle(patientNutritionLifestyles), HttpStatus.OK);
    }

    /**
     * Endpoint to delete an existing Patient Nutrition Lifestyle entry.
     *
     * @param patientNutritionLifestyle the Patient Nutrition Lifestyle object to be deleted
     * @return the removed PatientNutritionLifestyle object
     */
    @PostMapping("/remove")
    public SuccessResponse<PatientNutritionLifestyle> deletePatientNutritionLifestyle(
            @RequestBody PatientNutritionLifestyle patientNutritionLifestyle) {
        return new SuccessResponse<>(SuccessCode.PATIENT_NUTRITION_LIFESTYLE_DELETE,
                patientNutritionLifestyleService.removePatientNutritionLifestyle(patientNutritionLifestyle), HttpStatus.OK);
    }
}
