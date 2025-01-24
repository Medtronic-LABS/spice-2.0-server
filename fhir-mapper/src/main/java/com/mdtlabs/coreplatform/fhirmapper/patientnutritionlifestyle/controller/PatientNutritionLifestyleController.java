package com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.service.PatientNutritionLifestyleService;

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
    public PatientNutritionLifestyle addPatientNutritionLifestyle(
            @RequestBody PatientNutritionLifestyle patientNutritionLifestyle) {
        return patientNutritionLifestyleService.addPatientNutritionLifestyle(patientNutritionLifestyle);
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
    public List<PatientNutritionLifestyle> getPatientNutritionLifeStyleList(@RequestBody RequestDTO request) {
        return patientNutritionLifestyleService.getPatientNutritionLifeStyleList(request);
    }

    /**
     * <p>
     * Endpoint to update an existing Patient Nutrition Lifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyles the Patient Nutrition Lifestyle list to be updated
     * @return the updated Patient Nutrition Lifestyle list entries
     */
    @PutMapping("/update")
    public PatientNutritionLifestyleUpdateDTO updatePatientNutritionLifestyle(
            @RequestBody PatientNutritionLifestyleUpdateDTO patientNutritionLifestyleUpdateDTO) {
        return patientNutritionLifestyleService.updatePatientNutritionLifestyle(patientNutritionLifestyleUpdateDTO);
    }

    /**
     * <p>
     * Endpoint to delete an existing Patient Nutrition Lifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyle the Patient Nutrition Lifestyle object to be deleted
     */
    @PostMapping("/remove")
    public PatientNutritionLifestyle deletePatientNutritionLifestyle(@RequestBody PatientNutritionLifestyle patientNutritionLifestyle) {
        return patientNutritionLifestyleService.removePatientNutritionLifestyle(patientNutritionLifestyle);
    }

}
