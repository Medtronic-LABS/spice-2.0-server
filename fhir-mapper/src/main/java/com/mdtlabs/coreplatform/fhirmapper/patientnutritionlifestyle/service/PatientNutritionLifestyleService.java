package com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.service;

import java.util.List;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 *     This is a service interface for PatientNutritionLifestyle entity.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
public interface PatientNutritionLifestyleService {

    /**
     * <p>
     * Adds a new PatientNutritionLifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyle the PatientNutritionLifestyle object to be added
     * @return the added PatientNutritionLifestyle object
     */
    PatientNutritionLifestyle addPatientNutritionLifestyle(
            PatientNutritionLifestyle patientNutritionLifestyle);


    /**
     * <p>
     * Retrieves a list of PatientNutritionLifestyle entries based on the given RequestDTO.
     * </p>
     *
     * @param request the RequestDTO containing the request parameters
     * @return a list of PatientNutritionLifestyle entries
     */
    List<PatientNutritionLifestyle> getPatientNutritionLifeStyleList(RequestDTO request);

    /**
     * <p>
     * Updates an existing PatientNutritionLifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyles the PatientNutritionLifestyle list to be updated
     * @return the updated PatientNutritionLifestyle entries
     */
    PatientNutritionLifestyleUpdateDTO updatePatientNutritionLifestyle(PatientNutritionLifestyleUpdateDTO patientNutritionLifestyleUpdateDTO);

    /**
     * <p>
     * Removes an existing PatientNutritionLifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyle the PatientNutritionLifestyle object to be removed
     * @return the removed PatientNutritionLifestyle object
     */
    PatientNutritionLifestyle removePatientNutritionLifestyle(PatientNutritionLifestyle patientNutritionLifestyle);

}
