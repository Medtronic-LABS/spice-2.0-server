package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.dto.ClassificationDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.MedicationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.Classification;
import com.mdtlabs.coreplatform.adminservice.model.entity.DosageForm;
import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;
import com.mdtlabs.coreplatform.adminservice.service.MedicationService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

/**
 * <p>
 * The MedicationController class is a REST controller that handles requests related to Medication.
 * </p>
 *
 * @author Karthick M
 */
@RestController
@RequestMapping("/medication")
public class MedicationController {

    private final MedicationService medicationService;

    public MedicationController(MedicationService medicationService) {
        this.medicationService = medicationService;
    }

    /**
     * Adds a new medication or multiple medications to the system.
     * <p>
     * This method handles a POST request to create new medication entries in the system. It accepts a list of
     * {@link MedicationDTO} objects representing the medications to be added. Each {@link MedicationDTO} contains
     * the necessary information to create a medication entry, such as name, dosage, and classification.
     * The method delegates the creation process to the {@link MedicationService#createMedication(List<MedicationDTO>)} method.
     * Upon successful creation of the medication entries, a {@link SuccessResponse} with a status of CREATED is returned,
     * indicating that the medications have been successfully added to the system.
     * </p>
     *
     * @param medications A list of {@link MedicationDTO} objects representing the medications to be added.
     * @return A {@link SuccessResponse<Medication>} object with the status CREATED, indicating successful addition of medications.
     */
    @PostMapping("/create")
    public SuccessResponse<Medication> addMedication(@RequestBody List<MedicationDTO> medications) {
        medicationService.createMedication(medications);
        return new SuccessResponse<>(SuccessCode.MEDICATION_SAVE, HttpStatus.CREATED);
    }

    /**
     * Updates the details of an existing medication.
     * <p>
     * This method handles a PUT request to update the details of an existing medication in the system.
     * It accepts a {@link MedicationDTO} object containing the updated medication details, such as name, dosage, and classification.
     * The {@link MedicationService#updateMedication(MedicationDTO)} method is called to process the update.
     * Upon successful update, a {@link SuccessResponse} with a status of OK is returned, indicating that the medication details have been successfully updated.
     * </p>
     *
     * @param medicationDto A {@link MedicationDTO} object containing the updated medication details.
     * @return A {@link SuccessResponse<Medication>} object with the status OK, indicating successful update of medication details.
     */
    @PutMapping("/update")
    public SuccessResponse<Medication> updateMedication(@RequestBody MedicationDTO medicationDto) {
        medicationService.updateMedication(medicationDto);
        return new SuccessResponse<>(SuccessCode.MEDICATION_UPDATE, HttpStatus.OK);
    }

    /**
     * Retrieves the details of a single medication by its ID.
     * <p>
     * This method handles a POST request to retrieve the details of a specific medication identified by its ID.
     * It accepts a {@link SearchRequestDTO} object containing the medication ID. The {@link MedicationService#getMedicationById(SearchRequestDTO)}
     * method is called to fetch the medication details. Upon successful retrieval, a {@link SuccessResponse} with a status of OK is returned,
     * containing the medication details.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the search criteria (medication ID).
     * @return A {@link SuccessResponse<Medication>} object with the status OK, containing the medication details.
     */
    @PostMapping("/details")
    public SuccessResponse<Medication> getMedicationById(@RequestBody SearchRequestDTO requestDto) {
        return new SuccessResponse<>(SuccessCode.GET_MEDICATION,
                medicationService.getMedicationById(requestDto), HttpStatus.OK);
    }

    /**
     * Retrieves medication details by name.
     * <p>
     * This method processes a POST request to retrieve the details of a medication identified by its name.
     * It accepts a {@link SearchRequestDTO} object containing the medication name and uses the {@link MedicationService#getMedicationByName(SearchRequestDTO)}
     * method to fetch the medication details. The response is returned as a {@link ResponseEntity} with the status OK.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the medication name.
     * @return A {@link ResponseEntity<Medication>} containing the medication details.
     */
    @PostMapping("/getByName")
    public ResponseEntity<Medication> getMedicationByName(@RequestBody SearchRequestDTO requestDto) {
        return ResponseEntity.ok().body(medicationService.getMedicationByName(requestDto));
    }

    /**
     * Retrieves all medication details.
     * <p>
     * This method processes a POST request to retrieve details of all medications. It accepts a {@link SearchRequestDTO}
     * object as a request parameter. The {@link MedicationService#getAllMedications(SearchRequestDTO)} method is called
     * to fetch the list of all medications. The response is wrapped in a {@link SuccessResponse} with the status OK,
     * including the list of medications and the total count.
     * </p>
     *
     * @param requestObject A {@link SearchRequestDTO} object containing the request parameters.
     * @return A {@link SuccessResponse<MedicationDTO>} containing the list of medications and the total count.
     */
    @PostMapping("/list")
    public SuccessResponse<MedicationDTO> getAllMedications(@RequestBody SearchRequestDTO requestObject) {
        ResponseListDTO<MedicationDTO> medications = medicationService.getAllMedications(requestObject);

        return new SuccessResponse<>(SuccessCode.GET_MEDICATIONS, medications.getData(), medications.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * Soft deletes a medication by its ID.
     * <p>
     * This method handles a POST request to mark a medication as deleted in the system based on its ID.
     * It accepts a {@link SearchRequestDTO} object containing the ID of the medication to be deleted.
     * The {@link MedicationService#deleteMedicationById(SearchRequestDTO)} method is called to perform the deletion.
     * A {@link SuccessResponse<Boolean>} is returned with a status of OK, indicating the successful deletion.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the ID of the medication to be deleted.
     * @return A {@link SuccessResponse<Boolean>} object with the status OK, indicating successful deletion.
     */
    @PostMapping("/remove")
    public SuccessResponse<Boolean> deleteMedicationById(@RequestBody SearchRequestDTO requestDto) {
        medicationService.deleteMedicationById(requestDto);
        return new SuccessResponse<>(SuccessCode.MEDICATION_STATUS_UPDATE, HttpStatus.OK);
    }

    /**
     * Validates a medication's data.
     * <p>
     * This method handles a POST request to validate the data of a medication. It accepts a {@link MedicationDTO} object
     * containing the medication data to be validated. The {@link MedicationService#validateMedication(MedicationDTO)} method
     * is called to perform the validation. A {@link ResponseEntity<Boolean>} is returned with a status of OK, indicating
     * whether the medication data is valid or not.
     * </p>
     *
     * @param medicationDto A {@link MedicationDTO} object containing the medication data to be validated.
     * @return A {@link ResponseEntity<Boolean>} object with the status OK, indicating the result of the validation.
     */
    @PostMapping("/validate")
    public ResponseEntity<Boolean> validateMedication(@RequestBody MedicationDTO medicationDto) {
        medicationService.validateMedication(medicationDto);
        return ResponseEntity.ok().body(true);
    }

    /**
     * Retrieves a list of classifications based on the provided country ID.
     * <p>
     * This method handles a POST request to retrieve a list of medication classifications available in a specific country.
     * It accepts a {@link SearchRequestDTO} object containing the country ID. The {@link MedicationService#getClassifications(String)}
     * method is called with the country ID to fetch the classifications. A {@link SuccessResponse<Classification>} is returned
     * with a status of OK, including the list of classifications and their total count.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the country ID.
     * @return A {@link SuccessResponse<ClassificationDTO>} object with the status OK, containing the list of classifications and their total count.
     */
    @PostMapping("/classification-list")
    public SuccessResponse<ClassificationDTO> getClassifications(@RequestBody SearchRequestDTO requestDto) {
        List<ClassificationDTO> classifications = medicationService.getClassifications(requestDto.getCountryId());
        return new SuccessResponse<>(SuccessCode.GET_MEDICATION_CLASSIFICATION, classifications, Long.valueOf(classifications.size()),
                HttpStatus.OK);
    }

    /**
     * Retrieves a list of all dosage forms available in the system.
     * <p>
     * This method handles a POST request to fetch a list of all dosage forms. It calls the
     * {@link MedicationService#getDosageForms()} method to retrieve the list. Upon successful retrieval,
     * a {@link SuccessResponse<DosageForm>} is returned with a status of OK, including the list of dosage forms
     * and their total count.
     * </p>
     *
     * @return A {@link SuccessResponse<DosageForm>} object with the status OK, containing the list of dosage forms and their total count.
     */
    @PostMapping("/dosageform-list")
    public SuccessResponse<DosageForm> getDosageForms() {
        List<DosageForm> dosageForms = medicationService.getDosageForms();
        return new SuccessResponse<>(SuccessCode.GET_DOSAGE_FORMS, dosageForms, Long.valueOf(dosageForms.size()),
                HttpStatus.OK);
    }

    /**
     * Searches for medications by their name.
     * <p>
     * This method processes a POST request to search for medications based on their name. It accepts a {@link SearchRequestDTO}
     * object containing the medication name as a search criterion. The {@link MedicationService#searchMedicationByName(SearchRequestDTO)}
     * method is then called to perform the search. Upon successful search, a {@link SuccessResponse<MedicationDTO>} is returned
     * with a status of OK, including the list of medications that match the search criteria and their total count. This allows
     * for efficient retrieval of medication details when only the name is known.
     * </p>
     *
     * @param requestDto A {@link SearchRequestDTO} object containing the medication name as the search criterion.
     * @return A {@link SuccessResponse<MedicationDTO>} object with the status OK, containing the list of matching medications and their total count.
     */
    @PostMapping("/search")
    public SuccessResponse<MedicationDTO> searchMedicationByName(@RequestBody SearchRequestDTO requestDto) {
        ResponseListDTO<MedicationDTO> medications = medicationService.searchMedicationByName(requestDto);
        return new SuccessResponse<>(SuccessCode.GET_MEDICATIONS, medications.getData(), medications.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * <p>
     *  Searches for medications by their ids.
     * </p>
     *
     * @param ids A {@link List<Long>} object containing the medication ids as the search criterion.
     * @return A {@link List<MedicationDTO>} list of matching medications.
     */
    @PostMapping("/search-by-ids")
    public List<MedicationDTO> searchMedicationByName(@RequestBody List<Long> ids) {
        return medicationService.getMedicationsByIds(ids);
    }
}
