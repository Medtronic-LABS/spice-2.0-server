package com.mdtlabs.coreplatform.cqlservice.cql.controller;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.cqlservice.cql.service.CqlService;
import com.mdtlabs.coreplatform.cqlservice.model.dto.AncResultDTO;
import com.mdtlabs.coreplatform.cqlservice.model.dto.CqlRequestDTO;

/**
 * <p>
 * This class is a controller class to perform operation on Cql operations.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024
 */
@RestController
@RequestMapping("/cql")
public class CqlController {

    private final CqlService cqlService;

    public CqlController(CqlService cqlService) {
        this.cqlService = cqlService;
    }

    /**
     * <p>
     * This method is used to evaluate the patient for the specified expressions.
     * </p>
     *
     * @param requestDTO the requestDto that contains the library, expression and the patient request.
     * @return the result as a response.
     */
    @PostMapping("/expression")
    public ResponseEntity<Map<String, Object>> evaluatePatientByLibraryAndExpressions(
            @RequestBody CqlRequestDTO requestDTO) {
        return new ResponseEntity<>(cqlService.evaluatePatientByLibraryAndExpressions(requestDTO),
                HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to evaluate the patient for the specified library.
     * </p>
     *
     * @param requestDTO the requestDto that contains the library and patient request.
     * @return the result as a response.
     */
    @PostMapping("/library")
    public ResponseEntity<Map<String, Object>> evaluatePatientByLibrary(@RequestBody CqlRequestDTO requestDTO) {
        return new ResponseEntity<>(cqlService.evaluatePatientByLibrary(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to evaluate the patient for the predefined cql library.
     * </p>
     *
     * @param requestDTO the requestDto that contains the requests.
     * @return the result as a response.
     */
    @PostMapping("/evaluate")
    public ResponseEntity<Map<String, Object>> evaluatePatient(@RequestBody CqlRequestDTO requestDTO) {
        return new ResponseEntity<>(cqlService.evaluatePatient(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to evaluate the cql for the patient by the specified encounter id.
     * </p>
     *
     * @param encounterId the id of the encounter.
     * @return the evaluated result for the patient.
     */
    @PostMapping("/evaluate-encounter")
    public ResponseEntity<Map<String, Object>> evaluateByEncounterId(
            @RequestParam(name = Constants.ID) String encounterId) {
        return new ResponseEntity<>(cqlService.evaluateByEncounter(encounterId), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to retrieve the evaluated result of the patient by given patient id.
     * </p>
     *
     * @param patientId the id of the patient.
     * @return the result of the patient.
     */
    @GetMapping("/result")
    public ResponseEntity<Map<String, Object>> getResultByPatientId(
            @RequestParam(name = Constants.ID) String patientId) {
        return new ResponseEntity<>(cqlService.getResultByPatientId(patientId), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to retrieve the evaluated result of the patient by given patient id.
     * </p>
     *
     * @param patientId the id of the patient.
     * @return the result of the patient.
     */
    @GetMapping("/anc-result")
    public ResponseEntity<Map<String, Object>> getAncResultByPatientId(
            @RequestParam(name = Constants.ID) String patientId) {
        return new ResponseEntity<>(cqlService.getAncResultByPatientId(patientId), HttpStatus.OK);
    }

    /**
     * <p>
     * This endpoint is used to retrieve the ANC results for patients based on given village IDs and a creation date.
     * </p>
     *
     * @param requestDTO The request body containing the village IDs and the creation date.
     * @return A ResponseEntity containing a list of ANC results for the specified conditions.
     */
    @PostMapping("/anc-result/list")
    public ResponseEntity<List<AncResultDTO>> getAncResultByVillages(@RequestBody CqlRequestDTO requestDTO) {
        return new ResponseEntity<>(cqlService.getAncResultByVillageIdsAndCreatedAtDate(requestDTO), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to clear the cql and terminology builders.
     * </p>
     *
     * @return the success response.
     */
    @GetMapping("/clear")
    public ResponseEntity<String> clearCqlAndTerminology() {
        cqlService.clearCqlAncTerminology();
        return new ResponseEntity<>("Cleared Successfully", HttpStatus.OK);
    }
}
