package com.mdtlabs.coreplatform.cqlservice.cql.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.cqlservice.model.dto.AncResultDTO;
import com.mdtlabs.coreplatform.cqlservice.model.dto.CqlRequestDTO;

/**
 * <p>
 * This class is a service class to perform Cql evaluation operations.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024
 */
public interface CqlService {

    /**
     * <p>
     * This method is used to evaluate the patient details by given library.
     * </p>
     *
     * @param requestDTO the requestDTO contains the patient details, and library.
     * @return the evaluated results.
     */
    Map<String, Object> evaluatePatientByLibrary(CqlRequestDTO requestDTO);

    /**
     * <p>
     * This method is used to evaluate the patient details by given library and expressions.
     * </p>
     *
     * @param requestDTO the requestDTO contains the patient details,library, and expressions.
     * @return the evaluated results.
     */
    Map<String, Object> evaluatePatientByLibraryAndExpressions(CqlRequestDTO requestDTO);

    /**
     * <p>
     * This method is used to evaluate the patient details.
     * </p>
     *
     * @param requestDTO the requestDTO contains the patient details.
     * @return the evaluated results.
     */
    Map<String, Object> evaluatePatient(CqlRequestDTO requestDTO);

    /**
     * <p>
     * This method is used to retrieve the evaluated result for patient by given patientId.
     * </p>
     *
     * @param patientId the id of the patient.
     * @return the evaluated results.
     */
    Map<String, Object> getResultByPatientId(String patientId);

    /**
     * <p>
     * This method is used to retrieve the evaluated anc results for the patient by given patientId.
     * </p>
     *
     * @param patientId the id of the patient.
     * @return the evaluated anc result.
     */
    Map<String, Object> getAncResultByPatientId(String patientId);

    /**
     * <p>
     * This method is used to retrieve the evaluated ANC results
     * for patients based on given village IDs and a creation date.
     * </p>
     *
     * @param cqlRequestDTO the request DTO which contains the list of village IDs and the creation date.
     * @return a list of evaluated ANC results.
     */
    List<AncResultDTO> getAncResultByVillageIdsAndCreatedAtDate(CqlRequestDTO cqlRequestDTO);

    /**
     * <p>
     * This method is used to evaluate the patient details by given encounterId.
     * </p>
     *
     * @param encounterId the encounter id of the patient.
     * @return the evaluated results.
     */
    Map<String, Object> evaluateByEncounter(String encounterId);

    /**
     * <p>
     * This method is used to clear and rest the cql and terminology builder.
     * </p>
     */
    void clearCqlAncTerminology();
}
