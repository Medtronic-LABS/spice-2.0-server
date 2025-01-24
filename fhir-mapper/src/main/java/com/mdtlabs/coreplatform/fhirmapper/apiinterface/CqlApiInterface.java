package com.mdtlabs.coreplatform.fhirmapper.apiinterface;

import java.util.Map;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * Defines the client interface for interacting with the CQL (Clinical Quality Language) service.
 * <p>
 * This interface facilitates communication with the CQL service by defining methods that correspond
 * to specific API endpoints. Each method is annotated to indicate the HTTP operation and the path
 * it targets on the server. The {@link FeignClient} annotation specifies the name of the service
 * and its URL, which is configured in the application's properties.
 * </p>
 * <p>
 * The interface methods use {@link ResponseEntity} to encapsulate the responses, allowing for flexible
 * error handling and access to HTTP status codes. Parameters for the API calls are annotated with
 * {@link RequestParam} or {@link RequestHeader} to indicate how they should be passed in the request.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024
 */
@FeignClient(name = "cql-service", url = "${app.cql-service}")
public interface CqlApiInterface {

    /**
     * Evaluates a CQL expression for a specific encounter.
     * <p>
     * This method posts a request to the CQL service to evaluate a CQL expression based on the
     * provided encounter ID. It requires authorization and client headers for authentication and
     * context information.
     * </p>
     *
     * @param encounterId The ID of the encounter to evaluate.
     * @param token       The authorization token.
     * @param client      The client identifier.
     * @return A {@link ResponseEntity} containing a map of the evaluation results.
     */
    @PostMapping("/cql/evaluate-encounter")
    ResponseEntity<Map<String, Object>> evaluateByEncounterId(
            @RequestParam(name = Constants.ID) String encounterId,
            @RequestHeader(Constants.AUTHORIZATION_HEADER) String token,
            @RequestHeader(Constants.HEADER_CLIENT) String client);

    /**
     * Retrieves ANC (Antenatal Care) results for a specific patient.
     * <p>
     * This method sends a GET request to the CQL service to retrieve ANC results based on the
     * patient ID. Similar to {@link #evaluateByEncounterId}, it requires authorization and client
     * headers.
     * </p>
     *
     * @param patientId The ID of the patient whose ANC results are to be retrieved.
     * @param token     The authorization token.
     * @param client    The client identifier.
     * @return A {@link ResponseEntity} containing a map of the ANC results.
     */
    @GetMapping("/cql/anc-result")
    ResponseEntity<Map<String, Object>> getAncResultByPatientId(
            @RequestParam(name = Constants.ID) String patientId,
            @RequestHeader(Constants.AUTHORIZATION_HEADER) String token,
            @RequestHeader(Constants.HEADER_CLIENT) String client);

}
