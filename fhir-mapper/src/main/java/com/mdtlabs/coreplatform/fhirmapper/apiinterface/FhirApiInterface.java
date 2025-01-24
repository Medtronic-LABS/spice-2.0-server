package com.mdtlabs.coreplatform.fhirmapper.apiinterface;

import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.FeignConfig;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Interface for interacting with FHIR (Fast Healthcare Interoperability Resources) server APIs.
 * <p>
 * This interface defines methods for creating and retrieving FHIR resources from a FHIR server.
 * It uses Feign, a declarative web service client, to simplify calling RESTful services.
 * </p>
 * <p>
 * The {@link FeignClient} annotation specifies the name of the FHIR server and its URL, which is
 * configured in the application's properties. The {@link FeignConfig} class is used for additional
 * Feign client configuration.
 * </p>
 */
@FeignClient(name = "fhir-server", url = "${app.fhir-server-url}", configuration = FeignConfig.class)
public interface FhirApiInterface {

    /**
     * Creates a FHIR bundle.
     * <p>
     * This method sends a POST request to the FHIR server to create a new FHIR bundle with the
     * provided data.
     * </p>
     *
     * @param request The FHIR bundle data to create.
     * @return A {@link ResponseEntity} containing the response from the FHIR server.
     */
    @PostMapping("/fhir")
    public ResponseEntity<Object> createBundle(@RequestBody Map<String, Object> request);

    /**
     * Retrieves a FHIR Patient resource.
     * <p>
     * This method sends a POST request to the FHIR server to retrieve a FHIR Patient resource
     * based on the provided search criteria.
     * </p>
     *
     * @param request The search criteria for retrieving the patient.
     * @param count   The maximum number of patients to return.
     * @param offset  The offset for pagination of the patient list.
     * @return A {@link ResponseEntity} containing the response from the FHIR server.
     */
    @PostMapping("/fhir/Patient")
    public ResponseEntity<Object> getPatient(@RequestParam String request, @RequestParam String count, @RequestParam String offset);

}
