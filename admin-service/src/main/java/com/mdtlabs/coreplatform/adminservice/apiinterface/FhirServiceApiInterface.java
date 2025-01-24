package com.mdtlabs.coreplatform.adminservice.apiinterface;

import java.util.List;
import java.util.Map;

import feign.FeignException;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;

/**
 * <p>
 * This API interface used to interact with FHIR mapper
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 13, 2024
 */
@FeignClient(name = "fhir-service", url = "${app.fhir-service}")
public interface FhirServiceApiInterface {

    /**
     * <p>
     * Creates a new health facility in the FHIR service.
     * This method sends a POST request to the FHIR service to create a new health facility with the provided details.
     * It includes an authorization token, cookie and a client identifier in the headers, and the health facility details in the body.
     * </p>
     *
     * @param token                    The authorization token to access the FHIR service.
     * @param cookie                   The authorization cookie to access the FHIR service.
     * @param client                   The client identifier making the request.
     * @param healthFacilityRequestDTO The health facility details to be created.
     * @return The created {@link HealthFacilityRequestDTO} with assigned identifiers and details.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/organization/create")
    HealthFacilityRequestDTO createOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody HealthFacilityRequestDTO healthFacilityRequestDTO) throws FeignException;

    /**
     * <p>
     * Updates an existing health facility in the FHIR service.
     * This method sends a POST request to the FHIR service to update an existing health facility with the provided details.
     * It includes an authorization token, cookie and a client identifier in the headers, and the updated health facility details in the body.
     * </p>
     *
     * @param token                    The authorization token required to access the FHIR service.
     * @param cookie                   The authorization cookie to access the FHIR service.
     * @param client                   The client identifier making the request, used for tracking or permissions.
     * @param healthFacilityRequestDTO The updated health facility details.
     * @return The updated {@link HealthFacilityRequestDTO} with the new details.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service, such as network errors or unauthorized access.
     */
    @PostMapping("/organization/update")
    HealthFacilityRequestDTO updateOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody HealthFacilityRequestDTO healthFacilityRequestDTO) throws FeignException;

    /**
     * <p>
     * Deletes an existing health facility from the FHIR service.
     * This method sends a POST request to the FHIR service to delete a health facility identified by its unique identifier.
     * It includes an authorization token, cookie and a client identifier in the headers, and the unique identifier of the health facility to be deleted in the body.
     * </p>
     *
     * @param token            The authorization token required to access the FHIR service.
     * @param cookie           The authorization cookie to access the FHIR service.
     * @param client           The client identifier making the request, used for tracking or permissions.
     * @param healthFacilityId The unique identifier of the health facility to be deleted.
     * @return The {@link HealthFacilityRequestDTO} of the deleted health facility as a confirmation of deletion.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service, such as network errors or unauthorized access.
     */
    @PostMapping("/organization/delete")
    HealthFacilityRequestDTO deleteOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody String healthFacilityId) throws FeignException;

    @PostMapping("/fhir-resources")
    List<String> getAllFhirResources(@RequestHeader(Constants.AUTHORIZATION) String token,
                                     @RequestHeader(Constants.CLIENT) String client) throws FeignException;

    @PostMapping("/fhir-resources/{resource}")
    List<Map<String, String>> getKeysForFhirResource(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                     @RequestHeader(Constants.CLIENT) String client,
                                                     @PathVariable String resource) throws FeignException;


}
