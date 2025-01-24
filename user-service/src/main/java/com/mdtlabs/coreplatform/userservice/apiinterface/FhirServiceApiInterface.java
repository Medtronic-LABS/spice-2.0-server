package com.mdtlabs.coreplatform.userservice.apiinterface;

import feign.FeignException;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * <p>
 * This API interface used to interact with FHIR mapper
 * </p>
 *
 * @author Ragul Venkatesan created on Mar 04, 2024
 */

@FeignClient(name = "fhir-service", url = "${app.fhir-service}")
public interface FhirServiceApiInterface {

    /**
     * <p>
     * Creates a new user in the FHIR service.
     * This method sends a POST request to the FHIR service to create a new user with the provided details.
     * </p>
     *
     * @param token          The authorization token to access the FHIR service.
     * @param cookie         The authorization cookie to access the FHIR service.
     * @param client         The client identifier making the request.
     * @param userRequestDTO The user details to be created.
     * @return The created {@link UserRequestDTO} with assigned ID and other generated fields.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/user/create")
    UserRequestDTO createUser(@RequestHeader(Constants.AUTHORIZATION) String token,
                              @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                              @RequestHeader(Constants.CLIENT) String client,
                              @RequestBody UserRequestDTO userRequestDTO) throws FeignException;

    /**
     * <p>
     * Creates a new health facility in the FHIR service.
     * This method sends a POST request to the FHIR service to create a new health facility with the provided details.
     * The request includes an authorization token, cookie and a client identifier in the headers, and the health facility details in the body.
     * </p>
     *
     * @param token                    The authorization token to access the FHIR service.
     * @param cookie                   The authorization cookie to access the FHIR service.
     * @param client                   The client identifier making the request.
     * @param healthFacilityRequestDTO The health facility details to be created.
     * @return The created {@link HealthFacilityRequestDTO} with assigned ID and other generated fields.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/organization/create")
    HealthFacilityRequestDTO createOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody HealthFacilityRequestDTO healthFacilityRequestDTO) throws FeignException;

    /**
     * <p>
     * Updates an existing user in the FHIR service.
     * This method sends a POST request to the FHIR service to update an existing user with the provided details.
     * The request includes an authorization token, cookie and a client identifier in the headers, and the updated user details in the body.
     * </p>
     *
     * @param token          The authorization token to access the FHIR service.
     * @param cookie         The authorization cookie to access the FHIR service.
     * @param client         The client identifier making the request.
     * @param userRequestDTO The updated user details.
     * @return The updated {@link UserRequestDTO} with the new details.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/user/update")
    UserRequestDTO updateUser(@RequestHeader(Constants.AUTHORIZATION) String token,
                              @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                              @RequestHeader(Constants.CLIENT) String client,
                              @RequestBody UserRequestDTO userRequestDTO) throws FeignException;

    /**
     * <p>
     * Updates an existing health facility in the FHIR service.
     * This method sends a POST request to the FHIR service to update an existing health facility with the provided details.
     * The request includes an authorization token, cookie and a client identifier in the headers, and the updated health facility details in the body.
     * </p>
     *
     * @param token                    The authorization token to access the FHIR service.
     * @param cookie                   The authorization cookie to access the FHIR service.
     * @param client                   The client identifier making the request.
     * @param healthFacilityRequestDTO The updated health facility details.
     * @return The updated {@link HealthFacilityRequestDTO} with the new details.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/organization/update")
    HealthFacilityRequestDTO updateOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody HealthFacilityRequestDTO healthFacilityRequestDTO) throws FeignException;

    /**
     * <p>
     * Deletes a user from the FHIR service.
     * This method sends a POST request to the FHIR service to delete a user by their unique identifier.
     * The request includes an authorization token, cookie and a client identifier in the headers, and the user's unique identifier in the body.
     * </p>
     *
     * @param token  The authorization token to access the FHIR service.
     * @param cookie The authorization cookie to access the FHIR service.
     * @param client The client identifier making the request.
     * @param userId The unique identifier of the user to be deleted.
     * @return The {@link UserRequestDTO} of the deleted user.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/user/delete")
    UserRequestDTO deleteUser(@RequestHeader(Constants.AUTHORIZATION) String token,
                              @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                              @RequestHeader(Constants.CLIENT) String client,
                              @RequestBody String userId) throws FeignException;

    /**
     * <p>
     * Deletes a health facility from the FHIR service.
     * This method sends a POST request to the FHIR service to delete a health facility by its unique identifier.
     * The request includes an authorization token, cookie and a client identifier in the headers, and the health facility's unique identifier in the body.
     * </p>
     *
     * @param token            The authorization token to access the FHIR service.
     * @param cookie           The authorization cookie to access the FHIR service.
     * @param client           The client identifier making the request.
     * @param healthFacilityId The unique identifier of the health facility to be deleted.
     * @return The {@link HealthFacilityRequestDTO} of the deleted health facility.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service.
     */
    @PostMapping("/organization/delete")
    HealthFacilityRequestDTO deleteOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody String healthFacilityId) throws FeignException;

    /**
     * <p>
     * Deletes multiple users from the FHIR service.
     * This method sends a POST request to the FHIR service to delete multiple users identified by their unique identifiers.
     * The request includes an authorization token, cookie and a client identifier in the headers, and a list of the users' unique identifiers in the body.
     * </p>
     *
     * @param token   The authorization token required to access the FHIR service.
     * @param cookie  The authorization cookie required to access the FHIR service.
     * @param client  The client identifier making the request, used for tracking or permissions.
     * @param userIds A list of unique identifiers for the users to be deleted.
     * @return The {@link UserRequestDTO} of the last user in the list to be deleted, as a confirmation.
     * @throws FeignException If there is an issue with the Feign client or the FHIR service, such as network errors or unauthorized access.
     */
    @PostMapping("/users/delete")
    UserRequestDTO deleteUsers(@RequestHeader(Constants.AUTHORIZATION) String token,
                               @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                               @RequestHeader(Constants.CLIENT) String client,
                               @RequestBody List<String> userIds) throws FeignException;

    /**
     * <p>
     * This method is used to activate or deactivate the organization from fhir
     * </p>
     *
     * @param token    {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                 authenticate the user making the request
     * @param client {@link String} The client name used to authenticate the user based on the type of user(Mobile or Admin)
     * @param userIds  {@link List<String>} The data required to activate or deactivate the user
     * @return {@link UserRequestDTO} Updated user information
     */
    @PostMapping("/users/activate-deactivate")
    UserRequestDTO activateDeactivateUsers(@RequestHeader(Constants.AUTHORIZATION) String token,
                                           @RequestHeader(Constants.CLIENT) String client,
                                           @RequestBody List<String> userIds,
                                           @RequestParam Boolean isActivate) throws FeignException;

    /**
     * <p>
     * This method is used to activate or deactivate the organization from fhir
     * </p>
     *
     * @param token    {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                 authenticate the user making the request
     * @param client {@link String} The client name used to authenticate the user based on the type of user(Mobile or Admin)
     * @param orgIds  {@link List<String>} The data required to activate or deactivate the organization
     */
    @PostMapping("/organization/activate-deactivate")
    void activateDeactivateOrganization(@RequestHeader(Constants.AUTHORIZATION) String token,
                               @RequestHeader(Constants.CLIENT) String client,
                               @RequestBody List<String> orgIds, @RequestParam Boolean isActivate) throws FeignException;


}
