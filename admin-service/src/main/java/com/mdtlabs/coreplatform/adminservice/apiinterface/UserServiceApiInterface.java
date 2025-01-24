package com.mdtlabs.coreplatform.adminservice.apiinterface;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;


import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * <p>
 * UserApiInterface is a interface that is used to communicate with user service.
 * </p>
 *
 * @author Karthick Murugesan Created on Jan 11, 2024
 */
@FeignClient(name = "user-service", url = "${app.user-service}")
public interface UserServiceApiInterface {

    /**
     * Adds a new administrator user.
     * <p>
     * This method sends a POST request to the user-service to add a new administrator user with the provided details.
     * It requires an authorization token, cookie and a client identifier in the headers for authentication and
     * authorization purposes.
     * </p>
     *
     * @param authorization The authorization token required to access the user-service.
     * @param cookie        The authorization cookie required to access the user-service.
     * @param client        The client identifier making the request.
     * @param request       The details of the administrator user to be added.
     * @return A {@link ResponseEntity} containing the added {@link UserResponseDTO} with the user's details.
     */
    @PostMapping("/organization/add-user")
    public ResponseEntity<UserResponseDTO> addAdmin(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                    @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                    @RequestHeader(Constants.CLIENT) String client, @RequestBody UserRequestDTO request);

    /**
     * Updates an existing administrator user's details.
     * <p>
     * This method sends a POST request to the user-service to update an existing administrator user with the provided details.
     * It requires an authorization token, cookie and a client identifier in the headers for authentication and authorization purposes.
     * </p>
     *
     * @param authorization The authorization token required to access the user-service.
     * @param cookie        The authorization cookie required to access the user-service.
     * @param client        The client identifier making the request.
     * @param request       The updated details of the administrator user.
     * @return A {@link ResponseEntity} containing the updated {@link UserResponseDTO} with the user's details.
     */
    @PostMapping("/organization/update-user")
    public ResponseEntity<UserResponseDTO> updateAdmin(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                       @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                       @RequestHeader(Constants.CLIENT) String client, @RequestBody UserRequestDTO request);

    /**
     * Removes an administrator user.
     * <p>
     * This method sends a POST request to the user-service to remove an administrator user identified by the provided search criteria.
     * It requires an authorization token, cookie and a client identifier in the headers for authentication and authorization purposes.
     * </p>
     *
     * @param authorization The authorization token required to access the user-service.
     * @param cookie        The authorization cookie required to access the user-service.
     * @param client        The client identifier making the request.
     * @param request       The search criteria to identify the administrator user to be removed.
     * @return A {@link ResponseEntity} containing the {@link UserResponseDTO} of the removed user as a confirmation of the action.
     */
    @PostMapping("/organization/remove-user")
    public ResponseEntity<UserResponseDTO> removeAdmin(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                       @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                       @RequestHeader(Constants.CLIENT) String client, @RequestBody SearchRequestDTO request);

    /**
     * Retrieves a list of peer supervisors for a given tenant.
     * <p>
     * This method sends a POST request to the user-service to retrieve a list of peer supervisors based on the provided search criteria.
     * It is secured with an authorization token, cookie and requires a client identifier in the headers for authentication and authorization purposes.
     * The method is useful for fetching supervisors who are peers within the same organizational structure or tenant.
     * </p>
     *
     * @param authorization The authorization token required to access the user-service.
     * @param cookie        The authorization cookie required to access the user-service.
     * @param client        The client identifier making the request.
     * @param request       The search criteria used to identify the tenant and potentially filter the supervisors.
     * @return A {@link ResponseEntity} containing a list of {@link UserResponseDTO} representing the peer supervisors.
     */
    @PostMapping("/user/tenant-peer-supervisors")
    public ResponseEntity<List<UserResponseDTO>> getPeerSupervisors(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                                    @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                                    @RequestHeader(Constants.CLIENT) String client, @RequestBody SearchRequestDTO request);

    /**
     * Updates the health facility information associated with an organization.
     * <p>
     * This method sends a POST request to the user-service to update the health facility information for an organization.
     * It requires an authorization token, cookie and a client identifier in the headers for authentication and authorization purposes.
     * The request body must contain the updated health facility information. This method is crucial for maintaining accurate
     * and up-to-date health facility data within the organization's records.
     * </p>
     *
     * @param authorization The authorization token required to access the user-service.
     * @param cookie        The authorization cookie required to access the user-service.
     * @param client        The client identifier making the request.
     * @param request       The health facility information to be updated.
     * @return A {@link ResponseEntity} indicating the success or failure of the update operation.
     */
    @PostMapping("/organization/update-healthfacility")
    public ResponseEntity<Boolean> updateHealthFacilityOrganization(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                                    @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                                    @RequestHeader(Constants.CLIENT) String client, @RequestBody HealthFacilityRequestDTO request);

    /**
     * <p>
     * This method is used to retrieve details of an organization by its ID, using authorization and tenant ID headers.
     * </p>
     *
     * @param authorization        {@link String} The "Authorization" header in the HTTP request, which is typically used
     *                     to send a token or credentials for authentication purposes is given
     * @param userTenantId {@link Long} The ID of the tenant to which the user associated is given
     * @param id           {@link Long} The ID of the organization which need to searched is given
     * @return {@link ResponseEntity<Organization>} The organization for the given id is returned with status
     */
    @GetMapping("/organization/details/{id}")
    public ResponseEntity<Organization> getOrganizationById(@RequestHeader("Authorization") String authorization,
        @RequestHeader(Constants.HEADER_TENANT_ID) Long userTenantId, @PathVariable("id") Long id, @RequestHeader(Constants.CLIENT) String client);

    /**
     * <p>
     * This method is used to retrieve a list of user organization DTOs based on a list of tenant IDs and the
     * user's own tenant ID.
     * </p>
     *
     * @param authorization        {@link String} The "Authorization" header in the HTTP request, which is typically used
     *                     to send a token or credentials for authentication purposes is given
     * @param request {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of user information
     * @return {@link List<UserResponseDTO>} A list of user organization DTOs for given details is retrieved
     * and returned.
     */
    @PostMapping("/user/get-by-tenants")
    public List<UserResponseDTO> getUsersByTenantIds(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                     @RequestHeader(Constants.CLIENT) String client, @RequestBody SearchRequestDTO request);

    /**
     * <p>
     * This method is used to update an organization with the provided organization DTO, using the authorization
     * token and user tenant ID from the request headers.
     * </p>
     *
     * @param token           {@link String} The "Authorization" header in the HTTP request, which is typically used to
     *                        send a token or credentials for authentication purposes is given
     * @param userTenantId    {@link Long} The ID of the tenant to which the user associated is given
     * @param organizationDto {@link OrganizationDTO} The updated information for an organization is given
     */
    @PutMapping("/organization/update")
    public void updateOrganization(@RequestHeader("Authorization") String token,
                                           @RequestHeader(Constants.HEADER_TENANT_ID) Long userTenantId,
                                           @RequestBody OrganizationDTO organizationDto, @RequestHeader(Constants.CLIENT) String client);

    /**
     * <p>
     * This method is used to activate or deactivate a list of organizations based on a boolean value.
     * </p>
     *
     * @param token        {@link String} The "Authorization" header in the HTTP request, which is typically used
     *                     to send a token or credentials for authentication purposes is given
     * @param userTenantId {@link Long} The ID of the tenant to which the user associated is given
     * @param tenantIds    {@link List<Long>} The IDs of the tenants for which the organization's active status
     *                     needs to be updated is given
     * @param isActive     {@link Boolean} A boolean value that is used to specify whether the organization
     *                     should be activated or deactivated is given
     */
    @PostMapping("/organization/activate-deactivate")
    public void activateOrDeactivateOrg(@RequestHeader("Authorization") String token,
                                           @RequestHeader(Constants.HEADER_TENANT_ID) Long userTenantId,
                                            @RequestBody List<Long> tenantIds,
                                        @RequestParam Boolean isActive,
                                           @RequestParam List<String> fhirIds
                                           , @RequestHeader(Constants.CLIENT) String client);

    /**
     * <p>
     * This method is used to update the active status of a user for a list of tenant IDs.
     * </p>
     *
     * @param token        {@link String} The "Authorization" header in the HTTP request, which is typically used to
     *                     send a token or credentials for authentication purposes is given
     * @param userTenantId {@link Long} The ID of the tenant to which the user associated is given
     * @param tenantIds    {@link List<Long>} The IDs of the tenants for which the user's active status
     *                     needs to be updated is given
     * @param isActive     A boolean value that is used to specify whether the user should be activated or
     *                     deactivated is given
     */
    @PostMapping("/user/update-active-status")
    public void activateOrDeactivateUser(@RequestHeader("Authorization") String token,
                                         @RequestHeader(Constants.HEADER_TENANT_ID) Long userTenantId, @RequestBody List<Long> tenantIds,
                                         @RequestParam("isActive") boolean isActive, @RequestHeader(Constants.CLIENT) String client);

    @PostMapping("/user/validate-peer-supervisors")
    public ResponseEntity<Boolean> validatePeerSupervisors(@RequestHeader(Constants.AUTHORIZATION) String authorization,
            @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
            @RequestHeader(Constants.CLIENT) String client, @RequestBody SearchRequestDTO requestDTO);
    
    
}
