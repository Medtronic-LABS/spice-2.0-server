package com.mdtlabs.coreplatform.spiceservice.apiinterface;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageDTO;


/**
 * Feign client interface for interacting with the user-service.
 * <p>
 * This interface defines the API calls that can be made to the user-service. It uses Feign, a declarative web service client,
 * to simplify calling RESTful web services. The {@code @FeignClient} annotation specifies the name of the microservice
 * and the base URL for the service calls. Methods within this interface are mapped to specific service endpoints.
 * </p>
 */
@FeignClient(name = "user-service", url = "${app.user-service}")
public interface UserServiceApiInterface {

    /**
     * Retrieves the villages associated with a given user.
     * <p>
     * This method sends a POST request to the user-service to fetch the villages associated with the specified user ID.
     * The authorization token and client information are passed in the request headers.
     * </p>
     *
     * @param authorization The authorization token to be included in the request header.
     * @param client        The client information to be included in the request header.
     * @param userId        The ID of the user whose villages are to be retrieved.
     * @return A {@link ResponseEntity} containing a {@link UserResponseDTO} with the user's village information.
     */
    @PostMapping("/user/user-villages/{userId}")
    public ResponseEntity<UserResponseDTO> getUserVillages(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                           @RequestHeader(Constants.HEADER_CLIENT) String client, @PathVariable("userId") Long userId);

    /**
     * Retrieves all mobile users.
     * <p>
     * This method sends a POST request to the user-service to fetch all users marked as mobile users.
     * The authorization token and client information are passed in the request headers.
     * </p>
     *
     * @param authorization The authorization token to be included in the request header.
     * @param client        The client information to be included in the request header.
     * @return A list of {@link UserResponseDTO} objects, each representing a mobile user.
     */
    @PostMapping("/user/mobile-users")
    List<UserResponseDTO> getAllMobileUsers(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                            @RequestHeader(Constants.CLIENT) String client);

    /**
     * Retrieves all mobile users.
     * <p>
     * This method sends a POST request to the user-service to fetch all users marked as mobile users.
     * The authorization token and client information are passed in the request headers.
     * </p>
     *
     * @param authorization The authorization token to be included in the request header.
     * @param cookie        The cookie to be included in the request header
     * @param client        The client information to be included in the request header.
     * @return A list of {@link UserResponseDTO} objects, each representing a mobile user.
     */
    @PostMapping("/user/peer-supervisor/chw-list")
    List<UserVillageDTO> getUsersByPeerSupervisorId(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                    @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                    @RequestHeader(Constants.CLIENT) String client, @RequestBody
                                                            SearchRequestDTO request);

    /**
     * <p>
     * Retrieves a list of organizations by the specified form name.
     * </p>
     * 
     * @param authorization the authorization token for the request
     * @param client        the client identifier for the request
     * @param formName      the name of the form associated with the organizations
     * @return a list of organizations that match the specified form name
     */
    @PostMapping("/organization/organizations-by-form-name/{formName}")
    List<Organization> getOrganizationsByFormName(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                  @RequestHeader(Constants.CLIENT) String client, @PathVariable(Constants.FORM_NAME) String formName);

    /**
     * Used to get users based on role name
     * @param authToken - user token
     * @param tenantId  - user tenant Id
     * @param roleName  - name of the role
     * @return
     */
    @PostMapping("/user/get-by-rolename")
    public ResponseEntity<List<UserResponseDTO>> getUsersByRoleName(@RequestHeader("Authorization") String authToken,
                                                                    @RequestHeader(Constants.HEADER_TENANT_ID) Long tenantId, @RequestBody String roleName);
}
