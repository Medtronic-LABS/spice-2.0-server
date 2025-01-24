package com.mdtlabs.coreplatform.fhirmapper.user.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

/**
 * <p>
 * A class that create users and  organizations.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Mar 04, 2024
 */
@RestController
@Validated
public class UserController {

    private final UserService userService;

    @Autowired
    public UserController(UserService userService) {
        this.userService = userService;
    }

    /**
     * Creates a new user based on the provided UserRequestDTO object.
     *
     * @param userRequestDTO The `userRequestDTO` parameter is of type `UserRequestDTO` and it represents
     *                       the data that is sent in the request body when creating a new user. The `@RequestBody` annotation
     *                       indicates that the `userRequestDTO` object will be automatically populated from the request body
     *                       JSON data.
     * @return `UserRequestDTO`
     */
    @PostMapping("/user/create")
    public UserRequestDTO createUser(@RequestBody UserRequestDTO userRequestDTO) {
        return userService.createUser(userRequestDTO);
    }

    /**
     * Creates a new organization using the provided HealthFacilityRequestDTO object.
     *
     * @param healthFacilityRequestDTO The `healthFacilityRequestDTO` parameter is of type
     *                                 `HealthFacilityRequestDTO`, which is a data transfer object used to transfer data related to
     *                                 creating a health facility organization. In this case, it is being passed as a request body in a
     *                                 POST request to create a new organization. The
     * @return `HealthFacilityRequestDTO`
     */
    @PostMapping("/organization/create")
    public HealthFacilityRequestDTO createOrganization(@RequestBody HealthFacilityRequestDTO healthFacilityRequestDTO) {
        return userService.createOrganization(healthFacilityRequestDTO);
    }

    /**
     * Update a user in FHIR system.
     * It takes a UserRequestDTO object as input, which contains the updated details of the user.
     * The method then calls the updateUser method of the UserService class, passing the UserRequestDTO object as an argument.
     * The UserService class handles the logic for updating the user in the database.
     *
     * @param userRequestDTO The UserRequestDTO object containing the updated details of the user.
     * @return The updated UserRequestDTO object.
     */
    @PostMapping("/user/update")
    public UserRequestDTO updateUser(@RequestBody UserRequestDTO userRequestDTO) {
        return userService.updateUser(userRequestDTO);
    }

    /**
     * Update an organization in FHIR system.
     * It takes a HealthFacilityRequestDTO object as input, which contains the updated details of the organization.
     * The method then calls the updateOrganization method of the UserService class, passing the HealthFacilityRequestDTO object as an argument.
     * The UserService class handles the logic for updating the organization in the database.
     *
     * @param healthFacilityRequestDTO The HealthFacilityRequestDTO object containing the updated details of the organization.
     * @return The updated HealthFacilityRequestDTO object.
     */
    @PostMapping("/organization/update")
    public HealthFacilityRequestDTO updateOrganization(@RequestBody HealthFacilityRequestDTO healthFacilityRequestDTO) {
        return userService.updateOrganization(healthFacilityRequestDTO);
    }

    /**
     * Deactivate a user in the FHIR server.
     *
     * @param userId The ID of the user to be deactivated.
     */
    @PostMapping("/user/delete")
    public void deleteUser(@RequestBody String userId) {
        userService.deleteUser(userId);
    }

    /**
     * Deactivate an organization in the FHIR server.
     *
     * @param organizationId The ID of the organization to be deactivated.
     */
    @PostMapping("/organization/delete")
    public void deleteOrganization(@RequestBody String organizationId) {
        userService.deleteOrganization(organizationId);
    }

    /**
     * Endpoint to deactivate multiple users in the FHIR server.
     * <p>
     * This method accepts a list of user IDs and deactivates each user in the system. It leverages the
     * {@link UserService#deleteUsers(List<String>)} method to perform the deactivation process. The user IDs are
     * provided in the request body as a JSON array.
     * </p>
     *
     * @param userIds A list of user IDs to be deactivated. The {@link RequestBody} annotation ensures that
     *                the list is automatically populated from the JSON array provided in the request body.
     */
    @PostMapping("/users/delete")
    public void deleteUsers(@RequestBody List<String> userIds) {
        userService.deleteUsers(userIds);
    }

    /**
     * <p>
     * This method is used to activate or deactivate an user of the given list of user ids.
     * </p>
     *
     * @param userIds {@link List<Long>} The list of userIds that belongs to the users which to
     *                  be activated or deactivated is given
     * @param isActivate  {@link Boolean} Active status of the organization is given
     * @return {@link Boolean} Returns true if the users are activated or deactivated otherwise false
     */
    @PostMapping("/users/activate-deactivate")
    public void activateDeactivateUsers(@RequestBody List<String> userIds, @RequestParam Boolean isActivate) {
        userService.activateDeactivateUsers(userIds, isActivate);
    }

    /**
     * <p>
     * This method is used to activate or deactivate an organization of the given list of tenant ids.
     * </p>
     *
     * @param orgIds {@link List<Long>} The list of orgIds that belongs to the organizations which to
     *                  be activated or deactivated is given
     * @param isActivate  {@link Boolean} Active status of the organization is given
     * @return {@link Boolean} Returns true if the organizations are activated or deactivated otherwise false
     */
    @PostMapping("/organization/activate-deactivate")
    public void activateDeactivateOrganization(@RequestBody List<String> orgIds,  @RequestParam Boolean isActivate) {
        userService.activateDeactivateOrganizations(orgIds, isActivate);
    }

}
