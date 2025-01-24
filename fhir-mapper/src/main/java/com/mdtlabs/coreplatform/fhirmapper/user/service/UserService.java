package com.mdtlabs.coreplatform.fhirmapper.user.service;

import java.util.List;

import org.hl7.fhir.r4.model.Practitioner;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;

/**
 * Interface for user-related operations in a FHIR server context.
 * <p>
 * Provides methods for creating, updating, and deleting user and organization records within a FHIR server.
 * This includes operations for both individual users and health facility organizations, allowing for the management
 * of user data and associated health facilities in compliance with FHIR standards.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Mar 04, 2024
 */
public interface UserService {

    /**
     * Creates a new user in a FHIR server based on the provided
     * UserRequestDTO data.
     *
     * @param userRequestDTO The `createUser` method takes a `UserRequestDTO` object as a parameter. This object
     *                       contains information about a user that needs to be created in a system. The method then processes
     *                       this information to create a new user record in the system using FHIR (Fast Healthcare
     *                       Interoperability Resources)
     * @return `UserRequestDTO` object after creating a new user in the
     * system based on the information provided in the `UserRequestDTO` parameter.
     */
    UserRequestDTO createUser(UserRequestDTO userRequestDTO);

    /**
     * Update a Practitioner object in a FHIR server.
     * It retrieves the Practitioner object by its ID, maps the details from a UserRequestDTO object to the Practitioner object,
     * and sends a PUT request to the FHIR server to update the Practitioner.
     *
     * @param userRequestDTO The UserRequestDTO object containing the details to be updated.
     * @return The UserRequestDTO object after the update.
     */
    UserRequestDTO updateUser(UserRequestDTO userRequestDTO);

    /**
     * Creates a new health facility organization in a FHIR server based
     * on the provided data.
     *
     * @param healthFacilityRequestDTO The `createOrganization` method takes a `HealthFacilityRequestDTO`
     *                                 object as a parameter. This object contains information about a health facility, such as its name,
     *                                 address, phone number, district, country, postal code, and a list of users associated with the
     *                                 facility.
     * @return `HealthFacilityRequestDTO`
     */
    HealthFacilityRequestDTO createOrganization(HealthFacilityRequestDTO healthFacilityRequestDTO);

    /**
     * Update an Organization object in a FHIR server.
     * It retrieves the Organization object by its ID, maps the details from a HealthFacilityRequestDTO object to the Organization object,
     * and sends a PUT request to the FHIR server to update the Organization.
     *
     * @param healthFacilityRequestDTO The HealthFacilityRequestDTO object containing the details to be updated.
     * @return The HealthFacilityRequestDTO object after the update.
     */
    HealthFacilityRequestDTO updateOrganization(HealthFacilityRequestDTO healthFacilityRequestDTO);

    /**
     * Deactivates a user in the FHIR server.
     * <p>
     * This method deactivates a user's record in the FHIR server based on the provided user ID.
     * </p>
     *
     * @param userId The ID of the user to be deactivated.
     */
    void deleteUser(String userId);

    /**
     * Deactivates an organization in the FHIR server.
     * <p>
     * This method deactivates an organization's record in the FHIR server based on the provided organization ID.
     * </p>
     *
     * @param organizationId The ID of the organization to be deactivated.
     */
    void deleteOrganization(String organizationId);

    /**
     * Deactivates a list of users in the FHIR server.
     * <p>
     * This method deactivates multiple user records in the FHIR server based on the provided list of user IDs.
     * </p>
     *
     * @param userIds The list of user IDs to be deactivated.
     */
    void deleteUsers(List<String> userIds);

    /**
     * Retrieves a user by their ID from the FHIR server.
     * <p>
     * This method fetches a user's details from the FHIR server based on the provided user ID. It returns a
     * {@link Practitioner} object containing the user's details.
     * </p>
     * <p>
     * This method is used to activate or deactivate the users from fhir
     * </p>
     *
     * @param userIds  {@link List<String>} The data required to activate or deactivate the user
     * @param isActivate {@link Boolean} It contains the district active and inactive status
     */
    void activateDeactivateUsers(List<String> userIds, Boolean isActivate);

    /**
     * get user by userId
     *
     * @param userId The ID of the user to retrieve.
     * @return The details of the retrieved user.
     */
    Practitioner getUserById(String userId);

    /**
     * <p>
     * This method is used to activate or deactivate the organization from fhir
     * </p>
     *
     * @param orgIds  {@link List<String>} The data required to activate or deactivate the organization
     * @param isActivate {@link Boolean} It contains the district active and inactive status
     */
    void activateDeactivateOrganizations(List<String> orgIds, Boolean isActivate);
    }
