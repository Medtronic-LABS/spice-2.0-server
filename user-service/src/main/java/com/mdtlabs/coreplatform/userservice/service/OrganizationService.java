package com.mdtlabs.coreplatform.userservice.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;

/**
 * <p>
 * OrganizationService interface is an interface for organization module that can be implemented
 * in any class.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public interface OrganizationService {

    /**
     * <p>
     * Creates a new organization.
     * This method takes an Organization object as input, which includes all necessary details for the organization to be created.
     * It persists the organization details in the database and returns the newly created Organization object.
     * </p>
     *
     * @param organization The Organization object containing the details of the organization to be created.
     * @return Organization The newly created Organization object with its details persisted in the database.
     */
    Organization createOrganization(Organization organization);

    /**
     * <p>
     * Updates an existing organization with the details provided in the OrganizationDTO object.
     * This method is responsible for applying changes to an organization's information in the database.
     * </p>
     *
     * @param organization The OrganizationDTO object containing the updated details of the organization.
     * @return Long The ID of the updated organization, indicating a successful update operation.
     */
    Long updateOrganization(OrganizationDTO organization);

    /**
     * <p>
     * Retrieves an organization based on the provided tenant ID.
     * This method is useful for fetching the details of an organization that is associated with a specific tenant.
     * It allows for easy access to an organization's information using the tenant's unique identifier.
     * </p>
     *
     * @param tenantId The unique identifier of the tenant whose organization details are to be retrieved.
     * @return Organization The Organization entity containing all the details of the requested organization.
     */
    Organization getOrganization(Long tenantId);

    /**
     * <p>
     * Retrieves a list of child organizations for a given parent organization ID.
     * This method is useful for hierarchical organization structures where an organization can have multiple child organizations.
     * </p>
     *
     * @param id The unique identifier of the parent organization.
     * @return List<Organization> A list of child organizations belonging to the specified parent organization.
     */
    List<Organization> getChildOrganization(Long id);

    /**
     * <p>
     * Deletes an organization by its unique identifier.
     * This method removes the organization from the database, effectively making it inaccessible for future operations.
     * </p>
     *
     * @param id The unique identifier of the organization to be deleted.
     */
    void deleteOrganization(Long id);

    /**
     * <p>
     * Validates the provided organization name and form name.
     * This method checks for the validity of the organization by name and form name.
     * </p>
     *
     * @param name     The name of the organization to validate.
     * @param formName The name of the form associated with the organization to validate.
     */
    void validateOrganization(String name, String formName);

    /**
     * <p>
     * Adds an admin to the organization.
     * This method takes a UserRequestDTO object containing the details of the admin to be added.
     * It processes the request to add the admin and returns a UserResponseDTO object containing the details of the added admin.
     * </p>
     *
     * @param request The UserRequestDTO object containing the details of the admin to be added.
     * @return UserResponseDTO The response object containing the details of the newly added admin.
     */
    UserResponseDTO addAdmin(UserRequestDTO request);

    /**
     * <p>
     * Updates an existing admin user.
     * This method accepts a UserRequestDTO object containing updated details for an existing admin.
     * It updates the admin details and returns a UserResponseDTO object with the updated details.
     * </p>
     *
     * @param request The UserRequestDTO object containing the updated details of the admin.
     * @return UserResponseDTO The response object containing the updated details of the admin.
     */
    UserResponseDTO updateAdmin(UserRequestDTO request);

    /**
     * <p>
     * Creates a new region.
     * This method takes a CountryRequestDTO object containing the details for the new region to be created.
     * It processes the request to create a new region and returns a Country object representing the newly created region.
     * </p>
     *
     * @param request The CountryRequestDTO object containing the details of the new region to be created.
     * @return Country The newly created region represented as a Country object.
     */
    Country createRegion(CountryRequestDTO request);

    /**
     * <p>
     * Creates a new health facility based on the provided request details.
     * This method processes the HealthFacilityRequestDTO to create and persist a new HealthFacility entity.
     * </p>
     *
     * @param request The HealthFacilityRequestDTO containing the details for the new health facility to be created.
     * @return HealthFacilityRequestDTO
     */
    HealthFacility createHealthFacility(HealthFacilityRequestDTO request);

    /**
     * <p>
     * Removes an admin from an organization.
     * This method processes the SearchRequestDTO to identify and remove the specified admin from an organization,
     * returning details of the removed admin.
     * </p>
     *
     * @param request The SearchRequestDTO containing the identification details of the admin to be removed.
     * @return UserResponseDTO The response object containing the details of the removed admin.
     */
    UserResponseDTO removeAdmin(SearchRequestDTO request);

    /**
     * <p>
     * Deletes a health facility based on the provided request details.
     * This method processes the SearchRequestDTO to identify and delete the specified health facility.
     * </p>
     *
     * @param request The SearchRequestDTO containing the identification details of the health facility to be deleted.
     */
    void deleteHealthFacility(SearchRequestDTO request);

    /**
     * <p>
     * Updates the health facility.
     * This method processes the HealthFacilityRequestDTO to update the organization details associated with a specific health facility.
     * </p>
     *
     * @param request The HealthFacilityRequestDTO containing the updated details for the health facility's organization.
     */
    void updateHealthFacility(HealthFacilityRequestDTO request);

    /**
     * <p>
     * This method is used to retrieve the organization for the given id.
     * </p>
     *
     * @param organizationId The id of the organization for which the organization to be retrieved
     * @return {@link Organization} The organization of the given id is returned
     */
    Organization getOrganizationById(long organizationId);

    /**
     * <p>
     * This method is used to create an district for the provided district details.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The District details which
     *                   to be created is given
     */
    void createDistrict(DistrictRequestDTO districtRequestDTO);

    /**
     * <p>
     * This method is used to activate or deactivate an organization of the given list of tenant ids.
     * </p>
     *
     * @param tenantIds {@link List<Long>} The list of tenantIds that belongs to the organizations which to
     *                  be activated or deactivated is given
     * @param isActive  {@link Boolean} Active status of the organization is given
     * @return {@link Boolean} Returns true if the organizations are activated or deactivated otherwise false
     */
    Boolean activateOrDeactivateOrganization(List<Long> tenantIds, boolean isActive, List<String> fhirIds);

    /**
     * <p>
     * This method is used to get a map of child organizations ids for a given tenant ID and form name.
     * </p>
     *
     * @param tenantId The tenant id of the organization is given
     * @param formName {@link String} The form name of the organization is given
     * @return {@link Map} A map of key and list of ids as value is returned
     */
    Map<String, List<Long>> getChildOrganizations(long tenantId, String formName);

    /**
     * <p>
     * This method is used to create the chiefdom for the provided chiefdom details.
     * </p>
     *
     * @param chiefdomRequestDTO {@link ChiefdomRequestDTO} The chiefdom details that
     *                         to be created is given
     */
    void createChiefdom(ChiefdomRequestDTO chiefdomRequestDTO);

    /**
     * <p>
     * Retrieves a list of organizations by the specified form name.
     * </p>
     *
     * @param formName the name of the form associated with the organizations
     * @return a list of organizations that match the specified form name
     */
    List<Organization> getOrganizations(String formName);

    /**
     * <p>
     * To validate parent organization.
     * </p>
     *
     * @param parentOrganizationId - parent organization Id
     * @param user - user
     */
    void validateParentOrganization(Long parentOrganizationId, User user);
}
