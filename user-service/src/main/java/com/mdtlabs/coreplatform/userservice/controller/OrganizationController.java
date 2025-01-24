package com.mdtlabs.coreplatform.userservice.controller;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ChiefdomRequestDTO;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import jakarta.validation.Valid;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.userservice.message.SuccessCode;
import com.mdtlabs.coreplatform.userservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.userservice.service.OrganizationService;
import com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictOrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;

import java.util.List;

/**
 * <p>
 * Organization Controller class that defines various REST API endpoints for managing organizations,
 * including updating, retrieving, creating, and deleting various entities such as admin users, countries,
 * and health facility.
 * </p>
 *
 * @author JeyahariniTA created on feb 14, 2023
 */
@RestController
@RequestMapping("/organization")
public class OrganizationController {

    private final OrganizationService organizationService;

    private final ModelMapper mapper;

    public OrganizationController(OrganizationService organizationService, ModelMapper mapper) {
        this.organizationService = organizationService;
        this.mapper = mapper;
    }

    /**
     * <p>
     * Creates an Organization.
     * This method takes an OrganizationDTO as a parameter, maps it to an Organization entity,
     * creates the organization using the organizationService, and returns the created organization.
     * </p>
     *
     * @param request The OrganizationDTO containing the details of the organization to be created.
     * @return A ResponseEntity containing the created Organization and the HTTP status.
     */
    @PostMapping("/create")
    public ResponseEntity<Organization> createOrganization(@RequestBody OrganizationDTO request) {
        Organization organization = mapper.map(request, Organization.class);
        organization = organizationService.createOrganization(organization);
        return new ResponseEntity<>(organization, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Updates an Organization.
     * This method takes an OrganizationDTO as a parameter, updates the organization using the organizationService, and returns the tenantId of the updated organization.
     * </p>
     *
     * @param request The OrganizationDTO containing the updated details of the organization.
     * @return A ResponseEntity containing the tenantId of the updated Organization and the HTTP status.
     */
    @PutMapping("/update")
    public ResponseEntity<Long> updateOrganization(@RequestBody OrganizationDTO request) {
        Long tenantId = organizationService.updateOrganization(request);
        return new ResponseEntity<>(tenantId, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Retrieves an Organization's details.
     * This method takes a tenantId as a parameter, retrieves the organization using the organizationService, and returns the organization's details.
     * </p>
     *
     * @param tenantId The tenantId of the organization to be retrieved.
     * @return A ResponseEntity containing the OrganizationDTO of the retrieved organization and the HTTP status.
     */
    @PostMapping("/details")
    public ResponseEntity<OrganizationDTO> getOrganization(@RequestParam("tenantId") Long tenantId) {
        Organization organization = organizationService.getOrganization(tenantId);
        return new ResponseEntity<>(mapper.map(organization, OrganizationDTO.class), HttpStatus.OK);
    }

    /**
     * Deletes an Organization.
     * This method takes an id as a parameter, deletes the organization using the organizationService, and returns a success message.
     *
     * @param id The id of the organization to be deleted.
     * @return A ResponseEntity containing a success message and the HTTP status.
     */
    @PutMapping("/delete")
    public ResponseEntity<String> deleteOrganization(@RequestParam("id") Long id) {
        organizationService.deleteOrganization(id);
        return new ResponseEntity<>("Organization deleted succesfully", HttpStatus.OK);
    }

    /**
     * <p>
     * Adds an admin user to an Organization.
     * This method takes a UserRequestDTO as a parameter, adds the admin user to the organization using the organizationService, and returns the UserResponseDTO of the added admin user.
     * </p>
     *
     * @param request The UserRequestDTO containing the details of the admin user to be added.
     * @return A ResponseEntity containing the UserResponseDTO of the added admin user and the HTTP status.
     */
    @PostMapping("/add-user")
    public ResponseEntity<UserResponseDTO> addAdmin(@RequestBody UserRequestDTO request) {
        return new ResponseEntity<>(organizationService.addAdmin(request), HttpStatus.CREATED);
    }

    /**
     * <p>
     * Updates an admin user in an Organization.
     * This method takes a UserRequestDTO as a parameter, updates the admin user in the organization using the organizationService, and returns the UserResponseDTO of the updated admin user.
     * </p>
     *
     * @param request The UserRequestDTO containing the updated details of the admin user.
     * @return A ResponseEntity containing the UserResponseDTO of the updated admin user and the HTTP status.
     */
    @PostMapping("/update-user")
    public ResponseEntity<UserResponseDTO> updateAdmin(@RequestBody UserRequestDTO request) {
        return new ResponseEntity<>(organizationService.updateAdmin(request), HttpStatus.OK);
    }

    /**
     * <p>
     * Removes an admin user from an Organization.
     * This method takes a SearchRequestDTO as a parameter, removes the admin user from the organization using the organizationService, and returns the UserResponseDTO of the removed admin user.
     * </p>
     *
     * @param request The SearchRequestDTO containing the details of the admin user to be removed.
     * @return A ResponseEntity containing the UserResponseDTO of the removed admin user and the HTTP status.
     */
    @PostMapping("/remove-user")
    public ResponseEntity<UserResponseDTO> deleteAdmin(@RequestBody SearchRequestDTO request) {
        return new ResponseEntity<>(organizationService.removeAdmin(request), HttpStatus.OK);
    }

    /**
     * <p>
     * Creates a country organization.
     * This method takes a CountryRequestDTO as a parameter, creates the country organization using the organizationService, and returns a success response.
     * </p>
     *
     * @param request The CountryRequestDTO containing the details of the country organization to be created.
     * @return A SuccessResponse containing the created Country and the HTTP status.
     */
    @PostMapping("/create-country")
    public SuccessResponse<Country> createCountry(@RequestBody CountryRequestDTO request) {
        organizationService.createRegion(request);
        return new SuccessResponse<>(SuccessCode.SAVE_COUNTRY, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Creates a health facility organization.
     * This method takes a HealthFacilityRequestDTO as a parameter, creates the health facility organization using the organizationService, and returns a success response.
     * </p>
     *
     * @param request The HealthFacilityRequestDTO containing the details of the health facility organization to be created.
     * @return A SuccessResponse containing the created HealthFacilityRequestDTO and the HTTP status.
     */
    @PostMapping("/create-healthfacility")
    public SuccessResponse<HealthFacility> createHealthFacility(@RequestBody HealthFacilityRequestDTO request) {
        organizationService.createHealthFacility(request);
        return new SuccessResponse<>(SuccessCode.SAVE_HEALTH_FACILITY, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Deletes a health facility organization.
     * This method takes a SearchRequestDTO as a parameter, deletes the health facility organization using the organizationService, and returns a success response.
     * </p>
     *
     * @param request The SearchRequestDTO containing the details of the health facility organization to be deleted.
     * @return A SuccessResponse containing the deleted HealthFacility and the HTTP status.
     */
    @PostMapping("/delete-healthfacility")
    public SuccessResponse<HealthFacility> deleteHealthFacility(@RequestBody SearchRequestDTO request) {
        organizationService.deleteHealthFacility(request);
        return new SuccessResponse<>(SuccessCode.ORGANIZATION_DELETE, HttpStatus.OK);
    }

    /**
     * <p>
     * Updates a health facility organization.
     * This method takes a HealthFacilityRequestDTO as a parameter, updates the health facility organization using the organizationService, and returns a success response.
     * </p>
     *
     * @param request The HealthFacilityRequestDTO containing the details of the health facility organization to be updated.
     * @return A ResponseEntity containing a Boolean value indicating the success of the operation and the HTTP status.
     */
    @PostMapping("/update-healthfacility")
    public ResponseEntity<Boolean> updateHealthFacilityOrganization(@RequestBody HealthFacilityRequestDTO request) {
        organizationService.updateHealthFacility(request);
        return new ResponseEntity<>(Boolean.TRUE, HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to get organization information by organization id.
     * </p>
     *
     * @param organizationId - the id of the organization
     * @return Organization - organization information
     */
    @GetMapping("/details/{id}")
    public ResponseEntity<Organization> getOrganizationById(
            @PathVariable(value = Constants.ID) long organizationId) {
        return ResponseEntity.ok().body(organizationService.getOrganizationById(organizationId));
    }

    /**
     * <p>
     * This method is used to create an district for the provided district details.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The District details which
     *                               to be created is given
     * @return {@link SuccessResponse<DistrictOrganizationDTO>} The District for the given details is created
     * and retrieved with status
     */
    @UserTenantValidation
    @PostMapping("/create-district")
    public SuccessResponse<DistrictOrganizationDTO> createDistrict(
            @Valid @RequestBody DistrictRequestDTO districtRequestDTO) {
        organizationService.createDistrict(districtRequestDTO);
        return new SuccessResponse<>(SuccessCode.ORGANIZATION_SAVE, HttpStatus.CREATED);
    }


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
    @PostMapping("/activate-deactivate")
    public Boolean activateOrDeactivateOrganization(@RequestBody List<Long> tenantIds, @RequestParam Boolean isActive,
                                                    @RequestParam List<String> fhirIds) {
        return organizationService.activateOrDeactivateOrganization(tenantIds, isActive, fhirIds);

    }

    /**
     * <p>
     * This method is used to create the chiefdom for the provided chiefdom details.
     * </p>
     *
     * @param chiefdomRequestDTO {@link ChiefdomRequestDTO} The chiefdom details that
     *                                     to be created is given
     * @return {@link SuccessResponse<ChiefdomRequestDTO>} The chiefdom for the given details is created
     * and retrieved with status
     */
    @UserTenantValidation
    @PostMapping("/create-chiefdom")
    public SuccessResponse<ChiefdomRequestDTO> createOperatingUnit(
            @Valid @RequestBody ChiefdomRequestDTO chiefdomRequestDTO) {
        organizationService.createChiefdom(chiefdomRequestDTO);
        return new SuccessResponse<>(SuccessCode.ORGANIZATION_SAVE, HttpStatus.CREATED);
    }

    /**
     * <p>
     * Retrieves a list of organizations by the specified form name.
     * </p>
     *
     * @param formName the name of the form associated with the organizations
     * @return a list of organizations that match the specified form name
     */
    @PostMapping("/organizations-by-form-name/{formName}")
    public List<Organization> getOrganizationsByFormName(@PathVariable String formName) {
        return organizationService.getOrganizations(formName);
    }
}
