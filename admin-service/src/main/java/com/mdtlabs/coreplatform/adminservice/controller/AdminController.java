package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;
import java.util.Objects;

import jakarta.validation.Valid;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityFilterDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.RequestDTO;
import com.mdtlabs.coreplatform.adminservice.service.ChiefdomService;
import com.mdtlabs.coreplatform.adminservice.service.CountryService;
import com.mdtlabs.coreplatform.adminservice.service.DistrictService;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;

/**
 * <p>
 * The AdminController class is a REST controller that handles requests related to Admin.
 * </p>
 *
 * @author Karthick M Created on 11 Jan 2024
 */
@RestController
@RequestMapping
public class AdminController {

    private final HealthFacilityService healthFacilityService;
    private final CountryService countryService;
    private final DistrictService districtService;
    private final ChiefdomService chiefdomService;
    private ModelMapper modelMapper = new ModelMapper();


    @Autowired
    public AdminController(HealthFacilityService healthFacilityService, CountryService countryService,
                           DistrictService districtService, ChiefdomService chiefdomService) {
        this.healthFacilityService = healthFacilityService;
        this.countryService = countryService;
        this.districtService = districtService;
        this.chiefdomService = chiefdomService;
    }


    /**
     * Add a new country.
     * <p>
     * This endpoint handles the creation of a new country entity based on the provided country details.
     * The {@link CountryRequestDTO} contains all necessary information.
     * Upon successful creation, the newly created {@link Country} entity is returned.
     * </p>
     *
     * @param request The country details encapsulated within a {@link CountryRequestDTO}.
     * @return The created {@link Country} entity.
     */
    @PostMapping("/country/create")
    public Country createCountry(@RequestBody CountryRequestDTO request) {
        return countryService.createCountry(request);
    }

    /**
     * Updates the details of an existing country.
     * <p>
     * This method handles the HTTP PUT request to update the details of a country specified by the {@link CountryRequestDTO}.
     * It logs the operation, updates the country details through the {@code countryService}, and returns a success response.
     * </p>
     *
     * @param request The country details to update, encapsulated within a {@link CountryRequestDTO}.
     * @return A {@link SuccessResponse} containing the status of the update operation.
     */
    @PutMapping("/country/update")
    public SuccessResponse<Country> updateCountry(@RequestBody CountryRequestDTO request) {
        Logger.logInfo("Updates a Country");
        countryService.updateCountry(request);
        return new SuccessResponse<>(SuccessCode.COUNTRY_UPDATE, HttpStatus.OK);
    }

    /**
     * Retrieves the details of a single country by its ID.
     * <p>
     * This method handles the HTTP POST request to fetch details of a country using its ID.
     * It logs the operation, retrieves the country details by ID through the {@code countryService}, and returns a success response.
     * The country ID is extracted from the {@link SearchRequestDTO} provided in the request body.
     * </p>
     *
     * @param request The search criteria encapsulated within a {@link SearchRequestDTO}, containing the country ID.
     * @return A {@link SuccessResponse} containing the {@link CountryDTO} of the requested country.
     */
    @PostMapping("/country/details")
    public SuccessResponse<CountryDTO> getCountryById(@RequestBody SearchRequestDTO request) {
        Logger.logInfo("Getting a Country details by on ID");
        return new SuccessResponse<>(SuccessCode.GET_COUNTRY,
                countryService.getCountryDetails(request), HttpStatus.OK);
    }

    /**
     * Adds a new region administrator user.
     * <p>
     * This endpoint is responsible for creating a new region administrator user. It accepts user details encapsulated
     * within a {@link UserRequestDTO} object. Upon successful creation, it returns a {@link SuccessResponse} object with a CREATED
     * status, indicating that the region admin user has been successfully added.
     * </p>
     *
     * @param request The {@link UserRequestDTO} containing the details of the region admin user to be added.
     * @return A {@link SuccessResponse<UserResponseDTO>} indicating the successful creation of the region admin user.
     */
    @PostMapping("/country/user-add")
    public SuccessResponse<UserResponseDTO> addRegionAdmin(@RequestBody UserRequestDTO request) {
        countryService.addRegionAdmin(request);
        return new SuccessResponse<>(SuccessCode.REGION_ADMIN_SAVE, HttpStatus.CREATED);
    }

    /**
     * Updates an existing region administrator user.
     * <p>
     * This method handles the update of an existing region administrator user's details. It takes a {@link UserRequestDTO} object
     * containing the updated user details. Upon successful update, it returns a {@link SuccessResponse} object with an OK status,
     * indicating that the region admin user's details have been successfully updated.
     * </p>
     *
     * @param request The {@link UserRequestDTO} containing the updated details of the region admin user.
     * @return A {@link SuccessResponse<UserResponseDTO>} indicating the successful update of the region admin user.
     */
    @PutMapping("/country/user-update")
    public SuccessResponse<UserResponseDTO> updateRegionAdmin(@RequestBody UserRequestDTO request) {
        countryService.updateRegionAdmin(request);
        return new SuccessResponse<>(SuccessCode.REGION_ADMIN_UPDATE, HttpStatus.OK);
    }

    /**
     * Removes a region administrator user.
     * <p>
     * This endpoint handles the deletion of a region administrator user based on the provided search criteria encapsulated within a {@link SearchRequestDTO}.
     * Upon successful deletion, it returns a {@link SuccessResponse} object with an OK status, indicating that the region admin user has been successfully removed.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the search criteria to identify the region admin user to be removed.
     * @return A {@link SuccessResponse<UserResponseDTO>} indicating the successful deletion of the region admin user.
     */
    @DeleteMapping("/country/user-remove")
    public SuccessResponse<UserResponseDTO> removeRegionAdmin(@RequestBody SearchRequestDTO request) {
        countryService.removeAdmin(request);
        return new SuccessResponse<>(SuccessCode.REGION_ADMIN_DELETE, HttpStatus.OK);
    }

    /**
     * Adds a new health facility.
     * <p>
     * This endpoint is responsible for creating a new health facility. It accepts health facility details encapsulated
     * within a {@link HealthFacilityRequestDTO} object. Upon successful creation, the newly created {@link HealthFacility} entity is returned.
     * </p>
     *
     * @param request The {@link HealthFacilityRequestDTO} containing the details of the health facility to be added.
     * @return The created {@link HealthFacility} entity.
     */
    @PostMapping("/healthfacility/create")
    public HealthFacility addHealthFacility(@RequestBody HealthFacilityRequestDTO request) {
        return healthFacilityService.createHealthFacility(request);
    }


    /**
     * Updates an existing health facility.
     * <p>
     * This method handles the update of an existing health facility's details. It takes a {@link HealthFacilityRequestDTO} object
     * containing the updated health facility details. Upon successful update, it returns a {@link SuccessResponse} object with an OK status,
     * indicating that the health facility's details have been successfully updated.
     * </p>
     *
     * @param request The {@link HealthFacilityRequestDTO} containing the updated details of the health facility.
     * @return A {@link SuccessResponse<HealthFacility>} indicating the successful update of the health facility.
     */
    @PutMapping("/healthfacility/update")
    public SuccessResponse<HealthFacility> updateHealthFacility(@RequestBody HealthFacilityRequestDTO request) {
        healthFacilityService.updateHealthFacility(request);
        return new SuccessResponse<>(SuccessCode.HEALTH_FACILITY_UPDATE, HttpStatus.OK);
    }

    /**
     * Retrieves a list of health facilities based on the provided search criteria.
     * <p>
     * This method handles a POST request to fetch a list of health facilities. The search criteria, encapsulated within a {@link SearchRequestDTO}.
     * The response includes a list of {@link HealthFacilityDTO} objects along with
     * the total count of matching health facilities, encapsulated within a {@link SuccessResponse}.
     * </p>
     *
     * @param requestDto The search criteria for fetching health facilities.
     * @return A {@link SuccessResponse<HealthFacilityDTO>} containing the list of health facilities and the total count.
     */
    @PostMapping("/healthfacility/list")
    public SuccessResponse<HealthFacilityDTO> getHealthFacilityList(@RequestBody SearchRequestDTO requestDto) {
        ResponseListDTO<HealthFacilityDTO> response = healthFacilityService.getHealthFacilities(requestDto);
        return new SuccessResponse<>(SuccessCode.GET_HEALTH_FACILITY, response.getData(), response.getTotalCount(), HttpStatus.OK);
    }

    /**
     * <p>
     * Get HealthFacility list based on organization id.
     * </p>
     *
     * @param requestDto - request data containing search term, pagination details, etc.,
     * @return List(HealthFacility) - List of HealthFacility Entities
     */
    @PostMapping("/health-facility/cfr-list")
    public ResponseEntity<ResponseListDTO<HealthFacilityDTO>> getHealthFacilityListForCfr(@RequestBody SearchRequestDTO requestDto) {
        ResponseListDTO<HealthFacilityDTO> response = healthFacilityService.getHealthFacilities(requestDto);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    /**
     * Fetches all health facilities within a specific district.
     * <p>
     * This method handles a POST request to retrieve all health facilities located within a specified district. The district ID is provided
     * within the {@link SearchRequestDTO}. The method returns a list of {@link HealthFacilityDTO} objects representing each health facility
     * within the district. The total number of facilities is also included in the response.
     * </p>
     *
     * @param requestDto The search criteria containing the district ID.
     * @return A {@link SuccessResponse<HealthFacilityDTO>} containing the list of health facilities within the specified district and the total count.
     */
    @PostMapping("/healthfacilities-by-district-id")
    public SuccessResponse<HealthFacilityDTO> getAllHealthFacilities(@RequestBody SearchRequestDTO requestDto) {
        List<HealthFacilityDTO> healthFacilityDTOS = healthFacilityService.getAllHealthFacilitiesByDistrictId(requestDto);
        return new SuccessResponse<>(SuccessCode.GET_HEALTH_FACILITIES_BY_DISTRICT_ID,
                healthFacilityDTOS,
                Long.valueOf(healthFacilityDTOS.size()),
                HttpStatus.OK);
    }

    /**
     * Retrieves the details of a health facility based on the provided search criteria.
     * <p>
     * This method handles a POST request to fetch detailed information of a health facility. The search criteria, encapsulated within a {@link SearchRequestDTO},
     * may include identifiers such as health facility ID or tenant ID. The response includes detailed information of the health facility,
     * encapsulated within a {@link SuccessResponse}.
     * </p>
     *
     * @param requestDto The search criteria for fetching health facility details.
     * @return A {@link SuccessResponse<HealthFacilityDTO>} containing the detailed information of the health facility.
     */
    @PostMapping("/healthfacility/details")
    public SuccessResponse<HealthFacilityDTO> getHealthFacilityDetails(@RequestBody SearchRequestDTO requestDto) {
        return new SuccessResponse<>(SuccessCode.GET_HEALTH_FACILITY, healthFacilityService.getHealthFacilityDetails(requestDto), HttpStatus.OK);
    }


    /**
     * Fetches a HealthFacility by its FHIR ID.
     *
     * @param requestDTO - A SearchRequestDTO object containing the FHIR ID of the HealthFacility to be fetched.
     * @return HealthFacilityRequestDTO - A HealthFacilityRequestDTO object representing the HealthFacility with the given FHIR ID.
     */
    @PostMapping("/healthfacility/details-by-fhir-id")
    public HealthFacilityRequestDTO getHealthFacilityByFhirId(@RequestBody RequestDTO requestDTO) {
        return healthFacilityService.getHealthFacilityByFhirId(requestDTO);
    }

    /**
     * Adds a new health facility administrator user.
     * <p>
     * This endpoint is responsible for creating a new administrator user for a health facility. It accepts user details encapsulated
     * within a {@link UserRequestDTO} object. Depending on whether the tenant ID is provided, it assigns either a super admin or a health facility admin role
     * to the user. Upon successful creation, it returns a {@link SuccessResponse} object with a CREATED status, indicating that the admin user has been successfully added.
     * </p>
     *
     * @param user The {@link UserRequestDTO} containing the details of the admin user to be added.
     * @return A {@link SuccessResponse<UserResponseDTO>} indicating the successful creation of the health facility admin user.
     */
    @PostMapping("/healthfacility/user-add")
    public SuccessResponse<UserResponseDTO> addHealthFacilityUser(@RequestBody UserRequestDTO user) {
        healthFacilityService.addHealthFacilityUser(user);
        return new SuccessResponse<>(Objects.isNull(user.getTenantId()) ? SuccessCode.SUPER_ADMIN_SAVE : SuccessCode.HEALTH_FACILITY_ADMIN_SAVE,
                HttpStatus.CREATED);
    }

    /**
     * Updates an existing health facility administrator user.
     * <p>
     * This method handles the HTTP PUT request to update the details of an existing health facility administrator user.
     * It accepts a {@link UserRequestDTO} object containing the updated user details. Upon successful update, it returns
     * a {@link SuccessResponse} object with an OK status, indicating that the health facility administrator user's details
     * have been successfully updated.
     * </p>
     *
     * @param user The {@link UserRequestDTO} containing the updated details of the health facility administrator user.
     * @return A {@link SuccessResponse<UserResponseDTO>} indicating the successful update of the health facility administrator user.
     */
    @PutMapping("/healthfacility/user-update")
    public SuccessResponse<UserResponseDTO> updateHealthfacilityAdmin(@RequestBody UserRequestDTO user) {
        healthFacilityService.updateHealthFacilityUser(user);
        return new SuccessResponse<>(SuccessCode.HEALTH_FACILITY_ADMIN_UPDATE, HttpStatus.OK);
    }

    /**
     * Deletes an existing health facility administrator user.
     * <p>
     * This method handles the HTTP POST request to delete a health facility administrator user based on the provided
     * search criteria encapsulated within a {@link SearchRequestDTO}. The search criteria include user ID and tenant ID.
     * Upon successful deletion, it returns a {@link SuccessResponse} object with an OK status, indicating that the health
     * facility administrator user has been successfully removed.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the search criteria to identify the health facility administrator user to be removed.
     * @return A {@link SuccessResponse<UserResponseDTO>} indicating the successful deletion of the health facility administrator user.
     */
    @PostMapping("/healthfacility/user-remove")
    public SuccessResponse<UserResponseDTO> deleteHealthFacilityAdmin(@RequestBody SearchRequestDTO request) {
        healthFacilityService.deleteHealthFacilityAdmin(request);
        return new SuccessResponse<>(SuccessCode.HEALTH_FACILITY_ADMIN_REMOVE, HttpStatus.OK);
    }

    /**
     * Retrieves a list of health facilities based on the provided country ID.
     * <p>
     * This method handles a POST request to fetch a list of health facilities located within a specified country.
     * The country ID is provided as a path variable. It returns a list of {@link HealthFacilityDTO} objects representing
     * each health facility within the specified country.
     * </p>
     *
     * @param countryId The ID of the country for which health facilities are to be retrieved.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities within the specified country.
     */
    @PostMapping("/healthfacility/country-list/{countryId}")
    public List<HealthFacilityDTO> getHealthFacilitiesByCountry(@PathVariable("countryId") Long countryId) {
        List<HealthFacilityDTO> response = healthFacilityService.getHealthFacilitiesByCountryId(countryId);
        return response;
    }

    /**
     * Retrieves a list of health facilities based on the provided chiefdom ID.
     * <p>
     * This method handles a POST request to fetch a list of health facilities located within a specified chiefdom.
     * The chiefdom ID is provided as a path variable. It returns a list of {@link HealthFacilityDTO} objects representing
     * each health facility within the specified chiefdom.
     * </p>
     *
     * @param chiefdomId The ID of the chiefdom for which health facilities are to be retrieved.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities within the specified chiefdom.
     */
    @PostMapping("/healthfacility/chiefdom-list/{chiefdomId}")
    public List<HealthFacilityDTO> getHealthFacilitiesByChiefdom(@PathVariable("chiefdomId") Long chiefdomId) {
        List<HealthFacilityDTO> response = healthFacilityService.getHealthFacilitiesByChiefdomId(chiefdomId);
        return response;
    }

    /**
     * Retrieves a list of health facilities based on the provided tenant IDs.
     * <p>
     * This endpoint handles a POST request to fetch a list of health facilities associated with the given tenant IDs.
     * The tenant IDs are provided in the request body as a list. It returns a list of {@link HealthFacilityDTO} objects
     * representing each health facility associated with the specified tenant IDs.
     * </p>
     *
     * @param tenantIds The list of tenant IDs for which health facilities are to be retrieved.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities associated with the specified tenant IDs.
     */
    @PostMapping("/healthfacility/tenants-list")
    public List<HealthFacilityDTO> getHealthFacilitiesByTenants(@RequestBody List<Long> tenantIds) {
        return healthFacilityService.getHealthFacilitiesByTenants(tenantIds);
    }

    /**
     * Retrieves a list of villages based on the provided tenant IDs.
     * <p>
     * This endpoint handles a POST request to fetch a list of villages associated with the health facilities of the given tenant IDs.
     * The tenant IDs are provided within a {@link SearchRequestDTO} in the request body. It returns a {@link SuccessResponse} object
     * containing a list of {@link VillageDTO} objects representing each village associated with the specified tenant IDs.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the tenant IDs for which villages are to be retrieved.
     * @return A {@link SuccessResponse<VillageDTO>} containing the list of villages associated with the specified tenant IDs.
     */
    @PostMapping("/healthfacility/villages-list")
    public SuccessResponse<VillageDTO> getVillagesByTenants(@RequestBody SearchRequestDTO request) {
        List<VillageDTO> response = healthFacilityService.getVillagesByFacility(request.getTenantIds());
        return new SuccessResponse<>(SuccessCode.GET_VILLAGE_LIST, response, HttpStatus.OK);
    }

    /**
     * Deletes a health facility based on the provided search criteria.
     * <p>
     * This endpoint handles a POST request to delete a specific health facility. The deletion criteria, encapsulated within a {@link SearchRequestDTO},
     * may include identifiers such as health facility ID or tenant ID. Upon successful deletion, it returns a {@link ResponseEntity} with an OK status,
     * indicating that the health facility has been successfully removed from the system.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the deletion criteria for the health facility.
     * @return A {@link ResponseEntity<String>} indicating the result of the deletion operation.
     */
    @PostMapping("/healthfacility/delete")
    public ResponseEntity<String> deleteHealthFacility(@RequestBody SearchRequestDTO request) {
        return new ResponseEntity<>(healthFacilityService.deleteHealthFacility(request), HttpStatus.OK);
    }

    /**
     * Retrieves a list of health facilities based on the provided tenant id.
     * <p>
     * This method handles a POST request to fetch a list of district health facilities located within a specified
     * tenant id.
     * The tenant ID is provided as a path variable. It returns a list of {@link HealthFacilityDTO} objects representing
     * each health facility within the specified health facility' district.
     * </p>
     *
     * @param tenantId The ID of the tenant id for which health facilities are to be retrieved.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities within the specified
     * health facility' district.
     */
    @PostMapping("/health-facility/all/{tenantId}")
    public List<HealthFacilityDTO> getDistrictHealthFacilitiesByTenantId(@PathVariable("tenantId") Long tenantId) {
        return healthFacilityService.getDistrictHealthFacilitiesByTenantId(tenantId);
    }

    /**
     * <p>
     * This method is used to retrieve a list of districts based on a search request.
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                         to get the list of districts is given
     * @return {@link SuccessResponse} Returns a success message and status with the retrieved
     * list of districts and total count
     */
    @UserTenantValidation
    @PostMapping("district/district-list")
    public SuccessResponse<DistrictDTO> getDistricts(@RequestBody SearchRequestDTO searchRequestDto) {
        Logger.logInfo("In admin controller, getting district list");
        ResponseListDTO<DistrictDTO> response = districtService.getDistricts(searchRequestDto);
        if ((Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount())) {
            return new SuccessResponse<>(SuccessCode.GET_DISTRICTS, response.getData(), null, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_DISTRICTS, response.getData(), response.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to get list of districts details with child organization counts.
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of districts is given
     * @return {@link SuccessResponse<DistrictListDTO>} Returns a success message and status with the retrieved
     * list of districts and total count
     */
    @UserTenantValidation
    @PostMapping("district/list")
    public SuccessResponse<DistrictListDTO> getDistrictList(@RequestBody SearchRequestDTO searchRequestDto) {
        Logger.logInfo("In Admin controller, getting district list");
        ResponseListDTO<DistrictListDTO> response = districtService.getDistrictList(searchRequestDto);
        if ((Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount())) {
            return new SuccessResponse<>(SuccessCode.GET_DISTRICTS, response.getData(), null,
                    HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_DISTRICTS, response.getData(),
                response.getTotalCount(), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to get list of district details based on given request
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of district details is given
     * @return {@link SuccessResponse<DistrictDTO>} Returns a success message and status with the retrieved
     * list of district details
     */
    @UserTenantValidation
    @PostMapping("district/details")
    public SuccessResponse<DistrictDTO> getDistrictDetails(@RequestBody SearchRequestDTO searchRequestDto) {
        Logger.logInfo("In admin controller, getting an district details");
        return new SuccessResponse<>(SuccessCode.GET_DISTRICT,
                districtService.getDistrictDetails(searchRequestDto), HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to create a new district using the districtRequestDTO data.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The district workflow dto that contains
     *                           necessary information to create district is given
     * @return {@link District} The district which is created for given district details is returned
     */
    @PostMapping("district/create")
    public District addDistrict(@Valid @RequestBody DistrictRequestDTO districtRequestDTO) {
        Logger.logInfo("In Admin controller, creating an district");
        return districtService.createDistrict(districtRequestDTO);
    }

    /**
     * <p>
     * This method is used to update a existing district using the accountWorkflowDto data.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The district workflow dto that contains
     *                           necessary information to update district is given
     * @return {@link SuccessResponse<District>} Returns a success message and status with the updated district
     */
    @PostMapping("district/update")
    public SuccessResponse<District> updateDistrict(@Valid @RequestBody DistrictRequestDTO districtRequestDTO) {
        Logger.logInfo("In Admin controller, updating district details");
        return new SuccessResponse<>(SuccessCode.UPDATE_DISTRICT, HttpStatus.OK,
                districtService.updateDistrict(districtRequestDTO).getName());
    }

    /**
     * <p>
     * This method is used to deactivate an district based on the provided request DTO.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The request contains necessary information
     *                   to deactivate the list of districts is given
     * @return {@link SuccessResponse} Returns a success message and status
     * after deactivating the district for the given id
     */
    @PostMapping("district/deactivate")
    public SuccessResponse<String> deactivateDistrict(@RequestBody DistrictRequestDTO districtRequestDTO) {
        Logger.logInfo("In Admin controller, deactivating district details");
        districtRequestDTO.setIsActive(false);
        districtRequestDTO.setTenantId(districtRequestDTO.getTenantId());
        districtService.activateOrDeactivateDistrict(districtRequestDTO);
        return new SuccessResponse<>(SuccessCode.DISTRICT_DEACTIVATE, HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to activate an district based on the provided request DTO.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The request contains necessary information
     *                   to activate the list of districts is given
     * @return {@link SuccessResponse} Returns a success message and status
     * after activating the district for the given id
     */
    @PutMapping("district/activate")
    public SuccessResponse<String> activateDistrictById(@RequestBody DistrictRequestDTO districtRequestDTO) {
        Logger.logInfo("In Admin controller, activating an district organization");
        districtRequestDTO.setIsActive(true);
        districtRequestDTO.setTenantId(districtRequestDTO.getTenantId());
        districtService.activateOrDeactivateDistrict(districtRequestDTO);
        return new SuccessResponse<>(SuccessCode.DISTRICT_ACTIVATE, HttpStatus.OK);
    }

    /**
     * <p>
     * This method is used to retrieve a list of deactivated districts based on a search request.
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                         to get the list of districts is given
     * @return {@link SuccessResponse} Returns a success message and status with the retrieved
     * list of districts and total count
     */
    @UserTenantValidation
    @PostMapping("district/deactivate-list")
    public SuccessResponse<DistrictDTO> getDeactivatedDistricts(@RequestBody SearchRequestDTO searchRequestDto) {
        Logger.logInfo("In admin controller, getting deactivated district list");
        ResponseListDTO<DistrictDTO> response = districtService.getDeactivatedDistricts(searchRequestDto);
        if ((Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount())) {
            return new SuccessResponse<>(SuccessCode.GET_DEACTIVATED_DISTRICTS, response.getData(), null, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_DEACTIVATED_DISTRICTS, response.getData(), response.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * <p>
     * Validates the edited HealthFacility Peer supervisors and villages
     * </p>
     *
     * @param requestDto - request Object with edited fields
     * @return Validation message
     */
    @PostMapping("/healthfacility/validate")
    public SuccessResponse<String> validateHealthFacility(@RequestBody SearchRequestDTO requestDto) {
        healthFacilityService.validateHealthFacility(requestDto);
        return new SuccessResponse<>(SuccessCode.GET_VILLAGE_LIST, HttpStatus.OK);
    }

    /**
     * Retrieves a list of villages IDs based on the provided tenant ID.
     * <p>
     * This endpoint handles request that requires to get IDs of villages linked to a HealthFacility by using
     * health facility's tenant ID. It return a collection of {@link List<Long>} with village IDs of the given HealthFacility
     * </p>
     *
     * @param tenantId {@link Long} Tenant ID of a Health Facility
     * @return a {@link List<Long>} which contains list of linked Village IDs.
     */
    @PostMapping("/healthfacility/villages/id")
    public List<Long> getFacilityVillageIdsByTenantId() {
        return healthFacilityService.getFacilityVillageIdsByTenantId();
    }

    /**
     * <p>
     * Handles the request to get a list of HealthFacility entities filtered based on the criteria provided in the {@link SearchRequestDTO}.
     * </p>
     *
     * @param requestDto the {@link SearchRequestDTO} containing the filter criteria for the search.
     * @return a {@link SuccessResponse} containing the list of filtered {@link HealthFacilityFilterDTO} objects and the total count of matching records.
     */
    @PostMapping("/healthfacility/filter-list")
    public SuccessResponse<HealthFacilityFilterDTO> getHealthFacilityFilterList(@RequestBody SearchRequestDTO requestDto) {
        ResponseListDTO<HealthFacilityFilterDTO> response = healthFacilityService.getHealthFacilitiesFilter(requestDto);
        return new SuccessResponse<>(SuccessCode.GET_HEALTH_FACILITY, response.getData(), response.getTotalCount(), HttpStatus.OK);
    }

    /** 
     * This method is used to get list of chiefdom DTOs using the given request.
     * </p>
     *
     * @param searchRequestDTO {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of chiefdom DTOs is given
     * @return {@link SuccessResponse<List>} Returns a success message and status with the retrieved
     * list of chiefdoms DTOs and total count
    */
   @UserTenantValidation
   @PostMapping("chiefdom/all")
   public SuccessResponse<ChiefdomDTO> getAllChiefdoms(@RequestBody SearchRequestDTO searchRequestDTO) {
       Logger.logInfo("In admin controller, getting all chiefdom list");
       ResponseListDTO<ChiefdomDTO> response = chiefdomService.getAllChiefdoms(searchRequestDTO);
       if (Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount()) {
           return new SuccessResponse<>(SuccessCode.GOT_CHIEFDOM, response.getData(), null, HttpStatus.OK);
       }
       return new SuccessResponse<>(SuccessCode.GOT_CHIEFDOM, response.getData(), response.getTotalCount(), HttpStatus.OK);
   }

   /**
    * <p>
    * This method is used to get list of  chiefdom details using the given request.
    * </p>
    *
    * @param requestDto {@link ChiefdomRequestDTO} The search request contains necessary information
    *                   to get the list of chiefdoms is given
    * @return {@link SuccessResponse<List>} Returns a success message and status with the retrieved
    * list of chiefdoms and total count
    */
   @UserTenantValidation
   @PostMapping("chiefdom/list")
   public SuccessResponse<ChiefdomDTO> getChiefdoms(
           @RequestBody ChiefdomRequestDTO requestDto) {
       Logger.logInfo("In admin controller, getting chiefdom list");
       ResponseListDTO<ChiefdomDTO> response = chiefdomService.getChiefdomList(requestDto);
       if (Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount()) {
           return new SuccessResponse<>(SuccessCode.GOT_CHIEFDOM, response.getData(), null, HttpStatus.OK);
       }
       return new SuccessResponse<>(SuccessCode.GOT_CHIEFDOM, response.getData(), response.getTotalCount(), HttpStatus.OK);
   }

   /**
    * <p>
    * This method is used to update a existing chiefdom using the chiefdom Dto.
    * </p>
    *
    * @param requestDto {@link ChiefdomRequestDTO} The chiefdom dto that contains necessary information
    *                         to update chiefdom is given
    * @return {@link SuccessResponse<Chiefdom>} Returns a success message and status after updating
    * the chiefdom
    */
   @UserTenantValidation
   @PutMapping("chiefdom/update")
   public SuccessResponse<Chiefdom> updateChiefdom(@RequestBody ChiefdomRequestDTO requestDto) {
       Logger.logInfo("In admin controller, updating the chiefdom");
       chiefdomService.updateChiefdom(modelMapper.map(requestDto, Chiefdom.class));
       return new SuccessResponse<>(SuccessCode.UPDATE_CHIEFDOM, HttpStatus.OK);
   }

   /**
    * <p>
    * This method is used to create a new chiefdom using the chiefdom data.
    * </p>
    *
    * @param chiefdomRequestDTO {@link ChiefdomRequestDTO} The chiefdom dto that contains
    *                         necessary information to create chiefdom is given
    * @return {@link Chiefdom} The chiefdom which is created for given chiefdom details is returned
    */
   @PostMapping("chiefdom/create")
   public Chiefdom createChiefdom(@RequestBody @Valid ChiefdomRequestDTO chiefdomRequestDTO) {
       Logger.logInfo("In admin controller, creating chiefdom");
       return chiefdomService.createChiefdom(modelMapper.map(chiefdomRequestDTO, Chiefdom.class), chiefdomRequestDTO.getVillages());
   }

   /**
    * <p>
    * This method is used to get list of chiefdom details based on given request
    * </p>
    *
    * @param searchRequestDTO {@link SearchRequestDTO} The search request contains necessary information
    *                         to get the list of chiefdom details is given
    * @return {@link SuccessResponse<ChiefdomDTO>} Returns a success message and status with the retrieved
    * list of chiefdom details
    */
   @UserTenantValidation
   @PostMapping("chiefdom/details")
   public SuccessResponse<ChiefdomDTO> getChiefdomDetails(
           @RequestBody SearchRequestDTO searchRequestDTO) {
       Logger.logInfo("In admin controller, getting chiefdom details");
       return new SuccessResponse<>(SuccessCode.GOT_CHIEFDOM,
               chiefdomService.getChiefdomDetails(searchRequestDTO), HttpStatus.OK);
   }

    /**
     * <p>
     * To add HealthFacility admin user.
     * </p>
     *
     * @param user - account admin user details
     * @return User - User entity
     */
    @PostMapping("/healthfacility/{tenantId}")
    public HealthFacility getHealthFacilityByTenantId(@PathVariable("tenantId") Long tenantId) {
        return healthFacilityService.getHealthFacilityByTenantId(tenantId);
    }

    /**
     * <p>
     * Handles the HTTP POST request to retrieve a list of sites based on the specified country.
     * </p>
     *
     * @param requestDto the request data transfer object containing the country criteria.
     * @return a SuccessResponse containing the list of sites, a success code, the total count of sites, and the HTTP status.
     */
    @PostMapping("/country/healthfacility-list")
    public SuccessResponse<HealthFacility> getSiteByCountry(@RequestBody CommonRequestDTO requestDto) {
        List<HealthFacility> response = healthFacilityService.getHealthFacilityByCountry(requestDto);
        ResponseListDTO<HealthFacility> responseListDto = new ResponseListDTO<>(response, (long) response.size());
        return new SuccessResponse<>(SuccessCode.GET_HEALTH_FACILITY, responseListDto.getData(), responseListDto.getTotalCount(),
                HttpStatus.OK);
    }
    
}