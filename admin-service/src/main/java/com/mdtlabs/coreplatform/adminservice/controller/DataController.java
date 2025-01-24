package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityTypesDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.CommonResponseDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DataRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.adminservice.model.entity.Unit;
import com.mdtlabs.coreplatform.adminservice.service.DataService;
import com.mdtlabs.coreplatform.adminservice.service.UnitService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;

/**
 * <p>
 * The DataController class is a REST controller that handles requests related to chiefdom, district.
 * </p>
 *
 * @author Karthick M
 */
@RestController
@RequestMapping
public class DataController {

    private final DataService dataService;

    @Autowired
    public DataController(DataService dataService, UnitService unitService) {
        this.dataService = dataService;
        this.unitService = unitService;
    }

    private final UnitService unitService;

    /**
     * Handles the request to retrieve a list of villages based on the criteria specified in {@link DataRequestDTO}.
     * <p>
     * This endpoint processes POST requests to fetch villages. The request body should contain the search criteria
     * encapsulated in a {@link DataRequestDTO} object. The method logs the action, retrieves the list of villages
     * matching the criteria using the {@link DataService}, and returns them wrapped in a {@link SuccessResponse}.
     * </p>
     *
     * @param request The {@link DataRequestDTO} containing the search criteria for villages.
     * @return A {@link SuccessResponse<VillageDTO>} containing the list of villages matching the criteria.
     */
    @PostMapping("/villages-list")
    public SuccessResponse<VillageDTO> getVillages(@RequestBody DataRequestDTO request) {
        Logger.logInfo("In Data Controller, getting villages list");
        List<VillageDTO> villages = dataService.getVillages(request);

        return new SuccessResponse<>(SuccessCode.GET_VILLAGE_LIST, villages,
                HttpStatus.OK);
    }

    /**
     * Handles the request to retrieve a list of chiefdoms based on the criteria specified in {@link DataRequestDTO}.
     * <p>
     * This method is used to get list of chiefdoms.
     * </p>
     *
     * @param requestObject - request dto
     * @return List of ChiefdomDTO Entities.
     */
    @PostMapping("/chiefdom-list")
    public SuccessResponse<ChiefdomDTO> getChiefdoms(@RequestBody DataRequestDTO request) {
        Logger.logInfo("In Data Controller, getting chiefdom list");
        List<ChiefdomDTO> chiefdoms = dataService.getChiefdoms(request);

        return new SuccessResponse<>(SuccessCode.GET_CHIEFDOM_LIST, chiefdoms,
            HttpStatus.OK);
    }


    /**
     * Handles the request to retrieve a list of districts based on the criteria specified in {@link DataRequestDTO}.
     * <p>
     * This method is used to get list of districts.
     * </p>
     *
     * @param requestObject - request dto
     * @return List of DistrictDTO Entities.
     */
    @PostMapping("/district-list")
    public SuccessResponse<DistrictDTO> getDistricts(@RequestBody DataRequestDTO request) {
        Logger.logInfo("In Data Controller, getting district list");
        List<DistrictDTO> districts = dataService.getDistricts(request);

        return new SuccessResponse<>(SuccessCode.GET_DISTRICT_LIST, districts,
            HttpStatus.OK);
    }

    /**
     * Retrieves region details based on the criteria specified in {@link SearchRequestDTO}.
     * <p>
     * This endpoint processes POST requests to fetch region details. The request body should contain the search criteria
     * encapsulated in a {@link SearchRequestDTO} object. The method logs the action, retrieves the region details
     * using the {@link DataService#getRegionDetails(SearchRequestDTO)} method, and returns them wrapped in a {@link SuccessResponse}.
     * The response includes a map of region details and the total count of regions matching the criteria.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the search criteria for regions.
     * @return A {@link SuccessResponse<Map<String, Object>>} containing the map of region details and the total count.
     */
    @PostMapping("/region-details")
    public SuccessResponse<Map<String, Object>> getRegionDetails(@RequestBody SearchRequestDTO request) {
        Logger.logInfo("In Data Controller, getting region details");
        ResponseListDTO<Map<String, Object>> response = dataService.getRegionDetails(request);

        return new SuccessResponse<>(SuccessCode.GET_COUNTRY, response.getData(), response.getTotalCount(),
                HttpStatus.OK);
    }

    /**
     * Retrieves API role permissions based on the specified service name.
     * <p>
     * This method handles a GET request to fetch API role permissions associated with a given service name.
     * The service name is passed as a path variable. It utilizes the {@link DataService#getApiRolePermissionsByServiceName(String)}
     * method to fetch the permissions. The response is a list of {@link ApiRolePermission} entities that match the provided service name.
     * </p>
     *
     * @param serviceName The name of the service for which API role permissions are to be retrieved.
     * @return A list of {@link ApiRolePermission} entities associated with the specified service name.
     */
    @GetMapping("/api-role-permissions/{serviceName}")
    public List<ApiRolePermission> getApiRolePermissions(@PathVariable(value = Constants.SERVICE_NAME) String serviceName) {
        return dataService.getApiRolePermissionsByServiceName(serviceName);
    }

    /**
     * Retrieves a list of health facility types.
     * <p>
     * This method handles a POST request to fetch all available health facility types. It logs the action and
     * uses the {@link DataService#getHealthFacilityTypes()} method to retrieve the list of health facility types.
     * The response is wrapped in a {@link SuccessResponse} with the status OK, indicating successful retrieval.
     * </p>
     *
     * @return A {@link SuccessResponse<HealthFacilityTypesDTO>} containing the list of all health facility types.
     */
    @PostMapping("/healthfacility-types")
    public SuccessResponse<HealthFacilityTypesDTO> getHealthFacilityTypes() {
        Logger.logInfo("In Data Controller, getting health facility type list");
        List<HealthFacilityTypesDTO> response = dataService.getHealthFacilityTypes();
        return new SuccessResponse<>(SuccessCode.GET_HEALTH_FACILITY_TYPES, response, HttpStatus.OK);
    }

    /**
     * Retrieves a list of villages within a specific district.
     * <p>
     * This method processes a POST request to fetch villages based on the district criteria specified in the {@link DataRequestDTO}.
     * The request body must contain the district criteria. The method logs the action and retrieves the list of villages
     * matching the criteria using the {@link DataService#getVillages(DataRequestDTO)} method.
     * </p>
     *
     * @param request The {@link DataRequestDTO} containing the search criteria for the district.
     * @return A {@link List<VillageDTO>} containing the villages within the specified district.
     */
    @PostMapping("/district/villages-list")
    public List<VillageDTO> getVillagesByDistrict(@RequestBody DataRequestDTO request) {
        Logger.logInfo("In Data Controller, getting villages list by country");
        return dataService.getVillages(request);
    }


    /**
     * Retrieves a list of villages by their IDs.
     * <p>
     * This method processes a POST request to fetch villages based on a list of provided village IDs.
     * The IDs are provided in the request body. It utilizes the {@link DataService#getVillagesByIds(List<Long>)} method
     * to fetch the specified villages. The method logs the action and returns the list of villages that match the provided IDs.
     * </p>
     *
     * @param ids The list of IDs for which villages are to be retrieved.
     * @return A {@link List<VillageDTO>} containing the villages that match the provided IDs.
     */
    @PostMapping("/villages-by-ids")
    public List<VillageDTO> getVillagesByIds(@RequestBody List<Long> ids) {
        Logger.logInfo("In Data Controller, getting villages list");
        return dataService.getVillagesByIds(ids);
    }

    /**
     * Retrieves a list of country codes.
     * <p>
     * This method handles a POST request to fetch a list of country phone codes. It logs the action and
     * uses the {@link DataService#getCountriesPhoneCodes()} method to retrieve the list of country codes.
     * The response is wrapped in a {@link SuccessResponse} with the status OK, indicating successful retrieval.
     * </p>
     *
     * @return A {@link SuccessResponse<String>} containing the list of all country phone codes.
     */
    @PostMapping("/country-codes")
    public SuccessResponse<String> getCountries() {
        Logger.logInfo("In Data Controller, getting country code list");
        List<String> response = dataService.getCountriesPhoneCodes();

        return new SuccessResponse<>(SuccessCode.GET_COUNTRY_CODES, response, HttpStatus.OK);
    }

    /**
     * Retrieves a list of cultures.
     * <p>
     * This method handles a POST request to fetch a list of cultures. It logs the action and
     * uses the {@link DataService#getCultures()} method to retrieve the list of cultures.
     * The response is wrapped in a {@link SuccessResponse} with the status OK, indicating successful retrieval.
     * </p>
     *
     * @return A {@link SuccessResponse<Culture>} containing the list of all cultures.
     */
    @PostMapping("/cultures")
    public SuccessResponse<Culture> getCultures() {
        Logger.logInfo("In Data Controller, getting culture list");
        List<Culture> response = dataService.getCultures();

        return new SuccessResponse<>(SuccessCode.GET_CULTURES, response, HttpStatus.OK);
    }

    /**
     * This method get Units By Type.
     *
     * @param type - Unit type.
     * @return List of Unit details
     */
    @GetMapping("/unit/list/{type}")
    public List<Unit> getUnitsByType(@PathVariable("type") String type) {
        return unitService.getUnitsByType(type);
    }

    /**
     * <p>
     * Gets country list with child organization counts.
     * </p>
     *
     * @param requestDto request data
     * @return List of countryListDTO
     */
    @PostMapping("/country/list")
    public SuccessResponse<CountryListDTO> getCountryList(@RequestBody SearchRequestDTO requestDto) {
        ResponseListDTO<CountryListDTO> response = dataService.getCountryList(requestDto);
        if ((Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount())) {
            return new SuccessResponse<>(SuccessCode.GET_COUNTRY, response.getData(), null,
                    HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_COUNTRY, response.getData(),
                response.getTotalCount(), HttpStatus.OK);
    }

    /**
     * <p>
     * This method gets the villages that are not linked to any HealthFacilities.
     * </p>
     *
     * @param request {@link DataRequestDTO}- Request Object with HealthFacility ID, CountryID, Chiefdom ID, etc.,
     * @return {@link SuccessResponse<VillageDTO>} - Response with Village
     */
    @PostMapping("/unlinked-villages-list")
    public SuccessResponse<VillageDTO> getUnlinkedVillages(@RequestBody DataRequestDTO request) {
        Logger.logInfo("In Data Controller, getting villages list");
        List<VillageDTO> villages = dataService.getUnlinkedVillages(request);
        return new SuccessResponse<>(SuccessCode.GET_VILLAGE_LIST, villages, HttpStatus.OK);
    }

    /**
     * <p>
     * Used to get Villages that are not linked to any user in tha given Health Facilities.
     * </p>
     *
     * @param request {@link SearchRequestDTO} - request data with health facility ID
     * @return {@link SuccessResponse<VillageDTO>} - List of villages not linked to users.
     */
    @PostMapping("/healthfacility/unlinked-villages-list")
    public SuccessResponse<VillageDTO> getUserVillageList(@RequestBody SearchRequestDTO request) {
        Logger.logInfo("In Data Controller, getting villages list");
        List<VillageDTO> villages = dataService.getVillagesWithoutUsers(request);
        return new SuccessResponse<>(SuccessCode.GET_VILLAGE_LIST, villages, HttpStatus.OK);
    }

    /**
     * <p>
     * Handles the request to retrieve a list of community units based on the criteria provided in the {@link SearchRequestDTO}.
     * </p>
     *
     * @param request the {@link SearchRequestDTO} containing the filter criteria for the search.
     * @return a {@link SuccessResponse} containing the list of {@link CommonResponseDTO} objects representing the community units,
     * and optionally, the total count of matching records if available.
     */
    @PostMapping("/community-units")
    public SuccessResponse<CommonResponseDTO> getCommunityUnits(@RequestBody SearchRequestDTO request) {
        Logger.logInfo("In Data Controller, getting community units");
        ResponseListDTO<CommonResponseDTO> response = dataService.getCommunityUnits(request);
        if ((Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount())) {
            return new SuccessResponse<>(SuccessCode.GET_COUNTRY, response.getData(), null,
                    HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_COUNTRY, response.getData(),
                response.getTotalCount(), HttpStatus.OK);
    }

    /**
     * Gets chiefdom by country id.
     * 
     * @param countryId
     * @return List<ChiefdomDTO>
     */
    @PostMapping("/data/chiefdoms/{countryId}")
    public List<ChiefdomDTO> getChiefdomsByCountryId(@PathVariable("countryId") Long countryId) {
        return dataService.getChiefdomsByCountryId(countryId);
    }

    /**
     * Gets district by country id.
     * 
     * @param countryId
     * @return List<DistrictDTO>
     */
    @PostMapping("/data/districts/{countryId}")
    public List<DistrictDTO> getDistrictsByCountryId(@PathVariable("countryId") Long countryId) {
        DataRequestDTO request = new DataRequestDTO();
        request.setCountryId(countryId);
        return dataService.getDistricts(request);
    }

    /**
     * Get Villages by country id
     *
     * @param countryId id of country
     * @return {@link List} List of villages
     */
    @PostMapping("/data/villages/{countryId}")
    List<VillageDTO> getVillagesByCountryId(@PathVariable("countryId") Long countryId) {
        return dataService.getVillagesByCountryId(countryId);
    }

    /**
     * <p>
     * Get site list of cities based on searchTerm.
     * </p>
     *
     * @param requestDto - request data containing search term, pagination details,
     *                   etc.,
     * @return List(Map) - List of cities
     */
    @PostMapping("healthfacility/list-cities")
    public List<Map<String, String>> getCitiesList(@RequestBody SearchRequestDTO requestDto) {
        return dataService.getCitiesList(requestDto);
    }
}
