package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.adminservice.model.dto.CommonResponseDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DataRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityTypesDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;

/**
 * <p>
 * This an interface class for Data module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
public interface DataService {

    /**
     * Retrieves a list of villages based on the criteria specified in the {@link DataRequestDTO}.
     * <p>
     * This method fetches village data, filtering based on the criteria provided in the DataRequestDTO,
     * such as location, population size, or other relevant filters. The result is a list of {@link VillageDTO}
     * objects that match the criteria.
     * </p>
     *
     * @param request The {@link DataRequestDTO} containing the filtering criteria for villages.
     * @return A list of {@link VillageDTO} objects that match the criteria.
     */
    List<VillageDTO> getVillages(DataRequestDTO request);

    /**
     * Retrieves a list of chiefdoms based on the criteria specified in the {@link DataRequestDTO}.
     * <p>
     * This method fetches chiefdom data, applying filters based on the criteria provided in the DataRequestDTO,
     * which could include geographic, demographic, or other relevant filters. The resulting list of {@link ChiefdomDTO}
     * objects represents the chiefdoms that meet these criteria.
     * </p>
     *
     * @param request The {@link DataRequestDTO} containing the filtering criteria for chiefdoms.
     * @return A list of {@link ChiefdomDTO} objects that match the filtering criteria.
     */
    List<ChiefdomDTO> getChiefdoms(DataRequestDTO request);

    /**
     * Retrieves a list of districts based on the criteria specified in the {@link DataRequestDTO}.
     * <p>
     * This method fetches district data, filtering based on the criteria provided in the DataRequestDTO,
     * such as geographic, demographic, or other relevant filters. The result is a list of {@link DistrictDTO}
     * objects that match the criteria.
     * </p>
     *
     * @param request The {@link DataRequestDTO} containing the filtering criteria for districts.
     * @return A list of {@link DistrictDTO} objects that match the criteria.
     */
    List<DistrictDTO> getDistricts(DataRequestDTO request);

    /**
     * Retrieves detailed region data based on the criteria specified in the {@link SearchRequestDTO}.
     * <p>
     * This method fetches detailed information about regions, applying filters based on the criteria provided in the SearchRequestDTO.
     * The resulting {@link ResponseListDTO} contains a map with string keys and object values representing the detailed data of regions
     * that meet these criteria.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the filtering criteria for region details.
     * @return A {@link ResponseListDTO} of maps with string keys and object values representing the detailed data of regions.
     */
    ResponseListDTO<Map<String, Object>> getRegionDetails(SearchRequestDTO request);

    /**
     * Retrieves API role permissions based on the specified service name.
     * <p>
     * This method fetches a list of {@link ApiRolePermission} entities that are associated with the specified service name.
     * It is useful for determining the permissions that are available or required for accessing various API services.
     * </p>
     *
     * @param serviceName The name of the service for which to retrieve API role permissions.
     * @return A list of {@link ApiRolePermission} entities associated with the specified service name.
     */
    List<ApiRolePermission> getApiRolePermissionsByServiceName(String serviceName);

    /**
     * Retrieves a list of all health facility types.
     * <p>
     * This method is responsible for fetching a comprehensive list of health facility types from the database.
     * Health facility types categorize the various types of health facilities available, such as hospitals, clinics, etc.
     * </p>
     *
     * @return A list of {@link HealthFacilityTypesDTO} representing all health facility types.
     */
    List<HealthFacilityTypesDTO> getHealthFacilityTypes();

    /**
     * Retrieves a list of villages by their IDs.
     * <p>
     * Given a list of village IDs, this method fetches the corresponding villages from the database.
     * This is useful for retrieving specific village details when their IDs are known.
     * </p>
     *
     * @param ids The list of village IDs for which to retrieve the village details.
     * @return A list of {@link VillageDTO} objects representing the villages with the specified IDs.
     */
    List<VillageDTO> getVillagesByIds(List<Long> ids);

    /**
     * Retrieves a list of country phone codes.
     * <p>
     * This method fetches a list of phone codes for all countries. Phone codes are essential for international
     * communication and data processing related to phone numbers.
     * </p>
     *
     * @return A list of Strings representing the phone codes of countries.
     */
    List<String> getCountriesPhoneCodes();

    /**
     * Retrieves a list of cultures.
     * <p>
     * This method fetches a list of all cultures. Cultures represent various societal identifiers, including language,
     * traditions, and customs, which can be associated with different regions or countries.
     * </p>
     *
     * @return A list of {@link Culture} representing different cultures.
     */
    List<Culture> getCultures();

    /**
     * <p>
     * This method gets the villages that are not linked to any HealthFacilities.
     * </p>
     *
     * @param request {@link DataRequestDTO} - Request Object with HealthFacility ID, CountryID, Chiefdom ID, etc.,
     * @return {@link List<VillageDTO>} - Response with Village
     */
    List<VillageDTO> getUnlinkedVillages(DataRequestDTO request);

    /**
     * <p>
     * Used to get Villages that are not linked to any user in tha given Health Facilities.
     * </p>
     *
     * @param requestDto {@link SearchRequestDTO} - request data with health facility ID
     * @return {@link List<VillageDTO>} - List of villages not linked to users.
     */
    List<VillageDTO> getVillagesWithoutUsers(SearchRequestDTO requestDto);

    /**
     * <p>
     * Constructs country list response consists the count of county, sub county and health facility.
     * Constructs country list response consists the count of district, chiefdom and health facility.
     * </p>
     *
     * @param requestDto {@link SearchRequestDTO}       - The search request for which the country entities retrieved is given
     * @return {@link ResponseListDTO} - The constructed country list is returned
     */
    ResponseListDTO<CountryListDTO> getCountryList(SearchRequestDTO requestDto);

    /**
     * <p>
     * Constructs country list response consists the count of district, chiefdom and health facility.
     * </p>
     *
     * @param requestDto {@link SearchRequestDTO}       - The search request for which the country entities retrieved is given
     * @return {@link ResponseListDTO} - The constructed country list is returned
     */
    ResponseListDTO<CommonResponseDTO> getCommunityUnits(SearchRequestDTO requestDto);

    /**
     * Gets Chiefdom by countryId.
     * 
     * @param countryId
     * @return List<ChiefdomDTO>
     */
    List<ChiefdomDTO> getChiefdomsByCountryId(Long countryId);

    /**
     * <p>
     * Get the villages by country id
     * </p>
     *
     * @param countryId The id of country
     * @return {@link List} List of villages
     */
    List<VillageDTO> getVillagesByCountryId(Long countryId);

    /**
     * <p>
     * Gets cities based on searchTerm.
     * </p>
     *
     * @param requestDto - searchTerm for get site.
     * @return List(Map) - List of cities
     */
    List<Map<String, String>> getCitiesList(SearchRequestDTO requestDto);
}
