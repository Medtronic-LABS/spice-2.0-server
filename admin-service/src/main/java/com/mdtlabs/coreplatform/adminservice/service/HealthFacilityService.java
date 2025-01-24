package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.RequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityFilterDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;

/**
 * <p>
 * This an interface class for Health Facility module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
public interface HealthFacilityService {

    /**
     * Creates a new health facility in the system.
     * <p>
     * This method is responsible for creating a new health facility based on the provided HealthFacilityRequestDTO.
     * The DTO contains all necessary information required to create a health facility, such as its name, location, and other relevant details.
     * </p>
     *
     * @param request The HealthFacilityRequestDTO containing the details for the new health facility.
     * @return The created HealthFacility entity.
     */
    HealthFacility createHealthFacility(HealthFacilityRequestDTO request);

    /**
     * Adds a user to a health facility.
     * <p>
     * This method associates a user with a health facility, effectively granting the user access or roles within that facility.
     * The UserRequestDTO contains the user's details and the identifier of the health facility to which the user is being added.
     * </p>
     *
     * @param request The UserRequestDTO containing the user's details and the health facility identifier.
     * @return A UserResponseDTO containing the details of the user after being added to the health facility.
     */
    UserResponseDTO addHealthFacilityUser(UserRequestDTO request);

    /**
     * Updates a health facility user's details.
     * <p>
     * This method updates the details of a user associated with a health facility. It can be used to change the user's roles,
     * permissions, or any other relevant information within the context of the health facility.
     * The UserRequestDTO contains the updated user information along with the identifier of the health facility.
     * </p>
     *
     * @param request The UserRequestDTO containing the updated user details and the health facility identifier.
     * @return A UserResponseDTO containing the updated details of the user.
     */
    UserResponseDTO updateHealthFacilityUser(UserRequestDTO request);

    /**
     * Retrieves detailed information about a specific health facility.
     * <p>
     * This method fetches detailed information about a health facility based on the criteria specified in the {@link SearchRequestDTO}.
     * The criteria can include various filters such as location, type of facility, etc. The result is a single {@link HealthFacilityDTO}
     * that matches the criteria, providing detailed information about the health facility.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the filtering criteria for retrieving health facility details.
     * @return A {@link HealthFacilityDTO} representing the detailed information of the health facility.
     */
    HealthFacilityDTO getHealthFacilityDetails(SearchRequestDTO request);

    /**
     * Retrieves a list of all health facilities based on the specified criteria.
     * <p>
     * This method fetches a list of health facilities that match the criteria specified in the {@link SearchRequestDTO}.
     * The criteria can include filters such as location, facility type, etc. The result is a {@link ResponseListDTO} containing
     * a list of {@link HealthFacilityDTO} objects, each representing a health facility that matches the criteria.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the filtering criteria for retrieving a list of health facilities.
     * @return A {@link ResponseListDTO} of {@link HealthFacilityDTO} objects representing the health facilities that match the criteria.
     */
    ResponseListDTO<HealthFacilityDTO> getHealthFacilities(SearchRequestDTO request);

    /**
     * Updates the information of an existing health facility.
     * <p>
     * This method updates the details of a health facility based on the information provided in the {@link HealthFacilityRequestDTO}.
     * The DTO contains updated information such as the facility's name, location, and other relevant details. The method returns the
     * updated {@link HealthFacility} entity.
     * </p>
     *
     * @param request The {@link HealthFacilityRequestDTO} containing the updated details for the health facility.
     * @return The updated {@link HealthFacility} entity.
     */
    HealthFacility updateHealthFacility(HealthFacilityRequestDTO request);

    /**
     * Deletes an administrator from a health facility.
     * <p>
     * This method removes an administrator's association with a health facility based on the criteria specified in the {@link SearchRequestDTO}.
     * The criteria can include various identifiers such as the health facility's ID, the administrator's user ID, or other relevant filters.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria for identifying the administrator to be deleted.
     * @return A {@link UserResponseDTO} indicating the result of the deletion operation.
     */
    UserResponseDTO deleteHealthFacilityAdmin(SearchRequestDTO request);

    /**
     * Retrieves a list of health facilities by country ID.
     * <p>
     * This method fetches health facilities located within a specific country, identified by the country ID.
     * It is useful for filtering health facilities based on geographic location at the country level.
     * </p>
     *
     * @param countryId The ID of the country for which to retrieve health facilities.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities located in the specified country.
     */
    List<HealthFacilityDTO> getHealthFacilitiesByCountryId(Long countryId);

    /**
     * Retrieves a list of health facilities associated with specific tenants.
     * <p>
     * This method fetches health facilities that are associated with one or more tenant IDs provided in the list.
     * It allows for the filtering of health facilities based on their association with specific tenants.
     * </p>
     *
     * @param tenantIds A list of tenant IDs for which to retrieve associated health facilities.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities associated with the specified tenants.
     */
    List<HealthFacilityDTO> getHealthFacilitiesByTenants(List<Long> tenantIds);

    /**
     * Retrieves a list of health facilities by chiefdom ID.
     * <p>
     * This method fetches health facilities located within a specific chiefdom, identified by the chiefdom ID.
     * It is useful for filtering health facilities based on their geographic location at the chiefdom level.
     * </p>
     *
     * @param chiefdomId The ID of the chiefdom for which to retrieve health facilities.
     * @return A list of {@link HealthFacilityDTO} objects representing the health facilities located in the specified chiefdom.
     */
    List<HealthFacilityDTO> getHealthFacilitiesByChiefdomId(Long chiefdomId);

    /**
     * Retrieves a list of villages associated with a specific health facility.
     * <p>
     * Given a list of tenant IDs, this method fetches the villages associated with the health facilities
     * that are managed by the specified tenants. This can be useful for understanding the geographic coverage
     * of health services provided by these facilities.
     * </p>
     *
     * @param tenantIds A list of tenant IDs for which to retrieve the associated villages.
     * @return A list of {@link VillageDTO} objects representing the villages associated with the specified health facilities.
     */
    List<VillageDTO> getVillagesByFacility(List<Long> tenantIds);

    /**
     * Fetches all health facilities from the database that are not deleted and are active.
     * <p>
     * This method retrieves all health facilities that are currently active and have not been marked as deleted
     * in the database. It then maps the HealthFacility entity to HealthFacilityDTO and returns a list of HealthFacilityDTO,
     * providing a comprehensive overview of all available health facilities.
     * </p>
     *
     * @return A list of {@link HealthFacilityDTO} representing all active and non-deleted health facilities.
     */
    List<HealthFacilityDTO> getAllHealthFacilities();

    /**
     * Fetches all health facilities by district ID from the database that are not deleted and are active.
     * <p>
     * This method retrieves all health facilities within a specified district that are currently active and have not been marked as deleted
     * in the database. It uses the {@link SearchRequestDTO} to filter health facilities by district ID. The result is a list of {@link HealthFacilityDTO},
     * providing a comprehensive overview of all available health facilities within the district.
     * </p>
     *
     * @param requestDTO The {@link SearchRequestDTO} containing the district ID for filtering the health facilities.
     * @return A list of {@link HealthFacilityDTO} representing all active and non-deleted health facilities within the specified district.
     */
    List<HealthFacilityDTO> getAllHealthFacilitiesByDistrictId(SearchRequestDTO requestDTO);
    
    /**
     * Deletes a health facility based on the criteria specified in the {@link SearchRequestDTO}.
     * <p>
     * This method removes a health facility from the system based on the criteria specified in the {@link SearchRequestDTO}.
     * The criteria can include various identifiers such as the health facility's ID. This operation marks the health facility as deleted
     * or removes it from the database, depending on the implementation.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the criteria for identifying the health facility to be deleted.
     * @return A String indicating the result of the deletion operation, such as a success message or an error message.
     */
    String deleteHealthFacility(SearchRequestDTO request);

    /**
     * Gets health facility by fhir id.
     *
     * @param requestDTO
     * @return HealthFacilityRequestDTO
     */
    HealthFacilityRequestDTO getHealthFacilityByFhirId(RequestDTO requestDTO);

    /**
     * <p>
     * Validates the edited HealthFacility Peer supervisors and villages
     * </p>
     *
     * @param request - request Object
     */
    void validateHealthFacility(SearchRequestDTO request);

    /**
     * Retrieves a list of village Ids linked to a particular Health facility by health facility's Tenant ID.
     * <p>
     * This method fetches IDs of the villages linked to health facility, identified by the tenant ID.
     * </p>
     *
     * @return A list of {@link Long}  representing the Village ID located in the specified Health Facility.
     */
    List<Long> getFacilityVillageIdsByTenantId();
    /**
     * <p>
     * This method is used to get a list of map containing operating unit IDs with corresponding count of sites that
     * is searched using the given district IDs.
     * </p>
     *
     * @param districtIds {@link List<Long>} The list of district IDs associated with the sites that need to counted
     *                   is given
     * @return {@link List<Map>} A list of map containing key as district IDs and value as count of sites
     * for the corresponding district IDs provided is returned
     */
    List<Map<String, Object>> getCountByDistrictIds(List<Long> districtIds);

    /**
     * <p>
     * To activate or deactivate list of sites based on its tenantIds.
     * </p>
     *
     * @param districtId       {@link Long} The district ID for which the site is being searched is given
     * @param chiefdomId {@link Long} The operating unit ID for which the site is being searched is given
     * @param countryId       {@link Long} The ID of the country associated with the sites that
     *                        are being searched is given
     * @param isActive        The boolean value that is used to filter the results of the query based
     *                        on whether the sites have been marked as active or not is given
     * @return The list of activated or deactivated sites for given search criteria is returned.
     */
    List<HealthFacility> activateOrDeactivateHealthFacility(Long countryId, Long districtId, Long chiefdomId, boolean isActive);

    /**
     * <p>
     * Gets health facility count by country ids and its active status
     * </p>
     *
     * @param countryIds {@link List}     - Country ids for which the health facility count to be retrieved is given
     * @param isActive   {@link Boolean} - Active status of the health facilities that need to be counted is given
     * @return List {@link List} - The health facility count based on country id is retrieved
     */
    List<Map<String, Object>> getHealthFacilityCountByCountryIds(List<Long> countryIds, Boolean isActive);

    /**
     * <p>
     * Retrieves a filtered list of health facilities based on the criteria specified in the {@link SearchRequestDTO}.
     * </p>
     *
     * @param request The {@link SearchRequestDTO} containing the filter criteria for the search.
     * @return A {@link ResponseListDTO<HealthFacilityFilterDTO>} containing the filtered list of health facilities and
     *         additional metadata like total records.
     */
    ResponseListDTO<HealthFacilityFilterDTO> getHealthFacilitiesFilter(SearchRequestDTO request);

    /**
     * <p>
     * This method is used to get a list of map containing chiefdom IDs with corresponding count of health facility that
     * is searched using the given chiefdom IDs.
     * </p>
     *
     * @param chiefdomIds {@link List<Long>} The list of chiefdom IDs associated with the health facility
     *                         that need to counted is given
     * @return {@link Map} A map containing key as chiefdom IDs and value as count of health facility
     * for the corresponding chiefdom IDs provided is returned
     */
    public List<Map<String, Object>> getHealthFacilityCountByChiefdomIds(List<Long> chiefdomIds, boolean isActive);

    /* 
     * Gets health facility by ids
     * 
     * @param ids
     * @return Set of health facility.
     */
    Set<HealthFacility> getHealthFacilitiesByIds(Set<Long> ids);

    /**
     * Gets health facility by tenant id
     * 
     * @param tenantId
     * @return HealthFacility
     */
    HealthFacility getHealthFacilityByTenantId(Long tenantId);

    /**
     * Gets health facilities by given tenant id's district
     *
     * @param tenantId
     * @return list of HealthFacilityDTO
     */
    List<HealthFacilityDTO> getDistrictHealthFacilitiesByTenantId(Long tenantId);

    /**
     * <p>
     * Handles the HTTP POST request to retrieve a list of sites based on the specified country.
     * </p>
     *
     * @param requestDto the request data transfer object containing the country criteria.
     * @return a SuccessResponse containing the list of sites, a success code, the total count of sites, and the HTTP status.
     */
    List<HealthFacility> getHealthFacilityByCountry(CommonRequestDTO requestDto);
}
