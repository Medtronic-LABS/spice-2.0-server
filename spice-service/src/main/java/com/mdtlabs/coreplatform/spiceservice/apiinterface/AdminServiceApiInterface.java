package com.mdtlabs.coreplatform.spiceservice.apiinterface;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RegionCustomizationDTO;
import org.springframework.cloud.openfeign.FeignClient;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WorkflowCustomization;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO.DistrictDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO.ChiefdomDTO;

/**
 * Feign client interface for making API calls to the admin-service.
 * <p>
 * This interface defines methods for interacting with various endpoints of the admin-service,
 * facilitating operations such as fetching health facilities by geographical divisions and
 * retrieving clinical workflows.
 * </p>
 */
@FeignClient(name = "admin-service", url = "${app.admin-service}")
public interface AdminServiceApiInterface {

    /**
     * Fetches a list of health facilities by country ID.
     * <p>
     * This method communicates with the admin-service to retrieve health facilities
     * located within a specified country, identified by its unique country ID.
     * </p>
     *
     * @param token     The authorization token.
     * @param client    The client information.
     * @param countryId The unique identifier of the country.
     * @return A list of {@link HealthFacilityDTO} representing the health facilities in the specified country.
     */
    @PostMapping("/healthfacility/country-list/{countryId}")
    List<HealthFacilityDTO> getHealthFacilitiesByCountry(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @PathVariable("countryId") Long countryId);

    /**
     * Fetches a list of health facilities by chiefdom ID.
     * <p>
     * This method communicates with the admin-service to retrieve health facilities
     * located within a specified chiefdom, identified by its unique chiefdom ID.
     * </p>
     *
     * @param token      The authorization token.
     * @param client     The client information.
     * @param chiefdomId The unique identifier of the chiefdom.
     * @return A list of {@link HealthFacilityDTO} representing the health facilities in the specified chiefdom.
     */
    @PostMapping("/healthfacility/chiefdom-list/{chiefdomId}")
    List<HealthFacilityDTO> getHealthFacilitiesByChiefdom(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @PathVariable("chiefdomId") Long chiefdomId);

    /**
     * Fetches a list of health facilities by tenant IDs.
     * <p>
     * This method communicates with the admin-service to retrieve health facilities
     * associated with specified tenant IDs.
     * </p>
     *
     * @param token  The authorization token.
     * @param client The client information.
     * @param ids    The list of tenant IDs.
     * @return A list of {@link HealthFacilityDTO} representing the health facilities associated with the specified tenants.
     */
    @PostMapping("/healthfacility/tenants-list")
    List<HealthFacilityDTO> getHealthFacilitiesByTenants(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody List<Long> ids);

    /**
     * Fetches a list of health facilities by tenant ID.
     * <p>
     * This method communicates with the admin-service to retrieve health facilities
     * associated with specified tenant IDs.
     * </p>
     *
     * @param token  The authorization token.
     * @param client The client information.
     * @return A list of {@link HealthFacilityDTO} representing the health facilities associated with the specified tenants.
     */
    @PostMapping("/health-facility/all/{tenantId}")
    List<HealthFacilityDTO> getAllHealthFacilitiesByTenantId(@RequestHeader("Authorization") String token,
                                                             @RequestHeader("client") String client,
                                                             @PathVariable("tenantId") Long tenantId);

    /**
     * Retrieves a list of villages by district.
     * <p>
     * This endpoint communicates with the admin-service to retrieve villages located within a specified district.
     * The district is identified by its unique ID provided in the request body.
     * </p>
     *
     * @param token   The authorization token provided in the "Authorization" header.
     * @param client  The client information provided in the "client" header.
     * @param request A map containing the district ID with the key as "districtId".
     * @return A list of {@link VillageDTO} representing the villages in the specified district.
     */
    @PostMapping("/district/villages-list")
    List<VillageDTO> getVillageByDistrict(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody Map<String, Long> request);

    /**
     * Retrieves a list of villages by their IDs.
     * <p>
     * This endpoint communicates with the admin-service to retrieve villages based on a list of village IDs provided in the request body.
     * </p>
     *
     * @param token   The authorization token provided in the "Authorization" header.
     * @param client  The client information provided in the "client" header.
     * @param request A list of village IDs for which the details are to be retrieved.
     * @return A list of {@link VillageDTO} representing the details of the specified villages.
     */
    @PostMapping("/villages-by-ids")
    List<VillageDTO> getVillageByIds(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody List<Long> request);

    /**
     * Retrieves a list of all health facilities.
     * <p>
     * This endpoint communicates with the admin-service to retrieve all health facilities without any filtering criteria.
     * </p>
     *
     * @param authorization The authorization token provided in the "Authorization" header.
     * @param client        The client information provided in the "client" header.
     * @return A list of {@link HealthFacilityDTO} representing all health facilities.
     */
    @PostMapping("/healthfacility/all")
    List<HealthFacilityDTO> getAllHealthFacilities(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                   @RequestHeader(Constants.CLIENT) String client);

    /**
     * Retrieves a list of clinical workflows by their IDs.
     * <p>
     * This endpoint communicates with the admin-service to retrieve clinical workflows based on a list of workflow IDs provided in the request body.
     * </p>
     *
     * @param token  The authorization token provided in the "Authorization" header.
     * @param client The client information provided in the "client" header.
     * @param ids    A list of clinical workflow IDs for which the details are to be retrieved.
     * @return A list of {@link ClinicalWorkflow} representing the details of the specified clinical workflows.
     */
    @PostMapping("/clinical-workflow/get-workflows")
    List<ClinicalWorkflow> getClinicalWorkflows(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody List<Long> ids);                                       

    /**
     * <p>
     * This method retrives a list of Village Ids that are linked to the given HealthFacility.
     * </p>
     *
     * @param authorization  The authorization token provided in the "Authorization" header.
     * @param client The client information provided in the "client" header.
     * @param tenantId  The tenant ID of the HealthFacility
     * @return A list of {@link Long} representing villages that are linked to a HealthFacility.
     */
    @PostMapping("/healthfacility/villages/id")
    List<Long> getFacilityVillagesByTenantId(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                             @RequestHeader(Constants.CLIENT) String client,
                                             @RequestHeader(Constants.HEADER_TENANT_ID) Long tenantId);


    /**
     * <p>
     * Gets list of customizations.
     * </p>
     *
     * @param authorization  The authorization token provided in the "Authorization" header.
     * @param client The client information provided in the "client" header.
     * @param category category of the region customization
     * @return {@link List} List of RegionCustomizations
     */
    @PostMapping("/region-customization/static-data/get-list/{category}")
    public List<RegionCustomizationDTO> getRegionCustomizationsByCategory(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                                          @RequestHeader(Constants.CLIENT) String client,
                                                                          @PathVariable(Constants.CATEGORY) String category);
    @PostMapping("/program/get-by-healthfacility-ids")
    public List<Program> getPrograms(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                        @RequestBody List<Long> ids);

    @PostMapping("/workflow-customization/static-data/get-list")
    public List<WorkflowCustomization> getWorkflowCustomization(@RequestHeader("Authorization") String token,
        @RequestHeader("TenantId") Long tenantId, @RequestBody SearchRequestDTO request);

    @PostMapping("/country-customization/static-data/get-list/{cultureId}")
    public List<CountryCustomization> getCountryCustomizations(@RequestHeader("Authorization") String token,
        @RequestHeader("TenantId") long tenantId, @PathVariable("cultureId") Long cultureId);

    @PostMapping("/country-customization/static-data/get-list/{cultureId}/{category}")
    public List<CountryCustomization> getCountryCustomizationsByCategory(@RequestHeader("Authorization") String token,
        @RequestHeader("TenantId") long tenantId, @PathVariable("cultureId") Long cultureId, @PathVariable(Constants.CATEGORY) String category);

    @PostMapping("/clinical-workflow/get-all-workflows")
    public List<ClinicalWorkflow> getUnSelectedClinicalWorkFlows(@RequestHeader("Authorization") String token,
        @RequestHeader("TenantId") Long tenantId, @RequestBody List<Long> ids);

    @PostMapping("/healthfacility/{tenantId}")
    HealthFacility getHealthFacilitiy(@RequestHeader(Constants.AUTHORIZATION) String authorization, @RequestHeader("TenantId") Long headerTenantId, @PathVariable("tenantId") Long tenantId);

    @PostMapping("/data/chiefdoms/{countryId}")
    List<ChiefdomDTO> getChiefdomsByCountryId(@RequestHeader(Constants.AUTHORIZATION) String authorization, @RequestHeader("TenantId") Long tenantId, @PathVariable("countryId") Long countryId);
    
    @PostMapping("/data/districts/{countryId}")
    List<DistrictDTO> getDistrictsByCountryId(@RequestHeader(Constants.AUTHORIZATION) String authorization, @RequestHeader("TenantId") Long tenantId, @PathVariable("countryId") Long countryId);


    /**
     * <p>
     * This method used to get all the lab tests based on search param.
     * </p>
     *
     * @param authorization  The authorization token provided in the "Authorization" header.
     * @param client The client information provided in the "client" header.
     * @return  {@link LabTestCustomizationDTO} Contains Lab test customization details.
     */
    @PostMapping("/lab-test-customization/search")
    public LabTestCustomizationDTO getLabTestCustomization(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                           @RequestHeader(Constants.CLIENT) String client, @RequestBody SearchRequestDTO request);

    /**
     * <p>
     * This method used to get all villages based on the country
     * </p>
     *
     * @param authorization The authorization token provided in the "Authorization" header.
     * @param client        The client information provided in the "client" header.
     * @param countryId     The id of country
     * @return {@link List} List of Villages
     */
    @PostMapping("/data/villages/{countryId}")
    List<VillageDTO> getVillagesByCountryId(@RequestHeader(Constants.AUTHORIZATION) String authorization, @RequestHeader(Constants.CLIENT) String client, @PathVariable("countryId") Long countryId);
}
