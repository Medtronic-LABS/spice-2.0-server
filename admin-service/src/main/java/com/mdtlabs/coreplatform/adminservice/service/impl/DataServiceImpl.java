package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.adminservice.model.dto.CommonResponseDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityTypesDTO;
import com.mdtlabs.coreplatform.adminservice.repository.CommunityUnitRepository;
import com.mdtlabs.coreplatform.adminservice.repository.DistrictRepository;
import com.mdtlabs.coreplatform.adminservice.repository.ChiefdomRepository;
import com.mdtlabs.coreplatform.adminservice.service.CountryService;
import com.mdtlabs.coreplatform.adminservice.service.DistrictService;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.adminservice.service.ChiefdomService;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CommunityUnit;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.repository.ApiRolePermissionRepository;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Order;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.mdtlabs.coreplatform.adminservice.model.dto.DataRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.HealthFacilityTypes;
import com.mdtlabs.coreplatform.adminservice.repository.CountryRepository;
import com.mdtlabs.coreplatform.adminservice.repository.CultureRespository;
import com.mdtlabs.coreplatform.adminservice.repository.HealthFacilityTypeRepository;
import com.mdtlabs.coreplatform.adminservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.adminservice.service.DataService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * This service class contain all the business logic for DataService
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
@Service
public class DataServiceImpl implements DataService {

    private final DistrictRepository districtRepository;
    private final ChiefdomRepository chiefdomRepository;
    private final VillageRepository villageRepository;
    private final ApiRolePermissionRepository apiRolePermissionRepository;
    private final HealthFacilityTypeRepository healthFacilityTypeRepository;
    private final CountryRepository countryRepository;
    private final CultureRespository cultureRespository;
    private final CountryService countryService;
    private final HealthFacilityService healthFacilityService;
    private final ChiefdomService chiefdomService;
    private final DistrictService districtService;
    private final CommunityUnitRepository communityUnitRepository;

    private final RestTemplate restTemplate;

    private ModelMapper mapper = new ModelMapper();

    @Autowired
    public DataServiceImpl(CountryService countryService, ChiefdomService chiefdomService, HealthFacilityService healthFacilityService,
                           DistrictService districtService, DistrictRepository districtRepository, VillageRepository villageRepository,
                           ApiRolePermissionRepository apiRolePermissionRepository, HealthFacilityTypeRepository healthFacilityTypeRepository,
                           CountryRepository countryRepository, CultureRespository cultureRespository, ChiefdomRepository chiefdomRepository,
                           CommunityUnitRepository communityUnitRepository, RestTemplate restTemplate) {
        this.districtService = districtService;
        this.chiefdomService = chiefdomService;
        this.countryService = countryService;
        this.healthFacilityService = healthFacilityService;
        this.countryRepository = countryRepository;
        this.chiefdomRepository = chiefdomRepository;
        this.districtRepository = districtRepository;
        this.cultureRespository = cultureRespository;
        this.healthFacilityTypeRepository = healthFacilityTypeRepository;
        this.villageRepository = villageRepository;
        this.apiRolePermissionRepository = apiRolePermissionRepository;
        this.communityUnitRepository = communityUnitRepository;
        this.restTemplate = restTemplate;
    }

    /**
     * {@inheritDoc}
     */
    public List<VillageDTO> getVillages(DataRequestDTO request) {
        List<Village> villages = villageRepository.getVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(), request.getSearchTerm());
        List<VillageDTO> villageResponse = new ArrayList<>();
        if (!Objects.isNull(villages) && !villages.isEmpty()) {
            villageResponse = mapper.map(villages, new TypeToken<List<VillageDTO>>() {
            }.getType());
        }
        return villageResponse;
    }

    /**
     * {@inheritDoc}
     */
    public List<ChiefdomDTO> getChiefdoms(DataRequestDTO request) {
        List<Chiefdom> chiefdoms = chiefdomRepository.findByCountryIdAndDistrictIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId(), request.getDistrictId());
        List<ChiefdomDTO> chiefdomResponse = new ArrayList<>();
        if (!Objects.isNull(chiefdoms) && !chiefdoms.isEmpty()) {
            chiefdomResponse = mapper.map(chiefdoms, new TypeToken<List<ChiefdomDTO>>() {
            }.getType());
        }
        return chiefdomResponse;
    }

    /**
     * {@inheritDoc}
     */
    public List<DistrictDTO> getDistricts(DataRequestDTO request) {
        List<District> districts = districtRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId());
        List<DistrictDTO> districtResponse = new ArrayList<>();
        if (!Objects.isNull(districts) && !districts.isEmpty()) {
            districtResponse = mapper.map(districts, new TypeToken<List<DistrictDTO>>() {
            }.getType());
        }
        return districtResponse;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<Map<String, Object>> getRegionDetails(SearchRequestDTO request) {
        ResponseListDTO<Map<String, Object>> response = new ResponseListDTO<>();
        String searchTerm = request.getSearchTerm();
        Pageable pageable = null;
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        if (0 < request.getLimit()) {
            List<Order> orders = List.of(Order.by("districtName"), Order.by("chiefdomName"), Order.by("villageName"));
            pageable = Pagination.setPagination(request.getSkip(), request.getLimit(), orders);
        }
        Page<Map<String, Object>> villages = villageRepository.getRegionDetailsByCountry(request.getCountryId(), searchTerm, pageable);

        response.setData((!StringUtils.isEmpty(searchTerm) && villages.isEmpty()) ? null : villages.toList());
        response.setTotalCount(villages.getTotalElements());
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public List<ApiRolePermission> getApiRolePermissionsByServiceName(String serviceName) {
        return apiRolePermissionRepository.findByServiceNameAndIsActiveTrueAndIsDeletedFalse(serviceName);
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacilityTypesDTO> getHealthFacilityTypes() {
        List<HealthFacilityTypes> healthFacilityTypes = healthFacilityTypeRepository.findAllByIsActiveTrueAndIsDeletedFalse();
        return new ModelMapper().map(healthFacilityTypes, new TypeToken<List<HealthFacilityTypesDTO>>() {}.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<VillageDTO> getVillagesByIds(List<Long> ids) {
        List<Map<String, Object>> villages = villageRepository.getVillagesByIds(ids);
        return new ModelMapper().map(villages, new TypeToken<List<VillageDTO>>() {}.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<String> getCountriesPhoneCodes() {
        List<Country> countries = countryRepository.findByIsDeletedFalseAndIsActiveTrue();
        return countries.stream().map(Country::getPhoneNumberCode).toList();
    }

    /**
     * {@inheritDoc}
     */
    public List<Culture> getCultures() {
        return cultureRespository.findAllByIsActiveTrueAndIsDeletedFalse();
    }

    /**
     * {@inheritDoc}
     */
    public List<VillageDTO> getUnlinkedVillages(DataRequestDTO request) {
        List<Village> villages = villageRepository.getUnlinkedVillages(request.getCountryId(), request.getDistrictId(),
                request.getChiefdomId(), request.getHealthFacilityId(), request.getSearchTerm());
        List<VillageDTO> villageResponse = new ArrayList<>();
        if (!Objects.isNull(villages) && !villages.isEmpty()) {
            villageResponse = mapper.map(villages, new TypeToken<List<VillageDTO>>() {}.getType());
        }
        return villageResponse;
    }

    /**
     * {@inheritDoc}
     */
    public List<VillageDTO> getVillagesWithoutUsers(SearchRequestDTO requestDto) {
        List<Map<String, Object>> villages = villageRepository.findUnlinkedVillagesByUserId(requestDto.getTenantIds(), requestDto.getUserId());
        List<VillageDTO> villageResponse = new ArrayList<>();
        if (!Objects.isNull(villages) && !villages.isEmpty()) {
            villageResponse = mapper.map(villages, new TypeToken<List<VillageDTO>>() {
            }.getType());
        }
        return villageResponse;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<CountryListDTO> getCountryList(SearchRequestDTO requestDto) {
        String searchTerm = requestDto.getSearchTerm();
        Long totalCount = Constants.LONG_ZERO;
        List<CountryListDTO> countryListDTOs = new ArrayList<>();

        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            return new ResponseListDTO<>(countryListDTOs, totalCount);
        }
        Pageable pageable = Pagination.setPagination(requestDto.getSkip(), requestDto.getLimit(), Constants.CREATED_AT,
                Constants.BOOLEAN_FALSE);
        Page<Country> countries = countryService.getCountries(searchTerm, pageable);

        if (Constants.ZERO == requestDto.getSkip()) {
            totalCount = countries.getTotalElements();
        }
        constructCountryListResponse(countries.stream().toList(), countryListDTOs);
        return new ResponseListDTO<>(countryListDTOs, totalCount);
    }

    /**
     * <p>
     * Constructs country list response consists the count of district, chiefdom and health facility.
     * </p>
     *
     * @param countries       - The list of country entities retrieved is given
     * @param countryListDTOs - The country list details to be constructed is given
     */
    private void constructCountryListResponse(List<Country> countries, List<CountryListDTO> countryListDTOs) {
        if (!countries.isEmpty()) {
            List<Map<String, Object>> healthFacilityCount = healthFacilityService.getHealthFacilityCountByCountryIds(
                    countries.stream().map(BaseEntity::getId).toList(), Constants.BOOLEAN_TRUE);
            List<Map<String, Object>> chiefdomCount = chiefdomService.getChiefdomCountByCountryIds(
                    countries.stream().map(BaseEntity::getId).toList(), Constants.BOOLEAN_TRUE);
            List<Map<String, Object>> districtCount = districtService.getDistrictCountByCountryIds(
                    countries.stream().map(BaseEntity::getId).toList(), Constants.BOOLEAN_TRUE);
            for (Country country : countries) {
                CountryListDTO countryListDTO = new CountryListDTO();
                countryListDTO.setId(country.getId());
                countryListDTO.setName(country.getName());
                countryListDTO.setTenantId(country.getTenantId());
                countryListDTO.setAppTypes(country.getAppTypes());
                countryListDTO.setDisplayValues(country.getDisplayValues());
                districtCount.forEach(district -> {
                    Long countryId = (Long) district.get(Constants.COUNTRY_ID);
                    if (country.getId().equals(countryId)) {
                        countryListDTO.setDistrictCount(((Long) district.get(Constants.COUNT)).intValue());
                    }
                });
                chiefdomCount.forEach(sc -> {
                    Long countryId = (Long) sc.get(Constants.COUNTRY_ID);
                    if (country.getId().equals(countryId)) {
                        countryListDTO.setChiefdomCount(((Long) sc.get(Constants.COUNT)).intValue());
                    }
                });
                healthFacilityCount.forEach(hf -> {
                    Long countryId = (Long) hf.get(Constants.COUNTRY_ID);
                    if (country.getId().equals(countryId) ) {
                        countryListDTO.setHealthFacilityCount(((Long) hf.get(Constants.COUNT)).intValue());
                    }
                });
                countryListDTOs.add(countryListDTO);
            }
        }
    }

    public ResponseListDTO<CommonResponseDTO> getCommunityUnits(SearchRequestDTO requestDTO) {
        ResponseListDTO<CommonResponseDTO> response = new ResponseListDTO<>();
        String searchTerm = requestDTO.getSearchTerm();
        Pageable pageable = null;
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }

        if (0 < requestDTO.getLimit()) {
            pageable = Pagination.setPagination(requestDTO.getSkip(), requestDTO.getLimit(),
                    Constants.NAME, Constants.BOOLEAN_FALSE);
        }
        Page<CommunityUnit> communityUnits = communityUnitRepository.getCommunityUnits(requestDTO.getCountryId(),
                requestDTO.getParentRegionId(), searchTerm, pageable);
        response.setData((!StringUtils.isEmpty(searchTerm) && communityUnits.isEmpty()) ? null
                : mapper.map(communityUnits.stream().toList(), new TypeToken<List<CommonResponseDTO>>() {}.getType()));
        response.setTotalCount(communityUnits.getTotalElements());
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public List<ChiefdomDTO> getChiefdomsByCountryId(Long countryId) {
        List<Chiefdom> chiefdoms = chiefdomRepository.findByCountryIdAndIsDeletedFalse(countryId);
        List<ChiefdomDTO> chiefdomResponse = new ArrayList<>();
        if (!Objects.isNull(chiefdoms) && !chiefdoms.isEmpty()) {
            chiefdomResponse = mapper.map(chiefdoms, new TypeToken<List<ChiefdomDTO>>() {}.getType());
        }
        return chiefdomResponse;
    }

    @Override
    public List<VillageDTO> getVillagesByCountryId(Long countryId) {
        List<VillageDTO> villageDTOList = new ModelMapper().map(villageRepository.findByCountryIdAndIsDeletedFalse(countryId), new TypeToken<List<VillageDTO>>() {
        }.getType());
        List<VillageDTO> otherVillageDTO = new ModelMapper().map(villageRepository.getOther(Constants.OTHER_VIILAGE), new TypeToken<List<VillageDTO>>() {
        }.getType());
        villageDTOList.addAll(otherVillageDTO);
        return villageDTOList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Map<String, String>> getCitiesList(SearchRequestDTO requestDTO) {
        HttpHeaders headers = new HttpHeaders();
        HttpEntity<String> entity = new HttpEntity<>(headers);
        List<Map<String, String>> citiesList = new ArrayList<>();
        String url = Constants.OSM_CITY_NAME_URL + requestDTO.getSearchTerm();
        ResponseEntity<String> responseValue = restTemplate.exchange(url, HttpMethod.GET, entity, String.class);
        if (HttpStatus.OK.equals(responseValue.getStatusCode())) {
            JSONArray cities = new JSONArray(responseValue.getBody());
            for (int index = 0; index < cities.length(); index++) {
                JSONObject locationObj = cities.getJSONObject(index);
                if (locationObj.getString(Constants.ADDRESSTYPE).equals(Constants.CITY)
                        || locationObj.getString(Constants.ADDRESSTYPE).equals(Constants.STATE)) {
                    Map<String, String> city = new HashMap<>();
                    city.put(Constants.VALUE, String.valueOf(locationObj.getLong(Constants.PLACE_ID)));
                    city.put(Constants.NAME, locationObj.getString(Constants.DISPLAY_NAME));
                    citiesList.add(city);
                }
            }
        }
        return citiesList;
    }
}
