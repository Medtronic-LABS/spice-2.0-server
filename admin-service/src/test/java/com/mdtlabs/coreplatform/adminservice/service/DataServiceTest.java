package com.mdtlabs.coreplatform.adminservice.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityTypesDTO;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Order;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.model.dto.CommonResponseDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DataRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.HealthFacilityTypes;
import com.mdtlabs.coreplatform.adminservice.repository.ChiefdomRepository;
import com.mdtlabs.coreplatform.adminservice.repository.CountryRepository;
import com.mdtlabs.coreplatform.adminservice.repository.CultureRespository;
import com.mdtlabs.coreplatform.adminservice.repository.DistrictRepository;
import com.mdtlabs.coreplatform.adminservice.repository.CommunityUnitRepository;
import com.mdtlabs.coreplatform.adminservice.repository.HealthFacilityTypeRepository;
import com.mdtlabs.coreplatform.adminservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.DataServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CommunityUnit;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.commonservice.common.repository.ApiRolePermissionRepository;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DataServiceTest {

    @InjectMocks
    private DataServiceImpl dataService;

    @Mock
    private VillageRepository villageRepository;

    @Mock
    private ChiefdomRepository chiefdomRepository;

    @Mock
    ApiRolePermissionRepository apiRolePermissionRepository;

    @Mock
    private DistrictRepository districtRepository;

    @Mock
    CultureRespository cultureRespository;

    @Mock
    private HealthFacilityTypeRepository healthFacilityTypeRepository;

    @Mock
    private CountryRepository countryRepository;

    @Mock
    private ChiefdomService chiefdomService;

    @Mock
    private CountryService countryService;

    @Mock
    private HealthFacilityService healthFacilityService;

    @Mock
    private DistrictService districtService;

    @Mock
    private CommunityUnitRepository communityUnitRepository;

    @Mock
    private ModelMapper modelMapper;

    @Test
    void getVillages() {
        DataRequestDTO request = new DataRequestDTO();
        List<Village> villages = null;

        request.setChiefdomId(1l);
        request.setCountryId(1l);
        request.setDistrictId(1l);

        when(villageRepository.getVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(), request.getSearchTerm())).thenReturn(villages);
        List<VillageDTO> response = dataService.getVillages(request);
        assertNotNull(response);
        assertEquals(response, new ArrayList<>());

        villages = new ArrayList<>();
        when(villageRepository.getVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(), request.getSearchTerm())).thenReturn(villages);
        response = dataService.getVillages(request);
        assertNotNull(response);
        assertEquals(response, new ArrayList<>());

        villages = List.of(TestDataProvider.getVillage());
        when(villageRepository.getVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(), request.getSearchTerm())).thenReturn(villages);
        response = dataService.getVillages(request);
        assertNotNull(response);
    }

    @Test
    void getChiefdoms() {
        DataRequestDTO request = new DataRequestDTO();
        List<Chiefdom> chiefdoms = null;

        request.setChiefdomId(1l);
        request.setCountryId(1l);
        request.setDistrictId(1l);

        when(chiefdomRepository.findByCountryIdAndDistrictIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId(), request.getDistrictId())).thenReturn(chiefdoms);
        List<ChiefdomDTO> response = dataService.getChiefdoms(request);
        assertNotNull(response);
        assertEquals(response, new ArrayList<>());

        chiefdoms = new ArrayList<>();
        when(chiefdomRepository.findByCountryIdAndDistrictIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId(), request.getDistrictId())).thenReturn(chiefdoms);
        response = dataService.getChiefdoms(request);
        assertNotNull(response);
        assertEquals(response, new ArrayList<>());

        chiefdoms = List.of(TestDataProvider.getChiefdom());
        when(chiefdomRepository.findByCountryIdAndDistrictIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId(), request.getDistrictId())).thenReturn(chiefdoms);
        response = dataService.getChiefdoms(request);
        assertNotNull(response);
    }

    @Test
    void getDistricts() {
        DataRequestDTO request = new DataRequestDTO();
        List<District> districts = null;

        request.setChiefdomId(1l);
        request.setCountryId(1l);
        request.setDistrictId(1l);

        when(districtRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId())).thenReturn(districts);
        List<DistrictDTO> response = dataService.getDistricts(request);
        assertNotNull(response);
        assertEquals(response, new ArrayList<>());

        districts = new ArrayList<>();
        when(districtRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId())).thenReturn(districts);
        response = dataService.getDistricts(request);
        assertNotNull(response);
        assertEquals(response, new ArrayList<>());

        districts = List.of(TestDataProvider.getDistrict());
        when(districtRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(request.getCountryId())).thenReturn(districts);
        response = dataService.getDistricts(request);
        assertNotNull(response);
    }

    @Test
    void getRegionDetails() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setSearchTerm("abc");
        requestDTO.setLimit(10);
        requestDTO.setUserBased(true);
        List<Order> orders = List.of(Order.by("districtName"), Order.by("chiefdomName"), Order.by("villageName"));
        Pageable pageable = Pagination.setPagination(0, 10, orders);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        List<Map<String, Object>> regionDetails = List.of(new HashMap<>());
        Page<Map<String, Object>> regionDetailsPage = new PageImpl<>(regionDetails);
        when(CommonUtil.isValidSearchData(requestDTO.getSearchTerm(), Constants.SEARCH_TERM)).thenReturn(false);
        ResponseListDTO<Map<String, Object>> response = dataService.getRegionDetails(requestDTO);
        assertNotNull(response);
        requestDTO.setSearchTerm("ab");
        when(CommonUtil.isValidSearchData(requestDTO.getSearchTerm(), Constants.SEARCH_TERM)).thenReturn(true);

        when(villageRepository.getRegionDetailsByCountry(requestDTO.getCountryId(), requestDTO.getSearchTerm(), pageable)).thenReturn(regionDetailsPage);
        response = dataService.getRegionDetails(requestDTO);
        assertNotNull(response);

        pageable = null;
        requestDTO.setLimit(0);
        when(villageRepository.getRegionDetailsByCountry(requestDTO.getCountryId(), requestDTO.getSearchTerm(), pageable)).thenReturn(regionDetailsPage);
        response = dataService.getRegionDetails(requestDTO);
        assertNotNull(response);

        TestDataProvider.cleanUp();
    }

    @Test
    void getApiRolePermissionsByServiceName() {
        //given
        List<ApiRolePermission> apiRolePermissions = new ArrayList<>();
        //when
        when(apiRolePermissionRepository.findByServiceNameAndIsActiveTrueAndIsDeletedFalse(TestConstants.FIRST_NAME)).thenReturn(apiRolePermissions);

        //then
        List<ApiRolePermission> response = dataService.getApiRolePermissionsByServiceName(TestConstants.FIRST_NAME);
        assertNotNull(response);
    }

    @Test
    void getHealthFacilityTypes() {
        List<HealthFacilityTypes> healthFacilityTypes = new ArrayList<>();
        when(healthFacilityTypeRepository.findAll()).thenReturn(healthFacilityTypes);

        List<HealthFacilityTypesDTO> response = dataService.getHealthFacilityTypes();
        assertNotNull(response);
    }

    @Test
    void getVillagesByIds() {
        List<Long> ids = List.of(1l);
        List<Map<String, Object>> villages = List.of(new HashMap<>());

        when(villageRepository.getVillagesByIds(ids)).thenReturn(villages);
        List<VillageDTO> response = dataService.getVillagesByIds(ids);
        assertNotNull(response);
    }


    @Test
    void getCountriesPhoneCodes() {
        //given
        List<Country> countries = List.of(TestDataProvider.getCountry());

        //when
        when(countryRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(countries);

        //then
        List<String> response = dataService.getCountriesPhoneCodes();
        assertNotNull(response);
    }

    @Test
    void getCultures() {
        //given
        List<Culture> cultures = List.of(TestDataProvider.getCulture());

        //when
        when(cultureRespository.findAll()).thenReturn(cultures);

        //then
        List<Culture> response = dataService.getCultures();
        assertNotNull(response);
    }

    @Test
    void getUnlinkedVillages() {
        DataRequestDTO request = new DataRequestDTO();
        List<Village> villages = null;

        request.setChiefdomId(1L);
        request.setCountryId(1L);
        request.setDistrictId(1L);
        request.setHealthFacilityId(1L);

        when(villageRepository.getUnlinkedVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(),
                request.getHealthFacilityId(), request.getSearchTerm())).thenReturn(villages);
        List<VillageDTO> response = dataService.getUnlinkedVillages(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response, new ArrayList<>());

        villages = new ArrayList<>();
        when(villageRepository.getUnlinkedVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(),
                request.getHealthFacilityId(), request.getSearchTerm())).thenReturn(villages);
        response = dataService.getVillages(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response, new ArrayList<>());

        villages = List.of(TestDataProvider.getVillage());
        when(villageRepository.getUnlinkedVillages(request.getCountryId(), request.getDistrictId(), request.getChiefdomId(),
                request.getHealthFacilityId(), request.getSearchTerm())).thenReturn(villages);
        response = dataService.getUnlinkedVillages(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(villages.size(), response.size());
    }

    @Test
    void getVillagesWithoutUsers() {
        SearchRequestDTO request = new SearchRequestDTO();
        List<Village> villages = null;
        List<Map<String, Object>> villageResponse = null;

        request.setUserId(1L);
        request.setTenantIds(List.of(1L));

        when(villageRepository.findUnlinkedVillagesByUserId(request.getTenantIds(), request.getUserId())).thenReturn(
                null);
        List<VillageDTO> response = dataService.getVillagesWithoutUsers(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response, new ArrayList<>());

        villages = new ArrayList<>();
        villageResponse = new ArrayList<>();
        when(villageRepository.findUnlinkedVillagesByUserId(request.getTenantIds(), request.getUserId())).thenReturn(villageResponse);
        response = dataService.getVillagesWithoutUsers(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response, new ArrayList<>());

        Village village = new Village();
        village.setId(1l);
        village.setName("village 1");

        villages = List.of(TestDataProvider.getVillage());
        villageResponse.add(Map.of("name", "Village 1", "id",1L));
        when(villageRepository.findUnlinkedVillagesByUserId(request.getTenantIds(), request.getUserId())).thenReturn(villageResponse);
        response = dataService.getVillagesWithoutUsers(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(villages.size(), response.size());
    }
    
    @Test
    void testGetCountryList() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getRequestDtoForPagination("a", Constants.ZERO,
                TestConstants.TEN);
        requestDTO.setTenantId(TestConstants.ONE);
        String formattedSearchTerm = "a".replaceAll(Constants.SEARCH_TERM, Constants.EMPTY);
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN, Sort.by(Constants.CREATED_AT).descending());
        Page<Country> countries = new PageImpl<>(TestDataProvider.getCountries());
        List<Map<String, Object>> sitesCount = List.of(Map.of(Constants.COUNTRY_ID, TestConstants.ONE,
                Constants.COUNT, TestConstants.ONE), Map.of(Constants.COUNTRY_ID, TestConstants.TWO,
                Constants.COUNT, TestConstants.ONE));
        List<Map<String, Object>> operatingUnitsCount = List.of(Map.of(Constants.COUNTRY_ID, TestConstants.ONE,
                Constants.COUNT, TestConstants.ONE), Map.of(Constants.COUNTRY_ID, TestConstants.TWO,
                Constants.COUNT, TestConstants.ONE));
        List<Map<String, Object>> accountsCount = List.of(Map.of(Constants.COUNTRY_ID, TestConstants.ONE,
                Constants.COUNT, TestConstants.ONE), Map.of(Constants.COUNTRY_ID, TestConstants.TWO,
                Constants.COUNT, TestConstants.ONE));
        List<CountryListDTO> countryListDTOs = TestDataProvider.getCountryListDTOs();
        List<Long> countryIds = List.of(TestConstants.ONE, TestConstants.TWO);

        //when
        when(countryService.getCountries(formattedSearchTerm, pageable)).thenReturn(countries);
        when(healthFacilityService.getHealthFacilityCountByCountryIds(
                countries.stream().map(BaseEntity::getId).toList(), Constants.BOOLEAN_TRUE)).thenReturn(sitesCount);
        when(chiefdomService.getChiefdomCountByCountryIds(
                countries.stream().map(BaseEntity::getId).toList(), Constants.BOOLEAN_TRUE)).thenReturn(operatingUnitsCount);
        when(districtService.getDistrictCountByCountryIds(
                countries.stream().map(BaseEntity::getId).toList(), Constants.BOOLEAN_TRUE)).thenReturn(accountsCount);

        //then
        ResponseListDTO<CountryListDTO> actualCountries = dataService.getCountryList(requestDTO);
        assertNotNull(actualCountries);
        assertFalse(Objects.isNull(actualCountries.getData()));
        assertFalse(Objects.isNull(actualCountries.getTotalCount()));
        assertEquals(countryListDTOs, actualCountries.getData());
        assertEquals(countryIds.size(), actualCountries.getTotalCount());
    }

    @Test
    void getEmptyCountryList() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getRequestDtoForPagination("%", Constants.ZERO,
                TestConstants.TEN);

        //then
        ResponseListDTO<CountryListDTO> responseListDTO = dataService.getCountryList(requestDTO);
        assertNotNull(responseListDTO);
        assertEquals(Constants.LONG_ZERO, responseListDTO.getTotalCount());
    }

    @Test
    void getCommunityUnitsReturnsNonEmptyListForValidRequest() {
        SearchRequestDTO requestDTO = TestDataProvider.getRequestDtoForPagination(Constants.DATA, Constants.ZERO,
                TestConstants.TEN);
        Pageable pageable = Pagination.setPagination(requestDTO.getSkip(), requestDTO.getLimit(), Constants.NAME, Constants.BOOLEAN_FALSE);
        Page<CommunityUnit> communityUnitsPage = new PageImpl<>(List.of(TestDataProvider.getCommunityUnit()));

        //when
        when(communityUnitRepository.getCommunityUnits(requestDTO.getCountryId(), requestDTO.getParentRegionId(), requestDTO.getSearchTerm(), pageable))
                .thenReturn(communityUnitsPage);
        when(modelMapper.map(communityUnitsPage, new TypeToken<List<CommonResponseDTO>>() {
        }.getType())).thenReturn(List.of(new CommonResponseDTO()));

        //then
        ResponseListDTO<CommonResponseDTO> response = dataService.getCommunityUnits(requestDTO);
        assertNotNull(response);
        assertFalse(response.getData().isEmpty());
        assertEquals(Constants.LONG_ONE, response.getTotalCount());
    }

    @Test
    void getCommunityUnitsReturnsEmptyListForInvalidSearchTerm() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setSearchTerm("/");

        //then
        ResponseListDTO<CommonResponseDTO> response = dataService.getCommunityUnits(requestDTO);
        assertNotNull(response);
        assertEquals(Constants.LONG_ZERO, response.getTotalCount());
        assertNull(response.getData());
    }

    @Test
    void getCommunityUnitsHandlesPaginationCorrectly() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getRequestDtoForPagination(Constants.DATA, Constants.ZERO,
                Constants.FOUR);
        Pageable pageable = Pagination.setPagination(requestDTO.getSkip(), requestDTO.getLimit(), Constants.NAME, Constants.BOOLEAN_FALSE);
        Page<CommunityUnit> communityUnitsPage = new PageImpl<>(List.of(TestDataProvider.getCommunityUnit(), TestDataProvider.getCommunityUnit()), pageable, 2);

        //when
        when(communityUnitRepository.getCommunityUnits(requestDTO.getCountryId(), requestDTO.getParentRegionId(),
                requestDTO.getSearchTerm(), pageable)).thenReturn(communityUnitsPage);
        when(modelMapper.map(communityUnitsPage, new TypeToken<List<CommonResponseDTO>>() {
        }.getType()))
                .thenReturn(List.of(TestDataProvider.getCommunityUnit(), TestDataProvider.getCommunityUnit()));

        //then
        ResponseListDTO<CommonResponseDTO> response = dataService.getCommunityUnits(requestDTO);
        assertNotNull(response);
        assertEquals(Constants.TWO, response.getData().size());
        assertEquals(Constants.TWO, response.getTotalCount());
    }

    @Test
    void getCommunityUnitsReturnsEmptyListWhenLimitIsZero() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setSearchTerm(TestConstants.ARGUMENT);
        requestDTO.setLimit(Constants.ZERO);
        Page<CommunityUnit> communityUnitsPage = new PageImpl<>(new ArrayList<>());

        //when
        when(communityUnitRepository.getCommunityUnits(requestDTO.getCountryId(), requestDTO.getParentRegionId(),
                requestDTO.getSearchTerm(), null)).thenReturn(communityUnitsPage);
        when(modelMapper.map(communityUnitsPage, new TypeToken<List<CommonResponseDTO>>() {
        }.getType()))
                .thenReturn(List.of(TestDataProvider.getCommunityUnit(), TestDataProvider.getCommunityUnit()));

        //then
        ResponseListDTO<CommonResponseDTO> response = dataService.getCommunityUnits(requestDTO);
        assertNotNull(response);
        assertEquals(Constants.LONG_ZERO, response.getTotalCount());
        assertNull(response.getData());
    }

    @Test
    void getChiefdomByCountryId() {
        Long countryId = 1L;
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        List<Chiefdom> chiefdoms = List.of(chiefdom);

        when(chiefdomRepository.findByCountryIdAndIsDeletedFalse(countryId)).thenReturn(chiefdoms);
        dataService.getChiefdomsByCountryId(countryId);
    }

    @Test
    void getVillagesByCountryId() {
        Long countryId = 1L;
        VillageDTO villageDTO = new VillageDTO();
        villageDTO.setCountryId(countryId);
        List<VillageDTO> villageDTOList = List.of(villageDTO);

        when(villageRepository.findByCountryIdAndIsDeletedFalse(countryId)).thenReturn(List.of());
        dataService.getVillagesByCountryId(countryId);
    }
}
