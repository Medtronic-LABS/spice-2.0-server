package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.dto.RequestDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityFilterDTO;
import com.mdtlabs.coreplatform.adminservice.service.ChiefdomService;
import com.mdtlabs.coreplatform.adminservice.service.CountryService;
import com.mdtlabs.coreplatform.adminservice.service.DistrictService;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
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
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

@ExtendWith(MockitoExtension.class)
class AdminControllerTest {

    @InjectMocks
    AdminController adminController;

    @Mock
    private HealthFacilityService healthFacilityService;

    @Mock
    private DistrictService districtService;

    @Mock
    private CountryService countryService;

    @Mock
    private ChiefdomService chiefdomService;

    @Mock
    private ModelMapper modelMapper;

    @Test
    void createCountry() {
        CountryRequestDTO request = new CountryRequestDTO();
        Country country = TestDataProvider.getCountry();

        when(countryService.createCountry(request)).thenReturn(country);

        Country response = adminController.createCountry(request);
        assertNotNull(response);
    }

    @Test
    void updateCountry() {
        CountryRequestDTO request = new CountryRequestDTO();
        Country country = TestDataProvider.getCountry();

        when(countryService.updateCountry(request)).thenReturn(country);

        SuccessResponse<Country> response = adminController.updateCountry(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void getCountryById() {
        SearchRequestDTO request = new SearchRequestDTO();
        CountryDTO countryDTO = new CountryDTO();

        when(countryService.getCountryDetails(request)).thenReturn(countryDTO);

        SuccessResponse<CountryDTO> response = adminController.getCountryById(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void addRegionAdmin() {
        UserRequestDTO userRequest = new UserRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(countryService.addRegionAdmin(userRequest)).thenReturn(userResponse);
        SuccessResponse<UserResponseDTO> response = adminController.addRegionAdmin(userRequest);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void updateRegionAdmin() {
        UserRequestDTO userRequest = new UserRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(countryService.updateRegionAdmin(userRequest)).thenReturn(userResponse);
        SuccessResponse<UserResponseDTO> response = adminController.updateRegionAdmin(userRequest);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void removeRegionAdmin() {
        SearchRequestDTO request = new SearchRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(countryService.removeAdmin(request)).thenReturn(userResponse);
        SuccessResponse<UserResponseDTO> response = adminController.removeRegionAdmin(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void addHealthFacility() {
        HealthFacilityRequestDTO request = new HealthFacilityRequestDTO();
        HealthFacility healthFacility = new HealthFacility();

        when(healthFacilityService.createHealthFacility(request)).thenReturn(healthFacility);

        HealthFacility response = adminController.addHealthFacility(request);
        assertNotNull(response);
    }

    @Test
    void updateHealthFacility() {
        HealthFacilityRequestDTO request = new HealthFacilityRequestDTO();
        HealthFacility healthFacility = new HealthFacility();

        when(healthFacilityService.updateHealthFacility(request)).thenReturn(healthFacility);

        SuccessResponse<HealthFacility> response = adminController.updateHealthFacility(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getHealthFacilityList() {
        SearchRequestDTO request = new SearchRequestDTO();
        ResponseListDTO<HealthFacilityDTO> healthFacility = new ResponseListDTO<>();

        when(healthFacilityService.getHealthFacilities(request)).thenReturn(healthFacility);

        SuccessResponse<HealthFacilityDTO> response = adminController.getHealthFacilityList(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getHealthFacilityListForCfrUser() {
        SearchRequestDTO request = new SearchRequestDTO();
        ResponseListDTO<HealthFacilityDTO> healthFacility = new ResponseListDTO<>();

        when(healthFacilityService.getHealthFacilities(request)).thenReturn(healthFacility);

        ResponseEntity<ResponseListDTO<HealthFacilityDTO>> response = adminController.getHealthFacilityListForCfr(request);
        assertNotNull(response);
        assertEquals(healthFacility, response.getBody());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getHealthFacilityDetails() {
        SearchRequestDTO request = new SearchRequestDTO();
        HealthFacilityDTO healthFacility = new HealthFacilityDTO();

        when(healthFacilityService.getHealthFacilityDetails(request)).thenReturn(healthFacility);

        SuccessResponse<HealthFacilityDTO> response = adminController.getHealthFacilityDetails(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void getHealthFacilityByFhirIdTest() {
        RequestDTO request = new RequestDTO();
        HealthFacilityRequestDTO requestDTO = new  HealthFacilityRequestDTO();

        when(healthFacilityService.getHealthFacilityByFhirId(request)).thenReturn(requestDTO);

        HealthFacilityRequestDTO response = adminController.getHealthFacilityByFhirId(request);

        Assertions.assertNotNull(response);
    }

    @Test
    void addHealthFacilityUser() {
        UserRequestDTO request = new UserRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(healthFacilityService.addHealthFacilityUser(request)).thenReturn(userResponse);

        SuccessResponse<UserResponseDTO> response = adminController.addHealthFacilityUser(request);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        request.setTenantId(TestConstants.ONE);
        response = adminController.addHealthFacilityUser(request);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void updateHealthfacilityAdmin() {
        UserRequestDTO request = new UserRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(healthFacilityService.updateHealthFacilityUser(request)).thenReturn(userResponse);

        SuccessResponse<UserResponseDTO> response = adminController.updateHealthfacilityAdmin(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteHealthFacilityAdmin() {
        SearchRequestDTO request = new SearchRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(healthFacilityService.deleteHealthFacilityAdmin(request)).thenReturn(userResponse);

        SuccessResponse<UserResponseDTO> response = adminController.deleteHealthFacilityAdmin(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getHealthFacilitiesByCountry() {
        Long id = 1l;
        List<HealthFacilityDTO> healthFacilityDTOs = new ArrayList<>();

        when(healthFacilityService.getHealthFacilitiesByCountryId(id)).thenReturn(healthFacilityDTOs);

        List<HealthFacilityDTO> response = adminController.getHealthFacilitiesByCountry(id);
        assertNotNull(response);
    }

    @Test
    void getHealthFacilitiesByChiefdom() {
        Long id = 1l;
        List<HealthFacilityDTO> healthFacilityDTOs = new ArrayList<>();

        when(healthFacilityService.getHealthFacilitiesByChiefdomId(id)).thenReturn(healthFacilityDTOs);

        List<HealthFacilityDTO> response = adminController.getHealthFacilitiesByChiefdom(id);
        assertNotNull(response);
    }

    @Test
    void getHealthFacilitiesByTenants() {
        List<Long> ids = List.of(1l);
        List<HealthFacilityDTO> healthFacilityDTOs = new ArrayList<>();

        when(healthFacilityService.getHealthFacilitiesByTenants(ids)).thenReturn(healthFacilityDTOs);

        List<HealthFacilityDTO> response = adminController.getHealthFacilitiesByTenants(ids);
        assertNotNull(response);
    }

    @Test
    void getVillagesByTenants() {
        SearchRequestDTO request = new SearchRequestDTO();
        List<VillageDTO> villageDTOs = new ArrayList<>();

        when(healthFacilityService.getVillagesByFacility(request.getTenantIds())).thenReturn(villageDTOs);

        SuccessResponse<VillageDTO> response = adminController.getVillagesByTenants(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    @DisplayName("Should return all health facilities by district id when exist")
    void shouldReturnAllHealthFacilitiesByDistrictIdWhenExist() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        List<HealthFacilityDTO> mockFacilities = List.of(new HealthFacilityDTO());

        when(healthFacilityService.getAllHealthFacilitiesByDistrictId(requestDTO)).thenReturn(mockFacilities);

        SuccessResponse<HealthFacilityDTO> response = adminController.getAllHealthFacilities(requestDTO);

        assertNotNull(response);
    }

    @Test
    @DisplayName("Should return empty list when no health facilities exist for district id")
    void shouldReturnEmptyListWhenNoHealthFacilitiesExistForDistrictId() {
        List<HealthFacilityDTO> mockFacilities = new ArrayList<>();
        SearchRequestDTO requestDTO = new SearchRequestDTO();

        when(healthFacilityService.getAllHealthFacilitiesByDistrictId(requestDTO)).thenReturn(mockFacilities);

        SuccessResponse<HealthFacilityDTO> response = adminController.getAllHealthFacilities(requestDTO);

        assertNotNull(response);
    }

    @Test
    void deleteHealthFacility() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();

        //when
        when(healthFacilityService.deleteHealthFacility(requestDTO)).thenReturn(TestConstants.STRING_ONE);

        //then
        ResponseEntity<String> response = adminController.deleteHealthFacility(requestDTO);
        Assertions.assertNotNull(response);
    }
    
    void getDistricts() {
        SearchRequestDTO request = new SearchRequestDTO();
        ResponseListDTO<DistrictDTO> mockedDistricts = new ResponseListDTO<>();
        when(districtService.getDistricts(request)).thenReturn(mockedDistricts);
        SuccessResponse<DistrictDTO> response = adminController.getDistricts(request);
        assertNotNull(response);
    }

    @Test
    void testValidateHealthFacility() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();

        doNothing().when(healthFacilityService).validateHealthFacility(requestDTO);
        SuccessResponse<String> response = adminController.validateHealthFacility(requestDTO);
        Assertions.assertNotNull(response);

    }

    void getDistrictList() {
        SearchRequestDTO request = new SearchRequestDTO();
        ResponseListDTO<DistrictListDTO> mockedDistricts = new ResponseListDTO<>();
        when(districtService.getDistrictList(request)).thenReturn(mockedDistricts);
        SuccessResponse<DistrictListDTO> response = adminController.getDistrictList(request);
        assertNotNull(response);
    }

    @Test
    void testGetFacilityVillageIdsByTenantId() {
        when(healthFacilityService.getFacilityVillageIdsByTenantId()).thenReturn(List.of(TestConstants.ONE));

        List<Long> response = adminController.getFacilityVillageIdsByTenantId();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(TestConstants.ONE, response.size());
    }

    void getDistrictDetails() {
        SearchRequestDTO request = new SearchRequestDTO();
        DistrictDTO mockedDistrictDetails = new DistrictDTO();
        when(districtService.getDistrictDetails(request)).thenReturn(mockedDistrictDetails);
        SuccessResponse<DistrictDTO> response = adminController.getDistrictDetails(request);
        assertNotNull(response);
    }

    @Test
    void addDistrict() {
        DistrictRequestDTO request = new DistrictRequestDTO();
        District mockeddistrict = new District();
        when(districtService.createDistrict(request)).thenReturn(mockeddistrict);
        District district = adminController.addDistrict(request);
        assertNotNull(district);
    }

    @Test
    void updateDistrict() {
        DistrictRequestDTO request = new DistrictRequestDTO();
        District mockeddistrict = new District();
        when(districtService.updateDistrict(request)).thenReturn(mockeddistrict);
        SuccessResponse<District> response = adminController.updateDistrict(request);
        assertNotNull(response);
    }

    @Test
    void deactivateDistrict() {
        DistrictRequestDTO request = new DistrictRequestDTO();
        when(districtService.activateOrDeactivateDistrict(request)).thenReturn(true);
        SuccessResponse<String> response = adminController.deactivateDistrict(request);
        assertNotNull(response);
    }

    @Test
    void activateDistrictById() {
        DistrictRequestDTO request = new DistrictRequestDTO();
        when(districtService.activateOrDeactivateDistrict(request)).thenReturn(true);
        SuccessResponse<String> response = adminController.activateDistrictById(request);
        assertNotNull(response);
    }

    @Test
    void getHealthFacilityFilterListWithResults() {
        //given
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        List<HealthFacilityFilterDTO> healthFacilityFilterDTOs = List.of(new HealthFacilityFilterDTO());
        ResponseListDTO<HealthFacilityFilterDTO> responseListDTO = new ResponseListDTO<>(healthFacilityFilterDTOs, Long.valueOf(healthFacilityFilterDTOs.size()));

        //when
        when(healthFacilityService.getHealthFacilitiesFilter(requestDto)).thenReturn(responseListDTO);

        //then
        SuccessResponse<HealthFacilityFilterDTO> response = adminController.getHealthFacilityFilterList(requestDto);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getHealthFacilityFilterListWithNoResults() {
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        ResponseListDTO<HealthFacilityFilterDTO> responseListDTO = new ResponseListDTO<>(Collections.emptyList(), 0L);

        //when
        when(healthFacilityService.getHealthFacilitiesFilter(requestDto)).thenReturn(responseListDTO);

        //then
        SuccessResponse<HealthFacilityFilterDTO> response = adminController.getHealthFacilityFilterList(requestDto);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getDeactivatedDistrict() {
        SearchRequestDTO searchRequestDto = new SearchRequestDTO();
        ResponseListDTO<DistrictDTO> mockedDistricts = new ResponseListDTO<>();
        when(districtService.getDeactivatedDistricts(searchRequestDto)).thenReturn(mockedDistricts);
        SuccessResponse<DistrictDTO> response = adminController.getDeactivatedDistricts(searchRequestDto);
        Assertions.assertNotNull(response);
    }

    @Test
    void getAllChiefdoms() {
        SearchRequestDTO searchRequestDto = new SearchRequestDTO();
        ResponseListDTO<ChiefdomDTO> mockedChiefdoms = new ResponseListDTO<>();
        when(chiefdomService.getAllChiefdoms(searchRequestDto)).thenReturn(mockedChiefdoms);
        SuccessResponse<ChiefdomDTO> response = adminController.getAllChiefdoms(searchRequestDto);
        Assertions.assertNotNull(response);
    }

    @Test
    void getChiefdoms() {
        ChiefdomRequestDTO chiefdomRequestDTO = new ChiefdomRequestDTO();
        ResponseListDTO<ChiefdomDTO> mockedChiefdoms = new ResponseListDTO<>();
        when(chiefdomService.getChiefdomList(chiefdomRequestDTO)).thenReturn(mockedChiefdoms);
        SuccessResponse<ChiefdomDTO> response = adminController.getChiefdoms(chiefdomRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateChiefdom() {
        ChiefdomRequestDTO chiefdomRequestDTO = new ChiefdomRequestDTO();
        Chiefdom mockedChiefdom = new Chiefdom();
        when(chiefdomService.updateChiefdom(mockedChiefdom)).thenReturn(mockedChiefdom);
        SuccessResponse<Chiefdom> response = adminController.updateChiefdom(chiefdomRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createChiefdom() {
        ChiefdomRequestDTO chiefdomRequestDTO = TestDataProvider.getChiefdomRequestDTO();
        List<Village> villages = List.of(TestDataProvider.getVillage());
        chiefdomRequestDTO.setVillages(villages);
        Chiefdom mockedChiefdom = new ModelMapper().map(chiefdomRequestDTO, Chiefdom.class);
        when(chiefdomService.createChiefdom(mockedChiefdom,villages)).thenReturn(mockedChiefdom);
        Chiefdom response = adminController.createChiefdom(chiefdomRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getChiefdomDetails() {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        ChiefdomDTO mockedChiefdomDetails = new ChiefdomDTO();
        when(chiefdomService.getChiefdomDetails(searchRequestDTO)).thenReturn(mockedChiefdomDetails);
        SuccessResponse<ChiefdomDTO> response = adminController.getChiefdomDetails(searchRequestDTO);
        Assertions.assertNotNull(response);
    }
}
