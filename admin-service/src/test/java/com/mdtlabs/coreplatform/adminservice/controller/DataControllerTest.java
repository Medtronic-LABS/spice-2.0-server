package com.mdtlabs.coreplatform.adminservice.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityTypesDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.adminservice.model.entity.Unit;
import com.mdtlabs.coreplatform.adminservice.service.UnitService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.CommonResponseDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DataRequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.service.DataService;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;

@ExtendWith(MockitoExtension.class)
class DataControllerTest {

    @InjectMocks
    private DataController dataController;

    @Mock
    private DataService dataService;

    @Mock
    private UnitService unitService;

    @Test
    void getVillages() {
        DataRequestDTO request = new DataRequestDTO();
        List<VillageDTO> villages = List.of(new VillageDTO());
        when(dataService.getVillages(request)).thenReturn(villages);

        SuccessResponse<VillageDTO> response = dataController.getVillages(request);
        assertNotNull(response);
    }

    @Test
    void getChiefdoms() {
        DataRequestDTO request = new DataRequestDTO();
        List<ChiefdomDTO> chiefdoms = List.of(new ChiefdomDTO());
        when(dataService.getChiefdoms(request)).thenReturn(chiefdoms);

        SuccessResponse<ChiefdomDTO> response = dataController.getChiefdoms(request);
        assertNotNull(response);    
    }

    @Test
    void getDistricts() {
        DataRequestDTO request = new DataRequestDTO();
        List<DistrictDTO> districts = List.of(new DistrictDTO());
        when(dataService.getDistricts(request)).thenReturn(districts);

        SuccessResponse<DistrictDTO> response = dataController.getDistricts(request);
        assertNotNull(response);    
    }

    @Test
    void getRegionDetails() {
        ResponseListDTO<Map<String, Object>> regionDetails = new ResponseListDTO<>();
        SearchRequestDTO request = new SearchRequestDTO();

        when(dataService.getRegionDetails(request)).thenReturn(regionDetails);

        SuccessResponse<Map<String, Object>> response = dataController.getRegionDetails(request);
        assertNotNull(response);    
    }

    @Test
    void getHealthFacilityTypes() {
        List<HealthFacilityTypesDTO> healthFacilityTypes = new ArrayList<>();

        when(dataService.getHealthFacilityTypes()).thenReturn(healthFacilityTypes);

        SuccessResponse<HealthFacilityTypesDTO> response = dataController.getHealthFacilityTypes();
        assertNotNull(response);
    } 

    @Test
    void getVillagesByDistrict() {
        DataRequestDTO request = new DataRequestDTO();
        List<VillageDTO> villages = List.of(new VillageDTO());
        when(dataService.getVillages(request)).thenReturn(villages);

        List<VillageDTO> response = dataController.getVillagesByDistrict(request);
        assertNotNull(response);    
    }

    @Test
    void getApiRolePermissions() {
        List<ApiRolePermission> villages = List.of(new ApiRolePermission());
        when(dataService.getApiRolePermissionsByServiceName(TestConstants.STRING_ONE)).thenReturn(villages);

        List<ApiRolePermission> response = dataController.getApiRolePermissions(TestConstants.STRING_ONE);
        assertNotNull(response);
    }

    @Test
    void getVillagesByIds() {
        List<VillageDTO> villages = List.of(new VillageDTO());
        when(dataService.getVillagesByIds(List.of(TestConstants.ONE))).thenReturn(villages);

        List<VillageDTO> response = dataController.getVillagesByIds(List.of(TestConstants.ONE));
        assertNotNull(response);
    }

    @Test
    void getCountries() {
        when(dataService.getCountriesPhoneCodes()).thenReturn(List.of(TestConstants.STRING_ONE));

        SuccessResponse<String> response = dataController.getCountries();
        assertNotNull(response);
    }

    @Test
    void getCultures() {
        List<Culture> cultures = List.of(new Culture());
        when(dataService.getCultures()).thenReturn(cultures);

        SuccessResponse<Culture> response = dataController.getCultures();
        assertNotNull(response);
    }

    @Test
    void getUnlinkedVillages() {
        DataRequestDTO request = new DataRequestDTO();
        List<VillageDTO> villages = List.of(new VillageDTO());
        when(dataService.getUnlinkedVillages(request)).thenReturn(villages);

        SuccessResponse<VillageDTO> response = dataController.getUnlinkedVillages(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getUnitsByTypeTest() {
        List<Unit> response = new ArrayList<>();
        when(unitService.getUnitsByType(TestConstants.UNIT_TYPE)).thenReturn(response);

        Assertions.assertNotNull(dataController.getUnitsByType(TestConstants.UNIT_TYPE));
    }

    @Test
    void getUnlinkedVillagesByHealthFacility() {
        SearchRequestDTO request = new SearchRequestDTO();
        List<VillageDTO> villages = List.of(new VillageDTO());
        when(dataService.getVillagesWithoutUsers(request)).thenReturn(villages);

        SuccessResponse<VillageDTO> response = dataController.getUserVillageList(request);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    void getCountryListWithCountries() {
        //given
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        List<CountryListDTO> countryList = List.of(TestDataProvider.getCountryListDTO());
        ResponseListDTO<CountryListDTO> responseListDTO = new ResponseListDTO<>(countryList, (long) countryList.size());

        //when
        when(dataService.getCountryList(requestDto)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CountryListDTO> response = dataController.getCountryList(requestDto);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getCountryListWithNullTotalCountForEmptyList() {
        //given
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        ResponseListDTO<CountryListDTO> responseListDTO = new ResponseListDTO<>(Collections.emptyList(), null);

        //when
        when(dataService.getCountryList(requestDto)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CountryListDTO> response = dataController.getCountryList(requestDto);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getCommunityUnitsWithUnits() {
        //given
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        List<CommonResponseDTO> communityUnits = List.of(new CommonResponseDTO());
        ResponseListDTO<CommonResponseDTO> responseListDTO = new ResponseListDTO<>(communityUnits, Long.valueOf(communityUnits.size()));

        //when
        when(dataService.getCommunityUnits(request)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CommonResponseDTO> response = dataController.getCommunityUnits(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getCommunityUnitsWithNullCount() {
        //given
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        ResponseListDTO<CommonResponseDTO> responseListDTO = new ResponseListDTO<>(Collections.emptyList(), null);

        //when
        when(dataService.getCommunityUnits(request)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CommonResponseDTO> response = dataController.getCommunityUnits(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getCountryListCountries() {
        //given
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        List<CountryListDTO> countryList = List.of(new CountryListDTO());
        ResponseListDTO<CountryListDTO> responseListDTO = new ResponseListDTO<>(countryList, Long.valueOf(countryList.size()));

        //when
        when(dataService.getCountryList(requestDto)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CountryListDTO> response = dataController.getCountryList(requestDto);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getCountryListWithEmptyList() {
        //given
        SearchRequestDTO requestDto = new SearchRequestDTO();
        ResponseListDTO<CountryListDTO> responseListDTO = new ResponseListDTO<>(Collections.emptyList(), null);

        //when
        when(dataService.getCountryList(requestDto)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CountryListDTO> response = dataController.getCountryList(requestDto);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getCommunityUnits() {
        //given
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        List<CommonResponseDTO> communityUnits = List.of(new CommonResponseDTO());
        ResponseListDTO<CommonResponseDTO> responseListDTO = new ResponseListDTO<>(communityUnits, (long) communityUnits.size());

        //when
        when(dataService.getCommunityUnits(request)).thenReturn(responseListDTO);

        //then
        SuccessResponse<CommonResponseDTO> response = dataController.getCommunityUnits(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

}
