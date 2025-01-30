package com.mdtlabs.coreplatform.adminservice.service;


import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.common.TestCommonMethods;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictListDTO;
import com.mdtlabs.coreplatform.adminservice.repository.DistrictRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.DistrictServiceImpl;
import com.mdtlabs.coreplatform.adminservice.service.impl.HealthFacilityServiceImpl;
import com.mdtlabs.coreplatform.adminservice.service.impl.ChiefdomServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.internal.InheritingConfiguration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DistrictServiceTest {
    
    @InjectMocks
    DistrictServiceImpl districtService;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private DistrictRepository districtRepository;

    @Mock
    private UserServiceApiInterface userApiInterface;

    @Mock
    private HealthFacilityServiceImpl healthFacilityService;

    @Mock
    private ChiefdomServiceImpl chiefdomService;

    @Mock
    private RedisTemplate<String, String> redisTemplate;


    @Test
    void testCreateDistrict() {
        //given
        DistrictRequestDTO districtRequestDTO = TestDataProvider.getDistrictRequestDTO();
        District district = new District();
        district.setName("test");

        //when
        when(modelMapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(modelMapper.map(districtRequestDTO, District.class)).thenReturn(district);
        when(districtRepository.save(district)).thenReturn(district);

        //then
        District actualDistrict = districtService.createDistrict(districtRequestDTO);
        assertNotNull(actualDistrict);
        assertEquals(districtRequestDTO.getName(), actualDistrict.getName());
    }

    @Test
    void testGetDistricts() {
        //given
        String searchTerm = "test";
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(searchTerm, Constants.ZERO,
                TestConstants.TEN);
        searchRequestDTO.setTenantId(TestConstants.ONE);
        String formattedSearchTerm = searchRequestDTO.getSearchTerm();
        Organization organization = TestDataProvider.getOrganization();
        ResponseEntity<Organization> expectedOrganization = new ResponseEntity<>(organization, HttpStatus.OK);
        Page<District> pages = new PageImpl<>(TestDataProvider.getDistricts());
        List<DistrictDTO> districtDTOs = pages.stream().map(page -> modelMapper.map(page, DistrictDTO.class)).toList();
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN, (Constants.BOOLEAN_FALSE ?
                Sort.by(Constants.UPDATED_AT)
                        .ascending() : Sort.by(Constants.UPDATED_AT).descending()));

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        TestCommonMethods.getStaticMockValidation(searchTerm);

        when(userApiInterface.getOrganizationById(TestConstants.TEST_TOKEN, TestConstants.ONE, TestConstants.ONE, TestConstants.CLIENT))
                .thenReturn(expectedOrganization);
        when(districtRepository.findDistrictList(Constants.EMPTY, TestConstants.ONE, pageable))
                .thenReturn(pages);
        when(districtRepository.findDistrictList(formattedSearchTerm, TestConstants.ONE, pageable))
                .thenReturn(pages);
        when(modelMapper.map(pages.stream().toList(), new TypeToken<List<DistrictDTO>>() {
        }.getType()))
                .thenReturn(List.of(districtDTOs));

        //then
        ResponseListDTO<DistrictDTO> actualDistricts = districtService.getDistricts(searchRequestDTO);
        assertEquals(Constants.TWO, actualDistricts.getTotalCount());
        assertNotNull(actualDistricts);
        assertEquals(List.of(districtDTOs), actualDistricts.getData());
        TestCommonMethods.cleanUp();
    }

    @Test
    void testGetDistrictList() {
        String searchTerm = "test";
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(searchTerm, Constants.ZERO,
                TestConstants.TEN);
        searchRequestDTO.setTenantId(TestConstants.ONE);
        ResponseListDTO responseListDTO = new ResponseListDTO();
        Organization organization = TestDataProvider.getOrganization();
        ResponseEntity<Organization> expectedOrganization = new ResponseEntity<>(organization, HttpStatus.OK);
        Page<District> districts = new PageImpl<>(TestDataProvider.getDistricts());
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN);
        List<Map<String, Object>> healthFacilityCount = List.of(Map.of(TestConstants.STRING_ONE, Constants.ONE,
                Constants.STRING_TWO, Constants.ONE));
        List<Map<String, Object>> chiefdomCount = (List.of(Map.of(TestConstants.STRING_ONE, Constants.ONE,
                Constants.STRING_TWO, Constants.ONE)));
        DistrictListDTO districtListDTO = TestDataProvider.getDistrictListDTO(TestConstants.ONE,
                "test", Constants.THREE);
        districtListDTO.setHealthFacilityCount(Constants.ONE);
        districtListDTO.setChiefdomCount(Constants.ONE);
        DistrictListDTO secondDistrictListDTO = TestDataProvider.getDistrictListDTO(TestConstants.ONE,
                "test", Constants.THREE);
        secondDistrictListDTO.setHealthFacilityCount(Constants.ONE);
        secondDistrictListDTO.setChiefdomCount(Constants.ONE);
        List<DistrictListDTO> districtListDTOS = new ArrayList<>();
        districtListDTOS.add(districtListDTO);
        districtListDTOS.add(secondDistrictListDTO);
        List<Long> districtIds = List.of(TestConstants.ONE, TestConstants.TWO);

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        when(userApiInterface.getOrganizationById(TestConstants.TEST_TOKEN, TestConstants.ONE, TestConstants.ONE, TestConstants.CLIENT))
                .thenReturn(expectedOrganization);
        when(districtRepository.findDistrictList(searchTerm, TestConstants.ONE, pageable)).thenReturn(districts);
        when(districtRepository.findDistrictList(Constants.EMPTY, TestConstants.ONE, pageable)).thenReturn(districts);
        responseListDTO.setTotalCount(districts.getTotalElements());
        when(healthFacilityService.getCountByDistrictIds(districtIds)).thenReturn(healthFacilityCount);
        when(chiefdomService.getChiefdomCountByDistrictIds(districtIds, Constants.BOOLEAN_TRUE))
                .thenReturn(chiefdomCount);

        //then
        ResponseListDTO actualDistricts = districtService.getDistrictList(searchRequestDTO);
        TestCommonMethods.cleanUp();
        assertEquals(Constants.TWO, actualDistricts.getTotalCount());
    }

    @Test
    void testGetDistrictDetails() {
        //given
        String searchTerm = "test";
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(searchTerm, Constants.ZERO,
                TestConstants.TEN);
        searchRequestDTO.setTenantId(TestConstants.ONE);
        searchRequestDTO.setId(TestConstants.ONE);
        District district = TestDataProvider.getDistrict();
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();

        //when
        when(districtRepository.findByIdAndTenantIdAndIsActiveAndIsDeleted(searchRequestDTO.getId(),
                searchRequestDTO.getTenantId(), true, false)).thenReturn(district);
        when(modelMapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(modelMapper.map(district, DistrictDTO.class)).thenReturn(new DistrictDTO());
        when(userApiInterface.getUsersByTenantIds(TestConstants.TEST_TOKEN, TestConstants.CLIENT, searchRequestDTO)).thenReturn(
                List.of(new UserResponseDTO()));

        //then
        DistrictDTO actualDistrictDTO = districtService.getDistrictDetails(searchRequestDTO);
        assertEquals(Constants.ONE, actualDistrictDTO.getUsers().size());
        TestCommonMethods.cleanUp();
    }

    @Test
    void testGetDistrictDetailsThrowsDataNotAcceptableException() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        assertThrows(DataNotAcceptableException.class, () -> districtService.getDistrictDetails(requestDTO));

    }

    @Test
    void testActivateOrDeactivateDistrictThrowsDataNotFoundException() {
        DistrictRequestDTO requestDTO = new DistrictRequestDTO();
        requestDTO.setId(1L);
        requestDTO.setTenantId(1L);
        requestDTO.setIsActive(true);
        when(districtRepository.findByTenantIdAndIsDeletedFalseAndIsActive(requestDTO.getTenantId(),
                false)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> districtService.activateOrDeactivateDistrict(requestDTO));
    }

    @Test
    void testUpdateDistrict() {
        //given
        DistrictRequestDTO districtRequestDTO = TestDataProvider.getDistrictRequestDTO();
        districtRequestDTO.setId(1L);
        District district = new District();
        district.setName("test");
        Organization organization = TestDataProvider.getOrganization();
        organization.setId(districtRequestDTO.getTenantId());
        organization.setName(districtRequestDTO.getName());

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        when(modelMapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(modelMapper.map(districtRequestDTO, District.class)).thenReturn(district);
        doNothing().when(userApiInterface).updateOrganization(TestConstants.TEST_TOKEN, TestConstants.ONE,
                new OrganizationDTO(), TestConstants.CLIENT);
        when(districtRepository.findByIdAndIsDeleted(districtRequestDTO.getId(), false)).thenReturn(district);
        when(districtRepository.existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(districtRequestDTO.getName(),
                districtRequestDTO.getId())).thenReturn(false);
        when(districtRepository.save(district)).thenReturn(district);

        //then
        District actualDistrict = districtService.updateDistrict(districtRequestDTO);
        TestCommonMethods.cleanUp();
        assertEquals(district.getId(), actualDistrict.getId());
    }

    @Test
    void activateOrDeactivateDistrict() {
        //given
        DistrictRequestDTO districtRequestDTO = TestDataProvider.getDistrictRequestDTO();
        districtRequestDTO.setId(1L);
        districtRequestDTO.setIsActive(false);
        District district = new District();
        district.setName("test");
        district.setId(1L);
        List<Long> tenantIds = List.of(1L,1L,1L);
        List<HealthFacility> healthFacilities = new ArrayList<>();
        healthFacilities.add(TestDataProvider.getHealthFacility());
        List<String> healthFacilityFhirIds = List.of("1");


        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        when(districtRepository.findByTenantIdAndIsDeletedFalseAndIsActive(districtRequestDTO.getTenantId(),
                !districtRequestDTO.getIsActive())).thenReturn(district);
        when(districtRepository.save(district)).thenReturn(district);
        when(chiefdomService.activateOrDeactivateChiefdoms(null, district.getId(),
                districtRequestDTO.getIsActive())).thenReturn(tenantIds);
        when(healthFacilityService.activateOrDeactivateHealthFacility(null, district.getId(), null,
                districtRequestDTO.getIsActive())).thenReturn(healthFacilities);
        doNothing().when(userApiInterface).activateOrDeactivateOrg(TestConstants.TEST_TOKEN, TestConstants.ONE,
                tenantIds, districtRequestDTO.getIsActive(), healthFacilityFhirIds, TestConstants.CLIENT);
        doNothing().when(userApiInterface).activateOrDeactivateUser(TestConstants.TEST_TOKEN, TestConstants.ONE,
                tenantIds, districtRequestDTO.getIsActive(), TestConstants.CLIENT);
        when(redisTemplate.delete(Constants.ORGANIZATION_REDIS_KEY)).thenReturn(null);

        //then
        Boolean isActivatedOrDeactivated = districtService.activateOrDeactivateDistrict(districtRequestDTO);
        assertTrue(isActivatedOrDeactivated);
        TestCommonMethods.cleanUp();
    }

    @Test
    void testGetDeactivatedDistricts() {
        //given
        String searchTerm = "test";
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(searchTerm, Constants.ZERO,
                TestConstants.TEN);
        Page<District> pages = new PageImpl<>(TestDataProvider.getDistricts());
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN, (Constants.BOOLEAN_FALSE ?
                Sort.by(Constants.UPDATED_AT)
                        .ascending() : Sort.by(Constants.UPDATED_AT).descending()));

        //When
        when(districtRepository.getDeactivatedDistricts(searchTerm, null, pageable)).thenReturn(pages);

        //Then
        ResponseListDTO<DistrictDTO> actualDistrictDTOs = districtService.getDeactivatedDistricts(searchRequestDTO);
        assertEquals(Constants.TWO, actualDistrictDTOs.getTotalCount());
    }

    @Test
    void getDistrictCountByCountryIdsReturnsNonEmptyListForValidIds() {
        //given
        List<Long> countryIds = List.of(1L, 2L);
        List<Map<String, Object>> expectedResults = List.of(
                Map.of("countryId", 1L, "count", 5),
                Map.of("countryId", 2L, "count", 3)
        );

        //when
        when(districtRepository.getDistrictCountByCountryIds(countryIds, true)).thenReturn(expectedResults);

        //then
        List<Map<String, Object>> results = districtService.getDistrictCountByCountryIds(countryIds, true);
        assertNotNull(results);
        assertFalse(results.isEmpty());
        assertEquals(2, results.size());
    }

    @Test
    void getDistrictCountByCountryIdsReturnsEmptyListForInvalidIds() {
        //given
        List<Long> countryIds = List.of(99L, 100L);

        //when
        when(districtRepository.getDistrictCountByCountryIds(countryIds, true)).thenReturn(Collections.emptyList());

        //then
        List<Map<String, Object>> results = districtService.getDistrictCountByCountryIds(countryIds, true);
        assertNotNull(results);
        assertTrue(results.isEmpty());
    }

    @Test
    void getDistrictCountByCountryIdsHandlesInactiveDistricts() {
        //given
        List<Long> countryIds = List.of(1L);

        //when
        when(districtRepository.getDistrictCountByCountryIds(countryIds, false)).thenReturn(Collections.emptyList());

        //then
        List<Map<String, Object>> results = districtService.getDistrictCountByCountryIds(countryIds, false);
        assertNotNull(results);
        assertTrue(results.isEmpty());
    }
}
