package com.mdtlabs.coreplatform.adminservice.service;


import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.common.TestCommonMethods;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.ChiefdomRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.HealthFacilityServiceImpl;
import com.mdtlabs.coreplatform.adminservice.service.impl.ChiefdomServiceImpl;
import com.mdtlabs.coreplatform.adminservice.service.impl.VillageServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ChiefdomServiceTest {
    
    @InjectMocks
    ChiefdomServiceImpl chiefdomService;

    @Mock
    private UserServiceApiInterface userApiInterface;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private ChiefdomRepository chiefdomRepository;

    @Mock
    private HealthFacilityServiceImpl healthFacilityService;

    @Mock
    private RedisTemplate<String, Map<String, List<String>>> redisTemplate;

    @Mock
    private VillageServiceImpl villageService;

    @Test
    void testAllGetChiefdoms() {
        //given
        String searchTerm = "test";
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(searchTerm, Constants.ZERO,
                TestConstants.TEN);
        String formattedSearchTerm = "test".strip();
        searchRequestDTO.setTenantId(TestConstants.ONE);
        ResponseListDTO responseListDTO = new ResponseListDTO();
        Organization organization = TestDataProvider.getOrganization();
        organization.setFormName(Constants.COUNTRY);
        ResponseEntity<Organization> expectedOrganization = new ResponseEntity<>(organization, HttpStatus.OK);
        Page<Map<String, Object>> chiefdoms = new PageImpl<>(TestDataProvider.getChiefdoms());
        responseListDTO.setTotalCount(chiefdoms.getTotalElements());
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN, Sort.by(Constants.UPDATED_AT).descending());

        List<ChiefdomDTO> chiefdomDTOs = TestDataProvider.getChiefdomDTOs();

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        TestCommonMethods.getStaticMockValidation(searchTerm);
        when(userApiInterface.getOrganizationById(TestConstants.TEST_TOKEN, TestConstants.ONE, TestConstants.ONE, TestConstants.CLIENT))
                .thenReturn(expectedOrganization);
        when(modelMapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(modelMapper.map(chiefdomDTOs.stream().toList(), new TypeToken<List<ChiefdomDTO>>() {
        }.getType())).thenReturn(chiefdomDTOs);
        when(chiefdomRepository.getChiefdoms(formattedSearchTerm, TestConstants.ONE, null, pageable))
                .thenReturn(chiefdoms);
        when(chiefdomRepository.getChiefdoms(searchTerm, TestConstants.ONE, null, pageable))
                .thenReturn(chiefdoms);

        //then
        ResponseListDTO actualChiefdoms = chiefdomService.getAllChiefdoms(searchRequestDTO);
        TestCommonMethods.cleanUp();
        assertNotNull(actualChiefdoms);
        assertEquals(Constants.TWO, actualChiefdoms.getTotalCount());
    }

    @Test
    void testGetChiefdoms() {
        //given
        String searchTerm = "test";
        ChiefdomRequestDTO requestDTO = new ChiefdomRequestDTO();
        requestDTO.setLimit(Constants.NUMBER_TEN);
        requestDTO.setSkip(Constants.ZERO);
        requestDTO.setSearchTerm(searchTerm);
        requestDTO.setTenantId(TestConstants.ONE);
        String formattedSearchTerm = "test".replaceAll(Constants.SEARCH_TERM, Constants.EMPTY);
        ResponseListDTO responseListDTO = new ResponseListDTO();
        Organization organization = TestDataProvider.getOrganization();
        organization.setFormName(Constants.COUNTRY);
        ResponseEntity<Organization> expectedOrganization = new ResponseEntity<>(organization, HttpStatus.OK);
        Page<Chiefdom> chiefdoms = new PageImpl<>(TestDataProvider.getChiefdomList());
        responseListDTO.setTotalCount(chiefdoms.getTotalElements());
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN);

        List<Long> chiefdomIds = List.of(TestConstants.ONE, TestConstants.TWO);
        List<Map<String, Object>> healthFacilityCount = List.of(Map.of(TestConstants.STRING_ONE, Constants.ONE,
                Constants.STRING_TWO, Constants.ONE));

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        when(userApiInterface.getOrganizationById(TestConstants.TEST_TOKEN, TestConstants.ONE, TestConstants.ONE, TestConstants.CLIENT))
                .thenReturn(expectedOrganization);
        when(chiefdomRepository.findChiefdoms(formattedSearchTerm, TestConstants.ONE, null, pageable))
                .thenReturn(chiefdoms);
        when(chiefdomRepository.findChiefdoms(searchTerm, TestConstants.ONE, null, pageable))
                .thenReturn(chiefdoms);
        when(healthFacilityService.getHealthFacilityCountByChiefdomIds(chiefdomIds, Boolean.TRUE)).thenReturn(healthFacilityCount);

        //then
        ResponseListDTO actualChiefdom = chiefdomService.getChiefdomList(requestDTO);
        TestCommonMethods.cleanUp();
        assertNotNull(actualChiefdom);
        assertEquals(Constants.TWO, actualChiefdom.getTotalCount());
    }

    @Test
    void testUpdateChiefdom() {
        //given
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        chiefdom.setId(TestConstants.ONE);
        chiefdom.setName(TestConstants.TEST_NAME);
        TestCommonMethods.init();

        //when
        TestCommonMethods.getStaticMock();
        when(chiefdomRepository
                .findByIdAndIsDeletedFalseAndIsActive(chiefdom.getId(), Constants.BOOLEAN_TRUE)).thenReturn(chiefdom);
        when(chiefdomRepository.existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(chiefdom.getName(), chiefdom.getId())).thenReturn(Boolean.FALSE);
        doNothing().when(userApiInterface).updateOrganization(TestConstants.TEST_TOKEN, TestConstants.ONE,
                new OrganizationDTO(), TestConstants.CLIENT);
        when(chiefdomRepository.save(chiefdom)).thenReturn(chiefdom);

        //then
        Chiefdom actualChiefdom = chiefdomService.updateChiefdom(chiefdom);
        TestCommonMethods.cleanUp();
        assertNotNull(chiefdom);
        assertEquals(chiefdom, actualChiefdom);
        assertEquals(TestConstants.TEST_NAME, actualChiefdom.getName());
    }

    @Test
    void testCreateChiefdom() {
        //given
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        chiefdom.setId(TestConstants.ONE);
        chiefdom.setName(TestConstants.TEST_NAME);
        List<Village> villages = List.of(TestDataProvider.getVillage());

        //when
        when(chiefdomRepository
                .findByNameIgnoreCaseAndIsDeletedFalse(chiefdom.getName().strip())).thenReturn(null);
        when(chiefdomRepository.save(chiefdom)).thenReturn(chiefdom);
        when(redisTemplate.delete(Constants.ORGANIZATION_REDIS_KEY)).thenReturn(Boolean.TRUE);
        doNothing().when(villageService).addVillagesToChiefdom(chiefdom, villages);

        //then
        Chiefdom actualChiefdom = chiefdomService.createChiefdom(chiefdom, villages);
        assertNotNull(chiefdom);
        assertEquals(chiefdom, actualChiefdom);
        assertEquals(TestConstants.TEST_NAME, actualChiefdom.getName());

    }

    @Test
    void testGetOperatingUnitDetails() {
        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        ChiefdomDTO chiefdomDTO = TestDataProvider.getChiefdomDTOs().getFirst();
        Map<String, Object> mockedChiefdom = TestDataProvider.getChiefdoms().getFirst();

        //when
        TestCommonMethods.init();
        TestCommonMethods.getStaticMock();
        when(chiefdomRepository.getChiefdomDetails(searchRequestDTO.getId())).thenReturn(mockedChiefdom);
        when(modelMapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(modelMapper.map(mockedChiefdom, ChiefdomDTO.class)).thenReturn(chiefdomDTO);
        when(userApiInterface.getUsersByTenantIds(TestConstants.TEST_TOKEN, TestConstants.CLIENT, searchRequestDTO)).thenReturn(
                List.of(new UserResponseDTO()));

        //then
        ChiefdomDTO actualChiefdomDetails = chiefdomService.getChiefdomDetails(searchRequestDTO);
        TestCommonMethods.cleanUp();
        assertNotNull(actualChiefdomDetails);
    }

    @Test
    void getChiefdomCountByCountryIdsReturnsNonEmptyListForValidIds() {
        List<Long> countryIds = List.of(1L, 2L);
        List<Map<String, Object>> expectedResults = List.of(
                Map.of("countryId", 1L, "count", 5),
                Map.of("countryId", 2L, "count", 3)
        );
        when(chiefdomRepository.getChiefdomCountByCountryIds(countryIds, true)).thenReturn(expectedResults);

        List<Map<String, Object>> results = chiefdomService.getChiefdomCountByCountryIds(countryIds, true);

        assertNotNull(results);
        assertFalse(results.isEmpty());
        assertEquals(2, results.size());
    }

    @Test
    void getChiefdomCountByCountryIdsReturnsEmptyListForInvalidIds() {
        List<Long> countryIds = List.of(99L, 100L);
        when(chiefdomRepository.getChiefdomCountByCountryIds(countryIds, true)).thenReturn(Collections.emptyList());

        List<Map<String, Object>> results = chiefdomService.getChiefdomCountByCountryIds(countryIds, true);

        assertNotNull(results);
        assertTrue(results.isEmpty());
    }

    @Test
    void getChiefdomCountByCountryIdsReturnsEmptyListWhenNoIdsProvided() {
        when(chiefdomRepository.getChiefdomCountByCountryIds(Collections.emptyList(), true)).thenReturn(Collections.emptyList());

        List<Map<String, Object>> results = chiefdomService.getChiefdomCountByCountryIds(Collections.emptyList(), true);

        assertNotNull(results);
        assertTrue(results.isEmpty());
    }

    @Test
    void getChiefdomCountByCountryIdsReturnsEmptyListForNullIsActive() {
        List<Long> countryIds = List.of(1L, 2L);
        when(chiefdomRepository.getChiefdomCountByCountryIds(countryIds, null)).thenReturn(Collections.emptyList());

        List<Map<String, Object>> results = chiefdomService.getChiefdomCountByCountryIds(countryIds, null);

        assertNotNull(results);
        assertTrue(results.isEmpty());
    }

    @Test
    void getChiefdomCountForNullCountryIds() {
        when(chiefdomRepository.getChiefdomCountByCountryIds(null, true)).thenReturn(new ArrayList<>());

        List<Map<String, Object>> results = chiefdomService.getChiefdomCountByCountryIds(null, true);

        assertNotNull(results);
        assertTrue(results.isEmpty());
    }

    @Test
    void testActivateOrDeactivateChiefdoms() {
        long countryId = 1L;
        long accountId = 1L;
        boolean isActive = true;
        when(chiefdomRepository.findByCountryIdAndDistrictIdAndIsActive(
                countryId, accountId, !isActive)).thenReturn(List.of(new Chiefdom(), new Chiefdom()));
        List<Long> result = chiefdomService.activateOrDeactivateChiefdoms(countryId, accountId, isActive);
        verify(chiefdomRepository, times(1)).saveAll(any());
        assertNotNull(result);
    }

    @Test
    void testGetChiefdomCountByDistrictIds() {
        List<Long> inputs = List.of(1L);
        when(chiefdomRepository.getChiefdomCountByDistrictIds(inputs, true)).thenReturn(null);
        List<Map<String, Object>> chiefdomCountByDistrictIds = chiefdomService.getChiefdomCountByDistrictIds(inputs, true);
        assertNull(chiefdomCountByDistrictIds);
    }

    @Test
    void testGetChiefdomDetailsThrowsDataNotAcceptableException() {
        assertThrows(DataNotAcceptableException.class, () -> chiefdomService.getChiefdomDetails(new SearchRequestDTO()));
    }

    @Test
    void testGetChiefdomDetailsThrowsDataNotFoundException() {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        searchRequestDTO.setTenantId(1L);
        searchRequestDTO.setId(1L);
        when(chiefdomRepository.getChiefdomDetails(searchRequestDTO.getId())).thenReturn(Map.of());
        assertThrows(DataNotFoundException.class, () -> chiefdomService.getChiefdomDetails(searchRequestDTO));
    }

    @Test
    void testCreateChiefdomThrowsDataConflictException() {
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setName("sample");
        when(chiefdomRepository
                .findByNameIgnoreCaseAndIsDeletedFalse(chiefdom.getName().strip())).thenReturn(new Chiefdom());
        assertThrows(DataConflictException.class, () -> chiefdomService.createChiefdom(chiefdom, new ArrayList<>()));
    }

    @Test
    void testUpdateChiefdomThrowsDataNotFoundException() {
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setName("sample");
        chiefdom.setId(1L);
        when(chiefdomRepository
                .findByIdAndIsDeletedFalseAndIsActive(chiefdom.getId(), Constants.BOOLEAN_TRUE)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> chiefdomService.updateChiefdom(chiefdom));
    }

    @Test
    void testUpdateChiefdomThrowsDataConflictException() {
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setName("sample");
        chiefdom.setId(1L);
        when(chiefdomRepository
                .findByIdAndIsDeletedFalseAndIsActive(chiefdom.getId(), Constants.BOOLEAN_TRUE)).thenReturn(chiefdom);
        when(chiefdomRepository.existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(
                        chiefdom.getName(), chiefdom.getId())).thenReturn(Boolean.TRUE);
        assertThrows(DataConflictException.class, () -> chiefdomService.updateChiefdom(chiefdom));
    }


}
