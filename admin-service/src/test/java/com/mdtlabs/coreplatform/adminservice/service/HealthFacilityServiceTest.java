package com.mdtlabs.coreplatform.adminservice.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.adminservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityFilterDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.RequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.HealthFacilityRepository;
import com.mdtlabs.coreplatform.adminservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.HealthFacilityServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.OrganizationUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HealthFacilityServiceTest {
    
    @InjectMocks
    HealthFacilityServiceImpl healthFacilityService;

    @Mock
    UserServiceApiInterface userServiceApiInterface;

    @Mock
    HealthFacilityRepository healthFacilityRepository;

    @Mock
    VillageRepository villageRepository;

    @Mock
    ModelMapper modelMapper;

    @Mock
    ClinicalWorkflowService clinicalWorkflowService;

    @Mock
    FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    OrganizationUtil organizationUtil;

    @Test
    void testcreateHealthFacility() {
        HealthFacilityRequestDTO requestDTO = TestDataProvider.getHealthFacilityRequestDTO();
        requestDTO.setFhirId(TestConstants.STRING_ONE);
        List<Long> workflowIds = new ArrayList<>(Arrays.asList(1L));
        requestDTO.setClinicalWorkflowIds(workflowIds);
        requestDTO.setCustomizedWorkflowIds(workflowIds);
        List<ClinicalWorkflow> clinicalWorkflows = new ArrayList<>(Arrays.asList(TestDataProvider.getClinicalWorkflow()));

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        healthFacility.setClinicalWorkflows(clinicalWorkflows);
        healthFacility.setFhirId(TestConstants.STRING_ONE);
        List<Village> villages = List.of(TestDataProvider.getVillage());
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(villageRepository.findByIdInAndIsDeletedAndIsActive(requestDTO.getLinkedVillageIds(), false, true)).thenReturn(villages);
        when(healthFacilityRepository.save(healthFacility)).thenReturn(healthFacility);
        when(clinicalWorkflowService.getWorkflowsByIds(workflowIds)).thenReturn(clinicalWorkflows);
        when(fhirServiceApiInterface.createOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(requestDTO);

        HealthFacility response = healthFacilityService.createHealthFacility(requestDTO);
        assertNotNull(response);
        when(fhirServiceApiInterface.createOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(null);
        Assertions.assertNotNull(healthFacilityService.createHealthFacility(requestDTO));

        when(fhirServiceApiInterface.createOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(requestDTO);
        requestDTO.setFhirId(null);
        Assertions.assertNotNull(healthFacilityService.createHealthFacility(requestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void testgetHealthFacilityDetails () {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        ResponseEntity<List<UserResponseDTO>> userResponse = new ResponseEntity<>(List.of(TestDataProvider.getUserResponseDTO()), HttpStatus.OK);
        SearchRequestDTO request2 = new SearchRequestDTO();
        request2.setTenantIds(List.of(requestDTO.getTenantId()));

        when(userServiceApiInterface.getPeerSupervisors(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), request2)).thenReturn(userResponse);
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> healthFacilityService.getHealthFacilityDetails(requestDTO));

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(healthFacility);

        HealthFacilityDTO response = healthFacilityService.getHealthFacilityDetails(requestDTO);
        assertNotNull(response);

        requestDTO.setId(null);
        assertThrows(BadRequestException.class, () -> healthFacilityService.getHealthFacilityDetails(requestDTO));

        requestDTO.setId(1l);
        requestDTO.setTenantId(null);
        assertThrows(BadRequestException.class, () -> healthFacilityService.getHealthFacilityDetails(requestDTO));
        TestDataProvider.cleanUp();

    }

    @Test
    void testupdateHealthFacility() {
        ResponseEntity<Boolean> responseEntity = new ResponseEntity<>(Boolean.TRUE, HttpStatus.CREATED);
        HealthFacilityRequestDTO requestDTO = TestDataProvider.getHealthFacilityRequestDTO();
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> healthFacilityService.updateHealthFacility(requestDTO));
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(healthFacility);
        when(healthFacilityRepository.save(healthFacility)).thenReturn(healthFacility);
        when(userServiceApiInterface.updateHealthFacilityOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), requestDTO)).thenReturn(responseEntity);
        when(userServiceApiInterface.updateHealthFacilityOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), requestDTO)).thenReturn(responseEntity);
        when(fhirServiceApiInterface.updateOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
        UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(requestDTO);

        HealthFacility response = healthFacilityService.updateHealthFacility(requestDTO);
        assertNotNull(response);

        requestDTO.setName(null);
        requestDTO.setParentTenantId(null);
        response = healthFacilityService.updateHealthFacility(requestDTO);
        assertNotNull(response);

        requestDTO.setName("abcd");
        requestDTO.setParentTenantId(3l);
        response = healthFacilityService.updateHealthFacility(requestDTO);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetHealthFacilities() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setSearchTerm("abc");
        requestDTO.setLimit(10);
        requestDTO.setUserBased(true);
        requestDTO.setTenantId(TestConstants.ONE);
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN,
        Sort.by("updatedAt").descending());
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        List<HealthFacility> healthFacilities = List.of(TestDataProvider.getHealthFacility());
        Page<HealthFacility> hfPage = new PageImpl<>(healthFacilities);
        List<Long> tenantIdsList = new ArrayList<>(Arrays.asList(TestConstants.ONE));
        Set<Long> tenantIds = Set.of(1l);
        when(CommonUtil.isValidSearchData(requestDTO.getSearchTerm(), Constants.SEARCH_TERM)).thenReturn(true);
        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, tenantIdsList));
        when(healthFacilityRepository.getHealthFacilities(requestDTO.getSearchTerm() , requestDTO.getCountryId(), tenantIds,  pageable)).thenReturn(hfPage);

        ResponseListDTO<HealthFacilityDTO> responseListDTO = healthFacilityService.getHealthFacilities(requestDTO);
        assertNotNull(responseListDTO);

        requestDTO.setUserBased(false);
        responseListDTO = healthFacilityService.getHealthFacilities(requestDTO);
        assertNotNull(responseListDTO);

        requestDTO.setUserBased(true);
        requestDTO.setLimit(0);
        pageable = null;
        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, tenantIdsList));
        when(healthFacilityRepository.getHealthFacilities(requestDTO.getSearchTerm() ,requestDTO.getCountryId(), tenantIds,  pageable)).thenReturn(hfPage);
        responseListDTO = healthFacilityService.getHealthFacilities(requestDTO);
        assertNotNull(responseListDTO);

        hfPage = null;
        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, tenantIdsList));
        when(healthFacilityRepository.getHealthFacilities(requestDTO.getSearchTerm() ,requestDTO.getCountryId(), tenantIds,  pageable)).thenReturn(hfPage);
        responseListDTO = healthFacilityService.getHealthFacilities(requestDTO);
        assertNotNull(responseListDTO);

        healthFacilities = new ArrayList<>();
        hfPage = new PageImpl<>(healthFacilities);
        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, tenantIdsList));
        when(healthFacilityRepository.getHealthFacilities(requestDTO.getSearchTerm() ,requestDTO.getCountryId(), tenantIds,  pageable)).thenReturn(hfPage);
        responseListDTO = healthFacilityService.getHealthFacilities(requestDTO);
        assertNotNull(responseListDTO);

        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, tenantIdsList));
        when(CommonUtil.isValidSearchData(requestDTO.getSearchTerm(), Constants.SEARCH_TERM)).thenReturn(false);
        responseListDTO = healthFacilityService.getHealthFacilities(requestDTO);
        assertNotNull(responseListDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void testAddAdmin() {
        UserRequestDTO requestDTO = TestDataProvider.getUserRequestDTO();
        ResponseEntity<UserResponseDTO> responseEntity = new ResponseEntity<>(new UserResponseDTO(), HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.addAdmin(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(responseEntity);

        UserResponseDTO responseDTO =healthFacilityService.addHealthFacilityUser(requestDTO);
        assertNotNull(responseDTO);
        TestDataProvider.cleanUp();
    }

    @Test 
    void updateAdmin() {
        UserRequestDTO requestDTO = TestDataProvider.getUserRequestDTO();
        ResponseEntity<UserResponseDTO> responseEntity = new ResponseEntity<>(new UserResponseDTO(), HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.updateAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(responseEntity);

        UserResponseDTO responseDTO =healthFacilityService.updateHealthFacilityUser(requestDTO);
        assertNotNull(responseDTO);
        TestDataProvider.cleanUp();

    }

    @Test
    void testDeleteAdmin () {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        ResponseEntity<UserResponseDTO> responseEntity = new ResponseEntity<>(new UserResponseDTO(), HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.removeAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(responseEntity);

        UserResponseDTO responseDTO =healthFacilityService.deleteHealthFacilityAdmin(requestDTO);
        assertNotNull(responseDTO);

        requestDTO.setTenantIds(List.of(1l,2l,3l));
        List<HealthFacility> healthFacilityList = List.of(TestDataProvider.getHealthFacility());
        when(healthFacilityRepository.findByTenantIdInAndIsDeleted(
                requestDTO.getTenantIds(), Boolean.FALSE)).thenReturn(healthFacilityList);
        responseDTO =healthFacilityService.deleteHealthFacilityAdmin(requestDTO);
        assertNotNull(responseDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void getHealthFacilitiesByCountryId() {
        Long countryId = 1l;
        List<HealthFacility> healthFacilities = List.of(TestDataProvider.getHealthFacility());

        when(healthFacilityRepository.findByCountryIdAndIsDeleted(countryId, false)).thenReturn(healthFacilities);

        List<HealthFacilityDTO> response = healthFacilityService.getHealthFacilitiesByCountryId(countryId);
        assertNotNull(response);
    }

    @Test
    void getHealthFacilitiesByTenants() {
        List<Long> tenantIds = List.of(4l);
        List<HealthFacility> healthFacilities = List.of(TestDataProvider.getHealthFacility());

        when(healthFacilityRepository.findByTenantIdInAndIsDeleted(tenantIds, false)).thenReturn(healthFacilities);
        List<HealthFacilityDTO> response = healthFacilityService.getHealthFacilitiesByTenants(tenantIds);
        assertNotNull(response);
    }

    @Test
    void getHealthFacilitiesByChiefdomId() {
        Long chiefdomId = 1l;
        List<HealthFacility> healthFacilities = List.of(TestDataProvider.getHealthFacility());

        when(healthFacilityRepository.findByChiefdomIdAndIsDeleted(chiefdomId, false)).thenReturn(healthFacilities);
        List<HealthFacilityDTO> response = healthFacilityService.getHealthFacilitiesByChiefdomId(chiefdomId);
        assertNotNull(response);
    }

    @Test
    void getVillagesByFacility() {
        List<Long> tenantIds = List.of(4l);
        List<HealthFacility> healthFacilities = List.of(TestDataProvider.getHealthFacility());

        when(healthFacilityRepository.findByTenantIdInAndIsDeleted(tenantIds, false)).thenReturn(healthFacilities);
        List<VillageDTO> response = healthFacilityService.getVillagesByFacility(tenantIds);
        assertNotNull(response);

        when(healthFacilityRepository.findByTenantIdInAndIsDeleted(tenantIds, false)).thenReturn(null);
        response = healthFacilityService.getVillagesByFacility(tenantIds);
        assertNotNull(response);

    }

    @Test
    void shouldReturnAllHealthFacilitiesWhenExist() {
        List<HealthFacility> mockFacilities = new ArrayList<>();
        HealthFacility mockFacility = new HealthFacility();
        mockFacilities.add(mockFacility);

        when(healthFacilityRepository.findAllByIsDeletedFalseAndIsActiveTrue()).thenReturn(mockFacilities);

        List<HealthFacilityDTO> response = healthFacilityService.getAllHealthFacilities();

        verify(healthFacilityRepository, times(1)).findAllByIsDeletedFalseAndIsActiveTrue();
        assertNotNull(response);
        assertEquals(1, response.size());
    }

    @Test
    void shouldReturnEmptyListWhenNoHealthFacilitiesExist() {
        List<HealthFacility> mockFacilities = new ArrayList<>();

        when(healthFacilityRepository.findAllByIsDeletedFalseAndIsActiveTrue()).thenReturn(mockFacilities);

        List<HealthFacilityDTO> response = healthFacilityService.getAllHealthFacilities();

        verify(healthFacilityRepository, times(1)).findAllByIsDeletedFalseAndIsActiveTrue();
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    @DisplayName("Should return all health facilities by district id when exist")
    void shouldReturnAllHealthFacilitiesByDistrictIdWhenExist() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setDistrictId(1L);
        List<HealthFacility> mockFacilities = List.of(new HealthFacility());

        when(healthFacilityRepository.findAllByDistrictIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getDistrictId())).thenReturn(mockFacilities);

        List<HealthFacilityDTO> response = healthFacilityService.getAllHealthFacilitiesByDistrictId(requestDTO);

        assertEquals(1, response.size());
    }

    @Test
    @DisplayName("Should throw DataNotFoundException when district id is null")
    void shouldThrowDataNotFoundExceptionWhenDistrictIdIsNull() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setDistrictId(null);

        assertThrows(DataNotFoundException.class, () -> healthFacilityService.getAllHealthFacilitiesByDistrictId(requestDTO));
    }

    @Test
    void validateHealthFacility() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO requestDto = new SearchRequestDTO();
        requestDto.setLinkedVillageIds(List.of(1L));
        requestDto.setHealthFacilityId(TestConstants.ONE);
        requestDto.setTenantId(TestConstants.ONE);
        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        Village village = TestDataProvider.getVillage();
        village.setId(2L);
        List<Village> villages = new ArrayList<>();
        villages.add(village);
        villages.add(TestDataProvider.getVillage());
        healthFacility.setLinkedVillages(villages);

        // DataNotFound Exception check
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(TestConstants.ONE,
                TestConstants.ONE, Boolean.FALSE, Boolean.TRUE)).thenReturn(null);

        Assertions.assertThrows(DataNotFoundException.class, () -> healthFacilityService.validateHealthFacility(requestDto));


        // Bad Request Exception check
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(TestConstants.ONE,
                TestConstants.ONE, Boolean.FALSE, Boolean.TRUE)).thenReturn(healthFacility);
        when(villageRepository.getUserLinkedVillage(List.of(2L))).thenReturn(List.of(village));
        when(userServiceApiInterface.validatePeerSupervisors(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), requestDto)).thenReturn(new ResponseEntity<>(Boolean.TRUE, HttpStatus.OK));

        Assertions.assertThrows(BadRequestException.class, () -> healthFacilityService.validateHealthFacility(requestDto));

        // Valid Request check
        when(villageRepository.getUserLinkedVillage(List.of(2L))).thenReturn(new ArrayList<>());

        healthFacilityService.validateHealthFacility(requestDto);
        verify(userServiceApiInterface, atLeastOnce()).validatePeerSupervisors(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), requestDto);
        TestDataProvider.cleanUp();
    }

    @Test
    void getFacilityVillageIdsByTenantId() {
        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(healthFacility);

        List<Long> response = healthFacilityService.getFacilityVillageIdsByTenantId();
        Assertions.assertNotNull(response);
        assertEquals(TestConstants.ONE, response.size());

        healthFacility.setLinkedVillages(new ArrayList<>());
        when(healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(healthFacility);

        response = healthFacilityService.getFacilityVillageIdsByTenantId();
        Assertions.assertNotNull(response);
        assertEquals(TestConstants.ZERO, response.size());

        healthFacility.setLinkedVillages(null);
        when(healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(healthFacility);

        response = healthFacilityService.getFacilityVillageIdsByTenantId();
        Assertions.assertEquals(new ArrayList<>(), response);

        when(healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> healthFacilityService.getFacilityVillageIdsByTenantId());
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetHealthFacilitiesFilter() {
        // Given
        HealthFacilityFilterDTO filterDTO = TestDataProvider.getHealthFacilityFilterDTO();
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(Constants.ACTIVE, Constants.ZERO, Constants.ZERO);
        searchRequestDTO.setTenantId(TestConstants.ONE);

        // When
        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, List.of(TestConstants.ONE)));
        when(healthFacilityRepository.getHealthFacilities(searchRequestDTO.getSearchTerm(),  searchRequestDTO.getCountryId(), Set.of(TestConstants.ONE), null)).thenReturn(new PageImpl<>(List.of(TestDataProvider.getHealthFacility())));

        //then
        ResponseListDTO<HealthFacilityFilterDTO> response = healthFacilityService.getHealthFacilitiesFilter(searchRequestDTO);
        assertNotNull(response);
        assertEquals(filterDTO, response.getData().getFirst());
        assertEquals(Constants.ONE, response.getData().size());
    }

    @Test
    void testGetHealthFacilitiesFilterWithTenantIds() {
        // Given
        HealthFacilityFilterDTO filterDTO = TestDataProvider.getHealthFacilityFilterDTO();
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(Constants.ACTIVE, Constants.ZERO, Constants.ZERO);
        searchRequestDTO.setTenantIds(List.of(TestConstants.ONE));

        // When
        when(organizationUtil.getParentChildTenantMap()).thenReturn(Map.of(TestConstants.ONE, List.of(TestConstants.ONE)));
        when(healthFacilityRepository.getHealthFacilities(searchRequestDTO.getSearchTerm(),  searchRequestDTO.getCountryId(), Set.of(TestConstants.ONE), null)).thenReturn(new PageImpl<>(List.of(TestDataProvider.getHealthFacility())));

        //then
        ResponseListDTO<HealthFacilityFilterDTO> response = healthFacilityService.getHealthFacilitiesFilter(searchRequestDTO);
        assertNotNull(response);
        assertEquals(filterDTO, response.getData().getFirst());
        assertEquals(Constants.ONE, response.getData().size());
    }

    @Test
    void testGetHealthFacilitiesFilterWithNullIds() {
        // Given
        HealthFacilityFilterDTO filterDTO = TestDataProvider.getHealthFacilityFilterDTO();
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDto(Constants.ACTIVE, Constants.ZERO, Constants.ZERO);
        searchRequestDTO.setTenantIds(List.of(TestConstants.ONE));

        // When
        when(organizationUtil.getParentChildTenantMap()).thenReturn(new HashMap<>());
        when(healthFacilityRepository.getHealthFacilities(searchRequestDTO.getSearchTerm(),  searchRequestDTO.getCountryId(), null, null)).thenReturn(new PageImpl<>(List.of(TestDataProvider.getHealthFacility())));

        //then
        ResponseListDTO<HealthFacilityFilterDTO> response = healthFacilityService.getHealthFacilitiesFilter(searchRequestDTO);
        assertNotNull(response);
        assertEquals(filterDTO, response.getData().getFirst());
        assertEquals(Constants.ONE, response.getData().size());
    }
    
    @Test
    void getHealthFacilityByFhirIdTest() {
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        when(healthFacilityRepository.findByFhirIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getFhirId())).thenReturn(TestDataProvider.getHealthFacility());
        when(modelMapper.map(any(), any())).thenReturn(TestDataProvider.getHealthFacilityRequestDTO());
        HealthFacilityRequestDTO response = healthFacilityService.getHealthFacilityByFhirId(requestDTO);
        assertNotNull(response);

        when(healthFacilityRepository.findByFhirIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getFhirId())).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> healthFacilityService.getHealthFacilityByFhirId(requestDTO));
    }

    @Test
    void deleteHealthFacilityTest() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        HealthFacility healthFacility =TestDataProvider.getHealthFacility();
        healthFacility.setFhirId("1234");
        when(healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(healthFacility);
        String response = healthFacilityService.deleteHealthFacility(requestDTO);
        assertEquals("1234",response);
    }

    @Test
    void activateOrDeactivateHealthFacility() {
        Long countryId = TestConstants.ONE;
        Long districtId = TestConstants.ONE;
        Long chiefdomId = TestConstants.ONE;
        boolean isActive = Boolean.TRUE;
        HealthFacility healthFacility = TestDataProvider.getHealthFacility();
        healthFacility.setActive(isActive);
        List<HealthFacility> healthFacilities = List.of(healthFacility);
        //when
        when(healthFacilityRepository.findHealthFacilityDistrict(countryId, districtId, chiefdomId, !isActive)).thenReturn(healthFacilities);
        when(healthFacilityRepository.saveAll(healthFacilities)).thenReturn(healthFacilities);
        //then
        List<HealthFacility> response = healthFacilityService.activateOrDeactivateHealthFacility(countryId, districtId, chiefdomId, isActive);
        assertNotNull(response);

    }

    @Test
    void getCountByDistrictIds() {
        List<Long> countyIds = List.of(TestConstants.ONE, TestConstants.TWO);
        //when
        when(healthFacilityRepository.getCountByDistrictIds(countyIds)).thenReturn(any());
        //then
        healthFacilityService.getCountByDistrictIds(countyIds);
        verify(healthFacilityRepository, atLeastOnce()).getCountByDistrictIds(countyIds);
    }
}
