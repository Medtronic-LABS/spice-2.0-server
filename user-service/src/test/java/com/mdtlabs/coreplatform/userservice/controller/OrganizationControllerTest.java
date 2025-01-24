package com.mdtlabs.coreplatform.userservice.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictOrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ChiefdomRequestDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.userservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.userservice.service.OrganizationService;
import com.mdtlabs.coreplatform.userservice.util.TestDataProvider;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class OrganizationControllerTest {

    @InjectMocks
    OrganizationController organizationController;

    @Mock
    OrganizationService organizationService;

    @Mock
    ModelMapper modelMapper;

    @Test
    void createOrganization() {
        OrganizationDTO request = TestDataProvider.getOrganizationDTO();

        ResponseEntity<Organization> response = organizationController.createOrganization(request);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void updateOrganization() {
        OrganizationDTO request = new OrganizationDTO();
        Long tenantId = 1L;
        when(organizationService.updateOrganization(request)).thenReturn(tenantId);

        ResponseEntity<Long> response = organizationController.updateOrganization(request);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void getOrganization() {
        Long tenantId = 1L;
        Organization organization = new Organization();
        when(organizationService.getOrganization(tenantId)).thenReturn(organization);

        ResponseEntity<OrganizationDTO> response = organizationController.getOrganization(tenantId);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteOrganization() {
        Long id = 1L;
        doNothing().when(organizationService).deleteOrganization(id);

        ResponseEntity<String> response = organizationController.deleteOrganization(id);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void addAdmin() {
        UserRequestDTO request = new UserRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(organizationService.addAdmin(request)).thenReturn(userResponse);

        ResponseEntity<UserResponseDTO> response = organizationController.addAdmin(request);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void updateAdmin() {
        UserRequestDTO request = new UserRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(organizationService.updateAdmin(request)).thenReturn(userResponse);

        ResponseEntity<UserResponseDTO> response = organizationController.updateAdmin(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteAdmin() {
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        UserResponseDTO userResponse = new UserResponseDTO();

        when(organizationService.removeAdmin(requestDTO)).thenReturn(userResponse);

        ResponseEntity<UserResponseDTO> response = organizationController.deleteAdmin(requestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createCountry() {
        CountryRequestDTO requestDTO = new CountryRequestDTO();
        Country country = new Country();
        when(organizationService.createRegion(requestDTO)).thenReturn(country);

        SuccessResponse<Country> response = organizationController.createCountry(requestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void createHealthFacility() {
        //GIVEN
        HealthFacilityRequestDTO requestDTO = new HealthFacilityRequestDTO();

        //WHEN
        when(organizationService.createHealthFacility(requestDTO)).thenReturn(null);

        //THEN
        SuccessResponse<HealthFacility> response = organizationController.createHealthFacility(requestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void deleteHealthFacility() {
        SearchRequestDTO request = new SearchRequestDTO();
        doNothing().when(organizationService).deleteHealthFacility(request);
        SuccessResponse<HealthFacility> response = organizationController.deleteHealthFacility(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createDistrict() {
        DistrictRequestDTO districtRequestDTO = new DistrictRequestDTO();

        doNothing().when(organizationService).createDistrict(districtRequestDTO);

        SuccessResponse<DistrictOrganizationDTO> response = organizationController.createDistrict(districtRequestDTO);
        assertNotNull(response);
    }

    @Test
    void activateOrDeactivateOrganization() {
        List<Long> tenantIds = new ArrayList<>();
        List<String> fhirIds = new ArrayList<>();

        when(organizationService.activateOrDeactivateOrganization(tenantIds, true, fhirIds)).thenReturn(true);

        Boolean response = organizationController.activateOrDeactivateOrganization(tenantIds, true, fhirIds);
        assertNotNull(response);
    }

    @Test
    void createOperatingUnit() {
        ChiefdomRequestDTO chiefdomRequestDTO = new ChiefdomRequestDTO();

        doNothing().when(organizationService).createChiefdom(chiefdomRequestDTO);

        SuccessResponse<ChiefdomRequestDTO> response = organizationController.createOperatingUnit(chiefdomRequestDTO);
        assertNotNull(response);
    }

    @Test
    void updateHealthFacilityOrganization() {
        HealthFacilityRequestDTO requestDTO = new HealthFacilityRequestDTO();

        doNothing().when(organizationService).updateHealthFacility(requestDTO);

        ResponseEntity<Boolean> response = organizationController.updateHealthFacilityOrganization(requestDTO);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getOrganizationsByFormName() {
        // given
        String formName = Constants.FORM_NAME;
        List<Organization> organizations = List.of(TestDataProvider.getOrganization());

        // when
        when(organizationService.getOrganizations(formName)).thenReturn(organizations);

        // then
        List<Organization> response = organizationController.getOrganizationsByFormName(formName);
        assertNotNull(response);
        assertEquals(organizations, response);
    }

    @Test
    void getOrganizationById() {
        Organization organization = TestDataProvider.getOrganization();
        long organizationId = 2L;
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organization);
        ResponseEntity<Organization> response = organizationController.getOrganizationById(organizationId);
        assertNotNull(response);
        assertEquals(organization, response.getBody());
    }
}
