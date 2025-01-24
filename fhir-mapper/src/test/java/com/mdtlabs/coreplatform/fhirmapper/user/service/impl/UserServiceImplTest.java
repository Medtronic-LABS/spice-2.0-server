package com.mdtlabs.coreplatform.fhirmapper.user.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import org.hl7.fhir.r4.model.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.stubbing.defaultanswers.ReturnsDeepStubs;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

/**
 * <p>
 * UserServiceImplTest class used to test all possible positive
 * and negative cases for all methods and conditions used in UserServiceImpl class.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Mar 11, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserServiceImplTest {

    @Mock
    FhirUtils fhirUtils;

    @Mock
    RestApiUtil restApiUtil;

    @InjectMocks
    UserServiceImpl userService;

    @Test
    void createOrganization() {
        HealthFacilityRequestDTO healthFacilityRequestDTO = TestDataProvider.getHealthFacilityRequestDTO();
        healthFacilityRequestDTO.setUsers(List.of(TestDataProvider.getUserRequestDTO()));
        Map<String, List<String>> fhirIdResponse = new HashMap<>();
        fhirIdResponse.put(String.valueOf(ResourceType.Organization),
                List.of(TestConstants.ONE_TWO_THREE, TestConstants.TWO_THREE_FOUR));
        fhirIdResponse.put(String.valueOf(ResourceType.Practitioner), List.of(TestConstants.ONE_TWO_THREE_FOUR,
                TestConstants.TWO_THREE_FOUR_FIVE));
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        ReflectionTestUtils.setField(userService, TestConstants.FHIR_SERVER_URL, TestConstants.URL);
        IGenericClient client = mock(IGenericClient.class, new ReturnsDeepStubs());
        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Organization),
                TestDataProvider.getHealthFacilityRequestDTO());
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.UNIQUE_ID);
        when(fhirUtils.getClient(TestConstants.URL, TestConstants.ADMIN, TestConstants.BEARER_TEST)).thenReturn(client);
        when(restApiUtil.postBatchRequest(TestConstants.URL, new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(fhirIdResponse);
        HealthFacilityRequestDTO response = userService.createOrganization(healthFacilityRequestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(TestConstants.ONE_TWO_THREE, response.getFhirId());
        TestDataProvider.cleanUp();
    }

    @Test
    void updateUser() {
        //given
        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        name.setPrefix(List.of(new StringType(TestConstants.FIRST_NAME)));

        UserRequestDTO userRequestDTO = TestDataProvider.getUserRequestDTO();
        Practitioner practitioner = new Practitioner();

        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(practitioner);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Practitioner),
                userRequestDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(userService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(practitioner.getId())).thenReturn(TestConstants.UNIQUE_ID);
        doNothing().when(fhirUtils).setBundle("", Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, practitioner, bundle);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        UserRequestDTO responseForGender = userService.updateUser(userRequestDTO);
        Assertions.assertNotNull(responseForGender);
    }

    @Test
    void updateOrganization() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Organization organization = new Organization();
        organization.setId(TestConstants.TWO_STR);
        HealthFacilityRequestDTO healthFacilityRequestDTO = TestDataProvider.getHealthFacilityRequestDTO();

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(organization);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Practitioner),
                healthFacilityRequestDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(userService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.ORGANIZATION_PARAMS, healthFacilityRequestDTO.getFhirId()))).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(organization.getId())).thenReturn(TestConstants.UNIQUE_ID);
        doNothing().when(fhirUtils).setBundle("", Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, organization, bundle);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        HealthFacilityRequestDTO response = userService.updateOrganization(healthFacilityRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getUserById() {
        //given
        Practitioner practitioner = new Practitioner();
        practitioner.setId(TestConstants.STRING_THREE);
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(practitioner);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        Practitioner response = userService.getUserById(TestConstants.STRING_FOUR);
        Assertions.assertNotNull(response);
    }

    @Test
    void deleteUser() {
        //given
        Practitioner practitioner = new Practitioner();
        practitioner.setId(TestConstants.STRING_THREE);
        practitioner.setActive(true);
        Bundle bundle = new Bundle();
        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(practitioner);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Practitioner),
                practitioner);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(userService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        doNothing().when(fhirUtils).setBundle("", Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, practitioner, bundle);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        userService.deleteUser(TestConstants.STRING_THREE);
        Mockito.verify(restApiUtil).getBatchRequest(any());
    }

    @Test
    void deleteOrganization() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Organization organization = new Organization();
        organization.setId(TestConstants.TWO_STR);
        organization.setActive(true);
        HealthFacilityRequestDTO healthFacilityRequestDTO = TestDataProvider.getHealthFacilityRequestDTO();

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(organization);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Practitioner),
                healthFacilityRequestDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(userService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.ORGANIZATION_PARAMS, TestConstants.TWO_STR))).thenReturn(bundle);
        when(fhirUtils.getIdFromHistoryUrl(organization.getId())).thenReturn(TestConstants.UNIQUE_ID);
        doNothing().when(fhirUtils).setBundle("", Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, organization, bundle);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        userService.deleteOrganization(TestConstants.TWO_STR);
        Mockito.verify(restApiUtil).getBatchRequest(String.format(Constants.ORGANIZATION_PARAMS, TestConstants.TWO_STR));
    }

    @Test
    void deleteUsers() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.Practitioner),
                bundle);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(userService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.PRACTIIONER_PARAMS,
                String.join(Constants.COMMA, TestConstants.TWO_STR)))).thenReturn(bundle);
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        userService.deleteUsers(List.of(TestConstants.TWO_STR));

        //given
        Practitioner practitioner = new Practitioner();
        practitioner.setId(TestConstants.STRING_THREE);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(practitioner);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        doNothing().when(fhirUtils).setBundle("", Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, practitioner, bundle);

        //then
        userService.deleteUsers(List.of(TestConstants.TWO_STR));
        Assertions.assertNotNull(bundle);
    }
}