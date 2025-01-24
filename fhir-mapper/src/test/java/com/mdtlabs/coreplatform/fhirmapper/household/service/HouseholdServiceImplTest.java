package com.mdtlabs.coreplatform.fhirmapper.household.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.CodeDetailsConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.household.service.impl.HouseholdServiceImpl;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;

/**
 * <p>
 * HouseholdServiceImplTest class used to test all possible positive and
 * negative cases for all methods and conditions used in HouseholdServiceImpl
 * class.
 * </p>
 *
 * @author Nandhakumar
 * @since Feb 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HouseholdServiceImplTest {

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private FhirMapper fhirMapper;

    @Mock
    private RestApiUtil restApiUtil;

    @InjectMocks
    private HouseholdServiceImpl householdService;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Test
    void createHousehold() {
        //given
        TestDataProvider.init();
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setNoOfPeople(TestConstants.INT_THREE);
        householdDTO.setHouseholdNo(TestConstants.LONG_THREE);
        householdDTO.setVillage(TestConstants.NAME);
        householdDTO.setLandmark(TestConstants.NAME);
        householdDTO.setLatitude(TestConstants.ONE_DOUBLE);
        householdDTO.setLongitude(TestConstants.ONE_DOUBLE);
        householdDTO.setVillageId(TestConstants.STRING_THREE);
        householdDTO.setOwnedHandWashingFacilityWithSoap(true);
        householdDTO.setOwnedAnImprovedLatrine(true);
        householdDTO.setOwnedTreatedBedNet(true);
        householdDTO.setProvenance(TestDataProvider.getProvenance());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);
        HouseholdMemberDTO householdMember = new HouseholdMemberDTO();
        householdMember.setAssignHousehold(Boolean.TRUE);
        householdMember.setProvenance(TestDataProvider.getProvenance());
        householdMember.setVillageId(TestConstants.TWO_STR);
        householdMember.setId(TestConstants.TWO_STR);

        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setInitial(TestConstants.NAME);
        householdMemberDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        householdMemberDTO.setGender(TestConstants.MALE);
        householdMemberDTO.setDateOfBirth(new Date());
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        householdMemberDTO.setPatientId(TestConstants.STRING_THREE);
        householdMemberDTO.setVillageId(TestConstants.STRING_THREE);
        householdMemberDTO.setHouseholdId(TestConstants.STRING_THREE);
        householdMemberDTO.setId(TestConstants.STRING_THREE);
        householdMemberDTO.setProvenance(TestDataProvider.getProvenance());
        householdMemberDTO.setId(TestConstants.STRING_THREE);

        householdDTO.setHouseholdMembers(List.of(householdMemberDTO, householdMember));

        Coding coding = new Coding();
        coding.setCode("isHandWashingFacility");
        coding.setSystem("http://mdtlabs.com/fhir/");
        coding.setDisplay("Owns hand washing facility with soap?");

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.addCoding(coding);
        codeableConcept.setText(CodeDetailsConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);

        Group.GroupCharacteristicComponent characteristicComponent = new Group.GroupCharacteristicComponent();
        characteristicComponent.setCode(codeableConcept);
        characteristicComponent.getValueCodeableConcept().addCoding(new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,Constants.YES_CODE,Constants.YES));
        characteristicComponent.getValueCodeableConcept().setText(Constants.YES);

        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setName(householdDTO.getName());
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(householdDTO.getNoOfPeople());
        group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                .setValue(householdDTO.getHouseholdNo().toString());
        group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(householdDTO.getVillageId());
        group.getCharacteristic().add(characteristicComponent);
        Bundle groupBundle = new Bundle();
        groupBundle.addEntry()
                .setResource(group)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, TestConstants.TWO_STR))
                .setMethod(Bundle.HTTPVerb.PUT);

        Address address = new Address();
        address.setCity(householdDTO.getVillage());
        address.setText(householdDTO.getLandmark());

        Location location = new Location();
        location.setName(householdDTO.getName());
        location.getPosition().setLatitude(householdDTO.getLatitude());
        location.getPosition().setLongitude(householdDTO.getLongitude());
        location.setAddress(address);

        Device device = new Device();
        device.addIdentifier().setSystem(FhirIdentifierConstants.PHONE_TYPE_SYSTEM_URL)
                .setValue(householdDTO.getHeadPhoneNumber());

        HumanName name = new HumanName();
        name.setText(householdMemberDTO.getName());
        name.setPrefix(List.of(new StringType(householdMemberDTO.getInitial())));

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setName(List.of(name));
        relatedPerson.setPatient(new Reference(householdMemberDTO.getPatientReference()));
        relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(TestConstants.MALE));
        relatedPerson.setActive(true);
        relatedPerson.setBirthDate(new Date());
        Bundle relaterPersonBundle = new Bundle();
        relaterPersonBundle.addEntry()
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, TestConstants.TWO_STR))
                .setMethod(Bundle.HTTPVerb.PUT);
        Map<String, List<String>> response = new HashMap<>();
        response.put("Group", List.of(TestConstants.ONE_STR, TestConstants.TWO_STR));

        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                householdMemberDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);

        List<String> ids = new ArrayList<>();
        ids.add(householdDTO.getId());

        List<String> memberIds = new ArrayList<>();
        memberIds.add(householdMemberDTO.getId());

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();

        ReflectionTestUtils.setField(householdService, "fhirServerUrl", "fhirServerUrl");
        HealthFacilityRequestDTO healthFacilityRequestDTO = new HealthFacilityRequestDTO();
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setCode(TestConstants.TWO_STR);
        healthFacilityRequestDTO.setChiefdom(chiefdom);

        //when
        TestDataProvider.getStaticMock();
        when(fhirUtils.getUniqueId()).thenReturn(TestConstants.UNIQUE_ID);
        when(fhirMapper.setGroup(any(), any())).thenReturn(group);
        when(adminServiceApiInterface.getHouseholdSequenceByVillageId(eq(TestConstants.BEARER_TEST), eq(TestConstants.ADMIN), any(SearchRequestDTO.class))).thenReturn(1l);
        when(fhirMapper.setLocation(new Location(), householdDTO)).thenReturn(location);
        doNothing().when(fhirUtils).setBundle("",
                "", Bundle.HTTPVerb.POST, location, bundle,
                householdDTO.getProvenance());
        when(fhirMapper.setDeviceEntity(householdDTO, new Device())).thenReturn(device);
        doNothing().when(fhirUtils).setBundle("", "", Bundle.HTTPVerb.POST, device, bundle, householdDTO.getProvenance());
        when(fhirMapper.setRelatedPerson(any(), any())).thenReturn(relatedPerson);
        doNothing().when(fhirMapper).setGroupCharacteristicComponent(any(), any(), any());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(fhirResponse)).thenReturn(new HashMap<>());
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, ids), ids.size()))).thenReturn(bundle);
        when(adminServiceApiInterface.getHealthFacilityByFhirId(any(), any(), any())).thenReturn(healthFacilityRequestDTO);
        when(adminServiceApiInterface.getVillageDetailsByVillageId(any(), any(), any())).thenReturn(TestDataProvider.getVillageDTO());
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=2&_count=1")).thenReturn(relaterPersonBundle);
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null,null&_count=2")).thenReturn(relaterPersonBundle);
        when(restApiUtil.getBatchRequest("Group?_id=1&_count=1")).thenReturn(groupBundle);
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null,urn:uuid:Locationabcd-efgh-ijkl-mnop&_count=2")).thenReturn(relaterPersonBundle);

        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(response);

        //then
        HouseholdDTO result = householdService.createHousehold(householdDTO);
        Assertions.assertNotNull(result);
        TestDataProvider.cleanUp();
    }

    @Test
    void getHousehold() {
        //given
        List<String> groupIds = new ArrayList<>();
        groupIds.add(TestConstants.STRING_THREE);

        Bundle bundle = new Bundle();

        Coding coding = new Coding();
        coding.setCode("isHandWashingFacility");
        coding.setSystem("http://mdtlabs.com/fhir/");
        coding.setDisplay("Owns hand washing facility with soap?");

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.addCoding(coding);
        codeableConcept.setText(CodeDetailsConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);

        Group.GroupCharacteristicComponent characteristicComponent = new Group.GroupCharacteristicComponent();
        characteristicComponent.setCode(codeableConcept);
        characteristicComponent.getValueCodeableConcept().addCoding(new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,Constants.YES_CODE,Constants.YES));
        characteristicComponent.getValueCodeableConcept().setText(Constants.YES);

        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setName(TestConstants.NAME);
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(TestConstants.INT_THREE);
        group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                .setValue(TestConstants.TWO_STR);
        group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(TestConstants.TWO_STR);
        group.getCharacteristic().add(characteristicComponent);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(group);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, groupIds), groupIds.size()))).thenReturn(bundle);
        when(fhirMapper.toHousehold(group, new HouseholdDTO())).thenReturn(any());

        //then
        HouseholdDTO response = householdService.getHousehold(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void createHouseholdMember() {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        TestDataProvider.init();
        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setId(TestConstants.STRING_THREE);
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setNoOfPeople(TestConstants.INT_THREE);
        householdDTO.setHouseholdNo(TestConstants.LONG_THREE);
        householdDTO.setVillage(TestConstants.NAME);
        householdDTO.setLandmark(TestConstants.NAME);
        householdDTO.setLatitude(TestConstants.ONE_DOUBLE);
        householdDTO.setLongitude(TestConstants.ONE_DOUBLE);
        householdDTO.setVillageId(TestConstants.STRING_THREE);
        householdDTO.setOwnedHandWashingFacilityWithSoap(true);
        householdDTO.setOwnedAnImprovedLatrine(true);
        householdDTO.setOwnedTreatedBedNet(true);
        householdDTO.setProvenance(TestDataProvider.getProvenance());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);

        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setInitial(TestConstants.NAME);
        householdMemberDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        householdMemberDTO.setGender(TestConstants.MALE);
        householdMemberDTO.setDateOfBirth(new Date());
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        householdMemberDTO.setPatientId(TestConstants.STRING_THREE);
        householdMemberDTO.setVillageId(TestConstants.STRING_THREE);
        householdMemberDTO.setHouseholdId(TestConstants.STRING_THREE);
        householdMemberDTO.setId(TestConstants.STRING_THREE);
        householdMemberDTO.setProvenance(TestDataProvider.getProvenance());

        householdDTO.setHouseholdMembers(List.of(householdMemberDTO));

        Coding coding = new Coding();
        coding.setCode("isHandWashingFacility");
        coding.setSystem("http://mdtlabs.com/fhir/");
        coding.setDisplay("Owns hand washing facility with soap?");

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.addCoding(coding);
        codeableConcept.setText(CodeDetailsConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);

        Group.GroupCharacteristicComponent characteristicComponent = new Group.GroupCharacteristicComponent();
        characteristicComponent.setCode(codeableConcept);
        characteristicComponent.getValueCodeableConcept().addCoding(new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,Constants.YES_CODE,Constants.YES));
        characteristicComponent.getValueCodeableConcept().setText(Constants.YES);

        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setName(householdDTO.getName());
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(householdDTO.getNoOfPeople());
        group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                .setValue(householdDTO.getHouseholdNo().toString());
        group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(householdDTO.getVillageId());
        group.getCharacteristic().add(characteristicComponent);
        Bundle groupBundle = new Bundle();
        groupBundle.addEntry()
                .setResource(group)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, TestConstants.TWO_STR))
                .setMethod(Bundle.HTTPVerb.PUT);
        HealthFacilityRequestDTO healthFacilityRequestDTO = new HealthFacilityRequestDTO();
        Chiefdom chiefdom = new Chiefdom();
        chiefdom.setCode(TestConstants.TWO_STR);
        healthFacilityRequestDTO.setChiefdom(chiefdom);

        List<String> ids = new ArrayList<>();
        ids.add(householdMemberDTO.getHouseholdId());

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                householdMemberDTO);

        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        ReflectionTestUtils.setField(householdService, "fhirServerUrl", "fhirServerUrl");

        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, ids), ids.size()))).thenReturn(bundle);
        when(fhirUtils.getFhirIdsFromResponse(fhirResponse)).thenReturn(new HashMap<>());
        when(restApiUtil.getBatchRequest("Group?_id=3&_count=1")).thenReturn(groupBundle);
        when(adminServiceApiInterface.getHealthFacilityByFhirId(any(), any(), any())).thenReturn(healthFacilityRequestDTO);
        when(adminServiceApiInterface.getHouseholdSequenceByVillageId(eq(TestConstants.BEARER_TEST), eq(TestConstants.ADMIN), any(SearchRequestDTO.class))).thenReturn(1l);
        when(adminServiceApiInterface.getHealthFacilityByFhirId(any(), any(), any())).thenReturn(healthFacilityRequestDTO);
        when(adminServiceApiInterface.getVillageDetailsByVillageId(any(), any(), any())).thenReturn(TestDataProvider.getVillageDTO());

        HouseholdMemberDTO response = householdService.createHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateHousehold() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setId(TestConstants.STRING_THREE);
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setNoOfPeople(TestConstants.INT_THREE);
        householdDTO.setHouseholdNo(TestConstants.LONG_THREE);
        householdDTO.setVillage(TestConstants.NAME);
        householdDTO.setLandmark(TestConstants.NAME);
        householdDTO.setLatitude(TestConstants.ONE_DOUBLE);
        householdDTO.setLongitude(TestConstants.ONE_DOUBLE);
        householdDTO.setVillageId(TestConstants.STRING_THREE);
        householdDTO.setOwnedHandWashingFacilityWithSoap(true);
        householdDTO.setOwnedAnImprovedLatrine(true);
        householdDTO.setOwnedTreatedBedNet(true);
        householdDTO.setProvenance(TestDataProvider.getProvenance());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);

        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setInitial(TestConstants.NAME);
        householdMemberDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        householdMemberDTO.setGender(TestConstants.MALE);
        householdMemberDTO.setDateOfBirth(new Date());
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        householdMemberDTO.setPatientId(TestConstants.STRING_THREE);
        householdMemberDTO.setVillageId(TestConstants.STRING_THREE);
        householdMemberDTO.setHouseholdId(TestConstants.STRING_THREE);
        householdMemberDTO.setId(TestConstants.STRING_THREE);
        householdMemberDTO.setProvenance(TestDataProvider.getProvenance());
        householdMemberDTO.setChildren(List.of());

        householdDTO.setHouseholdMembers(List.of(householdMemberDTO));

        Coding coding = new Coding();
        coding.setCode("isHandWashingFacility");
        coding.setSystem("http://mdtlabs.com/fhir/");
        coding.setDisplay("Owns hand washing facility with soap?");

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.addCoding(coding);
        codeableConcept.setText(CodeDetailsConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);

        Group.GroupCharacteristicComponent characteristicComponent = new Group.GroupCharacteristicComponent();
        characteristicComponent.setCode(codeableConcept);
        characteristicComponent.getValueCodeableConcept().addCoding(new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,Constants.YES_CODE,Constants.YES));
        characteristicComponent.getValueCodeableConcept().setText(Constants.YES);

        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setName(householdDTO.getName());
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(householdDTO.getNoOfPeople());
        group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                .setValue(householdDTO.getHouseholdNo().toString());
        group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(householdDTO.getVillageId());
        group.getCharacteristic().add(characteristicComponent);

        List<String> ids = new ArrayList<>();
        ids.add(householdMemberDTO.getId());

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(group);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                householdMemberDTO);

        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(householdService, "fhirServerUrl", "fhirServerUrl");
        TestDataProvider.init();

        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setFhirId("1234");

        HealthFacilityRequestDTO healthFacilityRequestDTO = new HealthFacilityRequestDTO();
        Chiefdom chiefdom = new Chiefdom("Chiefdom", "1234", 1l, 1l, 1l);
        healthFacilityRequestDTO.setChiefdom(chiefdom);
        //when
        TestDataProvider.getStaticMock();
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, ids), ids.size()))).thenReturn(bundle);
        when(fhirMapper.setGroup(any(), any())).thenReturn(group);
        when(adminServiceApiInterface.getHealthFacilityByFhirId(eq(TestConstants.BEARER_TEST), eq(TestConstants.ADMIN), any(RequestDTO.class))).thenReturn(healthFacilityRequestDTO);
        when(adminServiceApiInterface.getMemberSequenceByVillageId(eq(TestConstants.BEARER_TEST), eq(TestConstants.ADMIN), any(SearchRequestDTO.class))).thenReturn(1l);
        when(adminServiceApiInterface.getVillageDetailsByVillageId(eq(TestConstants.BEARER_TEST), eq(TestConstants.ADMIN), any(SearchRequestDTO.class))).thenReturn(TestDataProvider.getVillageDTO());
        doNothing().when(fhirUtils).setBundle(StringUtil.concatString(String.valueOf(ResourceType.Group), Constants.FORWARD_SLASH,
                        householdDTO.getId()),
                Constants.EMPTY_SPACE,
                Bundle.HTTPVerb.PUT,
                group,
                bundle,
                householdDTO.getProvenance());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(fhirResponse)).thenReturn(new HashMap<>());

        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, ids), ids.size()))).thenReturn(bundle);

        //then
        HouseholdDTO response = householdService.updateHousehold(householdDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response, householdDTO);
    }

    @Test
    void  updateHouseholdMember() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setInitial(TestConstants.NAME);
        householdMemberDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        householdMemberDTO.setGender(TestConstants.MALE);
        householdMemberDTO.setDateOfBirth(new Date());
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        householdMemberDTO.setPatientId(TestConstants.STRING_THREE);
        householdMemberDTO.setVillageId(TestConstants.STRING_THREE);
        householdMemberDTO.setHouseholdId(TestConstants.STRING_THREE);
        householdMemberDTO.setId(TestConstants.STRING_THREE);
        householdMemberDTO.setProvenance(TestDataProvider.getProvenance());

        HumanName name = new HumanName();
        name.setText(householdMemberDTO.getName());
        name.setPrefix(List.of(new StringType(householdMemberDTO.getInitial())));

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setName(List.of(name));
        relatedPerson.setPatient(new Reference(householdMemberDTO.getPatientReference()));
        relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(TestConstants.MALE));
        relatedPerson.setActive(true);
        relatedPerson.setBirthDate(new Date());

        List<String> ids = new ArrayList<>();
        ids.add(householdMemberDTO.getId());

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(relatedPerson);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();
        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                householdMemberDTO);

        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        ReflectionTestUtils.setField(householdService, "fhirServerUrl", "fhirServerUrl");


        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.HOUSEHOLD_MEMBER_LIST, String.join(Constants.COMMA, ids), ids.size()))).thenReturn(bundle);
        when(fhirMapper.setRelatedPerson(any(), any())).thenReturn(relatedPerson);

        bundle.setTotal(0);
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_ID, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, householdMemberDTO.getPatientId()))).thenReturn(bundle);
        doNothing().when(fhirUtils).setBundle("",
                "",
                Bundle.HTTPVerb.PUT,
                relatedPerson,
                bundle,
                householdMemberDTO.getProvenance());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(
                responseEntity);

        //then
        HouseholdMemberDTO response = householdService.updateHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdByMemberPatientId() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        HouseholdDTO householdDTO = new HouseholdDTO();
        householdDTO.setId(TestConstants.STRING_THREE);
        householdDTO.setName(TestConstants.NAME);
        householdDTO.setNoOfPeople(TestConstants.INT_THREE);
        householdDTO.setHouseholdNo(TestConstants.LONG_THREE);
        householdDTO.setVillage(TestConstants.NAME);
        householdDTO.setLandmark(TestConstants.NAME);
        householdDTO.setLatitude(TestConstants.ONE_DOUBLE);
        householdDTO.setLongitude(TestConstants.ONE_DOUBLE);
        householdDTO.setVillageId(TestConstants.STRING_THREE);
        householdDTO.setOwnedHandWashingFacilityWithSoap(true);
        householdDTO.setOwnedAnImprovedLatrine(true);
        householdDTO.setOwnedTreatedBedNet(true);
        householdDTO.setProvenance(TestDataProvider.getProvenance());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);

        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setName(TestConstants.NAME);
        householdMemberDTO.setInitial(TestConstants.NAME);
        householdMemberDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        householdMemberDTO.setGender(TestConstants.MALE);
        householdMemberDTO.setDateOfBirth(new Date());
        householdMemberDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        householdMemberDTO.setPhoneNumberCategory(TestConstants.PHONE_NUMBER_CATEGORY);
        householdMemberDTO.setPatientId(TestConstants.STRING_THREE);
        householdMemberDTO.setVillageId(TestConstants.STRING_THREE);
        householdMemberDTO.setHouseholdId(TestConstants.STRING_THREE);
        householdMemberDTO.setId(TestConstants.STRING_THREE);

        Coding coding = new Coding();
        coding.setCode("isHandWashingFacility");
        coding.setSystem("http://mdtlabs.com/fhir/");
        coding.setDisplay("Owns hand washing facility with soap?");

        CodeableConcept codeableConcept = new CodeableConcept();
        codeableConcept.addCoding(coding);
        codeableConcept.setText(CodeDetailsConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);

        Group.GroupCharacteristicComponent characteristicComponent = new Group.GroupCharacteristicComponent();
        characteristicComponent.setCode(codeableConcept);
        characteristicComponent.getValueCodeableConcept().addCoding(new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,Constants.YES_CODE,Constants.YES));
        characteristicComponent.getValueCodeableConcept().setText(Constants.YES);

        householdDTO.setHouseholdMembers(List.of(householdMemberDTO));
        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setName(householdDTO.getName());
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(householdDTO.getNoOfPeople());
        group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                .setValue(householdDTO.getHouseholdNo().toString());
        group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                .setValue(householdDTO.getVillageId());
        group.getCharacteristic().add(characteristicComponent);

        List<String> ids = new ArrayList<>();
        ids.add(householdMemberDTO.getId());

        List<String> householdMemberIds = new ArrayList<>();
        householdMemberIds.add(householdMemberDTO.getId());

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(group);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_GROUP_BY_MEMBER_ID_PARAM, householdMemberDTO.getId()))).thenReturn(bundle);
        when(fhirMapper.toHousehold(group, new HouseholdDTO())).thenReturn(any());
        when(restApiUtil.getBatchRequest(String.format(Constants.HOUSEHOLD_MEMBER_LIST, String.join(Constants.COMMA,
                householdMemberIds), householdMemberIds.size()))).thenReturn(bundle);

        //then
        HouseholdDTO response = householdService.getHouseholdByMemberPatientId(householdMemberDTO.getId(), List.of());
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdById() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        Group group = new Group();
        group.setActive(Boolean.TRUE);
        group.setType(Group.GroupType.PERSON);
        group.setActual(Boolean.TRUE);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(group);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        List<String> groupIds = List.of(TestConstants.STRING_THREE);

        //when
        when(restApiUtil.getBatchRequest(
                String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, groupIds), groupIds.size()))).thenReturn(bundle);
        doNothing().when(fhirMapper).mapHousehold(new HouseholdDTO(), group);

        //then
        HouseholdDTO response = householdService.getHouseholdById(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getRelatedPersonByPatientId() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        name.setPrefix(List.of(new StringType(TestConstants.FIRST_NAME)));

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setName(List.of(name));
        relatedPerson.setPatient(new Reference(TestConstants.PATIENT_REFERENCE));
        relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(TestConstants.MALE));
        relatedPerson.setActive(true);
        relatedPerson.setBirthDate(new Date());

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(relatedPerson);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_MEMBER_PARAMS, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, TestConstants.STRING_THREE))).thenReturn(bundle);

        //then
        RelatedPerson response = householdService.getRelatedPersonByPatientId(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getRelatedPersonCountByHouseholdId() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        bundle.setTotal(TestConstants.INT_ONE);

        //when
        when(restApiUtil.getBatchRequest(StringUtil.concatString(String.format(Constants.GET_HOUSEHOLD_MEMBER_PARAMS,
                FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL, TestConstants.STRING_THREE), Constants.PATIENT_ACTIVE_STATUS))).thenReturn(bundle);

        //then
        int response = householdService.getRelatedPersonCountByHouseholdId(TestConstants.STRING_THREE);
        Assertions.assertEquals(TestConstants.INT_ONE, response);
    }

    @Test
    void getHouseholdMemberByPatientId() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        name.setPrefix(List.of(new StringType(TestConstants.FIRST_NAME)));

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setName(List.of(name));
        relatedPerson.setPatient(new Reference(TestConstants.PATIENT_REFERENCE));
        relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(TestConstants.MALE));
        relatedPerson.setActive(true);
        relatedPerson.setBirthDate(new Date());

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(relatedPerson);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_MEMBER_PARAMS,
                FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, TestConstants.STRING_THREE))).thenReturn(bundle);
        when(fhirMapper.toHouseholdMembers(bundle)).thenReturn(List.of(householdMemberDTO));

        //then
        HouseholdMemberDTO response = householdService.getHouseholdMemberByPatientId(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdMemberById() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_MEMBER_ID, TestConstants.STRING_THREE))).thenReturn(bundle);
        //then
        HouseholdMemberDTO response = householdService.getHouseholdMemberById(TestConstants.STRING_THREE);
        Assertions.assertNull(response);

        //given
        HumanName name = new HumanName();
        name.setText(TestConstants.NAME);
        name.setPrefix(List.of(new StringType(TestConstants.FIRST_NAME)));

        RelatedPerson relatedPerson = new RelatedPerson();
        relatedPerson.setName(List.of(name));
        relatedPerson.setPatient(new Reference(TestConstants.PATIENT_REFERENCE));
        relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(TestConstants.MALE));
        relatedPerson.setActive(true);
        relatedPerson.setBirthDate(new Date());

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(relatedPerson);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_MEMBER_ID, TestConstants.STRING_THREE))).thenReturn(bundle);
        when(fhirMapper.toHouseholdMember(relatedPerson)).thenReturn(householdMemberDTO);

        //then
        response = householdService.getHouseholdMemberById(TestConstants.STRING_THREE);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateSignature() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setAssignHousehold(Boolean.TRUE);
        householdMemberDTO.setHouseholdId(TestConstants.BLANK_STRING);
        householdMemberDTO.setId(TestConstants.STRING_THREE);
        Bundle groupBundle = new Bundle();
        groupBundle.addEntry()
                .setResource(TestDataProvider.getGroup())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle patientBundle = new Bundle();
        patientBundle.setTotal(TestConstants.TEN);
        patientBundle.addEntry()
                .setResource(TestDataProvider.getPatient())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null")).thenReturn(bundle);
        when(fhirMapper.toHouseholdMember(any())).thenReturn(householdMemberDTO);
        when(restApiUtil.getBatchRequest("Group?_id=&_count=1")).thenReturn(groupBundle);
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=3&_count=1")).thenReturn(bundle);
        when(restApiUtil.getBatchRequest("Patient?identifier=nullpatient-id|patientId")).thenReturn(patientBundle);

        //then
        HouseholdMemberDTO result = householdService.updateSignature(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void getHouseholdMemberList() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setVillageIds(List.of(TestConstants.ONE_STR, TestConstants.TWO_STR, TestConstants.STRING_THREE));
        requestDTO.setLastSyncTime(new Date());
        requestDTO.setCurrentSyncTime(new Date());
        requestDTO.setSkip(TestConstants.INT_ONE);
        requestDTO.setLimit(TestConstants.INT_ONE);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getRelatedPersonBundle());

        //then
        List<HouseholdMemberDTO> result = householdService.getHouseholdMemberList(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void getHouseholdList() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setId(TestConstants.STRING_THREE);
        Bundle groupBundle = new Bundle();
        groupBundle.addEntry()
                .setResource(TestDataProvider.getGroup())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, TestConstants.TWO_STR))
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle locationBundle = new Bundle();
        locationBundle.addEntry()
                .setResource(TestDataProvider.getLocation())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, TestConstants.TWO_STR))
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle deviceBundle = new Bundle();
        deviceBundle.addEntry()
                .setResource(TestDataProvider.getDevice())
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, TestConstants.TWO_STR))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(restApiUtil.getBatchRequest("Group?_id=3&_count=1")).thenReturn(groupBundle);
        when(restApiUtil.getBatchRequest("Location?_id=null&_count=1")).thenReturn(locationBundle);
        when(restApiUtil.getBatchRequest("Device?_id=null&_count=1")).thenReturn(deviceBundle);

        //then
        List<HouseholdDTO> result = householdService.getHouseholdList(requestDTO);
        Assertions.assertNotNull(result);
    }

    @Test
    void getHouseholdMemberByVillagesWithPatientVitals() {
        //given
        List<String> villages = List.of(TestConstants.ONE_STR, TestConstants.TWO_STR);
        Date lastSyncTime = new Date();
        Date currentSyncTime = new Date();
        List<String> projections = List.of(TestConstants.ONE_STR, TestConstants.TWO_STR);
        int skip = TestConstants.INT_ONE;
        int limit = TestConstants.INT_ONE;

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        List<String> result = householdService.getHouseholdMemberByVillagesWithPatientVitals(villages, lastSyncTime, currentSyncTime, projections, skip, limit);
        Assertions.assertNotNull(result);
    }
}