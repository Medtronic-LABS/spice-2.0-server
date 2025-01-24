package com.mdtlabs.coreplatform.fhirmapper.household.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdReferenceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;

/**
 * <p>
 * This class is a service class to perform operation on Household
 * operations.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Feb 05, 2024
 */
@Service
public class HouseholdServiceImpl implements HouseholdService {

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final AdminServiceApiInterface adminServiceApiInterface;

    private final FhirMapper fhirMapper;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    public HouseholdServiceImpl(AdminServiceApiInterface adminServiceApiInterface, FhirMapper fhirMapper,
                                FhirAssessmentMapper fhirAssessmentMapper, FhirUtils fhirUtils, RestApiUtil restApiUtil) {
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.fhirMapper = fhirMapper;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
    }

    @Override
    public HouseholdDTO createHousehold(HouseholdDTO householdDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String uuid = fhirUtils.getUniqueId();
        householdDTO.setHouseholdNo(adminServiceApiInterface.getHouseholdSequenceByVillageId(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), new SearchRequestDTO(Long.valueOf(householdDTO.getVillageId()))));
        Group group = fhirMapper.setGroup(householdDTO, new Group());
        String locationUrl = postBundleLocation(uuid, householdDTO, bundle);
        group.getCharacteristic().add(new Group.GroupCharacteristicComponent().setValue(new Reference(locationUrl)));
        String deviceUrl = postBundleDevice(householdDTO, bundle);
        group.addMember(new Group.GroupMemberComponent().setEntity(new Reference(deviceUrl)));
        if (Objects.nonNull(householdDTO.getHouseholdMembers())) {
            for (HouseholdMemberDTO member : householdDTO.getHouseholdMembers()) {
                String url = Boolean.TRUE.equals(member.getAssignHousehold()) ? mapMemberToHousehold(member, bundle) : postBundleHouseholdMember(member, bundle);
                fhirMapper.setGroupCharacteristicComponent(group.getCharacteristic(), url, member);
            }
        }
        fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Group), Constants.FORWARD_SLASH,
                        Constants.FHIR_BASE_URL, String.valueOf(ResourceType.Group), uuid),
                StringUtil.concatString(Constants.FHIR_BASE_URL,
                        String.valueOf(ResourceType.Group), uuid),
                Bundle.HTTPVerb.POST,
                group,
                bundle,
                householdDTO.getProvenance());
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        constructHouseholdResponse(householdDTO, responseEntity.getBody());
        updateHouseholdIdInMemberResource(householdDTO);
        return householdDTO;
    }

    /**
     * Construct Response for household create, sets a Fhir id to household and member
     *
     * @param householdDTO    HouseHold and member Details
     * @param fhirResponseDTO Fhir household create Response
     * @return householdDTO - with FHIR id
     */
    private HouseholdDTO constructHouseholdResponse(HouseholdDTO householdDTO, FhirResponseDTO fhirResponseDTO) {
        if (Objects.isNull(householdDTO.getId())) {
            householdDTO.setId(getIdFromFhirResponse(fhirResponseDTO, String.valueOf(ResourceType.Group)));
        }
        Bundle groupBundle = getHouseholdByIds(List.of(householdDTO.getId()));
        Map<String, String> memberMap = new HashMap<>();
        groupBundle.getEntry().forEach(entry -> {
            Group groupRes = (Group) entry.getResource();
            groupRes.getCharacteristic().forEach(charV -> {
                if (charV.getValue() instanceof Reference) {
                    String[] parts = charV.getValueReference().getReference().split(Constants.FORWARD_SLASH);
                    memberMap.put(charV.getCode().getText(), parts[1]);
                }
            });
        });
        if (Objects.nonNull(householdDTO.getHouseholdMembers())) {
            householdDTO.getHouseholdMembers().forEach(member -> {
                member.setId(memberMap.get(member.getPatientId()));
                member.setHouseholdId(householdDTO.getId());
            });
        }
        return householdDTO;
    }

    /**
     * Update household ID in Member resource
     *
     * @param householdDTO HouseHold and member Details
     */
    private void updateHouseholdIdInMemberResource(HouseholdDTO householdDTO) {
        if (Objects.nonNull(householdDTO.getHouseholdMembers())) {
            List<String> memberIds = householdDTO.getHouseholdMembers().stream().map(HouseholdMemberDTO::getId).toList();
            Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            Bundle memberBundle = getHouseholdMemberByIds(memberIds);
            memberBundle.getEntry().forEach(entry -> {
                RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
                String id = fhirUtils.getIdFromHistoryUrl(relatedPerson.getId());
                Identifier identifier = new Identifier();
                identifier.setSystem(FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL);
                identifier.setValue(householdDTO.getId());
                relatedPerson.getIdentifier().add(identifier);
                fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, id),
                        StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), id),
                        Bundle.HTTPVerb.PUT, relatedPerson, bundle, householdDTO.getProvenance());
            });
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        }
    }

    /**
     * Retrieves the ID of a specific resource from a FHIR response DTO.
     *
     * @param fhirResponseDTO FhirResponseDTO is an object that contains the response data from a FHIR
     *                        (Fast Healthcare Interoperability Resources) API. It likely includes information about a specific
     *                        resource in the FHIR system, such as a patient, practitioner, or observation.
     * @param resource        Resource is a string parameter that represents the type of resource for which you
     *                        want to retrieve the ID from the FHIR response.
     * @return Returning the ID of a specific resource from a FHIR response. If the ID is
     * found in the response for the specified resource, it will return the first ID in the list. If the ID
     * is not found or the list is empty, it will return null.
     */
    private String getIdFromFhirResponse(FhirResponseDTO fhirResponseDTO, String resource) {
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(fhirResponseDTO);
        return (!Objects.isNull(response.get(resource)) && !response.get(resource).isEmpty())
                ? response.get(resource).getFirst() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdMemberDTO createHouseholdMember(HouseholdMemberDTO householdMemberDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (Objects.nonNull(householdMemberDTO.getHouseholdId())) {
            Bundle groupValue = getHouseholdByIds(List.of(householdMemberDTO.getHouseholdId()));
            groupValue.getEntry().forEach(entry -> {
                Group group = (Group) entry.getResource();
                String memberUrl = Boolean.TRUE.equals(householdMemberDTO.getAssignHousehold()) ? mapMemberToHousehold(householdMemberDTO, bundle) : postBundleHouseholdMember(householdMemberDTO, bundle);
                fhirMapper.setGroupCharacteristicComponent(group.getCharacteristic(), memberUrl, householdMemberDTO);
                fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Group), Constants.FORWARD_SLASH,
                                householdMemberDTO.getHouseholdId()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, householdMemberDTO.getHouseholdId()),
                        Bundle.HTTPVerb.PUT, group, bundle, householdMemberDTO.getProvenance());
            });
        } else {
            postBundleHouseholdMember(householdMemberDTO, bundle);
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        householdMemberDTO.setId(getIdFromFhirResponse(responseEntity.getBody(),
                String.valueOf(ResourceType.RelatedPerson)));
        return householdMemberDTO;
    }

    /**
     * Set Patient Id for household member
     *
     * @param householdMemberDTO Household Member Details
     */
    private void setPatientId(HouseholdMemberDTO householdMemberDTO) {
        RequestDTO healthFacilityRequest = new RequestDTO();
        healthFacilityRequest.setFhirId(householdMemberDTO.getProvenance().getOrganizationId());
        RequestDTO sequenceRequest = new RequestDTO();
        sequenceRequest.setVillageId(householdMemberDTO.getVillageId());
        HealthFacilityRequestDTO healthFacilityDTO =
                adminServiceApiInterface.getHealthFacilityByFhirId(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), healthFacilityRequest);
        householdMemberDTO.setPatientId(generatePatientId(Integer.parseInt(healthFacilityDTO.getChiefdom().getCode()),
                Integer.parseInt(householdMemberDTO.getVillageId()),
                householdMemberDTO.getProvenance().getSpiceUserId()));
    }

    /**
     * Generate Patient Id
     *
     * @param chiefdomCode   Chiefdom Code
     * @param villageId      VillageId
     * @param userId         User Id
     * @return Patient Id
     */
    private String generatePatientId(int chiefdomCode, int villageId, Long userId) {
        Long count = adminServiceApiInterface.getMemberSequenceByVillageId(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), new SearchRequestDTO(Long.valueOf(villageId)));
        String villageCode = adminServiceApiInterface.getVillageDetailsByVillageId(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), new SearchRequestDTO(Long.valueOf(villageId))).getCode();
        return StringUtil.concatString(String.format(Constants.THREE_DIGIT_FORMAT_SPECIFIER, chiefdomCode),
                String.format(Constants.FOUR_DIGIT_FORMAT_SPECIFIER, Long.valueOf(villageCode)),
                String.valueOf(userId),
                String.format(Constants.FOUR_DIGIT_FORMAT_SPECIFIER, count));
    }

    /**
     * Add Entry in bundle for member
     *
     * @param householdMemberDTO member details
     * @param bundle             FHIR bundle
     * @return member reference
     */
    private String postBundleHouseholdMember(HouseholdMemberDTO householdMemberDTO, Bundle bundle) {
        String uuid = fhirUtils.getUniqueId();
        String url = StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, householdMemberDTO.getHouseholdHeadRelationship(), uuid);
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL,
                householdMemberDTO.getHouseholdHeadRelationship(),
                uuid);
        setPatientId(householdMemberDTO);
        RelatedPerson relatedPerson = fhirMapper.setRelatedPerson(householdMemberDTO, new RelatedPerson());
        fhirUtils.setBundle(url,
                fullUrl,
                Bundle.HTTPVerb.POST,
                relatedPerson,
                bundle,
                householdMemberDTO.getProvenance());
        return url;
    }

    /**
     * Map member to Household
     *
     * @param householdMemberDTO member details
     * @param bundle             FHIR bundle
     * @return member reference
     */
    private String mapMemberToHousehold(HouseholdMemberDTO householdMemberDTO, Bundle bundle) {
        Bundle bundleResponse = getHouseholdMemberByIds(List.of(householdMemberDTO.getId()));
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, householdMemberDTO.getId());
        bundleResponse.getEntry().forEach(entry -> {
            RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
            relatedPerson =fhirMapper.setRelatedPerson(householdMemberDTO, relatedPerson);
            String url = StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                    Constants.FORWARD_SLASH, householdMemberDTO.getId());
            fhirUtils.setBundle(url,
                    fullUrl,
                    Bundle.HTTPVerb.PUT,
                    relatedPerson,
                    bundle,
                    householdMemberDTO.getProvenance());
        });
        return fullUrl;
    }

    /**
     * Add Entry in bundle for Device
     *
     * @param householdDTO Household details
     * @param bundle       FHIR Bundle Object
     * @return Device Reference
     */
    private String postBundleDevice(HouseholdDTO householdDTO, Bundle bundle) {
        String uuid = fhirUtils.getUniqueId();
        String url = StringUtil.concatString(String.valueOf(ResourceType.Device), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, String.valueOf(ResourceType.Device), uuid);
        Device device = fhirMapper.setDeviceEntity(householdDTO, new Device());
        fhirUtils.setBundle(url, StringUtil.concatString(Constants.FHIR_BASE_URL, String.valueOf(ResourceType.Device), uuid),
                Bundle.HTTPVerb.POST, device, bundle, householdDTO.getProvenance());
        return url;

    }

    /**
     * Add Entry in bundle for Location
     *
     * @param uuid         unique Id
     * @param householdDTO Household details
     * @param bundle       FHIR Bundle Object
     * @return Location Reference
     */
    private String postBundleLocation(String uuid, HouseholdDTO householdDTO, Bundle bundle) {
        String url = StringUtil.concatString(String.valueOf(ResourceType.Location),
                Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL,
                String.valueOf(ResourceType.Location),
                uuid);
        Location location = fhirMapper.setLocation(new Location(), householdDTO);
        fhirUtils.setBundle(url, StringUtil.concatString(Constants.FHIR_BASE_URL, String.valueOf(ResourceType.Location),
                uuid), Bundle.HTTPVerb.POST, location, bundle, householdDTO.getProvenance());
        return url;
    }

    @Override
    public HouseholdDTO updateHousehold(HouseholdDTO householdDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Bundle groupValue = getHouseholdByIds(List.of(householdDTO.getId()));
        groupValue.getEntry().forEach(entry -> {
            Group group = (Group) entry.getResource();
            group = fhirMapper.setGroup(householdDTO, group);
            updateLocationInGroup(householdDTO, bundle, group);
            updateMembersInGroup(householdDTO, bundle, group);
            updateDeviceInGroup(householdDTO, bundle, group);
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Group), Constants.FORWARD_SLASH,
                            householdDTO.getId()),
                    Constants.EMPTY_SPACE,
                    Bundle.HTTPVerb.PUT,
                    group,
                    bundle,
                    householdDTO.getProvenance());
        });
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        constructHouseholdResponse(householdDTO, responseEntity.getBody());
        return householdDTO;
    }

    /**
     * Update Location in Group
     *
     * @param householdDTO - Household details
     * @param bundle       - FHIR Bundle Object
     * @param group        - Group Object
     */
    private void updateLocationInGroup(HouseholdDTO householdDTO, Bundle bundle, Group group) {
        if (Objects.nonNull(householdDTO.getLatitude()) && Objects.nonNull(householdDTO.getLongitude())) {
            group.getCharacteristic().forEach(characteristicComponent -> {
                if (characteristicComponent.getValue() instanceof Reference) {
                    String valueReference = characteristicComponent.getValueReference().getReference();
                    if (valueReference.startsWith(String.valueOf(ResourceType.Location))) {
                        Bundle locationBundle = getLocationByIds(List.of(fhirUtils.getIdFromReference(valueReference)));
                        locationBundle.getEntry().forEach(resource -> {
                            Location location = (Location) resource.getResource();
                            fhirMapper.setLocation(location, householdDTO);
                            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Location),
                                            Constants.FORWARD_SLASH,
                                            fhirUtils.getIdFromHistoryUrl(location.getId())), Constants.EMPTY_SPACE,
                                    Bundle.HTTPVerb.PUT, location, bundle, householdDTO.getProvenance());
                        });
                    }
                }
            });
        }
    }

    /**
     * Update Members in Group
     *
     * @param householdDTO - Household details
     * @param bundle       - FHIR Bundle Object
     * @param group        - Group Object
     */
    private void updateMembersInGroup(HouseholdDTO householdDTO, Bundle bundle, Group group) {
        if (Objects.nonNull(householdDTO.getHouseholdMembers())) {
            for (HouseholdMemberDTO member : householdDTO.getHouseholdMembers()) {
                String url;
                if (Boolean.TRUE.equals(member.getAssignHousehold())) {
                    member.setHouseholdId(householdDTO.getId());
                    url = mapMemberToHousehold(member, bundle);
                }
                else {
                    url = postBundleHouseholdMember(member, bundle);
                }
                fhirMapper.setGroupCharacteristicComponent(group.getCharacteristic(), url, member);
            }
        }
    }

    /**
     * Update Device in Group
     *
     * @param householdDTO - Household details
     * @param bundle       - FHIR Bundle Object
     * @param group        - Group Object
     */
    private void updateDeviceInGroup(HouseholdDTO householdDTO, Bundle bundle, Group group) {
        if (Objects.nonNull(householdDTO.getHeadPhoneNumber())) {
            group.getMember().stream().forEach(groupMemberComponent -> {
                String valueReference = groupMemberComponent.getEntity().getReference();
                if (valueReference.startsWith(String.valueOf(ResourceType.Device))) {
                    Bundle deviceBundle = getDeviceByIds(List.of(fhirUtils.getIdFromReference(valueReference)));
                    deviceBundle.getEntry().forEach(resource -> {
                        Device device = (Device) resource.getResource();
                        device = fhirMapper.setDeviceEntity(householdDTO, device);
                        fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Device), Constants.FORWARD_SLASH,
                                        fhirUtils.getIdFromHistoryUrl(device.getId())), Constants.EMPTY_SPACE,
                                Bundle.HTTPVerb.PUT, device, bundle, householdDTO.getProvenance());
                    });
                }
            });
        }
    }

    /**
     * Get Patient details by Identifier patientId
     *
     * @param id patientId value
     * @return Bundle Object
     */
    public Bundle getPatientDetailsByPatientId(String id) {
        return restApiUtil.getBatchRequest(
                String.format(Constants.GET_PATIENT_BY_ID, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, id));
    }

    /**
     * update patient details based on household member
     *
     * @param id                 idPart
     * @param householdMemberDTO member details
     * @param bundle             Bundle object
     */
    private void updatePatient(String id, HouseholdMemberDTO householdMemberDTO, Bundle bundle) {
        Bundle patientBundle = getPatientDetailsByPatientId(id);
        if (Constants.ZERO < patientBundle.getTotal()) {
            Patient patient = (Patient) patientBundle.getEntry().getFirst().getResource();
            fhirAssessmentMapper.setPatient(patient, householdMemberDTO, null);
            fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH,
                            patient.getIdPart()), StringUtil.concatString(Constants.FHIR_BASE_URL, patient.getIdPart()), Bundle.HTTPVerb.PUT,
                    patient, bundle, householdMemberDTO.getProvenance());
        }
    }

    @Override
    public HouseholdMemberDTO updateHouseholdMember(HouseholdMemberDTO householdMemberDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (Boolean.TRUE.equals(householdMemberDTO.getAssignHousehold()) && (Objects.nonNull(householdMemberDTO.getHouseholdId()))) {
            Bundle groupValue = getHouseholdByIds(List.of(householdMemberDTO.getHouseholdId()));
            groupValue.getEntry().forEach(entry -> {
                Group group = (Group) entry.getResource();
                String memberUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, householdMemberDTO.getId());
                fhirMapper.setGroupCharacteristicComponent(group.getCharacteristic(), memberUrl, householdMemberDTO);
                fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Group), Constants.FORWARD_SLASH,
                                householdMemberDTO.getHouseholdId()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, householdMemberDTO.getHouseholdId()),
                        Bundle.HTTPVerb.PUT, group, bundle, householdMemberDTO.getProvenance());
            });

        }
        Bundle bundleResponse = getHouseholdMemberByIds(List.of(householdMemberDTO.getId()));
        bundleResponse.getEntry().forEach(entry -> {
            RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
            fhirMapper.setRelatedPerson(householdMemberDTO, relatedPerson);
            String url = StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                    Constants.FORWARD_SLASH, householdMemberDTO.getId());
            updatePatient(householdMemberDTO.getPatientId(), householdMemberDTO, bundle);
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, householdMemberDTO.getId());
            fhirUtils.setBundle(url,
                    fullUrl,
                    Bundle.HTTPVerb.PUT,
                    relatedPerson,
                    bundle,
                    householdMemberDTO.getProvenance());
        });
        restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        return householdMemberDTO;
    }

    @Override
    public HouseholdDTO getHousehold(String groupId) {
        Bundle bundle = getHouseholdByIds(List.of(groupId));
        HouseholdDTO householdDTO = new HouseholdDTO();
        bundle.getEntry().forEach(entry -> {
            Group group = (Group) entry.getResource();
            fhirMapper.toHousehold(group, householdDTO);
        });
        return householdDTO;
    }

    /**
     * Get Household member by Identifier patientId
     *
     * @param patientId patientId value
     * @return Bundle Object
     */
    private Bundle getRelatedPersonByPatientId(String system, String patientId) {
        return restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_MEMBER_PARAMS, system, patientId));
    }

    /**
     * get household members by patientId
     *
     * @param patientId a household member PatientId.
     * @return HouseholdMemberDTO object
     */
    public HouseholdMemberDTO getHouseholdMemberByPatientId(String patientId) {
        List<HouseholdMemberDTO> householdMemberDTOS = fhirMapper.toHouseholdMembers(
                getRelatedPersonByPatientId(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, patientId));
        if (Objects.nonNull(householdMemberDTOS) && !householdMemberDTOS.isEmpty()) {
            return householdMemberDTOS.getFirst();
        }
        throw new DataNotFoundException(1007);
    }

    /**
     * Get Household by id
     *
     * @param groupIds household ids
     * @return Bundle Object
     */
    private Bundle getHouseholdByIds(List<String> groupIds) {
        return restApiUtil.getBatchRequest(
                String.format(Constants.GET_HOUSEHOLD_PARAMS, String.join(Constants.COMMA, groupIds), groupIds.size()));
    }

    /**
     * Get Location by ids
     *
     * @param locationIds - Location ids
     * @return - Bundle Object
     */
    private Bundle getLocationByIds(List<String> locationIds) {
        return restApiUtil.getBatchRequest(
                String.format(Constants.GET_LOCATION_PARAMS, String.join(Constants.COMMA, locationIds),
                        locationIds.size()));
    }

    /**
     * Get Device by ids
     *
     * @param deviceIds - Device ids
     * @return - Bundle Object
     */
    private Bundle getDeviceByIds(List<String> deviceIds) {
        return restApiUtil.getBatchRequest(String.format(Constants.DEVICE_LIST, String.join(Constants.COMMA, deviceIds), deviceIds.size()));
    }

    /**
     * Get Household member by ids
     *
     * @param householdMemberIds - Household member ids
     * @return - Bundle Object
     */
    private Bundle getHouseholdMemberByIds(List<String> householdMemberIds) {
        return restApiUtil.getBatchRequest(String.format(Constants.HOUSEHOLD_MEMBER_LIST, String.join(Constants.COMMA, householdMemberIds), householdMemberIds.size()));
    }

    /**
     * Get Household member by ids
     *
     * @param id - Household member ids
     * @return - Bundle Object
     */
    public HouseholdMemberDTO getHouseholdMemberById(String id) {
        String url = String.format(Constants.GET_MEMBER_ID, id);
        Bundle bundle = restApiUtil.getBatchRequest(url);
        return bundle.getEntry().isEmpty() ? null : fhirMapper.toHouseholdMember((RelatedPerson) bundle.getEntry().getFirst().getResource());
    }

    /**
     * Get Household by villages
     *
     * @param villages        - List of villages
     * @param lastSyncTime    - Last Sync time
     * @param currentSyncTime - Current Sync time
     * @param projections     - List of elements to project
     * @param skip            - skip
     * @param limit           - limit
     * @return - Household bundle Object
     */
    private Bundle getHouseholdByVillages(List<String> villages, Date lastSyncTime,
                                          Date currentSyncTime, List<String> projections, int skip, int limit) {
        List<String> villageIdentifiers = villages.stream().map(village ->
                StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL, Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        String url = String.format(Constants.HOUSEHOLD_LIST_PARAMS, String.join(Constants.COMMA, villageIdentifiers), skip, limit);
        if (Objects.nonNull(lastSyncTime)) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.LAST_SYNC_QUERY_PARAMS,
                    lastSyncTime.toInstant().toString()));
        }
        if (Objects.nonNull(currentSyncTime)) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.CURRENT_SYNC_QUERY_PARAMS,
                    currentSyncTime.toInstant().toString()));
        }
        if (Objects.nonNull(projections) && !projections.isEmpty()) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.PROJECTION_PARAMS,
                    String.join(Constants.COMMA, projections)));
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Get Household member by villages
     *
     * @param villages        - List of villages
     * @param lastSyncTime    - Last Sync time
     * @param currentSyncTime - Current Sync time
     * @param projections     - List of elements to project
     * @param skip            - skip
     * @param limit           -limit
     * @return - Household member bundle Object
     */
    public Bundle getHouseholdMemberByVillages(List<String> villages, Date lastSyncTime,
                                               Date currentSyncTime, List<String> projections, int skip, int limit) {
        List<String> villageIdentifiers = villages.stream().map(village ->
                StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL, Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        String url = String.format(Constants.HOUSEHOLD_MEMBER_LIST_PARAMS, String.join(Constants.COMMA, villageIdentifiers), skip, limit);
        if (Objects.nonNull(lastSyncTime)) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.LAST_SYNC_QUERY_PARAMS,
                    lastSyncTime.toInstant().toString()));
        }
        if (Objects.nonNull(currentSyncTime)) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.CURRENT_SYNC_QUERY_PARAMS,
                    currentSyncTime.toInstant().toString()));
        }
        if (Objects.nonNull(projections) && !projections.isEmpty()) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.PROJECTION_PARAMS,
                    String.join(Constants.COMMA, projections)));
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Get Household member by villages that has patient vitals
     *
     * @param villages        - List of villages
     * @param lastSyncTime    - Last Sync time
     * @param currentSyncTime - Current Sync time
     * @param projections     - List of elements to project
     * @param skip            - skip
     * @param limit           -limit
     * @return - List of member Ids
     */
    public List<String> getHouseholdMemberByVillagesWithPatientVitals(List<String> villages, Date lastSyncTime,
                                                                      Date currentSyncTime, List<String> projections, int skip, int limit) {
        List<String> villageIdentifiers = villages.stream().map(village ->
                StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL, Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        List<String> patientVitalsIdentifier = List.of(Constants.ANC_VISIT_NUMBER, Constants.LAST_MENSTRUAL_PERIOD,
                Constants.PNC_VISIT_NUMBER, Constants.DATE_OF_DELIVERY, Constants.NO_OF_NEONATES,
                Constants.NEONATE_PATIENT_ID, Constants.CHILDHOOD_VISIT_NUMBER, Constants.NEONATE_DEATH_RECORDED_BY_PHU).stream().map(patientVital ->
                StringUtil.concatString(FhirIdentifierConstants.VITALS_TYPE_SYSTEM_URL, Constants.VERTICAL_BAR, String.valueOf(patientVital))).toList();
        List<String> status = Objects.isNull(lastSyncTime) ? List.of(Constants.PRELIMINARY) : List.of(Constants.PRELIMINARY, Constants.STRING_FINAL);
        String url = String.format(Constants.HOUSEHOLD_MEMBER_WITH_PATIENT_VITALS_LIST_PARAMS, String.join(Constants.COMMA, villageIdentifiers),
                String.join(Constants.COMMA, patientVitalsIdentifier), String.join(Constants.COMMA, status), skip, limit);
        if (Objects.nonNull(lastSyncTime)) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.LAST_SYNC_QUERY_PARAMS,
                    lastSyncTime.toInstant().toString()));
        }
        if (Objects.nonNull(currentSyncTime)) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.CURRENT_SYNC_QUERY_PARAMS,
                    currentSyncTime.toInstant().toString()));
        }
        Set<String> memberIds = new HashSet<>();
        Bundle bundle = restApiUtil.getBatchRequest(url);
        bundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(Observation.class::isInstance)
                .map(Observation.class::cast)
                .forEach(observation ->
                    observation.getPerformer().stream().filter(performer ->
                                    performer.getReference().startsWith(String.valueOf(ResourceType.RelatedPerson)))
                            .forEach(performer ->
                                    memberIds.add(fhirUtils.getIdFromReference(observation.getPerformer().getFirst().getReference())))
                );
        return memberIds.stream().toList();
    }

    /**
     * {@inheritDoc}
     */
    public List<HouseholdDTO> getHouseholdList(RequestDTO requestDTO) {
        List<HouseholdDTO> householdList = new ArrayList<>();
        HouseholdReferenceDTO householdReference = mapGroupsToHouseholds(requestDTO, householdList);
        if (!householdList.isEmpty()) {
            mapLocationToHouseholds(householdList, householdReference);
            mapDeviceToHouseholds(householdList, householdReference);
        }
        return householdList;
    }

    /**
     * {@inheritDoc}
     */
    public List<HouseholdMemberDTO> getHouseholdMemberList(RequestDTO request) {
        List<HouseholdMemberDTO> householdMemberList = new ArrayList<>();
        Bundle bundle = getHouseholdMemberByVillages(request.getVillageIds(), request.getLastSyncTime(),
                request.getCurrentSyncTime(), null, request.getSkip(), request.getLimit());
        bundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(RelatedPerson.class::isInstance)
                .map(RelatedPerson.class::cast)
                .forEach(relatedPerson -> {
                    HouseholdMemberDTO householdMemberDTO = fhirMapper.toHouseholdMember(relatedPerson);
                    householdMemberList.add(householdMemberDTO);
                });
        return householdMemberList;
    }

    /**
     * Map group to households
     *
     * @param request       - RequestDTO
     * @param householdList - List of household
     * @return - Household Reference DTO
     */
    private HouseholdReferenceDTO mapGroupsToHouseholds(RequestDTO request, List<HouseholdDTO> householdList) {
        HouseholdReferenceDTO householdReference = new HouseholdReferenceDTO();
        Map<String, String> location = new HashMap<>();
        Map<String, String> device = new HashMap<>();
        Bundle bundle = Objects.nonNull(request.getId()) ?
                getHouseholdByIds(List.of(request.getId())) :
                getHouseholdByVillages(request.getVillageIds(),
                        request.getLastSyncTime(), request.getCurrentSyncTime(), null,
                        request.getSkip(), request.getLimit());
        bundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(Group.class::isInstance)
                .map(Group.class::cast)
                .forEach(group -> {
                    HouseholdDTO householdDTO = new HouseholdDTO();
                    fhirMapper.mapHousehold(householdDTO, group);
                    List<String> memberLocationIds = new ArrayList<>();
                    List<String> deviceIds = new ArrayList<>();
                    group.getCharacteristic().forEach(characteristicComponent -> {
                        if (characteristicComponent.getValue() instanceof Reference) {
                            String valueReference = characteristicComponent.getValueReference().getReference();
                            if (valueReference.startsWith(String.valueOf(ResourceType.Location))) {
                                memberLocationIds.add(fhirUtils.getIdFromReference(valueReference));
                            }
                        }
                    });
                    group.getMember().stream().forEach(groupMemberComponent -> {
                        String valueReference = groupMemberComponent.getEntity().getReference();
                        if (valueReference.startsWith(String.valueOf(ResourceType.Device))) {
                            deviceIds.add(fhirUtils.getIdFromReference(valueReference));
                        }
                    });
                    location.put(householdDTO.getId(), memberLocationIds.getFirst());
                    device.put(householdDTO.getId(), deviceIds.getFirst());
                    householdList.add(householdDTO);
                });
        householdReference.setLocation(location);
        householdReference.setDevice(device);
        return householdReference;
    }

    /**
     * Map members to households
     *
     * @param householdList      - List of household
     * @param householdReference - Household Reference DTO
     */
    private void mapMembersToHouseholds(List<HouseholdDTO> householdList, HouseholdReferenceDTO householdReference) {
        Map<String, RelatedPerson> relatedPersonMap = new HashMap<>();
        Map<String, List<String>> householdMembers = householdReference.getHouseholdMembers();
        List<String> householdMemberIds = new ArrayList<>();
        householdMembers.values().stream().forEach(householdMemberIds::addAll);
        Bundle memberBundle = getHouseholdMemberByIds(householdMemberIds);
        memberBundle.getEntry().stream().forEach(entry -> {
            if (ResourceType.RelatedPerson.equals(entry.getResource().getResourceType())) {
                RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
                relatedPersonMap.put(fhirUtils.getIdFromHistoryUrl(relatedPerson.getId()), relatedPerson);
            }
        });
        householdList.stream().forEach(householdDTO -> {
            List<String> relatedPersonIds = householdMembers.get(householdDTO.getId());
            List<HouseholdMemberDTO> householdMemberDTOList = new ArrayList<>();
            relatedPersonIds.stream().forEach(relatedPersonId -> {
                RelatedPerson relatedPerson = relatedPersonMap.get(relatedPersonId);
                HouseholdMemberDTO householdMemberDTO = fhirMapper.toHouseholdMember(relatedPerson);
                householdMemberDTOList.add(householdMemberDTO);
            });
            householdDTO.setHouseholdMembers(householdMemberDTOList);
        });
    }

    /**
     * Map location to households
     *
     * @param householdList      - List of household
     * @param householdReference - Household Reference DTO
     */
    private void mapLocationToHouseholds(List<HouseholdDTO> householdList, HouseholdReferenceDTO householdReference) {
        Map<String, Location> locationMap = new HashMap<>();
        Map<String, String> locations = householdReference.getLocation();
        if (!Objects.isNull(locations)) {
            List<String> locationIds = locations.values().stream().toList();
            Bundle locationBundle = getLocationByIds(locationIds);
            locationBundle.getEntry().stream().forEach(entry -> {
                Location location = (Location) entry.getResource();
                locationMap.put(fhirUtils.getIdFromHistoryUrl(location.getId()), location);
            });
            householdList.stream().forEach(householdDTO -> {
                Location location = locationMap.get(locations.get(householdDTO.getId()));
                fhirMapper.mapHousehold(householdDTO, location);
            });
        }
    }

    /**
     * Map device to households
     *
     * @param householdList      - List of household
     * @param householdReference - Household Reference DTO
     */
    private void mapDeviceToHouseholds(List<HouseholdDTO> householdList, HouseholdReferenceDTO householdReference) {
        Map<String, Device> deviceMap = new HashMap<>();
        Map<String, String> devices = householdReference.getDevice();
        List<String> deviceIds = devices.values().stream().toList();
        Bundle deviceBundle = getDeviceByIds(deviceIds);
        deviceBundle.getEntry().forEach(entry -> {
            Device device = (Device) entry.getResource();
            deviceMap.put(fhirUtils.getIdFromHistoryUrl(device.getId()), (Device) entry.getResource());
        });
        householdList.forEach(householdDTO -> {
            Device device = deviceMap.get(devices.get(householdDTO.getId()));
            fhirMapper.mapHousehold(householdDTO, device);
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdDTO getHouseholdByMemberPatientId(String patientId, List<String> includes) {
        Bundle bundle = getHouseHoldByCharacteristicText(String.format(Constants.GET_GROUP_BY_MEMBER_ID_PARAM, patientId));
        HouseholdDTO householdDTO = new HouseholdDTO();
        HouseholdReferenceDTO householdReference = new HouseholdReferenceDTO();
        bundle.getEntry().forEach(entry -> {
            if (String.valueOf(ResourceType.Group).equals(entry.getResource().getResourceType().toString())) {
                Group group = (Group) entry.getResource();
                List<String> memberIds = new ArrayList<>();
                fhirMapper.toHousehold(group, householdDTO);
                group.getCharacteristic().forEach(characteristicComponent -> {
                    if (characteristicComponent.getValue() instanceof Reference) {
                        String valueReference = characteristicComponent.getValueReference().getReference();
                        if (includes.contains(String.valueOf(ResourceType.RelatedPerson)) && valueReference.startsWith(String.valueOf(ResourceType.RelatedPerson))
                                && patientId.equals(characteristicComponent.getCode().getText())) {
                            memberIds.add(fhirUtils.getIdFromReference(valueReference));
                        } else if (includes.contains(String.valueOf(ResourceType.Location))
                                && valueReference.startsWith(String.valueOf(ResourceType.Location))) {
                            householdReference.setLocation(Map.of(householdDTO.getId(),
                                    fhirUtils.getIdFromReference(valueReference)));
                        }
                    }
                });
                Map<String, List<String>> memberMap = new HashMap<>();
                memberMap.put(householdDTO.getId(), memberIds);
                householdReference.setHouseholdMembers(memberMap);
                mapLocationToHouseholds(List.of(householdDTO), householdReference);
                mapMembersToHouseholds(List.of(householdDTO), householdReference);
            }
        });
        return householdDTO;
    }

    /**
     * {@inheritDoc}
     */
    public HouseholdDTO getHouseholdById(String id) {
        Bundle bundle = getHouseholdByIds(List.of(id));
        HouseholdDTO householdDTO = new HouseholdDTO();
        bundle.getEntry().forEach(entry -> {
            Group group = (Group) entry.getResource();
            fhirMapper.mapHousehold(householdDTO, group);

        });
        return householdDTO;
    }

    /**
     * Get Household by url
     *
     * @param url url to get resources
     * @return - Bundle Object
     */
    private Bundle getHouseHoldByCharacteristicText(String url) {
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * {@inheritDoc}
     */
    public RelatedPerson getRelatedPersonByPatientId(String patientId) {
        RelatedPerson relatedPerson = null;
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.GET_HOUSEHOLD_MEMBER_PARAMS, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, patientId));
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            relatedPerson = (RelatedPerson) entry.getResource();
        }
        return relatedPerson;
    }

    /**
     * {@inheritDoc}
     */
    public int getRelatedPersonCountByHouseholdId(String householdId) {
        Bundle bundle = restApiUtil.getBatchRequest(StringUtil.concatString(String.format(Constants.GET_HOUSEHOLD_MEMBER_PARAMS,
                FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL, householdId), Constants.PATIENT_ACTIVE_STATUS));
        return bundle.getTotal();
    }

    /**
     * {@inheritDoc}
     */
    public HouseholdMemberDTO updateSignature(RequestDTO request) {
        HouseholdMemberDTO member = getHouseholdMemberById(request.getMemberId());
        member.setSignature(request.getSignature());
        member.setProvenance(request.getProvenance());
        return updateHouseholdMember(member);
    }
}
