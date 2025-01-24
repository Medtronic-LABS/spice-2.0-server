package com.mdtlabs.coreplatform.fhirmapper.user.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

/**
 * <p>
 * A Class that implements methods for creating users and organizations in a FHIR
 * server.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Mar 04, 2024
 */
@Service
public class UserServiceImpl implements UserService {

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    @Autowired
    public UserServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
    }

    /**
     * {@inheritDoc}
     */
    public UserRequestDTO createUser(UserRequestDTO userDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String uuid = fhirUtils.getUniqueId();

        Practitioner practitioner = new Practitioner();
        practitioner.setActive(Boolean.TRUE);

        String fullUrl = Constants.FHIR_BASE_URL.concat(uuid);
        String url =
                String.valueOf(ResourceType.Practitioner).concat(Constants.FORWARD_SLASH)
                        .concat(Constants.FHIR_BASE_URL)
                        .concat(uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, mapPractitioner(practitioner, userDTO), bundle);
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));

        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        userDTO.setFhirId(response.get(String.valueOf(ResourceType.Practitioner)).get(Constants.ZERO));

        return userDTO;
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacilityRequestDTO createOrganization(HealthFacilityRequestDTO healthFacilityRequestDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String uuid = fhirUtils.getUniqueId();

        Organization organization = new Organization();
        organization.setActive(Boolean.TRUE);
        organization.addType().addCoding().setCode(Constants.CLINIC).setSystem(Constants.LOINC_URL);

        String fullUrl = Constants.FHIR_BASE_URL.concat(uuid);
        String url =
                String.valueOf(ResourceType.Organization).concat(Constants.FORWARD_SLASH)
                        .concat(Constants.FHIR_BASE_URL)
                        .concat(uuid);
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, mapOrganization(organization, healthFacilityRequestDTO), bundle);
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));

        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        healthFacilityRequestDTO.setFhirId(response.get(String.valueOf(ResourceType.Organization)).get(Constants.ZERO));

        List<UserRequestDTO> userRequestDTOList = new ArrayList<>();
        for (UserRequestDTO userRequestDTO : healthFacilityRequestDTO.getUsers()) {
            userRequestDTOList.add(createUser(userRequestDTO));
        }
        healthFacilityRequestDTO.setUsers(userRequestDTOList);
        return healthFacilityRequestDTO;
    }

    /**
     * Retrieve a Practitioner object from a FHIR server by its ID.
     * It creates a FHIR client, performs a search operation on the Practitioner resource,
     * and returns the first Practitioner object that matches the given ID and is active.
     *
     * @param userId The ID of the Practitioner to be retrieved.
     * @return The Practitioner object that matches the given ID and is active.
     */
    public Practitioner getUserById(String userId) {
        Practitioner practitioner = null;
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.PRACTIIONER_PARAMS, userId));
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            practitioner = (Practitioner) entry.getResource();
        }
        return practitioner;
    }

    /**
     * Retrieve an Organization object from a FHIR server by its ID.
     * It creates a FHIR client, performs a search operation on the Organization resource,
     * and returns the first Organization object that matches the given ID and is active.
     *
     * @param organizationId The ID of the Organization to be retrieved.
     * @return The Organization object that matches the given ID and is active.
     */
    private Organization getOrganizationById(String organizationId) {
        Organization organization = null;
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.ORGANIZATION_PARAMS, organizationId));
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            organization = (Organization) entry.getResource();
        }
        return organization;
    }

    /**
     * {@inheritDoc}
     */
    public UserRequestDTO updateUser(UserRequestDTO userRequestDTO) {
        Practitioner practitioner = getUserById(userRequestDTO.getFhirId());
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url =
                String.valueOf(ResourceType.Practitioner).concat(Constants.FORWARD_SLASH)
                        .concat(fhirUtils.getIdFromHistoryUrl(practitioner.getId()));
        fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, mapPractitioner(practitioner, userRequestDTO), bundle);
        return userRequestDTO;
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacilityRequestDTO updateOrganization(HealthFacilityRequestDTO healthFacilityRequestDTO) {
        Organization organization = getOrganizationById(healthFacilityRequestDTO.getFhirId());
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getId())) {
            String url = String.valueOf(ResourceType.Organization).concat(Constants.FORWARD_SLASH)
                    .concat(fhirUtils.getIdFromHistoryUrl(organization.getId()));
            fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, mapOrganization(organization, healthFacilityRequestDTO), bundle);
        }
        return healthFacilityRequestDTO;
    }

    /**
     * {@inheritDoc}
     */
    public void deleteUser(String userId) {
        Practitioner practitioner = getUserById(userId);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url =
                String.valueOf(ResourceType.Practitioner).concat(Constants.FORWARD_SLASH)
                        .concat(practitioner.getIdPart());
        practitioner.setActive(Boolean.FALSE);
        fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, practitioner, bundle);
        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * {@inheritDoc}
     */
    public void deleteOrganization(String organizationId) {
        Organization organization = getOrganizationById(organizationId);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (Objects.nonNull(organization) && Objects.nonNull(organization.getId())) {
            String url = String.valueOf(ResourceType.Organization).concat(Constants.FORWARD_SLASH)
                    .concat(fhirUtils.getIdFromHistoryUrl(organization.getId()));
            organization.setActive(Boolean.FALSE);
            fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, organization, bundle);
        }
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * Map the details from a UserRequestDTO object to a Practitioner object.
     * It sets the name, gender, phone number, and email of the practitioner.
     *
     * @param practitioner The Practitioner object to which the details are to be mapped.
     * @param userDTO      The UserRequestDTO object from which the details are to be extracted.
     * @return The updated Practitioner object.
     */
    private Practitioner mapPractitioner(Practitioner practitioner, UserRequestDTO userDTO) {
        HumanName name = practitioner.getName().isEmpty() ? new HumanName() : practitioner.getNameFirstRep();
        if (name.hasGiven()) {
            name.getGiven().clear();
        }
        name.addGiven(userDTO.getFirstName());
        name.setFamily(userDTO.getLastName());
        name.setText(userDTO.getFirstName().concat(Constants.EMPTY_SPACE).concat(userDTO.getLastName()));
        if (practitioner.getName().isEmpty()) {
            practitioner.addName(name);
        }

        if (Objects.nonNull(userDTO.getGender())) {
            Enumerations.AdministrativeGender gender = Enumerations.AdministrativeGender.UNKNOWN;
            if (userDTO.getGender().equalsIgnoreCase(Constants.MALE)) {
                gender = Enumerations.AdministrativeGender.MALE;
            } else if (userDTO.getGender().equalsIgnoreCase(Constants.FEMALE)) {
                gender = Enumerations.AdministrativeGender.FEMALE;
            } else if (userDTO.getGender().equalsIgnoreCase(Constants.NON_BINARY)) {
                gender = Enumerations.AdministrativeGender.OTHER;
            }
            practitioner.setGender(gender);
        }

        ContactPoint phone = practitioner.getTelecom()
                .isEmpty() ? new ContactPoint() : practitioner.getTelecomFirstRep();
        phone.setSystem(ContactPoint.ContactPointSystem.PHONE);
        phone.setValue(userDTO.getPhoneNumber());
        if (practitioner.getTelecom().isEmpty()) {
            practitioner.addTelecom(phone);
        }

        ContactPoint email = practitioner.getTelecom().size() < 2 ? new ContactPoint() : practitioner.getTelecom()
                .get(1);
        email.setSystem(ContactPoint.ContactPointSystem.EMAIL);
        email.setValue(userDTO.getUsername());
        if (practitioner.getTelecom().size() < 2) {
            practitioner.addTelecom(email);
        }

        return practitioner;
    }

    /**
     * Map the details from a HealthFacilityRequestDTO object to an Organization object.
     * It sets the name of the organization, and creates or updates the telecom and address details of the organization.
     *
     * @param organization             The Organization object to which the details are to be mapped.
     * @param healthFacilityRequestDTO The HealthFacilityRequestDTO object from which the details are to be extracted.
     * @return The updated Organization object.
     */
    private Organization mapOrganization(Organization organization, HealthFacilityRequestDTO healthFacilityRequestDTO) {
        organization.setName(healthFacilityRequestDTO.getName());
        ContactPoint phone = organization.getTelecom()
                .isEmpty() ? new ContactPoint() : organization.getTelecomFirstRep();
        phone.setValue(healthFacilityRequestDTO.getPhuFocalPersonNumber());
        phone.setSystem(ContactPoint.ContactPointSystem.PHONE);
        phone.setUse(ContactPoint.ContactPointUse.WORK);
        if (organization.getTelecom().isEmpty()) {
            organization.addTelecom(phone);
        }

        Address address = organization.getAddress().isEmpty() ? new Address() : organization.getAddressFirstRep();
        address.setDistrict(healthFacilityRequestDTO.getDistrict().getName());
        address.setCountry(healthFacilityRequestDTO.getCountry().getName());
        address.setPostalCode(healthFacilityRequestDTO.getPostalCode());
        address.setUse(Address.AddressUse.WORK);
        address.setType(Address.AddressType.BOTH);
        address.setText(healthFacilityRequestDTO.getAddress());
        if (organization.getAddress().isEmpty()) {
            organization.addAddress(address);
        }
        return organization;
    }

    /**
     * {@inheritDoc}
     */
    public void deleteUsers(List<String> userIds) {
        Bundle responseBundle = restApiUtil.getBatchRequest(String.format(Constants.PRACTIIONER_PARAMS,
                String.join(Constants.COMMA, userIds)));
        Bundle requestBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (!Objects.isNull(responseBundle) && !responseBundle.getEntry().isEmpty()) {
            responseBundle.getEntry().forEach(resource -> {
                Practitioner practitioner = (Practitioner) resource.getResource();
                practitioner.setActive(Boolean.FALSE);
                String url =
                        String.valueOf(ResourceType.Practitioner).concat(Constants.FORWARD_SLASH)
                                .concat(practitioner.getIdPart());
                fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, practitioner, requestBundle);
            });
        }

        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(requestBundle));
    }

    /**
     * {@inheritDoc}
     */
    public void activateDeactivateUsers(List<String> userIds, Boolean isActivate) {
        Bundle responseBundle = restApiUtil.getBatchRequest(String.format(Constants.PRACTIIONER_PARAMS,
                String.join(Constants.COMMA, userIds)));
        Bundle requestBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (!Objects.isNull(responseBundle) && !responseBundle.getEntry().isEmpty()) {
            responseBundle.getEntry().forEach(resource -> {
                Practitioner practitioner = (Practitioner) resource.getResource();
                practitioner.setActive(isActivate);
                String url =
                        String.valueOf(ResourceType.Practitioner).concat(Constants.FORWARD_SLASH)
                                .concat(practitioner.getIdPart());
                fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, practitioner, requestBundle);
            });
        }

        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(requestBundle));
    }

    /**
     * {@inheritDoc}
     */
    public void activateDeactivateOrganizations(List<String> orgIds, Boolean isActivate) {
        Bundle responseBundle = restApiUtil.getBatchRequest(String.format(Constants.ORGANIZATION_PARAMS,
                String.join(Constants.COMMA, orgIds)));
        Bundle requestBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        if (!Objects.isNull(responseBundle) && !responseBundle.getEntry().isEmpty()) {
            responseBundle.getEntry().forEach(resource -> {
                Organization organization = (Organization) resource.getResource();
                organization.setActive(isActivate);
                String url =
                        String.valueOf(ResourceType.Organization).concat(Constants.FORWARD_SLASH)
                                .concat(organization.getIdPart());
                fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, organization, requestBundle);
            });
        }

        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(requestBundle));
    }

}
