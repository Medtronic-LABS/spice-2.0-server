package com.mdtlabs.coreplatform.fhirmapper.mapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import jakarta.validation.constraints.NotNull;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.DomainResource;
import org.hl7.fhir.r4.model.Dosage;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.Group.GroupCharacteristicComponent;
import org.hl7.fhir.r4.model.Group.GroupType;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 * mapper file is utilized to convert requests into FHIR resources.
 * </p>
 *
 * @author Nandhakumar created on Jan 24, 2024
 */
@Component
public class FhirMapper {

    private final FhirUtils fhirUtils;

    public FhirMapper(FhirUtils fhirUtils) {
        this.fhirUtils = fhirUtils;
    }

    /**
     * This method map householdRequestDTO to FHIR group.
     *
     * @param householdDTO households Details
     * @param group        FHIR group object
     * @return Group FHIR group object
     */
    public Group setGroup(HouseholdDTO householdDTO, Group group) {
        group.setActive(Boolean.TRUE);
        group.setName(householdDTO.getName());
        group.setType(GroupType.PERSON);
        group.setActual(Boolean.TRUE);
        group.setQuantity(householdDTO.getNoOfPeople());
        if (group.getIdentifier().isEmpty()) {
            group.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)
                    .setValue(householdDTO.getHouseholdNo().toString());
            group.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                    .setValue(householdDTO.getVillageId());
            group.addIdentifier().setSystem(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL)
                    .setValue(String.valueOf(new Date().getTime()));
            setGroupCharacteristicComponent(householdDTO, group);
        } else {
            if (group.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL))) {
                group.addIdentifier().setSystem(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL).setValue(String.valueOf(new Date().getTime()));
            }
            group.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)) {
                    identifier.setValue(householdDTO.getHouseholdNo().toString());
                }
                if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    identifier.setValue(householdDTO.getVillageId());
                }
                if (identifier.getSystem().equals(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL)) {
                    identifier.setValue(String.valueOf(new Date().getTime()));
                }
            });
            group.getCharacteristic().forEach(characteristicComponent -> {
                if (characteristicComponent.getValue() instanceof CodeableConcept || characteristicComponent.getValue() instanceof Quantity) {
                    if (characteristicComponent.getCode().getText()
                            .equals(Constants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP)) {
                        characteristicComponent.getValueCodeableConcept()
                                .setCoding(List.of(getCoding(householdDTO.isOwnedHandWashingFacilityWithSoap())));
                        characteristicComponent.getValueCodeableConcept().setText(householdDTO.isOwnedHandWashingFacilityWithSoap() ? Constants.YES : Constants.NO);
                    } else if (characteristicComponent.getCode().getText()
                            .equals(Constants.OWNED_AN_IMPROVED_LATRINE)) {
                        characteristicComponent.getValueCodeableConcept()
                                .setCoding(List.of(getCoding(householdDTO.isOwnedAnImprovedLatrine())));
                        characteristicComponent.getValueCodeableConcept().setText(householdDTO.isOwnedAnImprovedLatrine() ? Constants.YES : Constants.NO);
                    } else if (characteristicComponent.getCode().getText()
                            .equals(Constants.OWNED_TREATED_BED_NET)) {
                        characteristicComponent.getValueCodeableConcept()
                                .setCoding(List.of(getCoding(householdDTO.isOwnedTreatedBedNet())));
                        characteristicComponent.getValueCodeableConcept().setText(householdDTO.isOwnedTreatedBedNet() ? Constants.YES : Constants.NO);
                    }
                }
            });
            Optional<GroupCharacteristicComponent> bedNetComponentOpt = group.getCharacteristic().stream()
                    .filter(characteristicComponent -> characteristicComponent.getValue() instanceof Quantity &&
                            characteristicComponent.getCode().getText().equals(Constants.BED_NET_COUNT))
                    .findFirst();
            if (Objects.nonNull(householdDTO.getBedNetCount())) {
                bedNetComponentOpt.orElseGet(() -> {
                    GroupCharacteristicComponent groupComponent = getGroupCharacteristicComponent(Constants.BED_NET_COUNT, householdDTO.getBedNetCount());
                    group.getCharacteristic().add(groupComponent);
                    return groupComponent;
                }).getValueQuantity().setValue(householdDTO.getBedNetCount());
            } else {
                bedNetComponentOpt.ifPresent(group.getCharacteristic()::remove);
            }
        }
        return group;
    }

    /**
     * Returns a new instance of the Coding class with the system URL set to
     * `EXAMPLE_SYSTEM_URL`, the code and display values set based on the `value` parameter
     * (either `YES_CODE` and `YES` if `value` is true, or `NO_CODE` and
     * `NO` if `value` is false).
     *
     * @param value The `value` parameter is a boolean value that determines which code and display value
     *              to use when creating a new `Coding` object. If `value` is `true`, the code and display value will be
     *              set to YES_CODE and YES respectively. If `value` is `false`,
     * @return `Coding` Object
     */
    private Coding getCoding(boolean value) {
        return new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,
                value ? Constants.YES_CODE : Constants.NO_CODE,
                value ? Constants.YES : Constants.NO);
    }

    /**
     * Sets group characteristics based on householdDTO values.
     *
     * @param householdDTO The `householdDTO` parameter is an object of type `HouseholdDTO`, which contains
     *                     information about a household. It has properties such as `ownedHandWashingFacilityWithSoap`,
     *                     `ownedAnImprovedLatrine`, `ownedTreatedBedNet`, and `bedNetCount`.
     * @param group        The `group` parameter is an object of type `Group`, which likely represents a group of
     *                     entities or objects with certain characteristics. In the `setGroupCharacteristicComponent` method,
     *                     we are adding characteristic components to this group based on the values retrieved from the
     *                     `householdDTO` object.
     */
    private void setGroupCharacteristicComponent(HouseholdDTO householdDTO, Group group) {
        group.getCharacteristic()
                .add(getGroupCharacteristicComponent(Constants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP,
                        householdDTO.isOwnedHandWashingFacilityWithSoap()));
        group.getCharacteristic()
                .add(getGroupCharacteristicComponent(Constants.OWNED_AN_IMPROVED_LATRINE,
                        householdDTO.isOwnedAnImprovedLatrine()));
        group.getCharacteristic()
                .add(getGroupCharacteristicComponent(Constants.OWNED_TREATED_BED_NET,
                        householdDTO.isOwnedTreatedBedNet()));
        if (Objects.nonNull(householdDTO.getBedNetCount())) {
            group.getCharacteristic()
                    .add(getGroupCharacteristicComponent(Constants.BED_NET_COUNT,
                            householdDTO.getBedNetCount()));
        }
    }

    /**
     * Set Location Object using household location and village
     *
     * @param location     FHIR Location Object
     * @param householdDTO Household DTO
     * @return Location Object
     */
    public Location setLocation(Location location, HouseholdDTO householdDTO) {
        location.setName(householdDTO.getName());
        Address address = new Address();
        address.setCity(householdDTO.getVillage());
        address.setText(householdDTO.getLandmark());
        location.getPosition().setLatitude(householdDTO.getLatitude());
        location.getPosition().setLongitude(householdDTO.getLongitude());
        location.setAddress(address);
        return location;
    }

    /**
     * This method map HouseholdMemberDTO to FHIR RelatedPerson.
     *
     * @param member        member Details
     * @param relatedPerson FHIR group object
     * @return RelatedPerson FHIR group object
     */
    public RelatedPerson setRelatedPerson(HouseholdMemberDTO member, RelatedPerson relatedPerson) {

        if (Objects.nonNull(member.getName()) || Objects.nonNull(member.getInitial())) {
            HumanName name = new HumanName();
            name.setText(member.getName());
            name.setPrefix(List.of(new StringType(member.getInitial())));
            relatedPerson.setName(List.of(name));
        }

        if (Objects.nonNull(member.getSignature())) {
            Attachment attachment = new Attachment();
            attachment.setUrl(member.getSignature());
            relatedPerson.setPhoto(List.of(attachment));
        }

        if (!Objects.isNull(member.getPatientReference())) {
            relatedPerson.setPatient(new Reference(member.getPatientReference()));
        }

        relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(
                !Objects.isNull(member.getGender()) ? member.getGender().toLowerCase() : relatedPerson.getGender()
                        .toString()
                        .toLowerCase()));
        relatedPerson.setActive(Objects.isNull(member.getIsActive()) || member.getIsActive());
        relatedPerson.setBirthDate(!Objects.isNull(member.getDateOfBirth())
                ? member.getDateOfBirth() : relatedPerson.getBirthDate());

        if (relatedPerson.getTelecom().isEmpty()) {
            //Add mobileNumber
            ContactPoint contactPoint = new ContactPoint();
            contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
            contactPoint.setValue(member.getPhoneNumber());
            //Add Phone number Category
            contactPoint.setUse(getContactPointUse(member.getPhoneNumberCategory().toLowerCase()));
            relatedPerson.addTelecom(contactPoint);
        } else {
            relatedPerson.getTelecom().getFirst().setValue(member.getPhoneNumber());
            relatedPerson.getTelecom()
                    .getFirst()
                    .setUse(getContactPointUse(member.getPhoneNumberCategory().toLowerCase()));
        }

        if (relatedPerson.getIdentifier().isEmpty()) {
            relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)
                    .setValue(member.getPatientId());
            relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                    .setValue(member.getVillageId());
            relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL)
                    .setValue(String.valueOf(new Date().getTime()));
            if (Boolean.TRUE.equals(member.getIsChild())
                    && Objects.nonNull(member.getMotherPatientId())) {
                relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.MOTHER_PATIENT_ID_SYSTEM_URL)
                        .setValue(member.getMotherPatientId());
            }
        } else {
            if (relatedPerson.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL))) {
                relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL).setValue(String.valueOf(new Date().getTime()));
            }
            relatedPerson.getIdentifier().stream().forEach(identifier -> {
                if (identifier.getSystem()
                        .equals(FhirIdentifierConstants.MODIFIED_DATE_SYSTEM_URL)) {
                    identifier.setValue(String.valueOf(new Date().getTime()));
                }
            });
        }

        if (Objects.nonNull(member.getHouseholdId()) && (relatedPerson.getIdentifier().stream().noneMatch(i -> i.getSystem().equals(FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL)))) {
            relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL)
                    .setValue(member.getHouseholdId());
        }

        if (!Objects.isNull(member.getHouseholdHeadRelationship())) {
            // Set Codable Concept For Relationship
            CodeableConcept codeableConceptRelation = new CodeableConcept();
            Coding codingRelation = new Coding();
            codingRelation.setSystem(Constants.FHIR_CODE_SYSTEM_V3_ROLE_CODE);
            codingRelation.setCode(member.getHouseholdHeadRelationship());
            codeableConceptRelation.addCoding(codingRelation);
            codeableConceptRelation.setText(member.getHouseholdHeadRelationship());
            if (relatedPerson.getRelationship().isEmpty()) {
                relatedPerson.addRelationship(codeableConceptRelation);
            } else {
                relatedPerson.setRelationship(List.of(codeableConceptRelation));
            }
        }

        if (!Objects.isNull(member.getVillage())) {
            relatedPerson.setAddress(List.of(new Address().setCity(member.getVillage())));
        }

        setPregnancyStatus(member.getIsPregnant(), relatedPerson);

        return relatedPerson;
    }

    /**
     * Maps a given phone number category to a corresponding
     * ContactPointUse enum value.
     *
     * @param phoneNumberCategory The `phoneNumberCategory` parameter is a string that represents the
     *                            category of a phone number. It can have values like "PERSONAL", "FAMILY_MEMBER", or "FRIEND".
     * @return Returns the ContactPointUse enum value associated with the given phoneNumberCategory.
     */
    private ContactPoint.ContactPointUse getContactPointUse(String phoneNumberCategory) {
        Map<String, ContactPoint.ContactPointUse> map = new HashMap<>();
        map.put(Constants.PERSONAL, ContactPoint.ContactPointUse.MOBILE);
        map.put(Constants.FAMILY_MEMBER, ContactPoint.ContactPointUse.HOME);
        map.put(Constants.FRIEND, ContactPoint.ContactPointUse.TEMP);
        return map.get(phoneNumberCategory);
    }

    /**
     * Maps a ContactPointUse enum to corresponding category strings using a HashMap.
     *
     * @param contactPointUse ContactPoint.ContactPointUse is an enum representing the possible uses of a
     *                        contact point, such as MOBILE, HOME, or TEMP. The method getPhoneNumberCategory takes a
     *                        ContactPoint.ContactPointUse as input and returns a category based on the mapping defined in the
     *                        method.
     * @return Returns the category associated with the given ContactPointUse enum value.
     */
    private String getPhoneNumberCategory(ContactPoint.ContactPointUse contactPointUse) {
        Map<ContactPoint.ContactPointUse, String> map = new EnumMap<>(ContactPoint.ContactPointUse.class);
        map.put(ContactPoint.ContactPointUse.MOBILE, Constants.PERSONAL);
        map.put(ContactPoint.ContactPointUse.HOME, Constants.FAMILY_MEMBER);
        map.put(ContactPoint.ContactPointUse.TEMP, Constants.FRIEND);
        return map.get(contactPointUse);
    }

    /**
     * Set the pregnancy status of a Patient or a RelatedPerson resource.
     * It checks if the resource already has a pregnancy status extension. If it does, it updates the value.
     * If it doesn't, it creates a new extension and adds it to the resource.
     *
     * @param status   The pregnancy status to be set. It is a Boolean value.
     * @param resource The FHIR resource (RelatedPerson) to which the pregnancy status is to be set.
     */
    public void setPregnancyStatus(Boolean status, RelatedPerson resource) {
        boolean hasPregnantExtension = false;
        String value = fhirUtils.convertPregnancyType(status);
        for (Extension extension : resource.getExtension()) {
            if (FhirIdentifierConstants.IS_PREGNANT_EXTENSION_URL.equals(extension.getUrl())) {
                extension.setValue(new StringType(value));
                hasPregnantExtension = true;
            }
        }
        if (!hasPregnantExtension) {
            Extension extension = createIsPregnantExtension();
            extension.setValue(new StringType(value));
            resource.addExtension(extension);
        }
    }

    /**
     * Converts pregnancy type String to Boolean.
     *
     * @param value The pregnancy value to be converted. It is a String value.
     * @return Returns the Boolean value based on the pregnancy vale.
     */
    private Boolean convertPregnancyType(String value) {
        if (Constants.YES.equals(value)) {
            return Boolean.TRUE;
        }
        return Constants.NO.equals(value) ? Boolean.FALSE : null;
    }

    /**
     * Create a new FHIR extension for indicating pregnancy status.
     *
     * @return The newly created FHIR extension.
     */
    private Extension createIsPregnantExtension() {
        Extension extension = new Extension();
        extension.setUrl(FhirIdentifierConstants.IS_PREGNANT_EXTENSION_URL);
        return extension;
    }

    /**
     * This method is to set GroupCharacteristicComponent and relatedPerson person reference
     *
     * @param member Household and List of householdMembers
     * @param url    Related Person Reference
     */
    public void setGroupCharacteristicComponent(List<GroupCharacteristicComponent> groupList,
                                                String url,
                                                HouseholdMemberDTO member) {
        List<String> idList = new ArrayList<>();
        groupList.forEach(group -> idList.add(group.getCode().getText()));
        if (!Objects.isNull(member.getPatientId()) && !idList.contains(member.getPatientId())) {
            GroupCharacteristicComponent groupCharacteristicComponent = new GroupCharacteristicComponent();
            CodeableConcept codeableConcept = new CodeableConcept();
            Coding coding = new Coding();
            coding.setSystem(FhirIdentifierConstants.RELATIONSHIP_SYSTEM_URL);
            coding.setCode(Constants.ATTRIBUTED_TO);
            codeableConcept.setText(member.getPatientId());
            codeableConcept.addCoding(coding);
            groupCharacteristicComponent.setCode(codeableConcept);

            Reference reference = new Reference();
            reference.setReference(url);

            // set relatedPerson reference to group
            groupCharacteristicComponent.setValue(reference);
            groupList.add(groupCharacteristicComponent);
        }
    }

    /**
     * Creates and returns a GroupCharacteristicComponent object with a specified key and boolean value.
     *
     * @param key   Passed as a String and is used to retrieve the corresponding code using the `fhirUtils.setCodes`
     *              method.
     * @param value Boolean value that determines whether to set the coding to `YES_CODE` and `YES` if
     *              `value` is `true`, or to `NO_CODE` and `NO` if `value` is false
     * @return An instance of the GroupCharacteristicComponent class is being returned.
     */
    private GroupCharacteristicComponent getGroupCharacteristicComponent(String key, Boolean value) {
        GroupCharacteristicComponent groupCharacteristicComponent = new GroupCharacteristicComponent();
        groupCharacteristicComponent.setCode(fhirUtils.setCodes(key));
        groupCharacteristicComponent.getValueCodeableConcept()
                .addCoding(new Coding(FhirIdentifierConstants.FHIR_YES_NO_CODE,
                        Boolean.TRUE.equals(value) ? Constants.YES_CODE : Constants.NO_CODE,
                        Boolean.TRUE.equals(value) ? Constants.YES : Constants.NO));
        groupCharacteristicComponent.getValueCodeableConcept().setText(Boolean.FALSE.equals(value) ? Constants.NO : Constants.YES);
        return groupCharacteristicComponent;
    }

    /**
     * Creates and returns a `GroupCharacteristicComponent` object with a code set using a key and a value set as a Quantity.
     *
     * @param key   A string value that is passed to the `fhirUtils.setCodes` method to retrieve the appropriate code
     *              for the component.
     * @param value An Integer value that represents the value to be set in the `GroupCharacteristicComponent` object.
     * @return An instance of the GroupCharacteristicComponent class is being returned.
     */
    private GroupCharacteristicComponent getGroupCharacteristicComponent(String key, Integer value) {
        GroupCharacteristicComponent groupCharacteristicComponent = new GroupCharacteristicComponent();
        groupCharacteristicComponent.setCode(fhirUtils.setCodes(key));
        groupCharacteristicComponent.setValue(new Quantity(value));
        return groupCharacteristicComponent;
    }

    /**
     * This method creates a Device.
     *
     * @param householdRequestDTO List of household
     * @param device              bundle to add entry values
     */
    public Device setDeviceEntity(HouseholdDTO householdRequestDTO, Device device) {
        if (device.getIdentifier().isEmpty()) {
            device.addIdentifier().setSystem(FhirIdentifierConstants.PHONE_TYPE_SYSTEM_URL)
                    .setValue(householdRequestDTO.getHeadPhoneNumber());
            device.addIdentifier().setSystem(FhirIdentifierConstants.PHONE_CATEGORY_SYSTEM_URL)
                    .setValue(getContactPointUse(householdRequestDTO.getHeadPhoneNumberCategory().toLowerCase()).toString().toLowerCase());
        } else {
            device.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PHONE_TYPE_SYSTEM_URL)) {
                    identifier.setValue(householdRequestDTO.getHeadPhoneNumber());
                }
                if (identifier.getSystem().equals(FhirIdentifierConstants.PHONE_CATEGORY_SYSTEM_URL)) {
                    identifier.setValue(getContactPointUse(householdRequestDTO.getHeadPhoneNumberCategory().toLowerCase()).toString().toLowerCase());
                }
            });
        }
        return device;
    }

    /**
     * bundle
     * Map group FHIR Object to Household Object
     *
     * @param group Fhir bundle
     * @return Household object
     */
    public HouseholdDTO toHousehold(Group group, HouseholdDTO householdDTO) {
        householdDTO.setId(fhirUtils.getIdFromHistoryUrl(group.getId()));
        householdDTO.setName(group.getName());
        group.getIdentifier().forEach(identifier -> {
            if (FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL.equals(identifier.getSystem())) {
                householdDTO.setHouseholdNo(Long.valueOf(identifier.getValue()));
            }
        });
        householdDTO.setNoOfPeople(group.getQuantity());
        return householdDTO;
    }

    /**
     * bundle
     * Map group FHIR Object to Patient Object
     *
     * @param patient    Fhir bundle
     * @param patientDTO details
     * @return Patient object
     */
    public PatientDTO toPatient(Patient patient, PatientDTO patientDTO) {
        if (null != patient.getName() && null != patient.getName().getFirst()
                && null != patient.getName().getFirst().getText()) {
            patientDTO.setName(patient.getName().getFirst().getText());
        }
        patientDTO.setIsActive(patient.getActive());
        if (!StringUtils.isBlank(patient.getId())) {
            patientDTO.setId(fhirUtils.getIdFromHistoryUrl(patient.getId()));
        }
        if (null != patient.getGender()) {
            patientDTO.setGender(patient.getGender().toString());
        }
        if (null != patient.getTelecom() && null != patient.getTelecom().getFirst()) {
            patientDTO.setPhoneNumber(patient.getTelecom().getFirst().getValue());
            patientDTO.setPhoneNumberCategory(getPhoneNumberCategory(patient.getTelecom().getFirst().getUse()));
        }
        if (null != patient.getBirthDate()) {
            patientDTO.setBirthDate(patient.getBirthDate());
        }
        if (null != patient.getIdentifier()) {
            patient.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)) {
                    patientDTO.setPatientId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    patientDTO.setVillageId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.MOTHER_PATIENT_ID_SYSTEM_URL)) {
                    patientDTO.setMotherPatientId(identifier.getValue());
                } else if (identifier.getSystem().contains(Constants.SP_NATIONAL_ID)) {
                    patientDTO.setNationalId(identifier.getValue());
                }
            });
        }
        if (null != patient.getAddress() && null != patient.getAddress().getFirst()
                && null != patient.getAddress().getFirst().getCity()) {
            patientDTO.setVillage(patient.getAddress().getFirst().getCity());
        }

        if (!CollectionUtils.isEmpty(patient.getLink())) {
            patient.getLink().forEach(link -> {
                if (Objects.nonNull(link.getOther()) && StringUtils.isNotBlank(link.getOther().getReference()) &&
                        link.getOther().getReference().contains(ResourceType.RelatedPerson.name())) {
                    patientDTO.setMemberReference(link.getOther().getReference().split("/")[1]);
                }
            });
        }
        setPregnancyExtension(patient, patientDTO);
        return patientDTO;
    }

    /**
     * Set the pregnancy extension from a given FHIR resource (Patient or RelatedPerson)
     * to a given object (PatientDTO or HouseholdMemberDTO).
     *
     * @param resource The FHIR resource (Patient or RelatedPerson) from which to retrieve the extension.
     * @param object   The object (PatientDTO or HouseholdMemberDTO) to which to set the isPregnant property.
     */
    private void setPregnancyExtension(DomainResource resource, Object object) {
        if (resource instanceof Patient || resource instanceof RelatedPerson && (Objects.nonNull(resource.getExtension()) && !resource.getExtension().isEmpty())) {
            resource.getExtension().forEach(extension -> {
                if (extension.getUrl().equals(FhirIdentifierConstants.IS_PREGNANT_EXTENSION_URL)) {
                    if (object instanceof PatientDTO patientDTO) {
                        patientDTO.setIsPregnant(convertPregnancyType(((StringType) extension.getValue()).getValue()));
                    } else {
                        ((HouseholdMemberDTO) object).setIsPregnant(convertPregnancyType(((StringType) extension.getValue()).getValue()));
                    }
                }
            });
        }
    }

    /**
     * bundle
     * Map group FHIR Object to Patient Object
     *
     * @param relatedPerson relatedPerson bundle
     * @param patientDTO    details object
     * @return Patient object
     */
    public PatientDTO relatedPersonToPatient(RelatedPerson relatedPerson, PatientDTO patientDTO) {
        if (null != relatedPerson.getName() && null != relatedPerson.getName().getFirst()
                && null != relatedPerson.getName().getFirst().getText()) {
            patientDTO.setName(relatedPerson.getName().getFirst().getText());
        }
        patientDTO.setIsActive(relatedPerson.getActive());
        if (!StringUtils.isBlank(relatedPerson.getId())) {
            patientDTO.setId(fhirUtils.getIdFromHistoryUrl(relatedPerson.getId()));
        }
        if (null != relatedPerson.getGender()) {
            patientDTO.setGender(relatedPerson.getGender().toString());
        }
        if (null != relatedPerson.getTelecom() && null != relatedPerson.getTelecom().getFirst()) {
            patientDTO.setPhoneNumber(relatedPerson.getTelecom().getFirst().getValue());
        }
        if (null != relatedPerson.getBirthDate()) {
            patientDTO.setBirthDate(relatedPerson.getBirthDate());
        }
        if (null != relatedPerson.getIdentifier()) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)) {
                    patientDTO.setPatientId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    patientDTO.setVillageId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL)) {
                    patientDTO.setHouseholdId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.MOTHER_PATIENT_ID_SYSTEM_URL)) {
                    patientDTO.setMotherPatientId(identifier.getValue());
                }
            });
        }
        if (null != relatedPerson.getAddress() && null != relatedPerson.getAddress().getFirst()
                && null != relatedPerson.getAddress().getFirst().getCity()) {
            patientDTO.setVillage(relatedPerson.getAddress().getFirst().getCity());
        }
        if (!Objects.isNull(relatedPerson.getTelecom()) && !relatedPerson.getTelecom().isEmpty()) {
            patientDTO.setPhoneNumberCategory(getPhoneNumberCategory(relatedPerson.getTelecom().getFirst().getUse()));
        }

        setPregnancyExtension(relatedPerson, patientDTO);
        return patientDTO;
    }

    /**
     * Map RelatedPerson to household member
     *
     * @param relatedPerson -  Related person
     * @return - HouseholdMemberDTO
     */
    public HouseholdMemberDTO toHouseholdMember(RelatedPerson relatedPerson) {
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        householdMemberDTO.setIsActive(relatedPerson.getActive());
        HumanName name = relatedPerson.getName().getFirst();
        householdMemberDTO.setName(Objects.nonNull(name) ? name.getText() : null);
        householdMemberDTO.setInitial(Objects.nonNull(name) && Objects.nonNull(name.getPrefix()) &&
                !name.getPrefix().isEmpty() ? name.getPrefix().getFirst().getValue() : null);
        householdMemberDTO.setSignature(Objects.nonNull(relatedPerson.getPhoto()) && !relatedPerson.getPhoto().isEmpty() ?
                relatedPerson.getPhoto().getFirst().getUrl() : null);
        householdMemberDTO.setVersion(relatedPerson.getMeta().getVersionId());
        householdMemberDTO.setLastUpdated(relatedPerson.getMeta().getLastUpdated());
        householdMemberDTO.setGender(relatedPerson.getGender().toString().toLowerCase());
        householdMemberDTO.setDateOfBirth(relatedPerson.getBirthDate());
        householdMemberDTO.setPhoneNumberCategory((!Objects.isNull(relatedPerson.getTelecom())
                && !relatedPerson.getTelecom().isEmpty()) ? getPhoneNumberCategory(relatedPerson.getTelecom().getFirst().getUse()) : null);
        householdMemberDTO.setHouseholdHeadRelationship((!Objects.isNull(relatedPerson.getRelationship())
                && !relatedPerson.getRelationship().isEmpty()) ? relatedPerson.getRelationship()
                .getFirst()
                .getText() : null);
        householdMemberDTO.setPhoneNumber((!Objects.isNull(relatedPerson.getTelecom())
                && !relatedPerson.getTelecom().isEmpty()) ? relatedPerson.getTelecom().getFirst().getValue() : null);
        householdMemberDTO.setId(fhirUtils.getIdFromHistoryUrl(relatedPerson.getId()));
        householdMemberDTO.setVillage((!Objects.isNull(relatedPerson.getAddress())
                && !relatedPerson.getAddress().isEmpty()) ? relatedPerson.getAddress().getFirst().getCity() : null);
        if (!Objects.isNull(relatedPerson.getIdentifier()) && !relatedPerson.getIdentifier().isEmpty()) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL)) {
                    householdMemberDTO.setPatientId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    householdMemberDTO.setVillageId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.HOUSEHOLD_ID_SYSTEM_URL)) {
                    householdMemberDTO.setHouseholdId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.MOTHER_PATIENT_ID_SYSTEM_URL)) {
                    householdMemberDTO.setMotherPatientId(identifier.getValue());
                }
            });
        }
        setPregnancyExtension(relatedPerson, householdMemberDTO);
        return householdMemberDTO;
    }

    /**
     * Map RelatedPerson to household members
     *
     * @param bundle - bundle FHIR bundle object
     * @return - List of HouseholdMemberDTO
     */
    public List<HouseholdMemberDTO> toHouseholdMembers(Bundle bundle) {
        List<HouseholdMemberDTO> householdMemberDTOList = new ArrayList<>();
        bundle.getEntry().forEach(entry -> {
            RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
            householdMemberDTOList.add(toHouseholdMember(relatedPerson));
        });
        return householdMemberDTOList;
    }

    /**
     * Map HouseholdHead Phonenumber from Device(FHIR) resource
     *
     * @param householdDTO - HouseholdDTO
     * @param device       - Device FHIR Resource
     */
    public void mapHousehold(HouseholdDTO householdDTO, Device device) {
        device.getIdentifier().forEach(identifier -> {
            if (identifier.getSystem().equals(FhirIdentifierConstants.PHONE_TYPE_SYSTEM_URL)) {
                householdDTO.setHeadPhoneNumber(identifier.getValue());
            }
            if (identifier.getSystem().equals(FhirIdentifierConstants.PHONE_CATEGORY_SYSTEM_URL)) {
                ContactPoint.ContactPointUse contactPointUse = ContactPoint.ContactPointUse.valueOf(identifier.getValue().toUpperCase());
                householdDTO.setHeadPhoneNumberCategory(getPhoneNumberCategory(contactPointUse));
            }
        });
    }

    /**
     * Map Household's Village, Landmark, Latitude, Longitude from Location(FHIR) resource
     *
     * @param householdDTO - HouseholdDTO
     * @param location     - Location FHIR Resource
     */
    public void mapHousehold(HouseholdDTO householdDTO, Location location) {
        householdDTO.setVillage(location.getAddress().getCity());
        householdDTO.setLandmark(location.getAddress().getText());
        householdDTO.setLatitude(location.getPosition().getLatitude().doubleValue());
        householdDTO.setLongitude(location.getPosition().getLongitude().doubleValue());
    }

    /**
     * Map Household's Name, No., VillageId and some yes/no questions
     * from Group(FHIR) resource
     *
     * @param householdDTO - HouseholdDTO
     * @param group        - Group FHIR Resource
     */
    public void mapHousehold(HouseholdDTO householdDTO, Group group) {
        String householdId = fhirUtils.getIdFromHistoryUrl(group.getId());
        householdDTO.setId(householdId);
        householdDTO.setName(group.getName());
        householdDTO.setVersion(group.getMeta().getVersionId());
        householdDTO.setLastUpdated(group.getMeta().getLastUpdated());
        householdDTO.setNoOfPeople(group.getQuantity());
        group.getIdentifier().forEach(identifier -> {
            if (identifier.getSystem().equals(FhirIdentifierConstants.HOUSEHOLD_NO_SYSTEM_URL)) {
                householdDTO.setHouseholdNo(Long.valueOf(identifier.getValue()));
            }
            if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                householdDTO.setVillageId(identifier.getValue());
            }
        });
        group.getCharacteristic().forEach(characteristicComponent -> {
            if (characteristicComponent.getValue() instanceof CodeableConcept || characteristicComponent.getValue() instanceof Quantity) {
                if (characteristicComponent.getCode().getText()
                        .equals(Constants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP)) {
                    householdDTO.setOwnedHandWashingFacilityWithSoap(characteristicComponent.getValueCodeableConcept()
                            .getCoding().getFirst().getCode().equals(Constants.YES_CODE));
                } else if (characteristicComponent.getCode().getText()
                        .equals(Constants.OWNED_AN_IMPROVED_LATRINE)) {
                    householdDTO.setOwnedAnImprovedLatrine(characteristicComponent.getValueCodeableConcept()
                            .getCoding().getFirst().getCode().equals(Constants.YES_CODE));
                } else if (characteristicComponent.getCode().getText()
                        .equals(Constants.OWNED_TREATED_BED_NET)) {
                    householdDTO.setOwnedTreatedBedNet(characteristicComponent.getValueCodeableConcept()
                            .getCoding().getFirst().getCode().equals(Constants.YES_CODE));
                } else if (characteristicComponent.getCode().getText().equals(Constants.BED_NET_COUNT)) {
                    householdDTO.setBedNetCount(characteristicComponent.getValueQuantity().getValue().intValue());
                }
            }
        });
    }

    /**
     * This method maps the medicationRequest from bundle.
     *
     * @param medicationRequest Bundle Object
     * @return PrescriptionRequestDTO
     */
    public @NotNull PrescriptionDTO mapPrescriptionDTO(MedicationRequest medicationRequest) {
        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();
        medicationRequest.getIdentifier().forEach(identifier -> {
            if (identifier.getSystem().equals(FhirIdentifierConstants.MEDICATION_ID_SYSTEM_URL)) {
                prescriptionDTO.setMedicationId(identifier.getValue());
            }
        });
        prescriptionDTO.setIsActive(medicationRequest.getStatus().equals(MedicationRequest.MedicationRequestStatus.ACTIVE));
        if (!medicationRequest.getNote().isEmpty()) {
            Annotation annotation = medicationRequest.getNote().getFirst();
            prescriptionDTO.setDiscontinuedReason(annotation.getText());
            prescriptionDTO.setDiscontinuedDate(annotation.getTime());
        }
        prescriptionDTO.setEncounterId(fhirUtils.getIdFromReference(medicationRequest.getEncounter().getReference()));
        prescriptionDTO.setMedicationName(medicationRequest.getMedication() instanceof Reference ?
                medicationRequest.getMedicationReference().getDisplay() : medicationRequest.getMedicationCodeableConcept().getText());
        prescriptionDTO.setPrescribedSince(medicationRequest.getDispenseRequest().getValidityPeriod().getStart());
        prescriptionDTO.setEndDate(medicationRequest.getDispenseRequest().getValidityPeriod().getEnd());
        prescriptionDTO.setFrequency(medicationRequest.getDosageInstructionFirstRep().getTiming().getRepeat().getFrequency());
        prescriptionDTO.setFrequencyName(medicationRequest.getDosageInstructionFirstRep().getText());
        prescriptionDTO.setPrescribedDays(medicationRequest.getDosageInstructionFirstRep().getTiming().getRepeat().getPeriod().longValue());
        if (!medicationRequest.getDosageInstruction().isEmpty()) {
            setDosageDetails(medicationRequest, prescriptionDTO);
        }
        setDispenseRemainingDays(medicationRequest, prescriptionDTO);
        return prescriptionDTO;
    }

    /**
     * <p>
     * Sets prescription details from FHIR medication dispense entity
     *</p>
     *
     * @param medicationDispense The FHIR medication dispense entity
     *
     * @return PrescriptionRequestDTO contains dispensed prescription details
     */
    public @NotNull PrescriptionDTO mapDispensePrescriptionDTO(MedicationDispense medicationDispense) {
        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();
        medicationDispense.getIdentifier().forEach(identifier -> {
            if (identifier.getSystem().equals(FhirIdentifierConstants.MEDICATION_ID_SYSTEM_URL)) {
                prescriptionDTO.setMedicationId(identifier.getValue());
            } else if (identifier.getSystem().equals(FhirIdentifierConstants.PRESCRIBED_ID_URL)) {
                prescriptionDTO.setPrescriptionId(identifier.getValue());
            } else if (identifier.getSystem().equals(FhirIdentifierConstants.PRESCRIPTION_FILLED_DAYS_URL)) {
                prescriptionDTO.setPrescriptionFilledDays(Integer.valueOf(identifier.getValue()));
            }
        });
        int alreadyFilledDays = medicationDispense.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.PRESCRIPTION_ALL_READY_FILLED_DAYS_URL.equals(identifier.getSystem()))
                .map(identifier -> Integer.valueOf(identifier.getValue()))
                .findFirst()
                .orElse(Constants.ZERO);
        if (!medicationDispense.getNote().isEmpty()) {
            Annotation annotation = medicationDispense.getNote().getFirst();
            prescriptionDTO.setReason(annotation.getText());
        }
        prescriptionDTO.setMedicationName(medicationDispense.getMedication() instanceof Reference ?
                medicationDispense.getMedicationReference().getDisplay() : medicationDispense.getMedicationCodeableConcept().getText());
        prescriptionDTO.setFrequency(medicationDispense.getDosageInstructionFirstRep().getTiming().getRepeat().getFrequency());
        prescriptionDTO.setFrequencyName(medicationDispense.getDosageInstructionFirstRep().getText());
        prescriptionDTO.setPrescribedDays(medicationDispense.getDosageInstructionFirstRep().getTiming().getRepeat().getPeriod().longValue());
        if (!medicationDispense.getDosageInstruction().isEmpty()) {
            setDispensedDosageDetails(medicationDispense, prescriptionDTO);
        }
        prescriptionDTO.setDispenseRemainingDays(prescriptionDTO.getPrescribedDays().intValue()
                - alreadyFilledDays);
        prescriptionDTO.setLastReFillDate(medicationDispense.getMeta().getLastUpdated());
        return prescriptionDTO;
    }

    /**
     * <p>
     * Sets FHIR dosage details in MedicationRequest entity
     * based on given prescription details
     * </p>
     *
     * @param medicationRequest  The Fhir MedicationRequest entity.
     * @param prescriptionDTO    The PrescriptionDTO entity.
     *
     */
    private void setDosageDetails(MedicationRequest medicationRequest, PrescriptionDTO prescriptionDTO) {
        Dosage dosage = medicationRequest.getDosageInstruction().getFirst();
        if (Objects.nonNull(dosage.getPatientInstruction())) {
            prescriptionDTO.setInstructionNote(dosage.getPatientInstruction());
        }
        if (Objects.nonNull(dosage.getAdditionalInstruction()) &&
                !dosage.getAdditionalInstruction().isEmpty()) {
            prescriptionDTO.setDosageFrequencyName(
                    dosage.getAdditionalInstruction().getFirst().getText());
        }
        int prescriptionRemainingDays = getRemainingPrescriptionDays(prescriptionDTO.getEndDate());
        prescriptionDTO.setPrescriptionRemainingDays(prescriptionRemainingDays);
        prescriptionDTO.getDispenseRemainingDays();
        if (Objects.nonNull(dosage.getAdditionalInstruction()) &&
                !dosage.getDoseAndRate().isEmpty()) {
            Dosage.DosageDoseAndRateComponent dosageDoseAndRateComponent
                    = dosage.getDoseAndRate().getFirst();
            if (Objects.nonNull(dosageDoseAndRateComponent.getDose()) &&
                    dosageDoseAndRateComponent.getDose() instanceof Quantity quantity) {
                prescriptionDTO.setDosageUnitName(quantity.getUnit());
                prescriptionDTO.setDosageUnitValue(Objects.isNull(quantity.getValue()) ? null :  String.valueOf(quantity.getValue().doubleValue()));            }
            if (Objects.nonNull(dosageDoseAndRateComponent.getType())) {
                prescriptionDTO.setDosageFormName(dosageDoseAndRateComponent.getType().getText());
            }
        }
        prescriptionDTO.setIsActive(medicationRequest.getStatus()
                .equals(MedicationRequest.MedicationRequestStatus.ACTIVE));
        prescriptionDTO.setIsDeleted(medicationRequest.getStatus()
                .equals(MedicationRequest.MedicationRequestStatus.CANCELLED));

    }

    /**
     * <p>
     * Sets dispense dosage details from FHIR medication dispense entity
     * </p>
     *
     * @param medicationDispense The FHIR medication dispense entity
     * @param prescriptionDTO    The PrescriptionDTO entity
     *
     */
    private void setDispensedDosageDetails(MedicationDispense medicationDispense,
                                           PrescriptionDTO prescriptionDTO) {
        Dosage dosage = medicationDispense.getDosageInstruction().getFirst();
        if (Objects.nonNull(dosage.getPatientInstruction())) {
            prescriptionDTO.setInstructionNote(dosage.getPatientInstruction());
        }
        if (Objects.nonNull(dosage.getAdditionalInstruction()) &&
                !dosage.getAdditionalInstruction().isEmpty()) {
            prescriptionDTO.setDosageFrequencyName(
                    dosage.getAdditionalInstruction().getFirst().getText());
        }
        if (Objects.nonNull(dosage.getAdditionalInstruction()) &&
                !dosage.getDoseAndRate().isEmpty()) {
            Dosage.DosageDoseAndRateComponent dosageDoseAndRateComponent
                    = dosage.getDoseAndRate().getFirst();
            if (Objects.nonNull(dosageDoseAndRateComponent.getDose()) &&
                    dosageDoseAndRateComponent.getDose() instanceof Quantity quantity) {
                prescriptionDTO.setDosageUnitName(quantity.getUnit());
                prescriptionDTO.setDosageUnitValue(Objects.isNull(quantity.getValue()) ? null :  String.valueOf(quantity.getValue().doubleValue()));
            }
            if (Objects.nonNull(dosageDoseAndRateComponent.getType())) {
                prescriptionDTO.setDosageFormName(dosageDoseAndRateComponent.getType().getText());
            }
        }
    }

    /**
     * <p>
     * This method used to get remaining prescription details based on
     * given prescription end date
     * </p>
     *
     * @param endDate The prescription end date
     *
     * @return remaining prescription days
     */
    public int getRemainingPrescriptionDays(Date endDate) {
        int prescriptionRemainingDays = Constants.ZERO;
        if (DateUtil.isSameDate(new Date(), endDate)) {
            prescriptionRemainingDays = Constants.ONE;
        } else if (endDate.after(new Date())) {
            prescriptionRemainingDays = DateUtil.getCalendarDiff(new Date(), endDate)
                    + Constants.TWO;
        }
        return prescriptionRemainingDays;
    }

    /**
     * <p>
     * This method used to set remaining dispense details in
     *  prescriptionDTO entity
     * </p>
     *
     * @param medicationRequest The FHIR medication request entity
     * @param prescriptionDTO   The prescriptionDTO entity
     *
     */
    public void setDispenseRemainingDays(MedicationRequest medicationRequest,
                                         PrescriptionDTO prescriptionDTO) {
        int dispensedDays = Constants.ZERO;
        Duration dispensedDuration = null;
        if (Objects.nonNull(medicationRequest.getDispenseRequest().getInitialFill())) {
            dispensedDuration = medicationRequest.getDispenseRequest().getInitialFill()
                    .getDuration();
        }
        if (Objects.nonNull(dispensedDuration) && Objects.nonNull(dispensedDuration.getValue())) {
            dispensedDays = dispensedDuration.getValue().intValue();
        }
        int prescriedDays = medicationRequest.getDosageInstructionFirstRep()
                .getTiming().getRepeat().getPeriod().intValue();
        prescriptionDTO.setDispenseRemainingDays(prescriedDays - dispensedDays);
    }
}