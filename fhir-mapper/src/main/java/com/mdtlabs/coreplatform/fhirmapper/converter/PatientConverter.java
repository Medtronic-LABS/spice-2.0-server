package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 * Converts to FHIR Patient based on given patient details
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class PatientConverter {
    private final CommonConverter commonConverter;
    private final FhirUtils fhirUtils;

    @Autowired
    public PatientConverter(CommonConverter commonConverter, FhirUtils fhirUtils) {
        this.commonConverter = commonConverter;
        this.fhirUtils = fhirUtils;
    }

    /**
     * Converts to FHIR Patient entity based on given patient details
     *
     * @param patient       The patient details to convert
     * @param bioDataDTO    The patient bio data details
     * @param bioMetricsDTO The patient biometrics details
     * @param dateOfBirth   The patient date of birth
     *
     * @return Converted FHIR Patient entity.
     */
    public Patient createPatient(Patient patient, BioDataDTO bioDataDTO,
                                 BioMetricsDTO bioMetricsDTO, Date dateOfBirth) {
        if (Objects.isNull(patient)) {
            String userCountryId = String.valueOf(UserContextHolder.getUserDto().getCountry().getId());
            patient = new Patient();
            patient.addIdentifier()
                    .setSystem(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)
                    .setValue(bioDataDTO.getIdentityValue());
            patient.setActive(true);
            patient.addIdentifier()
                    .setSystem(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)
                    .setValue(Constants.NO);
            patient.addIdentifier()
                    .setSystem(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)
                    .setValue(Constants.NA);
            if (Objects.nonNull(userCountryId)) {
                patient.addIdentifier().setSystem(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL)
                        .setValue(userCountryId);
            }
        }
        patient.setName(List.of(commonConverter.createHumanName(bioDataDTO.getFirstName(),
                bioDataDTO.getMiddleName(), bioDataDTO.getLastName(), bioDataDTO.getInitial())));
        patient.setActive(true);
        if (Objects.nonNull(bioDataDTO.getPhoneNumber())
                && Objects.nonNull(bioDataDTO.getPhoneNumberCategory())) {
            patient.setTelecom(new ArrayList<>());
            patient.addTelecom()
                    .setSystem(ContactPoint.ContactPointSystem.PHONE)
                    .setValue(bioDataDTO.getPhoneNumber())
                    .setUse(this.commonConverter
                            .getFhirPhoneNumberCategory(bioDataDTO.getPhoneNumberCategory().toLowerCase()));
        }
        
        if (Objects.nonNull(bioMetricsDTO.getGender())) {
            Enumerations.AdministrativeGender gender = FhirConstants.GENDER_LIST.contains(bioMetricsDTO.getGender().toUpperCase())
                    ? Enumerations.AdministrativeGender.valueOf(bioMetricsDTO.getGender().toUpperCase())
                    : Enumerations.AdministrativeGender.OTHER;
            patient.setGender(gender);
        }
        if (Objects.nonNull(dateOfBirth)) {
            patient.setBirthDate(dateOfBirth);
        } else {
            patient.setBirthDate(commonConverter.calculateBirthDate(new Date(), bioMetricsDTO.getAge()));
        }
        if (!Objects.isNull(bioDataDTO.getVillage()) && !Objects.isNull(
                bioDataDTO.getVillage().getId())) {
            patient.addIdentifier()
                    .setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                    .setValue(bioDataDTO.getVillage().getId().toString());
        }
        List<Address> addresses = commonConverter.createOrUpdateAddress(patient.getAddress(), bioDataDTO);
        patient.setAddress(addresses);
        return patient;
    }

    /**
     * Creats patient from releted person.
     * 
     * @param member
     * @return
     */
    public Patient createPatientFromRelatedPerson(RelatedPerson member) {
        Patient patient = new Patient();
        patient.setName(member.getName());
        patient.setActive(true);
        patient.setIdentifier(member.getIdentifier());
        patient.setTelecom(member.getTelecom());
        patient.setBirthDate(member.getBirthDate());
        patient.setGender(member.getGender());
        patient.setAddress(member.getAddress());
        return patient;
    }


    /**
     * Converts a FHIR Patient entity to a PatientDetailsDTO.
     *
     * @param patient The FHIR Patient entity to convert.
     * @param patientDTO The PatientDetailsDTO to populate with patient details.
     * @return The populated PatientDetailsDTO.
     */
    public PatientDetailsDTO convertToPatientDetails(Patient patient, PatientDetailsDTO patientDTO) {
        if (Objects.nonNull(patient.getName())
                && Objects.nonNull(patient.getName().getFirst())
                && Objects.nonNull(patient.getName().getFirst().getGiven())
                && !patient.getName().getFirst().getGiven().isEmpty()
                && Objects.nonNull(patient.getName().getFirst().getGiven().getFirst())
                && Objects.nonNull(patient.getName().getFirst().getGiven().getLast())) {
            patientDTO.setName(StringUtil.concatString(patient.getName().getFirst().getGiven().getFirst().asStringValue(),
                    Constants.EMPTY_SPACE,
                    patient.getName().getFirst().getGiven().getLast().asStringValue()));
        }
        patientDTO.setIsActive(patient.getActive());
        if (Objects.nonNull(patient.getId())) {
            patientDTO.setPatientId(patient.getIdPart());
        }
        if (Objects.nonNull(patient.getGender())) {
            patientDTO.setGender(patient.getGender().toCode());
        }
        if (Objects.nonNull(patient.getBirthDate())) {
            BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();
            bioMetricsDTO.setDateOfBirth(patient.getBirthDate());
            patientDTO.setDateOfBirth(bioMetricsDTO.getDateOfBirth());
            patientDTO.setAge(bioMetricsDTO.getAge());
        }
        if (Objects.nonNull(patient.getIdentifier())) {
            patient.getIdentifier().forEach(identifier -> {
               if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)) {
                    patientDTO.setPatientStatus(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)) {
                   patientDTO.setIdentityType(Constants.NATIONAL_ID);
                   patientDTO.setIdentityValue(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)) {
                    patientDTO.setIsReferred(Constants.YES.equals(identifier.getValue()));
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)) {
                    patientDTO.setProgramId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    patientDTO.setVillageId(identifier.getValue());
                }  else if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)
                       && !Constants.NA.equals(identifier.getValue())) {
                    patientDTO.setRiskLevel(identifier.getValue());
               }
            });
        }
        if (Objects.nonNull(patient.getAddress()) && !patient.getAddress().isEmpty()) {
            patient.getAddress().forEach(address -> {
                if (Address.AddressUse.HOME.equals(address.getUse())) {
                    patientDTO.setVillage(address.getText());
                } else if (Address.AddressUse.TEMP.equals(address.getUse())) {
                    patientDTO.setLandmark(address.getText());
                }
            });
        }
        if (Objects.nonNull(patient.getTelecom())
                && !patient.getTelecom().isEmpty()
                && Objects.nonNull(patient.getTelecom().getFirst())
                && Objects.nonNull(patient.getTelecom().getFirst().getValue())) {
            patientDTO.setPhoneNumber(patient.getTelecom().getFirst().getValue());
        }
        if (Objects.nonNull(patient.getLink())
                && !patient.getLink().isEmpty()
                && Objects.nonNull(patient.getLink().getFirst())
                && Objects.nonNull(patient.getLink().getFirst().getOther())
                && Objects.nonNull(patient.getLink().getFirst().getOther().getReference())) {
            patientDTO.setId(fhirUtils.getIdFromReference(patient.getLink().getFirst().getOther().getReference()));
        }
        return patientDTO;
    }

    /**
     * Updates patient object with patient information from EnrollmentRequestDTO.
     *
     * @param patient The FHIR Patient entity to convert.
     * @param request The EnrollmentRequestDTO to populate with patient details.
     * @param mapType Type of conversion create or update patient.
     */
    public void mapPatient(Patient patient, EnrollmentRequestDTO request, String mapType) {
        updateIdentifiers(patient.getIdentifier(), request, mapType);
        patient.setName(List.of(commonConverter.createHumanName(request.getBioData().getFirstName(),
                request.getBioData().getMiddleName(),
                request.getBioData().getLastName(),
                request.getBioData().getInitial())));
        if (Constants.PATIENT_UPDATION_TYPE.equals(mapType) && Objects.nonNull(request.getBioData().getLandmark())) {
            patient.getAddress().forEach(address -> {
                if (Address.AddressUse.TEMP.equals(address.getUse())) {
                    address.setText(request.getBioData().getLandmark());
                }
            });
        } else {
            patient.setAddress(new ArrayList<>());
            patient.addAddress(commonConverter.createAddress(request.getBioData().getCountry(),
                    request.getBioData().getDistrict(), request.getBioData().getChiefdom(),
                    request.getBioData().getVillage(), request.getBioData().getOtherVillage()));
            patient.addAddress(new Address().setUse(Address.AddressUse.TEMP)
                    .setText(request.getBioData().getLandmark()));
        }
        if (!Objects.isNull(request.getBioMetrics())) {
            patient.setGender(Enumerations.AdministrativeGender.fromCode(request.getBioMetrics().getGender()));
            patient.setBirthDate(request.getBioMetrics().getDateOfBirth());
        }
        patient.setActive(true);
        patient.setTelecom(List.of(createContactPoint(request.getBioData().getPhoneNumber(),
                request.getBioData().getPhoneNumberCategory())));
        if (!StringUtils.isEmpty(request.getQrCode())) {
            patient.addPhoto(commonConverter.createAttachment(request.getQrCode()));
        }
        Extension educationextension = patient.getExtensionByUrl(Constants.EXTENSION_EDUCATION_URL);
        Extension enrollmentAtExtension = patient.getExtensionByUrl(Constants.EXTENSION_ENROLLMENT_URL);
        Extension occupationExtension = patient.getExtensionByUrl(Constants.EXTENSION_OCCUPATION_URL);
        if (Objects.isNull(educationextension)) {
            educationextension = new Extension();
            educationextension.setUrl(Constants.EXTENSION_EDUCATION_URL);
            educationextension.setValue(new CodeableConcept().setText(request.getBioData().getLevelOfEducation()));
            patient.addExtension(educationextension);
        } else {
            educationextension.setValue(new CodeableConcept().setText(request.getBioData().getLevelOfEducation()));
        }
        if (Objects.isNull(occupationExtension)) {
            occupationExtension = new Extension();
            occupationExtension.setUrl(Constants.EXTENSION_OCCUPATION_URL);
            occupationExtension.setValue(new CodeableConcept().setText(request.getBioData().getOccupation()));
            patient.addExtension(occupationExtension);
        } else {
            occupationExtension.setValue(new CodeableConcept().setText(request.getBioData().getOccupation()));
        }
        if (Objects.isNull(enrollmentAtExtension)) {
            enrollmentAtExtension = new Extension();
            enrollmentAtExtension.setUrl(Constants.EXTENSION_ENROLLMENT_URL);
            enrollmentAtExtension.setValue(new DateTimeType(request.getDateOfEnrollment()));
            patient.addExtension(enrollmentAtExtension);
        }
        patient.setLink(List.of(
                new Patient.PatientLinkComponent().setOther(
                        new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson),
                                Constants.FORWARD_SLASH,
                                request.getMemberReference())))));
        patient.setManagingOrganization(
                new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                        Constants.FORWARD_SLASH,
                        request.getHealthFacilityFhirId())));
    }

    /**
     * Adds identifies for patient.
     *
     * @param identifiers
     * @param request
     */
    private void updateIdentifiers(List<Identifier> identifiers, EnrollmentRequestDTO request, String mapType) {
        Map<String, Identifier> identifierMap = new HashMap<>();
        identifiers.forEach(identifier -> identifierMap.put(identifier.getSystem(), identifier));
        if (!StringUtils.isEmpty(request.getBioData().getIdentityType())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.IDENTITY_TYPE_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.IDENTITY_TYPE_SYSTEM_URL).setValue(
                        request.getBioData().getIdentityType());
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.IDENTITY_TYPE_SYSTEM_URL)
                        .setValue(request.getBioData().getIdentityType())));
            }
        }
        if (!StringUtils.isEmpty(request.getBioData().getIdentityValue())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)) {
                identifierMap.get(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID).setValue(
                        request.getBioData().getIdentityValue());
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)
                        .setValue(request.getBioData().getIdentityValue())));
            }
        }
        if (!Objects.isNull(request.getBioData().getVillage()) && !Objects.isNull(
                request.getBioData().getVillage().getId())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.VILLAGE_SYSTEM_URL).setValue(
                        request.getBioData().getVillage().getId().toString());
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                        .setValue(request.getBioData().getVillage().getId().toString())));
            }
        }
        if (Objects.nonNull(request.getVirtualId())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL).setValue(request.getVirtualId());
            } else {
                identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)
                        .setValue(request.getVirtualId()));
            }
        }
        if (identifierMap.containsKey(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)) {
            identifierMap.get(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue(Constants.ENROLLED);
        } else {
            identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                    .setValue(Constants.ENROLLED));
        }
        if (!identifierMap.containsKey(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)) {
            identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)
                    .setValue(Constants.NA));
        }
        if (!identifierMap.containsKey(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)) {
            identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)
                    .setValue(Constants.NO));
        }
        if (Objects.nonNull(request.getBioData())
                && Objects.nonNull(request.getBioData().getCountry())
                && Objects.nonNull(request.getBioData().getCountry().getId())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL).setValue(
                        request.getBioData().getCountry().getId().toString());
            } else {
                identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL)
                        .setValue(request.getBioData().getCountry().getId().toString()));
            }
        }
        if (Objects.nonNull(request.getHealthFacilityFhirId()) && !Constants.PATIENT_UPDATION_TYPE.equals(mapType)) {
            if (identifierMap.containsKey(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL).setValue(
                        request.getHealthFacilityFhirId());
            } else {
                identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)
                        .setValue(request.getHealthFacilityFhirId()));
            }
        }
        if (Objects.nonNull(request.getBioData().getProgram())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.PROGRAM_ID_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.PROGRAM_ID_SYSTEM_URL).setValue(
                        String.valueOf(request.getBioData().getProgram().getId()));
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.PROGRAM_ID_SYSTEM_URL)
                        .setValue(String.valueOf(request.getBioData().getProgram().getId()))));
            }
        }
    }
    /**
     * Creates Contact point where patients telecom details added.
     *
     * @param phoneNumber          Contact Information
     * @param phoneNumberCategory  Category of the contact information
     * @return ContactPoint        newly created ContactPoint
     */
    private ContactPoint createContactPoint(String phoneNumber, String phoneNumberCategory) {
        ContactPoint contactPoint = new ContactPoint();
        contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
        contactPoint.setValue(phoneNumber);
        contactPoint.setUse(commonConverter.getFhirPhoneNumberCategory(phoneNumberCategory.toLowerCase()));
        return contactPoint;
    }
    /**
     * Maps requests values to RelatedPerson resource.
     *
     * @param relatedPerson The relatedPerson to be updated.
     * @param request       Patient details to be updated with.
     */
    public void mapRelatedPerson(RelatedPerson relatedPerson, EnrollmentRequestDTO request, String mapType) {
        updateIdentifiers(relatedPerson.getIdentifier(), request, mapType);
        relatedPerson.setName(List.of(commonConverter.createHumanName(request.getBioData().getFirstName(),
                request.getBioData().getMiddleName(), request.getBioData().getLastName(), request.getBioData().getInitial())));
        if (!Objects.isNull(request.getBioMetrics())) {
            relatedPerson.setGender(Enumerations.AdministrativeGender.fromCode(request.getBioMetrics().getGender()));
            relatedPerson.setBirthDate(request.getBioMetrics().getDateOfBirth());  // if age given data should be calculated.
        }
        relatedPerson.setActive(true);
        relatedPerson.setTelecom(new ArrayList<>());
        relatedPerson.addTelecom(createContactPoint(request.getBioData().getPhoneNumber(),
                request.getBioData().getPhoneNumberCategory()));
        if (Objects.nonNull(request.getBioData().getCountry()) && Objects.nonNull(request.getBioData().getDistrict())
                && Objects.nonNull(request.getBioData().getChiefdom()) && Objects.nonNull(request.getBioData().getVillage())) {
            relatedPerson.setAddress(new ArrayList<>());
            relatedPerson.addAddress(createAddress(request.getBioData().getCountry(),
                    request.getBioData().getDistrict(), request.getBioData().getChiefdom(),
                    request.getBioData().getVillage(), request.getBioData().getOtherVillage()));
            relatedPerson.addAddress(new Address().setUse(Address.AddressUse.TEMP).setText(request.getBioData().getLandmark()));
        }
        if (mapType.equals(Constants.PATIENT_UPDATION_TYPE) && Objects.nonNull(request.getBioData().getLandmark())) {
            relatedPerson.getAddress().forEach(address -> {
                if (Address.AddressUse.TEMP.equals(address.getUse())) {
                    address.setText(request.getBioData().getLandmark());
                }
            });
        }
    }

    /**
     * Creates Address where patients address details added.
     *
     * @param country    Country data
     * @param district   District data
     * @param chiefdom   Chiefdom data
     * @param village    Village data
     * @return           new Created Address object.
     */
    private Address createAddress(BioDataDTO.DataDTO country, BioDataDTO.DataDTO district, BioDataDTO.DataDTO chiefdom,
                                  BioDataDTO.DataDTO village, String otherVillage) {
        Address address = new Address();
        address.setUse(Address.AddressUse.HOME);
        address.setCountry(country.getName());
        address.setDistrict(district.getName());
        address.setCity(chiefdom.getName());
        if (!Objects.isNull(village)) {
            address.setText(Objects.nonNull(otherVillage) && village.getName().equals(Constants.OTHER) ? otherVillage :
                    village.getName());
        }
        return address;
    }

    /**
     * Creates Address where patients address details added.
     *
     * @param patient    Country data
     * @param enrollmentResponseDTO   District data
     */
    public void convertToEnrollmentResponseDTO(Patient patient, EnrollmentResponseDTO enrollmentResponseDTO) {
        if (Objects.nonNull(patient.getName())
                && Objects.nonNull(patient.getName().getFirst())
                && Objects.nonNull(patient.getName().getFirst().getGiven())
                && !patient.getName().getFirst().getGiven().isEmpty()
                && Objects.nonNull(patient.getName().getFirst().getGiven().getFirst())
                && Objects.nonNull(patient.getName().getFirst().getGiven().getLast())) {
            enrollmentResponseDTO.setName(StringUtil.concatString(
                    patient.getName().getFirst().getGiven().getFirst().asStringValue(),
                    patient.getName().getFirst().getGiven().getLast().asStringValue()));
        }
        List<StringType> givenNames = patient.getName().getFirst().getGiven();
        if (givenNames.size() > Constants.TWO) {
            enrollmentResponseDTO.setLastName(givenNames.get(Constants.TWO).getValue());
            enrollmentResponseDTO.setMiddleName(givenNames.get(Constants.ONE).getValue());
        } else {
            enrollmentResponseDTO.setLastName(givenNames.get(Constants.ONE).getValue());
        }
        enrollmentResponseDTO.setFirstName(givenNames.getFirst().getValue());
        enrollmentResponseDTO.setIsActive(patient.getActive());
        if (Objects.nonNull(patient.getId())) {
            enrollmentResponseDTO.setId(patient.getIdPart());
        }
        if (Objects.nonNull(patient.getGender())) {
            enrollmentResponseDTO.setGender(patient.getGender().toCode());
        }
        if (Objects.nonNull(patient.getBirthDate())) {
            BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();
            bioMetricsDTO.setDateOfBirth(patient.getBirthDate());
            enrollmentResponseDTO.setDateOfBirth(bioMetricsDTO.getDateOfBirth());
            enrollmentResponseDTO.setAge(bioMetricsDTO.getAge());
        }
        if (Objects.nonNull(patient.getIdentifier())) {
            patient.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)) {
                    enrollmentResponseDTO.setPatientStatus(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)) {
                    enrollmentResponseDTO.setIdentityType(Constants.NATIONAL_ID);
                    enrollmentResponseDTO.setIdentityValue(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)) {
                    enrollmentResponseDTO.setProgramId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    enrollmentResponseDTO.setVillageId(identifier.getValue());
                }
            });
        }
        Extension educationExtension = patient.getExtensionByUrl(Constants.EXTENSION_EDUCATION_URL);
        CodeableConcept codeableConcept = (CodeableConcept) educationExtension.getValue();
        enrollmentResponseDTO.setLevelOfEducation(codeableConcept.getText());
        if (Objects.nonNull(patient.getAddress())
                && !patient.getAddress().isEmpty()
                && Objects.nonNull(patient.getAddress().getFirst())
                && Objects.nonNull(patient.getAddress().getFirst().getText())) {
            enrollmentResponseDTO.setVillage(patient.getAddress().getFirst().getText());
        }
        if (Objects.nonNull(patient.getTelecom())
                && !patient.getTelecom().isEmpty()
                && Objects.nonNull(patient.getTelecom().getFirst())
                && Objects.nonNull(patient.getTelecom().getFirst().getValue())) {
            enrollmentResponseDTO.setPhoneNumber(patient.getTelecom().getFirst().getValue());
        }
        if (Objects.nonNull(patient.getLink())
                && !patient.getLink().isEmpty()
                && Objects.nonNull(patient.getLink().getFirst())
                && Objects.nonNull(patient.getLink().getFirst().getOther())
                && Objects.nonNull(patient.getLink().getFirst().getOther().getReference())) {
            enrollmentResponseDTO.setMemberReference(fhirUtils.getIdFromReference(
                    patient.getLink().getFirst().getOther().getReference()));
        }
    }

}
