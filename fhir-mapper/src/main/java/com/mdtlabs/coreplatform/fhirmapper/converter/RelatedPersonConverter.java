package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;

/**
 * <p>
 * Converts to FHIR Related Person based on given related person details
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class RelatedPersonConverter {
    private final CommonConverter commonConverter;

    @Autowired
    public RelatedPersonConverter(CommonConverter commonConverter) {
        this.commonConverter = commonConverter;
    }

    /**
     * Converts to FHIR Related Person entity based on given patient details
     *
     * @param relatedPerson     The related person details to convert
     * @param bioDataDTO        The related person bio data details
     * @param bioMetricsDTO     The related person biometrics details
     * @param dateOfBirth       The related person date of birth
     *
     * @return Converted FHIR  Related Person entity.
     */
    public RelatedPerson createRelatedPerson(RelatedPerson relatedPerson,
                                             BioDataDTO bioDataDTO,
                                             BioMetricsDTO bioMetricsDTO,
                                             Date dateOfBirth,
                                             String siteId, String countryId, String isReferredYesOrNo) {
        if (Objects.isNull(relatedPerson)) {
            relatedPerson = new RelatedPerson();
            relatedPerson.setActive(true);
        }
        updateIdentifiers(relatedPerson, bioDataDTO, siteId, countryId, isReferredYesOrNo);

        relatedPerson.setName(List.of(commonConverter.createHumanName(bioDataDTO.getFirstName(),
                bioDataDTO.getMiddleName(), bioDataDTO.getLastName(), bioDataDTO.getInitial())));

        if (Objects.nonNull(bioDataDTO.getPhoneNumber())
                && Objects.nonNull(bioDataDTO.getPhoneNumberCategory())) {
            relatedPerson.setTelecom(new ArrayList<>());
            relatedPerson.addTelecom()
                    .setSystem(ContactPoint.ContactPointSystem.PHONE)
                    .setValue(bioDataDTO.getPhoneNumber())
                    .setUse(this.commonConverter
                            .getFhirPhoneNumberCategory(bioDataDTO.getPhoneNumberCategory().toLowerCase()));
        }

        if (Objects.nonNull(bioMetricsDTO.getGender())) {
            Enumerations.AdministrativeGender gender = FhirConstants.GENDER_LIST.contains(bioMetricsDTO.getGender().toUpperCase())
                    ? Enumerations.AdministrativeGender.valueOf(bioMetricsDTO.getGender().toUpperCase())
                    : Enumerations.AdministrativeGender.OTHER;
            relatedPerson.setGender(gender);
        }
        if (Objects.nonNull(dateOfBirth)) {
            relatedPerson.setBirthDate(dateOfBirth);
        } else {
            relatedPerson.setBirthDate(commonConverter.calculateBirthDate(new Date(), bioMetricsDTO.getAge()));
        }
        List<Address> addresses = commonConverter.createOrUpdateAddress(relatedPerson.getAddress(), bioDataDTO);
        relatedPerson.setAddress(addresses);
        return relatedPerson;
    }

    /**
     * Update identifies for relatedPerson.
     *
     * @param relatedPerson RelatedPerson to update
     * @param bioDataDTO bio data
     * @param siteId site id of the patient
     * @param countryId country id of the patient
     */
    private void updateIdentifiers(RelatedPerson relatedPerson, BioDataDTO bioDataDTO,
                                   String siteId, String countryId, String isReferredYesOrNo) {
        List<Identifier> identifiers = relatedPerson.getIdentifier();
        Map<String, Identifier> identifierMap = new HashMap<>();
        boolean identifierExists = relatedPerson.getIdentifier().stream()
                .anyMatch(identifier -> FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL
                        .equals(identifier.getSystem()));
        if (!identifierExists) {
            relatedPerson.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                    .setValue(FhirConstants.SCREENED);
        }
        identifiers.forEach(identifier -> identifierMap.put(identifier.getSystem(), identifier));
        if (!StringUtils.isEmpty(bioDataDTO.getIdentityType())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.IDENTITY_TYPE_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.IDENTITY_TYPE_SYSTEM_URL).setValue(
                        bioDataDTO.getIdentityType());
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.IDENTITY_TYPE_SYSTEM_URL)
                        .setValue(bioDataDTO.getIdentityType())));
            }
        }
        if (!StringUtils.isEmpty(bioDataDTO.getIdentityValue())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)) {
                identifierMap.get(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID).setValue(
                        bioDataDTO.getIdentityValue());
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)
                        .setValue(bioDataDTO.getIdentityValue())));
            }
        }
        if (Objects.nonNull(bioDataDTO.getVillage()) && Objects.nonNull(bioDataDTO.getVillage().getId())) {
            if (identifierMap.containsKey(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.VILLAGE_SYSTEM_URL).setValue(
                        bioDataDTO.getVillage().getId().toString());
            } else {
                identifiers.add((new Identifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)
                        .setValue(bioDataDTO.getVillage().getId().toString())));
            }
        }
        if (Objects.nonNull(countryId)) {
            if (identifierMap.containsKey(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL).setValue(countryId);
            } else {
                identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL)
                        .setValue(countryId));
            }
        }
        if (Objects.nonNull(siteId)) {
            if (identifierMap.containsKey(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL).setValue(siteId);
            } else {
                identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)
                        .setValue(siteId));
            }
        }
        if (Objects.nonNull(isReferredYesOrNo)) {
            if (identifierMap.containsKey(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)) {
                identifierMap.get(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL).setValue(isReferredYesOrNo);
            } else {
                identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)
                        .setValue(isReferredYesOrNo));
            }
        }
        if (!identifierMap.containsKey(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)) {
            identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)
                    .setValue(Constants.NA));
        }
    }
}
