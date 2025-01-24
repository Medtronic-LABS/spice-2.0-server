package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.Date;
import java.util.Objects;

import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 * Converts to FHIR Encounter based on given patient, related person
 * and location details
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class EncounterConverter {
    private final FhirUtils fhirUtils;

    @Autowired
    public EncounterConverter(FhirUtils fhirUtils) {
        this.fhirUtils = fhirUtils;
    }

    /**
     * Converts to FHIR Encounter entity based on given patient, related person
     * and location details
     *
     * @param patient       The FHIR Patient entity
     * @param relatedPerson The FHIR RelatedPerson entity
     * @param location      The FHIR Location entity
     * @param serviceType   Type of patient service
     * @param encounterDate Encounter taken time
     *
     * @return Converted FHIR Encounter entity.
     */
    public Encounter createEncounter(Patient patient,
                                      RelatedPerson relatedPerson,
                                      Location location,
                                      String serviceType,
                                      Date encounterDate
                                      ) {
        fhirUtils.initiateCodesMap();
        Encounter encounter = new Encounter();
        encounter.setStatus(Encounter.EncounterStatus.FINISHED);
        if (Objects.nonNull(location)) {
            Encounter.EncounterLocationComponent locationComponent
                    = new Encounter.EncounterLocationComponent();
            locationComponent.setLocation(new Reference(FhirConstants.LOCATION_IDENTIFIER_URL));
            encounter.addLocation(locationComponent);
        }

        if (Objects.nonNull(patient) && Objects.nonNull(patient.getIdPart())) {
            encounter.setSubject(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patient.getIdPart())));
        } else if (Objects.nonNull(patient)) {
            encounter.setSubject(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
        }

        if (Objects.nonNull(serviceType)) {
            encounter.setServiceType(fhirUtils.setCodes(serviceType.toLowerCase()));
        }

        if (Objects.nonNull(relatedPerson) && Objects.nonNull(relatedPerson.getIdPart())) {
            Encounter.EncounterParticipantComponent
                    participantComponent = new Encounter.EncounterParticipantComponent();
            participantComponent.setIndividual(new Reference(
                    String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart())
            ));
            encounter.addParticipant(participantComponent);
        } else if (Objects.nonNull(relatedPerson)) {
            Encounter.EncounterParticipantComponent
                    participantComponent = new Encounter.EncounterParticipantComponent();
            participantComponent.setIndividual(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
            encounter.addParticipant(participantComponent);
        }

        if (Objects.nonNull(encounterDate)) {
            encounter.setPeriod(new Period().setStartElement(new DateTimeType(encounterDate)));
        }
        return encounter;
    }

    public void setEncounterClassHistory(Encounter encounter, String type) {
        if (Objects.nonNull(encounter)) {
            Encounter.ClassHistoryComponent classHistory = new Encounter.ClassHistoryComponent();
            CodeableConcept codeableConcept = fhirUtils.setCodes(type.toLowerCase());

            if (!codeableConcept.getCoding().isEmpty()) {
                classHistory.setClass_(codeableConcept.getCoding().get(0));
                encounter.addClassHistory(classHistory);
            } else {
                encounter.addClassHistory(new Encounter.ClassHistoryComponent().setClass_(new Coding().setDisplay(type)));

            }
        }
   }

   /**
    * Creates encounter with patient and member reference.

    * @param patientReference
    * @param memberReference
    * @param systemeUrl
    * @param value
    * @param provenance
    * @return Encounter
    */
   public Encounter createEncounter(String patientReference, String memberReference, String systemeUrl, String value, ProvenanceDTO provenance) {
        Encounter encounter = new Encounter();
        Date visitDate = new Date();
        Period period = new Period();
        period.setStart(visitDate);
        period.setEnd(visitDate);
        encounter.setPeriod(period);
        encounter.addIdentifier().setSystem(systemeUrl).setValue(value);

        encounter.setServiceProvider(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
            Constants.FORWARD_SLASH, provenance.getOrganizationId())));
        encounter.setStatus(Encounter.EncounterStatus.INPROGRESS);

        Encounter.EncounterParticipantComponent encounterParticipantPerson = new Encounter.EncounterParticipantComponent();
        encounterParticipantPerson.setIndividual(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, memberReference));
        encounter.addParticipant(encounterParticipantPerson);

        if (!Objects.isNull(patientReference)) {
            encounter.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, patientReference));
        }
        return encounter;
   }
}
