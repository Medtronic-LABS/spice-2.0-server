package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.Date;

import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class EncounterConverterTest {

    @InjectMocks
    EncounterConverter encounterConverter;

    @Mock
    FhirUtils fhirUtils;

    @Test
    void createEncounterTest() {
        Encounter encounter = null;
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getFhirRelatedPerson();
        Location location = TestDataProvider.getFhirLocation();

        // then
        encounter = encounterConverter.createEncounter(patient, relatedPerson, location,
                TestConstants.LANDMARK, new Date());

        // Assertions
        Assertions.assertNotNull(encounter);
    }

    @Test
    void createEncounterTestWithDateAsNull() {
        Encounter encounter = null;
        Patient patient = TestDataProvider.getPatient();
        RelatedPerson relatedPerson = TestDataProvider.getFhirRelatedPerson();
        Location location = TestDataProvider.getFhirLocation();

        // when
        encounter = encounterConverter.createEncounter(patient, relatedPerson, location,
                TestConstants.LANDMARK, null);

        // Assertions
        Assertions.assertNotNull(encounter);
    }

    @Test
    void setEncounterClassHistoryTest() {
        Encounter encounter = TestDataProvider.getEncounter();

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());

        //then
        encounterConverter.setEncounterClassHistory(encounter, TestConstants.TEXT);

        // Assertions
        Assertions.assertNotEquals(0, encounter.getClassHistory().size());
    }

    @Test
    void createEncounter() {
        //given
        String patientReference = TestConstants.PATIENT_REFERENCE;
        String memberReference = TestConstants.PATIENT_REFERENCE;
        String systemeUrl = TestConstants.URL;
        String value = TestConstants.STRING_VALUE;
        ProvenanceDTO provenance = TestDataProvider.getProvenance();

        //then
        Encounter response = encounterConverter.createEncounter(patientReference, memberReference, systemeUrl, value, provenance);
        Assertions.assertNotNull(response);
    }
}
