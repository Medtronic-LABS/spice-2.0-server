package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.StringType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SpiceConverterTest {

    @InjectMocks
    SpiceConverter spiceConverter;

    @Test
    void setHeightDetailsTest() {
        Observation observation = TestDataProvider.getHeightObservation();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();

        spiceConverter.setHeightDetails(observation, bioMetricsDTO);

        //Assertions
        Assertions.assertEquals(observation.getValueQuantity().getValue().doubleValue(),
                bioMetricsDTO.getHeight());
    }

    @Test
    void setWeightDetailsTest() {
        Observation observation = TestDataProvider.getWeightObservation();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();

        spiceConverter.setWeightDetails(observation, bioMetricsDTO);

        //Assertions
        Assertions.assertEquals(observation.getValueQuantity().getValue().doubleValue(),
                bioMetricsDTO.getWeight());
    }

    @Test
    void setBmiDetailssetWeightDetailsTest() {
        Observation observation = TestDataProvider.getBmiObservation();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();

        spiceConverter.setBmiDetails(observation, bioMetricsDTO);

        //Assertions
        Assertions.assertEquals(observation.getValueQuantity().getValue().doubleValue(),
                bioMetricsDTO.getBmi());
    }

    @Test
    void setRegularSmokerDetailsTest() {
        Observation observation = TestDataProvider.getRegularSmokerObservation();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();

        spiceConverter.setRegularSmokerDetails(observation, bioMetricsDTO);

        //Assertions
        Assertions.assertFalse(bioMetricsDTO.getIsRegularSmoker());
    }

    @Test
    void setSuicideScreenerDetailsTest() {
        Observation observation = TestDataProvider.getSuicideScreenerObservation();
        CodeableConcept concept = new CodeableConcept();
        concept.setText("Some Default Text");
        observation.getComponent().getFirst().setValue(concept);
        observation.getComponent().getLast().setValue(concept);
        Map<String, String> suicideDetails = new HashMap<>();

        spiceConverter.setSuicideScreenerDetails(observation, suicideDetails);

        //Assertions
        Assertions.assertEquals(observation.getComponent().size(), suicideDetails.entrySet().size());
    }

    @Test
    void setSubstanceDetailsTest() {
        Observation observation = TestDataProvider.getSubstanceAbuseObservation();
        Map<String, String> substanceDetails = new HashMap<>();

        spiceConverter.setSubstanceDetails(observation, substanceDetails);

        //Assertions
        Assertions.assertEquals(observation.getComponent().size(), substanceDetails.entrySet().size());
    }

    @Test
    void setRiskDetailsTest() {
        Observation observation = TestDataProvider.getMentalHealthObservation();
        Map<String, String> riskDetails = new HashMap<>();

        spiceConverter.setRiskDetails(observation, riskDetails);

        //Assertions
        Assertions.assertEquals(observation.getComponent().size(), riskDetails.entrySet().size());
    }

    @Test
    void setGlucoseLogDTOTest() {
        Observation observation = TestDataProvider.getBloodGlucoseObservation();
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        glucoseLogDTO.setHba1c(23.5);

        spiceConverter.setGlucoseLogDTO(observation, glucoseLogDTO);

        //Assertions
        Assertions.assertEquals(observation.getComponent().size(),
                glucoseLogDTO.getDiabetes().size() + 1);
    }

    @Test
    void setBpLogDTOTest() {
        Observation observation = TestDataProvider.getBloodPressureObservation();
        BpLogDTO bpLogDTO = new BpLogDTO();

        spiceConverter.setBpLogDTO(observation, bpLogDTO);

        //Assertions
        Assertions.assertNotNull(bpLogDTO.getBpLogDetails());
    }

    @Test
    void setBioDataDetailsTest() {
        RelatedPerson relatedPerson = TestDataProvider.getFhirRelatedPerson();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();
        BioDataDTO bioDataDTO = new BioDataDTO();

        spiceConverter.setBioDataDetails(relatedPerson, bioMetricsDTO, bioDataDTO);

        //Assertions
        Assertions.assertNull(bioDataDTO.getIdentityValue());
    }

    @Test
    void setPregnancyDetailsTest() {
        Observation observation = TestDataProvider.getPregnancyAncObservation();
        PregnancyDetailsDTO pregnancyAncDTO =  new PregnancyDetailsDTO();

        spiceConverter.setPregnancyDetails(observation, pregnancyAncDTO);

        //Assertions
        Assertions.assertNotNull(pregnancyAncDTO.getPregnancySymptoms().getFirst());
    }

    @Test
    void setObservationDetailsTest() {
        SearchPersonDetailsDTO personDetailsDTO = TestDataProvider.getPersonDetailsDTO();
        personDetailsDTO.getBpObservation().setIdentifier(Arrays.asList(new Identifier().setValue("bloodPressure")));
        personDetailsDTO.getBgObservation().setIdentifier(Arrays.asList(new Identifier().setValue("bloodGlucose")));
        personDetailsDTO.getPregnancyObservation().setIdentifier(Arrays.asList(new Identifier().setValue("vitalsigns")));
        personDetailsDTO.getMentalHealthObservation().setIdentifier(Arrays.asList(new Identifier().setValue("vitalsigns")));
        personDetailsDTO.getRegularSmokerObservation().setIdentifier(Arrays.asList(new Identifier().setValue("vitalsigns")));
        personDetailsDTO.getSuicideScreenerObservation().setIdentifier(Arrays.asList(new Identifier().setValue("vitalsigns")));

        spiceConverter.setObservationDetails(personDetailsDTO.getBpObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getBgObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getHeightObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getWeightObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getBmiObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getPregnancyObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getMentalHealthObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getRegularSmokerObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getSuicideScreenerObservation(), personDetailsDTO);
        spiceConverter.setObservationDetails(personDetailsDTO.getSubstanceAbuseObservation(), personDetailsDTO);

        //Assertions
        Assertions.assertNotNull(personDetailsDTO.getBpObservation());
        Assertions.assertNotNull(personDetailsDTO.getBgObservation());
        Assertions.assertNotNull(personDetailsDTO.getMentalHealthObservation());
        Assertions.assertNotNull(personDetailsDTO.getPhq4());
        Assertions.assertNotNull(personDetailsDTO.getPregnancyObservation());
    }

    @Test
    void setPatientBioDetails() {
        //given
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        relatedPerson.getName().getFirst().setPrefix(new ArrayList<>());
        List<StringType> names = new ArrayList<>();
        names.add(new StringType("Jane"));
        names.add(new StringType("Smith"));
        relatedPerson.getIdentifier().getLast().setSystem("village-id");
        relatedPerson.getName().getFirst().setGiven(names);
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();

        //then
        spiceConverter.setPatientBioDetails(relatedPerson, assessmentDTO);
        Assertions.assertNotNull(assessmentDTO.getBioData());
    }
}
