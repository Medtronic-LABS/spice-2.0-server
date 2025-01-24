package com.mdtlabs.coreplatform.fhirmapper.converter;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.StringType;
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
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class VitalSignsConverterTest {

    @InjectMocks
    private VitalSignsConverter vitalSignsConverter;

    @Mock
    private RestApiUtil restApiUtil;

    @Test
    void createOrUpdateVitalSigns() {
        //when
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(TestConstants.TWO_STR);
        vitalSignsDTO.setScreenedLandmark(TestConstants.LANDMARK);
        CodeableConcept code = new CodeableConcept();
        Observation redRiskObservation = new Observation();
        redRiskObservation.setComponent(TestDataProvider.getObservstionComponentList());
        vitalSignsDTO.setRedRiskObservation(redRiskObservation);
        code.setText("Blood Glucose Level");
        Bundle bundle = new Bundle();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        Observation observation = vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO, new Bundle());
        Assertions.assertNotNull(observation);
    }

    @Test
    void createOrUpdateVitalSignsBundleNotNull() {
        //when
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(TestConstants.TWO_STR);
        Observation bgObservation = new Observation();
        bgObservation.setComponent(TestDataProvider.getObservstionComponentList());
        CodeableConcept code = new CodeableConcept();
        code.setText("Blood Glucose Level");
        bgObservation.setCode(code);
        Observation redRiskObservation = new Observation();
        redRiskObservation.setId(TestConstants.TWO_STR);
        StringType type = new StringType("riskLevel");
        redRiskObservation.setValue(type);
        Observation bpObservation = new Observation();
        bpObservation.setComponent(TestDataProvider.getObservstionComponentList());
        vitalSignsDTO.setBgObservation(bgObservation);
        vitalSignsDTO.setBpObservation(bpObservation);
        vitalSignsDTO.setRedRiskObservation(redRiskObservation);
        vitalSignsDTO.setScreenedLandmark("tree");
        vitalSignsDTO.setHeightObservation(new Observation());
        vitalSignsDTO.setWeightObservation(new Observation());
        vitalSignsDTO.setBmiObservation(new Observation());
        vitalSignsDTO.setTemperatureObservation(new Observation());
        vitalSignsDTO.setSuicideObservation(new Observation());
        vitalSignsDTO.setSubstanceAbuseObservation(new Observation());
        vitalSignsDTO.setPregnancyObservation(new Observation());
        vitalSignsDTO.setMentalHealthObservation(new Observation());
        vitalSignsDTO.setRegularSmokerObservation(new Observation());
        Bundle bundle = TestDataProvider.getObservationBundle();
        Bundle vitalResponsBundle = new Bundle();
        for (String observationCode : TestDataProvider.getObservationCode()) {
            Observation observation = new Observation();
            observation.setId(TestConstants.TWO_STR);
            observation.setCode(new CodeableConcept().setText(observationCode));
            vitalResponsBundle.addEntry().setResource(observation);
        }

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(restApiUtil.getBatchRequest("Observation?_id=2&_include=Observation:derived-from&_include=Observation:subject")).thenReturn(vitalResponsBundle);

        //then
        Observation response = vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO, new Bundle());
        Assertions.assertNotNull(response);
    }

    @Test
    void createVitalSignsObservation() {
        //given
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();

        //then
        Observation observation = vitalSignsConverter.createVitalSignsObservation(vitalSignsDTO);
        Assertions.assertNotNull(observation);
    }

    @Test
    void createVitalSignsObservationIsNotEmpty() {
        //given
        VitalSignsDTO vitalSignsDTO = TestDataProvider.getVitalSignsDTO();

        //then
        Observation observation = vitalSignsConverter.createVitalSignsObservation(vitalSignsDTO);
        Assertions.assertNotNull(observation);
    }
}