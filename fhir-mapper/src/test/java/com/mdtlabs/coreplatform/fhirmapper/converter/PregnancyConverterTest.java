package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.List;

import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Observation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PregnancyConverterTest {

    @Mock
    FhirUtils fhirUtils;

    @Mock
    CommonConverter commonConverter;

    @Mock
    FhirAssessmentMapper fhirAssessmentMapper;

    @InjectMocks
    PregnancyConverter pregnancyConverter;

    @Test
    void createPregnancyObservationTest() {
        PregnancyDetailsDTO pregnancyAncDTO = TestDataProvider.getPregnancyAncRequest();
        Observation observation = null;
        pregnancyAncDTO.setIsPregnancyAnc(Boolean.TRUE);

        // when
        doNothing().when(fhirAssessmentMapper).createObservationComponent(anyBoolean(), anyString(), anyList());

        // response
        observation = pregnancyConverter.createPregnancyObservation(pregnancyAncDTO);
        Assertions.assertNotNull(observation);
    }

    @Test
    void createPregnancyObservationWithDateAsNull() {
        PregnancyDetailsDTO pregnancyAncDTO = TestDataProvider.getPregnancyAncRequest();
        Observation observation = null;

        // when
        doNothing().when(fhirAssessmentMapper).createObservationComponent(anyBoolean(), anyString(), anyList());

        // response
        observation = pregnancyConverter.createPregnancyObservation(pregnancyAncDTO);
        Assertions.assertNotNull(observation);

    }

    @Test
    void createPregnancyObservationWithIsPregnantFalse() {
        PregnancyDetailsDTO pregnancyAncDTO = TestDataProvider.getPregnancyAncRequest();
        pregnancyAncDTO.setIsPregnant(Boolean.FALSE);
        pregnancyAncDTO.getProvenance().setModifiedDate(null);
        pregnancyAncDTO.setIsPregnancyRisk(Boolean.TRUE);
        Observation observation = null;

        // when
        doNothing().when(fhirAssessmentMapper).createObservationComponent(anyBoolean(), anyString(), anyList());

        // response
        observation = pregnancyConverter.createPregnancyObservation(pregnancyAncDTO);
        Assertions.assertNotNull(observation);

    }

    @Test
    void convertObservationToPregnancyDetails() {
        //given
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        Observation observation = new Observation();
        observation.setStatus(Observation.ObservationStatus.PRELIMINARY);
        List<Observation.ObservationComponentComponent> theComponent = TestDataProvider.getObservstionComponentList();
        observation.setComponent(theComponent);

        //then
        pregnancyConverter.convertObservationToPregnancyDetails(pregnancyDetailsDTO, observation);
        Assertions.assertNotNull(observation.getComponent());
    }

    @Test
    void updatePregnancyObservation() {
        //given
        Observation oldPregnancyObservation = new Observation();
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        pregnancyDetailsDTO.setIsInterestedToEnroll(Boolean.TRUE);
        oldPregnancyObservation.setComponent(TestDataProvider.getObservstionComponentList());
        CodeableConcept metacode = TestDataProvider.getCodeableConceptTest();
        metacode.setText("isIptDrugProvided");

        //when
        when(fhirUtils.setCodes(anyString())).thenReturn(metacode);

        //then
        Observation response = pregnancyConverter.updatePregnancyObservation(oldPregnancyObservation, pregnancyDetailsDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePregnancyObservationWithIsPregnantFalse() {
        //given
        Observation oldPregnancyObservation = new Observation();
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setProvenance(TestDataProvider.getProvenance());
        pregnancyDetailsDTO.setIsInterestedToEnroll(Boolean.TRUE);
        pregnancyDetailsDTO.setIsPregnant(Boolean.TRUE);
        pregnancyDetailsDTO.getProvenance().setModifiedDate(null);
        oldPregnancyObservation.setComponent(TestDataProvider.getObservstionComponentList());
        CodeableConcept metacode = TestDataProvider.getCodeableConceptTest();
        metacode.setText("isIptDrugProvided");

        //when
        when(fhirUtils.setCodes(anyString())).thenReturn(metacode);

        //then
        Observation response = pregnancyConverter.updatePregnancyObservation(oldPregnancyObservation, pregnancyDetailsDTO);
        Assertions.assertNotNull(response);
    }


}
