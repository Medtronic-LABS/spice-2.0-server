package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.Observation;
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
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiabetesDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class BloodGlucoseConverterTest {
    @Mock
    FhirUtils fhirUtils;

    @InjectMocks
    BloodGlucoseConverter bloodGlucoseConverter;

    @Mock
    FhirAssessmentMapper fhirAssessmentMapper;

    /**
     * Creates new blood glucose observation
     * with glucose type as rbs
     * Unit Test case
     */
    @Test
    void createBloodGlucoseObservationWithFbs() {
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = bloodGlucoseConverter.createBloodGlucoseObservation(new GlucoseLogDTO());
        Assertions.assertNotNull(observation);
    }

    /**
     * Creates new blood glucose observation with
     * glucose type as rbs
     * Unit Test case
     */
    @Test
    void createBloodGlucoseObservationWithRbs() {
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        glucoseLogDTO.setGlucoseType(FhirConstants.RBS);
        glucoseLogDTO.setDiabetes(List.of(new DiabetesDTO()));
        glucoseLogDTO.getDiabetes().get(0).setName(TestConstants.TEXT);
        glucoseLogDTO.setGlucoseDateTime(null);
        glucoseLogDTO.setGlucoseValue(22.0);
        glucoseLogDTO.setLastMealTime(new Date());
        glucoseLogDTO.setHba1c(22.0);
        glucoseLogDTO.setHba1cUnit("Hba1c");
        glucoseLogDTO.setDiabetes(TestDataProvider.getGlucoseLogRequest().getDiabetes());
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = bloodGlucoseConverter.createBloodGlucoseObservation(glucoseLogDTO);
        Assertions.assertNotNull(observation);
    }

    @Test
    void convertObservationToGlucoseLogDTO() {
        //given
        Observation observation = TestDataProvider.getBloodGlucoseObservation();

        //then
        GlucoseLogDTO response = bloodGlucoseConverter.convertObservationToGlucoseLogDTO(observation);
        Assertions.assertNotNull(response);
    }
}
