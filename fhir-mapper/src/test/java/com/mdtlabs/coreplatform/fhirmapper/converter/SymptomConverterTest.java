package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
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
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiabetesDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.SymptomDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SymptomConverterTest {

    @Mock
    FhirUtils fhirUtils;

    @InjectMocks
    SymptomConverter symptomConverter;

    @Mock
    private RedisTemplate<String, Map<String, List<MetaDataDTO>>> metaRedisTemplate;

    @Mock
    private ValueOperations<String, Map<String, List<MetaDataDTO>>> valueOperations;


    @Test
    void createComplianceTest() {
        List<SymptomDTO> symptomDTOList = TestDataProvider.getSymptomList();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = symptomConverter.createSymptomObservation(symptomDTOList, new Date());
        Assertions.assertNotNull(observation);
    }


    @Test
    void createComplianceTestWithDateAsNull() {
        List<SymptomDTO> symptomDTOList = TestDataProvider.getSymptomList();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = symptomConverter.createSymptomObservation(symptomDTOList, null);
        Assertions.assertNotNull(observation);
    }

    @Test
    void getSymptomListByBundle() {
        //given
        Bundle bundle = new Bundle();
        TestDataProvider.init();
        Map<String, MetaCodeDetails> codeDetails = new HashMap<>();
        Map<String, List<MetaDataDTO>> valueMap = new HashMap<>();
        for (String code : TestDataProvider.getSymptomName()) {
            MetaCodeDetails metaCodeDetails = TestDataProvider.getMetaCodeDetails();
            assert metaCodeDetails != null;
            metaCodeDetails.setFormName(code);
            Observation observation = new Observation();
            MetaDataDTO metaDataDTO = new MetaDataDTO();
            metaDataDTO.setValue(code);
            metaDataDTO.setType(Constants.HYPERTENSION);
            metaDataDTO.setDisplayValue(code);
            Observation.ObservationComponentComponent component = new Observation.ObservationComponentComponent();
            component.setCode(new CodeableConcept().setText(code));
            observation.addComponent(component);
            bundle.addEntry(new Bundle.BundleEntryComponent().setResource(observation));
            codeDetails.put(code, metaCodeDetails);
            valueMap.put(code, List.of(metaDataDTO));
        }

        when(fhirUtils.getCodeDetails()).thenReturn(codeDetails);
        when(metaRedisTemplate.opsForValue()).thenReturn(valueOperations);
        when(metaRedisTemplate.opsForValue().get(anyString())).thenReturn(valueMap);

        //then
        List<String> response = symptomConverter.getSymptomListByBundle(bundle, Boolean.TRUE);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createSymptomObservation() {
        //given
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        List<DiabetesDTO> symptoms = new ArrayList<>();
        for (String name : TestDataProvider.getSymptomName()) {
            DiabetesDTO diabetesDTO = new DiabetesDTO();
            diabetesDTO.setName(name);
            symptoms.add(diabetesDTO);
        }
        glucoseLogDTO.setDiabetes(symptoms);
        Date date = new Date();

        //then
        Observation result = symptomConverter.createSymptomObservation(glucoseLogDTO, date);
        Assertions.assertNotNull(result);
    }

}
