package com.mdtlabs.coreplatform.spiceservice.symptom.service;

import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.SymptomRepository;
import com.mdtlabs.coreplatform.spiceservice.symptom.service.impl.SymptomServiceImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;
import java.util.Set;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SymptomServiceImplTest {
    @InjectMocks
    private SymptomServiceImpl symptomService;

    @Mock
    private SymptomRepository symptomRepository;

    @Test
    void getSymptoms() {
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());
        when(symptomRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(symptoms);
        List<Symptom> result = symptomService.getSymptoms();
        Assertions.assertEquals(symptoms, result);
    }

    @Test
    void getSymptomsById() {
        Symptom symptom = TestDataProvider.getSymptom();
        Set<Long> ids = Set.of(symptom.getId());
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());
        when(symptomRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(symptoms);
        List<Symptom> result = symptomService.getSymptoms(ids);
        Assertions.assertEquals(symptoms, result);
    }
}
