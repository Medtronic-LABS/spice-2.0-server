package com.mdtlabs.coreplatform.spiceservice.common;

import java.util.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.dto.RiskAlgorithmDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RiskAlgorithmTest {

    @InjectMocks
    RiskAlgorithm riskAlgorithm;

    @Mock
    StaticDataService staticDataService;

    RiskAlgorithmDTO riskAlgorithmDto;

    @BeforeEach
    void setup() {
        riskAlgorithmDto = new RiskAlgorithmDTO();
    }

    @Test
    void testGetRiskLevelInAssessmentDbm_Pregnant() {
        // Given
        riskAlgorithmDto.setIsPregnant(true);

        // When
        String riskLevel = riskAlgorithm.getRiskLevelInAssessmentDbm(riskAlgorithmDto);

        // Then
        assertEquals(Constants.HIGH, riskLevel);
    }

    @Test
    void testGetRiskLevelInAssessmentDbm_WithRiskLevel() {
        // Given
        riskAlgorithmDto.setIsPregnant(false);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        riskAlgorithmDto.setAvgSystolic(150.0);
        riskAlgorithmDto.setAvgDiastolic(100.0);

        // When
        String riskLevel = riskAlgorithm.getRiskLevelInAssessmentDbm(riskAlgorithmDto);

        // Then
        assertNotNull(riskLevel);
    }

    @Test
    void testCalculateRiskLevelFromGlucoseData_HighRisk() {
        // Given
        riskAlgorithmDto.setGlucoseValue(30.0);
        riskAlgorithmDto.setGlucoseType(Constants.RBS);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        String updatedRiskLevel = riskAlgorithm.calculateRiskLevelFromGlucoseData(
                riskAlgorithmDto.getGlucoseValue(), riskAlgorithmDto.getGlucoseType(), symptoms);

        // Then
        assertEquals(Constants.HIGH, updatedRiskLevel);
    }

    @Test
    void testCalculateRiskLevelFromGlucoseData_ModerateRisk() {
        // Given
        riskAlgorithmDto.setGlucoseValue(11.0);
        riskAlgorithmDto.setGlucoseType(Constants.RBS);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String updatedRiskLevel = riskAlgorithm.calculateRiskLevelFromGlucoseData(
                riskAlgorithmDto.getGlucoseValue(), riskAlgorithmDto.getGlucoseType(), symptoms);

        // Then
        assertEquals(Constants.LOWER_MODERATE, updatedRiskLevel);
    }

    @Test
    void testCalculateRiskLevelFromGlucoseData_LowRisk() {
        // Given
        riskAlgorithmDto.setGlucoseValue(5.5);
        riskAlgorithmDto.setGlucoseType(Constants.RBS);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String updatedRiskLevel = riskAlgorithm.calculateRiskLevelFromGlucoseData(
                riskAlgorithmDto.getGlucoseValue(), riskAlgorithmDto.getGlucoseType(), symptoms);

        // Then
        assertEquals(Constants.LOW, updatedRiskLevel);
    }

    @Test
    void testCalculateRiskLevelFromGlucoseData_Moderate() {
        // Given
        riskAlgorithmDto.setGlucoseValue(14.5);
        riskAlgorithmDto.setGlucoseType(Constants.RBS);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String updatedRiskLevel = riskAlgorithm.calculateRiskLevelFromGlucoseData(
                riskAlgorithmDto.getGlucoseValue(), riskAlgorithmDto.getGlucoseType(), symptoms);

        // Then
        assertEquals(Constants.MODERATE, updatedRiskLevel);
    }

    @Test
    void testCalculateRiskLevelFromGlucoseData_Higher_Moderate() {
        // Given
        riskAlgorithmDto.setGlucoseValue(17.5);
        riskAlgorithmDto.setGlucoseType(Constants.RBS);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String updatedRiskLevel = riskAlgorithm.calculateRiskLevelFromGlucoseData(
                riskAlgorithmDto.getGlucoseValue(), riskAlgorithmDto.getGlucoseType(), symptoms);

        // Then
        assertEquals(Constants.HIGHER_MODERATE, updatedRiskLevel);
    }

    @Test
    void testCalculateRiskLevelFromSymptoms_CategoryFive() {
        // Given
        Map<String, Integer> symptomsCount = new HashMap<>();
        symptomsCount.put(Constants.CATEGORY_FIVE_COUNT, 1);
        symptomsCount.put(Constants.CATEGORY_FOUR_COUNT, 1);
        symptomsCount.put(Constants.CATEGORY_THREE_COUNT, 1);
        symptomsCount.put(Constants.CATEGORY_TWO_COUNT, 1);
        riskAlgorithmDto.setRiskLevel(Constants.LOW);
        riskAlgorithmDto.setAvgSystolic(22D);
        riskAlgorithmDto.setAvgDiastolic(22D);

        // When
        String symptomRiskLevel = riskAlgorithm.calculateRiskLevelFromSymptoms(symptomsCount, riskAlgorithmDto);

        // Then
        assertEquals(Constants.HIGH, symptomRiskLevel);
    }

    @Test
    void testCalculateRiskLevelFromBpReading_HighRisk() {
        // Given
        Double avgSystolic = 180.0;
        Double avgDiastolic = 110.0;
        String existingRiskLevel = Constants.MODERATE;
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String riskLevel = riskAlgorithm.calculateRiskLevelFromBpReading(avgSystolic, avgDiastolic, existingRiskLevel, symptoms);

        // Then
        assertEquals(Constants.HIGHER_MODERATE, riskLevel);
    }

    @Test
    void testCalculateRiskLevelFromBpReading_LowerModerate() {
        // Given
        Double avgSystolic = 145.0;
        Double avgDiastolic = 95.0;
        String existingRiskLevel = Constants.LOWER_MODERATE;
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String riskLevel = riskAlgorithm.calculateRiskLevelFromBpReading(avgSystolic, avgDiastolic, existingRiskLevel, symptoms);

        // Then
        assertEquals(Constants.LOWER_MODERATE, riskLevel);
    }

    @Test
    void testCalculateRiskLevelFromBpReading_Low() {
        // Given
        Double avgSystolic = 100.0;
        Double avgDiastolic = 70.0;
        String existingRiskLevel = Constants.LOW;
        List<Symptom> symptoms = List.of(TestDataProvider.getSymptom());

        // When
        String riskLevel = riskAlgorithm.calculateRiskLevelFromBpReading(avgSystolic, avgDiastolic, existingRiskLevel, symptoms);

        // Then
        assertEquals(Constants.LOW, riskLevel);
    }

    @Test
    void testGetPatientSymptomsCategoriesCount() {
        // Given
        Symptom symptom1 = new Symptom();
        symptom1.setId(1L);
        symptom1.setName("Symptom1");
        symptom1.setCategories(Map.of(
                Constants.STRING_TWO, true, Constants.STRING_FOUR, true));
        when(staticDataService.getSymptoms()).thenReturn(List.of(symptom1));
        riskAlgorithmDto.setSymptoms(new HashSet<>());

        // When
        Map<String, Integer> result = riskAlgorithm.getPatientSymptomsCategoriesCount(riskAlgorithmDto);

        // Then
        assertNotNull(result);
    }

    @Test
    void testCalculateSymptomsCategoriesCount() {
        // Given
        Set<Long> symptoms = Set.of(1L);
        Map<String, List<Long>> symptomsCategories = new HashMap<>();
        symptomsCategories.put(Constants.CATEGORY_TWO, List.of(1L));
        symptomsCategories.put(Constants.CATEGORY_THREE, List.of(1L));
        symptomsCategories.put(Constants.CATEGORY_FOUR, List.of(1L));
        symptomsCategories.put(Constants.CATEGORY_FIVE, List.of(1L));

        // When
        Map<String, Integer> result = riskAlgorithm.calculateSymptomsCategoriesCount(symptoms, symptomsCategories);

        // Then
        assertEquals(1, result.get(Constants.CATEGORY_TWO_COUNT));
        assertEquals(1, result.get(Constants.CATEGORY_THREE_COUNT));
        assertEquals(1, result.get(Constants.CATEGORY_FOUR_COUNT));
        assertEquals(1, result.get(Constants.CATEGORY_FIVE_COUNT));
    }
}
