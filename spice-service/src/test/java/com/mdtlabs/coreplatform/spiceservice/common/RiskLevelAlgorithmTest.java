package com.mdtlabs.coreplatform.spiceservice.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;

import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RiskLevelAlgorithmTest {
    @InjectMocks
    RiskLevelAlgorithm riskLevelAlgorithm;

    @Test
    void calculatePhq9RiskLevel_1() {
        int phq9RiskScore = 2;
        //then
        String response = riskLevelAlgorithm.calculatePhq9RiskLevel(phq9RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq9RiskLevel_5() {
        int phq9RiskScore = 6;
        //then
        String response = riskLevelAlgorithm.calculatePhq9RiskLevel(phq9RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq9RiskLevel_10() {
        int phq9RiskScore = 11;
        //then
        String response = riskLevelAlgorithm.calculatePhq9RiskLevel(phq9RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq9RiskLevel_15() {
        int phq9RiskScore = 16;
        //then
        String response = riskLevelAlgorithm.calculatePhq9RiskLevel(phq9RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq9RiskLevel_20() {
        int phq9RiskScore = 21;
        //then
        String response = riskLevelAlgorithm.calculatePhq9RiskLevel(phq9RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculateGad7RiskLevel_1() {
        int gad7RiskScore = 2;
        //then
        String response = riskLevelAlgorithm.calculateGad7RiskLevel(gad7RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculateGad7RiskLevel_3() {
        int gad7RiskScore = 4;
        //then
        String response = riskLevelAlgorithm.calculateGad7RiskLevel(gad7RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculateGad7RiskLevel_6() {
        int gad7RiskScore = 7;
        //then
        String response = riskLevelAlgorithm.calculateGad7RiskLevel(gad7RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculateGad7RiskLevel_9() {
        int gad7RiskScore = 10;
        //then
        String response = riskLevelAlgorithm.calculateGad7RiskLevel(gad7RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq4RiskLevel_0() {
        int phq4RiskScore = 1;
        //then
        String response = riskLevelAlgorithm.calculatePhq4RiskLevel(phq4RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq4RiskLevel_4() {
        int phq4RiskScore = 5;
        //then
        String response = riskLevelAlgorithm.calculatePhq4RiskLevel(phq4RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq4RiskLevel_6() {
        int phq4RiskScore = 7;
        //then
        String response = riskLevelAlgorithm.calculatePhq4RiskLevel(phq4RiskScore);
        Assertions.assertNotNull(response);
    }

    @Test
    void calculatePhq4RiskLevel_others() {
        int phq4RiskScore = 10;
        //then
        String response = riskLevelAlgorithm.calculatePhq4RiskLevel(phq4RiskScore);
        Assertions.assertNotNull(response);
    }



}
