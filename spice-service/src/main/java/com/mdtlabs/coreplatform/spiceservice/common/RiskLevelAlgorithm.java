package com.mdtlabs.coreplatform.spiceservice.common;

import org.springframework.stereotype.Service;

/**
 * <p>
 * This class is for risk level algorithm calculation.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 13, 2024
 */
@Service
public class RiskLevelAlgorithm {

    /**
     * <p>
     * Determines the PHQ-9 risk level based on the provided PHQ-9 score.
     * </p>
     *
     * @param phq9RiskScore The PHQ-9 score, which is an integer between 0 and 27 inclusive.
     * @return A string representing the PHQ-9 risk level.
     *         Possible values are defined in the Constants class: MINIMAL, MILD, MODERATE, MODERATELY_SEVERE, SEVERE.
     */
    public String calculatePhq9RiskLevel(int phq9RiskScore) {
        String phq9RiskLevel = null;
        if (phq9RiskScore >= 1 && phq9RiskScore <= 4) {
            phq9RiskLevel = Constants.MINIMAL;
        } else if (phq9RiskScore >= 5 && phq9RiskScore <= 9) {
            phq9RiskLevel = Constants.MILD;
        } else if (phq9RiskScore >= 10 && phq9RiskScore <= 14) {
            phq9RiskLevel = Constants.MODERATE;
        } else if (phq9RiskScore >= 15 && phq9RiskScore <= 19) {
            phq9RiskLevel = Constants.MODERATELY_SEVERE;
        } else if (phq9RiskScore >= 20 && phq9RiskScore <= 27) {
            phq9RiskLevel = Constants.SEVERE;
        }
        return phq9RiskLevel;
    }

    /**
     * <p>
     * Determines the PHQ-9 risk level based on the provided PHQ-9 score.
     * </p>
     *
     * @param gad7RiskScore The PHQ-9 score, which is an integer between 0 and 27 inclusive.
     * @return A string representing the PHQ-9 risk level.
     *         Possible values are defined in the Constants class: NORMAL, MILD, MODERATE, SEVERE.
     */
    public String calculateGad7RiskLevel(int gad7RiskScore) {
        String gad7RiskLevel = null;
        if (gad7RiskScore >= 1 && gad7RiskScore <= 2) {
            gad7RiskLevel = Constants.NORMAL;
        } else if (gad7RiskScore >= 3 && gad7RiskScore <= 5) {
            gad7RiskLevel = Constants.MILD;
        } else if (gad7RiskScore >= 6 && gad7RiskScore <= 8) {
            gad7RiskLevel = Constants.MODERATE;
        } else if (gad7RiskScore >= 9 && gad7RiskScore <= 21) {
            gad7RiskLevel = Constants.SEVERE;
        }
        return gad7RiskLevel;
    }

    /**
     * <p>
     * Determines the PHQ-9 risk level based on the provided PHQ-9 score.
     * </p>
     *
     * @param phq4RiskScore The PHQ-9 score, which is an integer between 0 and 27 inclusive.
     * @return A string representing the PHQ-9 risk level.
     *         Possible values are defined in the Constants class: NORMAL, MILD, MODERATE, SEVERE.
     */
    public String calculatePhq4RiskLevel(int phq4RiskScore) {
        String phq4RiskLevel;
        if(phq4RiskScore >= 0 && phq4RiskScore <= 3) {
            phq4RiskLevel = Constants.NORMAL;
        } else if(phq4RiskScore >= 4 && phq4RiskScore <= 5) {
            phq4RiskLevel = Constants.MILD;
        } else if(phq4RiskScore >= 6 && phq4RiskScore <= 8) {
            phq4RiskLevel = Constants.MODERATE;
        } else {
            phq4RiskLevel = Constants.SEVERE;
        }
        return phq4RiskLevel;
    }
}
