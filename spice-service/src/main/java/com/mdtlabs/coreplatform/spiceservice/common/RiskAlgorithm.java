package com.mdtlabs.coreplatform.spiceservice.common;

import com.mdtlabs.coreplatform.spiceservice.common.dto.RiskAlgorithmDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;
import com.mdtlabs.coreplatform.spiceservice.symptom.service.SymptomService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 * This class is for risk algorithm calculation.
 * </p>
 *
 * @author Gopinath created on Aug 21, 2024
 */
@Component
public class RiskAlgorithm {

    private final StaticDataService staticDataService;

    private final SymptomService symptomService;

    public RiskAlgorithm(StaticDataService staticDataService, SymptomService symptomService) {
        this.staticDataService = staticDataService;
        this.symptomService = symptomService;
    }

    /**
     * <p>
     * This function calculates the risk level based on pregnancy status and blood pressure readings.
     * </p>
     *
     * @param riskAlgorithmDto {@link RiskAlgorithmDTO} It is an object of type RiskAlgorithmDTO which contains information
     *                         related to a patient's risk assessment, such as their blood pressure readings, symptoms, and
     *                         pregnancy status is given
     * @return {@link String} The method is returning a String variable named "riskLevel" is returned
     */
    public String getRiskLevelInAssessmentDbm(RiskAlgorithmDTO riskAlgorithmDto) {
        if (Boolean.TRUE.equals(riskAlgorithmDto.getIsPregnant())) {
            return Constants.HIGH;
        }
        List<Symptom> symptoms = (!Objects.isNull(riskAlgorithmDto.getNcdSymptoms()) && !riskAlgorithmDto.getNcdSymptoms().isEmpty()) ?
                symptomService.getSymptoms(riskAlgorithmDto.getNcdSymptoms()) : new ArrayList<>();
        String bpRiskLevel = null;
        if (!Objects.isNull(riskAlgorithmDto.getAvgSystolic()) && !Objects.isNull(riskAlgorithmDto.getAvgDiastolic())) {
            bpRiskLevel = calculateRiskLevelFromBpReading(riskAlgorithmDto.getAvgSystolic(),
                    riskAlgorithmDto.getAvgDiastolic(), riskAlgorithmDto.getRiskLevel(), symptoms);
        }
        String bgRiskLevel = calculateRiskLevelFromGlucoseData(riskAlgorithmDto.getGlucoseValue(),
                riskAlgorithmDto.getGlucoseType(), symptoms);
        return calculateRisk(bpRiskLevel, bgRiskLevel);
    }

    /**
     * This method is used to calculate risk level form bp and bg risk level
     *
     * @param bpRiskLevel - bp risk level
     * @param bgRiskLevel - bg risk level
     * @return String - risk level
     */
    public String calculateRisk(String bpRiskLevel, String bgRiskLevel) {
        if (Objects.isNull(bpRiskLevel) && Objects.isNull(bgRiskLevel)) {
            return null;
        }
        int bpRiskLevelIndex = Constants.RED_RISK_ORDER.indexOf(bpRiskLevel);
        int bgRiskLevelIndex = Constants.RED_RISK_ORDER.indexOf(bgRiskLevel);
        return bgRiskLevelIndex >= bpRiskLevelIndex ? bgRiskLevel : bpRiskLevel;
    }

    /**

     * This method is used to calculate risk level from glucose data.
     *
     * @param glucoseValue      - glucose value
     * @param glucoseType       - glucose type
     * @param symptoms          - List of symptoms
     * @return String - risk level
     */
    public String calculateRiskLevelFromGlucoseData(Double glucoseValue, String glucoseType, List<Symptom> symptoms) {
        String riskLevel = null;
        List<Symptom> bgSymptoms = symptoms.stream().filter(symptom -> !Objects.isNull(symptom.getType()) && symptom.getType().equals(Constants.DIABETES)).toList();
        boolean isRedRiskSymptoms = bgSymptoms.stream().anyMatch(obj -> !Objects.isNull(obj.getCategory()) && obj.getCategory().equals(Constants.RED_RISK));
        if (Objects.isNull(glucoseValue)) {
            return null;
        }
        if ((glucoseType.equals(Constants.FBS) && (glucoseValue >= 4.4 && glucoseValue <= 7.2))
                || (glucoseType.equals(Constants.RBS) && (glucoseValue >= 4.4 && glucoseValue <= 9.9))) {
            riskLevel = Constants.LOW;
        }

        if (((glucoseType.equals(Constants.FBS) && ((glucoseValue >= 7.3 && glucoseValue <= 13.9) || (glucoseValue >= 3.9 && glucoseValue <= 4.4)))
                || (glucoseType.equals(Constants.RBS) && ((glucoseValue >= 10 && glucoseValue <= 13.9)
                || (glucoseValue >= 3.9 && glucoseValue <= 4.4)))) && !isRedRiskSymptoms) {
            riskLevel = Constants.LOWER_MODERATE;
        }

        if ((glucoseValue >= 13.9 && glucoseValue <= 16.7) && !isRedRiskSymptoms) {
            riskLevel = Constants.MODERATE;
        }

        if (((glucoseValue >= 16.7 && glucoseValue <= 27.8) || glucoseValue < 3.9) && !isRedRiskSymptoms) {
            riskLevel = Constants.HIGHER_MODERATE;
        }

        if (glucoseValue > 27.8 || isRedRiskSymptoms) {
            riskLevel = Constants.HIGH;
        }
        return riskLevel;
    }

    /**
     * <p>
     * The function calculates the risk level of a patient based on their symptoms and other health
     * data using a set of predefined rules.
     * </p>
     *
     * @param patientSymptomsCount {@link Map<String, Integer>} A map containing the count of different symptom categories for a
     *                             patient is given
     * @param riskAlgorithmDto     {@link RiskAlgorithmDTO} RiskAlgorithmDTO is an object that contains information related to the
     *                             patient's vital signs and other risk factors that can contribute to their overall risk level is given
     * @return {@link String} The method is returning a String value which represents the calculated risk level based
     * on the patient's symptoms count and the risk algorithm DTO is returned
     */
    public String calculateRiskLevelFromSymptoms(Map<String, Integer> patientSymptomsCount,
                                                 RiskAlgorithmDTO riskAlgorithmDto) {
        String symptomRiskLevel = riskAlgorithmDto.getRiskLevel();
        if (patientSymptomsCount.get(Constants.CATEGORY_FOUR_COUNT) >= Constants.ONE) {
            symptomRiskLevel = Constants.HIGHER_MODERATE;
        }
        if (patientSymptomsCount.get(Constants.CATEGORY_FIVE_COUNT) >= Constants.ONE
                || patientSymptomsCount.get(Constants.CATEGORY_TWO_COUNT) >= Constants.ONE) {
            symptomRiskLevel = Constants.HIGH;
        }
        if ((riskAlgorithmDto.getAvgSystolic() <= 90 || riskAlgorithmDto.getAvgDiastolic() <= 60)
                && patientSymptomsCount.get(Constants.CATEGORY_THREE_COUNT) >= Constants.ONE) {
            symptomRiskLevel = Constants.HIGH;
        }
        return symptomRiskLevel;
    }

    /**
     * <p>
     * This Java function returns a map of the count of symptoms in different categories based on a
     * given risk algorithm DTO.
     * </p>
     *
     * @param riskAlgorithmDTO {@link RiskAlgorithmDTO} It is an object of type RiskAlgorithmDTO which contains information
     *                         related to a risk algorithm. It is used to calculate the risk of a patient based on their
     *                         symptoms is given
     * @return {@link Map} A Map<String, Integer> containing the count of symptoms in each category (categories 2,
     * 3, 4, and 5) for the given RiskAlgorithmDTO is returned
     */
    public Map<String, Integer> getPatientSymptomsCategoriesCount(RiskAlgorithmDTO riskAlgorithmDTO) {
        Map<String, List<Long>> symptomsCategories = Map.of(Constants.CATEGORY_TWO, new ArrayList<>(),
                Constants.CATEGORY_THREE, new ArrayList<>(), Constants.CATEGORY_FOUR, new ArrayList<>(),
                Constants.CATEGORY_FIVE, new ArrayList<>());
        List<Symptom> symptoms = staticDataService.getSymptoms();
        for (Symptom symptom : symptoms) {
            if (!Constants.OTHER.equals(symptom.getName())) {
                addSymptomCategories(symptomsCategories, symptom);
            }
        }
        return calculateSymptomsCategoriesCount(riskAlgorithmDTO.getSymptoms(), symptomsCategories);

    }

    /**
     * <p>
     * The function calculates the count of symptoms in different categories for a given set of patient
     * symptoms and a map of symptom categories.
     * </p>
     *
     * @param patientSymptoms    {@link Set<Long>} A set of Long values representing the IDs of symptoms that a patient is
     *                           experiencing is given
     * @param symptomsCategories {@link Map> } A map where the keys are category names (strings) and the values are
     *                           lists of symptom IDs (longs) that belong to that category is given
     * @return {@link Map} The method is returning a Map<String, Integer> object which contains the count of
     * patient symptoms in each category defined in the symptomsCategories map is returned
     */
    public Map<String, Integer> calculateSymptomsCategoriesCount(Set<Long> patientSymptoms,
                                                                 Map<String, List<Long>> symptomsCategories) {
        Map<String, Integer> symptomsCategoriesCount = new HashMap<>();
        symptomsCategoriesCount.put(Constants.CATEGORY_TWO_COUNT, Constants.ZERO);
        symptomsCategoriesCount.put(Constants.CATEGORY_THREE_COUNT, Constants.ZERO);
        symptomsCategoriesCount.put(Constants.CATEGORY_FOUR_COUNT, Constants.ZERO);
        symptomsCategoriesCount.put(Constants.CATEGORY_FIVE_COUNT, Constants.ZERO);

        for (Long symptomId : patientSymptoms) {
            if (symptomsCategories.get(Constants.CATEGORY_TWO).contains(symptomId)) {
                symptomsCategoriesCount.replace(Constants.CATEGORY_TWO_COUNT,
                        symptomsCategoriesCount.get(Constants.CATEGORY_TWO_COUNT) + Constants.ONE);
            }
            if (symptomsCategories.get(Constants.CATEGORY_THREE).contains(symptomId)) {
                symptomsCategoriesCount.replace(Constants.CATEGORY_THREE_COUNT,
                        symptomsCategoriesCount.get(Constants.CATEGORY_THREE_COUNT) + Constants.ONE);
            }
            if (symptomsCategories.get(Constants.CATEGORY_FOUR).contains(symptomId)) {
                symptomsCategoriesCount.replace(Constants.CATEGORY_FOUR_COUNT,
                        symptomsCategoriesCount.get(Constants.CATEGORY_FOUR_COUNT) + Constants.ONE);
            }
            if (symptomsCategories.get(Constants.CATEGORY_FIVE).contains(symptomId)) {
                symptomsCategoriesCount.replace(Constants.CATEGORY_FIVE_COUNT,
                        symptomsCategoriesCount.get(Constants.CATEGORY_FIVE_COUNT) + Constants.ONE);
            }

        }
        return symptomsCategoriesCount;
    }

    /**
     * <p>
     * The function adds symptom IDs to their respective categories in a map based on the categories
     * specified in the symptom object.
     * </p>
     *
     * @param symptomsCategories {@link Map>} A map that contains symptom categories as keys and lists of symptom
     *                           IDs as values is given
     * @param symptom            {@link Symptom} an object of type Symptom that contains information about a particular symptom,
     *                           including its ID and categories is returned
     */
    private void addSymptomCategories(Map<String, List<Long>> symptomsCategories, Symptom symptom) {
        if (!Objects.isNull(symptom.getCategories())
                && Boolean.TRUE.equals(symptom.getCategories().get(Constants.STRING_TWO))) {
            symptomsCategories.get(Constants.CATEGORY_TWO).add(symptom.getId());
        }
        if (!Objects.isNull(symptom.getCategories())
                && Boolean.TRUE.equals(symptom.getCategories().get(Constants.STRING_THREE))) {
            symptomsCategories.get(Constants.CATEGORY_THREE).add(symptom.getId());
        }
        if (!Objects.isNull(symptom.getCategories())
                && Boolean.TRUE.equals(symptom.getCategories().get(Constants.STRING_FOUR))) {
            symptomsCategories.get(Constants.CATEGORY_FOUR).add(symptom.getId());
        }
        if (!Objects.isNull(symptom.getCategories())
                && Boolean.TRUE.equals(symptom.getCategories().get(Constants.STRING_FIVE))) {
            symptomsCategories.get(Constants.CATEGORY_FIVE).add(symptom.getId());
        }
    }

    /**
     * <p>
     * This Java function calculates the risk level based on average systolic and diastolic blood
     * pressure readings, and updates the risk level if certain conditions are met.
     * </p>
     *
     * @param avgSystolic       {@link Integer} The average systolic blood pressure of a patient is given
     * @param avgDiastolic      {@link Integer} The average diastolic blood pressure of a patient is given
     * @param existingRiskLevel {@link String} The current risk level of the patient, which is a String value is given
     * @return The method returns a String representing the calculated risk level based on the average
     * systolic and diastolic blood pressure readings, along with the existing risk level is returned
     */
    public String calculateRiskLevelFromBpReading(Double avgSystolic, Double avgDiastolic, String existingRiskLevel, List<Symptom> symptoms) {
        int maxSystolic = 140;
        int maxDiastolic = 90;
        int minSystolic = 90;
        int minDiastolic = 60;
        String riskLevel = null;
        List<Symptom> bpSymptoms = symptoms.stream().filter(symptom -> symptom.getType().equals(Constants.HYPERTENSION)).toList();
        boolean isRedRiskSymptoms = bpSymptoms.stream().anyMatch(obj -> obj.getCategory().equals(Constants.RED_RISK));
        if ((avgSystolic >= 180 || avgDiastolic >= 110) || (avgSystolic < 90 || avgDiastolic < 60)) {
            riskLevel = isRedRiskSymptoms ? Constants.HIGH : Constants.HIGHER_MODERATE;
        } else if ((avgSystolic >= 160 && avgSystolic <= 179) || (avgDiastolic >= 100 && avgDiastolic <= 109)) {
            riskLevel = Constants.MODERATE;
        } else if ((avgSystolic >= 140 && avgSystolic <= 159) || (avgDiastolic >= 90 && avgDiastolic <= 99)) {
            riskLevel = Constants.LOWER_MODERATE;
        } else if ((avgSystolic >= minSystolic && avgSystolic < maxSystolic) &&
                (avgDiastolic >= minDiastolic && avgDiastolic < maxDiastolic)) {
            riskLevel = Constants.LOW;
        }
        return riskLevel;
    }

}
