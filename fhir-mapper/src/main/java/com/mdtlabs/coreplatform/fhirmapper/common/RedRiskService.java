package com.mdtlabs.coreplatform.fhirmapper.common;

import java.util.Objects;

import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.RedRiskDTO;

/**
 * <p>
 * This an interface class for redrisk module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Sep 2, 2024
 */
@Service
public class RedRiskService {


    /**
     * <p>
     * Get patient risk level only if patient enrolled and completed the initial
     * medical review.
     * </p>
     * @param redRiskDto - dto
     * @return redRisk - dto
     */
    public String getPatientRiskLevel(RedRiskDTO redRiskDto) {
        return getRiskLevelForNewPatient(redRiskDto);
    }


    /**
     * Get risk level for new patient based on patient comorbidities, risk factors,
     * diabetes diagnosis and average BP.
     *
     * @param redRiskDto     RedRiskDTO object
     * @return riskLevel of the new Patient.
     */
    public String getRiskLevelForNewPatient(RedRiskDTO redRiskDto) {
        String riskLevel = null;
        if (!Objects.isNull(redRiskDto.getPatientObservationDetails().getIsPregnant()) && Boolean.TRUE.equals(redRiskDto.getPatientObservationDetails().getIsPregnant())) {
            riskLevel = Constants.HIGH;
        } else if (Constants.ONE <= redRiskDto.getComorbiditiesCount() || Objects
            .equals(redRiskDto.getDiabetesDiagControlledType(), Constants.DIABETES_UNCONTROLLED_OR_POORLY_CONTROLLED)) {
            riskLevel = Constants.HIGH;
        } else {
            int riskFactorsCount = getRiskFactorsCount(redRiskDto);
            riskLevel = calculateRiskFromBpAndRiskFactors(riskFactorsCount, redRiskDto);
        }
        return riskLevel;
    }


    /**
     * Get patient Risk factor count.
     *
     * @param redRiskDTO RedRiskDTO object
     * @return riskFactorCount risk factor count
     */
    public int getRiskFactorsCount(RedRiskDTO redRiskDTO) {
        int riskFactorCount = Constants.ZERO;
        if (Constants.GENDER_MALE.equals(redRiskDTO.getPatientObservationDetails().getGender())) {
            riskFactorCount++;
        }

        if ((Constants.GENDER_MALE.equals(redRiskDTO.getPatientObservationDetails().getGender()) && redRiskDTO.getPatientObservationDetails().getAge() >= 55)
            || (redRiskDTO.getPatientObservationDetails().getGender().equals(Constants.GENDER_FEMALE) && redRiskDTO.getPatientObservationDetails().getAge() >= 65)) {
            riskFactorCount++;
        }

        if (Boolean.TRUE.equals(redRiskDTO.getPatientObservationDetails().getIsRegularSmoker())) {
            riskFactorCount++;
        }

        if (!Objects.isNull(redRiskDTO.getPatientObservationDetails().getGlucoseType()) 
            && !Objects.isNull(redRiskDTO.getPatientObservationDetails().getGlucoseValue())
            && redRiskDTO.getPatientObservationDetails().getGlucoseType().equals(Constants.FBS)
            && redRiskDTO.getPatientObservationDetails().getGlucoseValue() >= 5.6 && redRiskDTO.getPatientObservationDetails().getGlucoseValue() <= 6.9) {
            riskFactorCount++;
        }
        if (redRiskDTO.getPatientObservationDetails().getBmi() >= 30) {
            riskFactorCount++;
        }
        return riskFactorCount;
    }

    /**
     * Calculate the patient risk level from patient BP level, risk factors count
     * and diabetes diagnosis status.
     *
     * @param riskFactorsCount riskFactor count
     * @param redRiskDto       ResRiskDTO object
     * @return risk level of the patient.
     * @author Niraimathi S
     */
    private String calculateRiskFromBpAndRiskFactors(int riskFactorsCount,
        RedRiskDTO redRiskDto) {
        String riskLevel = null;
        if (Objects.nonNull(redRiskDto.getPatientObservationDetails().getAvgSystolic())
                && Objects.nonNull(redRiskDto.getPatientObservationDetails().getAvgDiastolic())) {
            if ((redRiskDto.getPatientObservationDetails().getAvgSystolic() >= 180 || redRiskDto.getPatientObservationDetails().getAvgDiastolic() >= 110)) {
                riskLevel = Constants.HIGH;
            } else if ((redRiskDto.getPatientObservationDetails().getAvgSystolic() >= 160 && redRiskDto.getPatientObservationDetails().getAvgSystolic() <= 179)
                    || (redRiskDto.getPatientObservationDetails().getAvgDiastolic() >= 100 && redRiskDto.getPatientObservationDetails().getAvgDiastolic() <= 109)) {
                riskLevel = setsRiskLevel(riskFactorsCount, redRiskDto);
            } else if ((redRiskDto.getPatientObservationDetails().getAvgSystolic() >= 140 && redRiskDto.getPatientObservationDetails().getAvgSystolic() <= 159)
                || (redRiskDto.getPatientObservationDetails().getAvgDiastolic() >= 90 && redRiskDto.getPatientObservationDetails().getAvgDiastolic() <= 99)) {
                riskLevel = setRiskLevel(riskFactorsCount, redRiskDto);
            } else if (redRiskDto.getPatientObservationDetails().getAvgSystolic() <= 139 || redRiskDto.getPatientObservationDetails().getAvgDiastolic() <= 89) {
                riskLevel = setRiskLevelOnCount(riskFactorsCount, redRiskDto);
            }
        }
        return riskLevel;
    }

        /**
     * <p>
     * Sets risk level by risk factors count.
     * </p>
     *
     * @param riskFactorsCount - risk factor count
     * @param redRiskDto - red risk dto
     * @return String - risk level
     */
    private String setsRiskLevel(int riskFactorsCount, RedRiskDTO redRiskDto) {
        String riskLevel;
        if (riskFactorsCount > 0 || Objects.equals(redRiskDto.getDiabetesDiagControlledType(), Constants.DIABETES_WELL_CONTROLLED)
                || Objects
                .equals(redRiskDto.getDiabetesDiagControlledType(), Constants.PRE_DIABETES)) {
            riskLevel = Constants.HIGH;
        } else {
            riskLevel = Constants.MODERATE;
        }
        return riskLevel;
    }

    /**
     * <p>
     * Sets risk level on count.
     * </p>
     *
     * @param riskFactorsCount - risk factors count
     * @param redRiskDto - red risk dto
     * @return String - risk level
     */
    private String setRiskLevelOnCount(int riskFactorsCount, RedRiskDTO redRiskDto) {
        String riskLevel;
        if (riskFactorsCount >= 3
                || Objects
                .equals(redRiskDto.getDiabetesDiagControlledType(), Constants.DIABETES_WELL_CONTROLLED)) {
            riskLevel = Constants.MODERATE;
        } else {
            riskLevel = Constants.LOW;
        }
        return riskLevel;
    }

    /**
     * <p>
     * Sets risk level on count.
     * </p>
     *
     * @param riskFactorsCount - risk factors count
     * @param redRiskDto - red risk dto
     * @return String - risk level
     */
    private String setRiskLevel(int riskFactorsCount, RedRiskDTO redRiskDto) {
        String riskLevel;
        if (riskFactorsCount >= 3
                || Objects
                .equals(redRiskDto.getDiabetesDiagControlledType(), Constants.DIABETES_WELL_CONTROLLED)) {
            riskLevel = Constants.HIGH;
        } else if ((riskFactorsCount > 0 && riskFactorsCount <= 2)
                || Objects
                .equals(redRiskDto.getDiabetesDiagControlledType(), Constants.PRE_DIABETES)) {
            riskLevel = Constants.MODERATE;
        } else {
            riskLevel = Constants.LOW;
        }
        return riskLevel;
    }

}