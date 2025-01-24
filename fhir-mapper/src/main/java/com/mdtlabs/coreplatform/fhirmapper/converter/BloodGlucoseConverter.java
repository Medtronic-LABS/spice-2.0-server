package com.mdtlabs.coreplatform.fhirmapper.converter;


import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Quantity;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiabetesDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

/**
 * <p>
 * Converts GlucoseLogDTO to FHIR Observation entity for Blood Glucose observations.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class BloodGlucoseConverter {
    private final FhirUtils fhirUtils;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    public BloodGlucoseConverter(FhirUtils fhirUtils,
            FhirAssessmentMapper fhirAssessmentMapper) {
        this.fhirUtils = fhirUtils;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
    }



    /**
     * Converts GlucoseLogDTO to FHIR Observation entity.
     *
     * @param glucoseLogDTO   The glucose log details to convert.
     *
     * @return The FHIR Observation entity representing the Blood Glucose observation.
     */
    public Observation createBloodGlucoseObservation(GlucoseLogDTO glucoseLogDTO) {
        Observation glucoseLogObservation = new Observation();
        glucoseLogObservation.setStatus(Observation.ObservationStatus.FINAL);
        DateTimeType effectiveDateTime;
        glucoseLogObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL).setValue(
                Constants.OBSERVATION_BLOOD_GLUCOSE);
        glucoseLogObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.BLOOD_GLUCOSE_KEY));
        Quantity glucoseQuantity = new Quantity();
        if (Objects.nonNull(glucoseLogDTO.getGlucoseValue())) {
            glucoseQuantity.setValue(glucoseLogDTO.getGlucoseValue());
            glucoseQuantity.setUnit(glucoseLogDTO.getGlucoseUnit());
        }
        glucoseLogObservation.setValue(glucoseQuantity);
        setHba1cComponent(glucoseLogDTO, glucoseLogObservation);
        setGlucoseTypeObservation(glucoseLogDTO, glucoseLogObservation);
        if (Objects.nonNull(glucoseLogDTO.getGlucoseDateTime())) {
            effectiveDateTime = new DateTimeType(glucoseLogDTO.getGlucoseDateTime());
        } else {
            effectiveDateTime = new DateTimeType(new Date());
        }
        glucoseLogObservation.setEffective(effectiveDateTime);

        if (Objects.nonNull(glucoseLogDTO.getLastMealTime())) {
            Observation.ObservationComponentComponent lastMealTimeObservation =
                    new Observation.ObservationComponentComponent();
            DateTimeType lastMealTime = new DateTimeType(glucoseLogDTO.getLastMealTime());

            lastMealTimeObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.LAST_MEAL_TIME_KEY));
            lastMealTimeObservation.setValue(lastMealTime);
            glucoseLogObservation.addComponent(lastMealTimeObservation);
        }

        fhirAssessmentMapper.createObservationComponent(glucoseLogDTO.getIsBeforeDiabetesDiagnosis(), 
            MetaCodeConstants.HAVE_YOU_BEEN_DIAGNOSED_WITH_HIGH_BLOOD_SUGAR_BEFORE_KEY, glucoseLogObservation.getComponent());

        return glucoseLogObservation;
    }

    /**
     * <p>
     *     Used to set the glucose type component to glucose observation
     * <p/>
     *
     * @param glucoseLogDTO - Patient glucose log details
     * @param observation - Patient glucose log observation
     */
    private void setGlucoseTypeObservation(GlucoseLogDTO glucoseLogDTO, Observation observation) {
        if (Objects.nonNull(glucoseLogDTO.getGlucoseType())) {
            Observation.ObservationComponentComponent glucoseTypeComponent
                    = new Observation.ObservationComponentComponent();
            glucoseTypeComponent.setCode(fhirUtils.setCodes(glucoseLogDTO.getGlucoseType()));
            glucoseTypeComponent.getCode().setText(glucoseLogDTO.getGlucoseType());
            observation.addComponent(glucoseTypeComponent);
        }
    }

    /**
     * <p>
     *     Used to set the hba1c component to glucose observation
     * <p/>
     *
     * @param glucoseLogDTO - Patient glucose log details
     * @param observation - Patient glucose log observation
     */
    private void setHba1cComponent(GlucoseLogDTO glucoseLogDTO, Observation observation) {
        if (Objects.nonNull(glucoseLogDTO.getHba1c())) {
            Observation.ObservationComponentComponent hba1cObservation
                    = new Observation.ObservationComponentComponent();
            hba1cObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.HBA1C));
            Quantity hba1cValue = new Quantity();
            hba1cValue.setValue(glucoseLogDTO.getHba1c());
            hba1cValue.setUnit(glucoseLogDTO.getHba1cUnit());
            hba1cObservation.setValue(hba1cValue);
            observation.addComponent(hba1cObservation);
        }
    }

    /**
     * Converts the given observation to the glucose log information.
     *
     * @param observation the blood glucose log observation
     * @return GlucoseLogDTO
     */
    public GlucoseLogDTO convertObservationToGlucoseLogDTO(Observation observation) {
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        if (Objects.nonNull(observation.getValueQuantity())
                && Objects.nonNull(observation.getValueQuantity().getValue())) {
            glucoseLogDTO.setGlucoseUnit(observation.getValueQuantity().getUnit());
            glucoseLogDTO.setGlucoseValue(Double.parseDouble(observation.getValueQuantity().getValue().toString()));
        }
        List<DiabetesDTO> diabetesDTOS = new ArrayList<>();
        for (Observation.ObservationComponentComponent observationComponent : observation.getComponent()) {
            setComponents(glucoseLogDTO, diabetesDTOS, observationComponent);
        }
        glucoseLogDTO.setDiabetes(diabetesDTOS);

        if (Objects.nonNull(observation.getEffectiveDateTimeType())) {
            glucoseLogDTO.setGlucoseDateTime(observation.getEffectiveDateTimeType().getValue());
        }
        glucoseLogDTO.setCreatedAt(observation.getMeta().getLastUpdated());
        glucoseLogDTO.setEncounterId(fhirUtils.getIdFromReference(observation.getEncounter().getReference()));
        return glucoseLogDTO;
    }

    /**
     * Set the component of the glucose log
     *
     * @param glucoseLogDTO the glucose log to set the component
     * @param diabetesDTOS diabetes dto
     * @param observationComponent the component to set into the glucose log
     */
    private void setComponents(GlucoseLogDTO glucoseLogDTO, List<DiabetesDTO> diabetesDTOS,
                               Observation.ObservationComponentComponent observationComponent) {
        switch (observationComponent.getCode().getText()) {
            case MetaCodeConstants.HBA1C -> {
                glucoseLogDTO.setHba1cUnit(observationComponent.getValueQuantity().getUnit());
                glucoseLogDTO.setHba1c(Double.parseDouble(
                        observationComponent.getValueQuantity().getValue().toString()));
            }
            case MetaCodeConstants.LAST_MEAL_TIME_KEY ->
                    glucoseLogDTO.setLastMealTime(observationComponent.getValueDateTimeType().getValue());
            case MetaCodeConstants.HAVE_YOU_BEEN_DIAGNOSED_WITH_HIGH_BLOOD_SUGAR_BEFORE_KEY ->
                    glucoseLogDTO.setIsBeforeDiabetesDiagnosis(true);
            case MetaCodeConstants.FREQUENT_URINATION_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.FREQUENT_URINATION));
            case MetaCodeConstants.EXCESSIVE_THIRST_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.EXCESSIVE_THIRST));
            case MetaCodeConstants.SIGNIFICANT_HUNGER_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.SIGNIFICANT_HUNGER));
            case MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND));
            case MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN_KEY));
            case MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS));
            case MetaCodeConstants.UNPLANNED_WEIGHT_LOSS_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.UNPLANNED_WEIGHT_LOSS));
            case MetaCodeConstants.NO_SYMPTOMS_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.NO_SYMPTOMS));
            case MetaCodeConstants.FBS, MetaCodeConstants.RBS ->
                    glucoseLogDTO.setGlucoseType(observationComponent.getCode().getText());
            case MetaCodeConstants.OTHER_KEY ->
                    diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.OTHER));
            default -> diabetesDTOS.add(constructDiabetesDTO(MetaCodeConstants.BLANK_STRING));
        }
    }

    /**
     * Construct the diabetes dto
     *
     * @param name name of the diabetes
     * @return DiabetesDTO
     */
    private DiabetesDTO constructDiabetesDTO(String name) {
        DiabetesDTO diabetesDTO = new DiabetesDTO();
        diabetesDTO.setName(name);
        return diabetesDTO;
    }

}
