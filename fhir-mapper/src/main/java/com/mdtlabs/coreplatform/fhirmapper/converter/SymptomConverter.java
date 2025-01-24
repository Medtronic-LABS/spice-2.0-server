package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiabetesDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.SymptomDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 * Converts symptom DTO to FHIR Observation entity.
 * </p>
 *
 * @author Gopinath
 * @version 1.0
 * @since 2024-08-21
 */
@Component
public class SymptomConverter {
    private final FhirUtils fhirUtils;

    @Autowired
    public SymptomConverter(FhirUtils fhirUtils) {
        this.fhirUtils = fhirUtils;
    }

    /**
     * Converts symptom to FHIR Observation entity.
     *
     * @param symptoms List of patient symptoms.
     * @return The FHIR Observation entity representing.
     */
    public Observation createSymptomObservation(List<SymptomDTO> symptoms, Date recorderDate) {
        Observation symptomObservation = new Observation();
        symptomObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_PATIENT_SYMPTOM);
        DateTimeType effectiveDateTime;
        if (Objects.nonNull(recorderDate)) {
            effectiveDateTime = new DateTimeType(recorderDate);
        } else {
            effectiveDateTime = new DateTimeType(new Date());
        }
        for (SymptomDTO symptomDTO : symptoms) {
            Observation.ObservationComponentComponent symptomComponent =
                    new Observation.ObservationComponentComponent();
            switch (symptomDTO.getName()) {
                case MetaCodeConstants.FREQUENT_URINATION -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.FREQUENT_URINATION_KEY));
                case MetaCodeConstants.FAINTING -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.FAINTING_KEY));
                case MetaCodeConstants.SEIZURES -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SEIZURES_KEY));
                case MetaCodeConstants.BLURRY_VISION_AND_SUDDEN_VISION_LOSS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.BLURRY_VISION_AND_SUDDEN_VISION_LOSS_KEY));
                case MetaCodeConstants.NUMBNESS_ON_SIDE_OF_BODY -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.NUMBNESS_ON_SIDE_OF_BODY_KEY));
                case MetaCodeConstants.SWELLING_OF_LIMBS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SWELLING_OF_LIMBS_KEY));
                case MetaCodeConstants.DIZZINESS_UPON_STANDING -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.DIZZINESS_UPON_STANDING_KEY));
                case MetaCodeConstants.CHEST_PAIN_OR_DISCOMFORT -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.CHEST_PAIN_OR_DISCOMFORT_KEY));
                case MetaCodeConstants.SWELLING_OF_TONGUE_OR_LIPS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SWELLING_OF_TONGUE_OR_LIPS_KEY));
                case MetaCodeConstants.PALPITATIONS_OR_IRREGULAR_HEARTBEATS ->
                        symptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.PALPITATIONS_OR_IRREGULAR_HEARTBEATS_KEY));
                case MetaCodeConstants.SHORTNESS_OF_BREATH_WITH_USUAL_ACTIVITIES ->
                        symptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.SHORTNESS_OF_BREATH_WITH_USUAL_ACTIVITIES_KEY));
                case MetaCodeConstants.EXCESSIVE_THIRST -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.EXCESSIVE_THIRST_KEY));
                case MetaCodeConstants.SIGNIFICANT_HUNGER -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SIGNIFICANT_HUNGER_KEY));
                case MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND ->
                        symptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND_KEY));
                case MetaCodeConstants.FOOT_NUMBNESS_TINGLING_OR_PAIN -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN_KEY));
                case MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS_KEY));
                case MetaCodeConstants.UNPLANNED_WEIGHT_LOSS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.UNPLANNED_WEIGHT_LOSS_KEY));
                case MetaCodeConstants.NO_SYMPTOMS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.NO_SYMPTOMS_KEY));
                case MetaCodeConstants.OTHER -> {
                    symptomComponent.setCode(fhirUtils.setCodes(
                            MetaCodeConstants.OTHER_KEY));
                    if (Objects.nonNull(symptomDTO.getOtherSymptom())) {
                        symptomComponent.setValue(new StringType(symptomDTO.getOtherSymptom()));
                    }
                }
                default -> Logger.logInfo("No match found");
            }
            symptomObservation.addComponent(symptomComponent);
        }
        symptomObservation.setEffective(effectiveDateTime);

        return symptomObservation;
    }

    /**
     * <p>
     * Creates an Observation object to record a symptom based on the given GlucoseLogDTO and recording date.
     * </p>
     *
     * This method takes a GlucoseLogDTO object, which contains details about a glucose log entry,
     * and a Date object representing the date when the observation is recorded. It then creates
     * and returns an Observation object that encapsulates this information for further processing
     * or storage.
     *
     * @param glucoseLogDTO the data transfer object containing details about the glucose log entry
     * @param recorderDate the date when the observation is recorded
     * @return an Observation object representing the recorded symptom
     */
    public Observation createSymptomObservation(GlucoseLogDTO glucoseLogDTO, Date recorderDate) {
        List<DiabetesDTO> symptoms = glucoseLogDTO.getDiabetes();
        Observation symptomObservation = new Observation();
        symptomObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_PATIENT_SYMPTOM);
        DateTimeType effectiveDateTime;
        if (Objects.nonNull(recorderDate)) {
            effectiveDateTime = new DateTimeType(recorderDate);
        } else {
            effectiveDateTime = new DateTimeType(new Date());
        }
        for (DiabetesDTO symptomDTO : symptoms) {
            Observation.ObservationComponentComponent symptomComponent =
                    new Observation.ObservationComponentComponent();
            switch (symptomDTO.getName()) {
                case MetaCodeConstants.FREQUENT_URINATION -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.FREQUENT_URINATION_KEY));
                case MetaCodeConstants.FAINTING -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.FAINTING_KEY));
                case MetaCodeConstants.SEIZURES -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SEIZURES_KEY));
                case MetaCodeConstants.BLURRY_VISION_AND_SUDDEN_VISION_LOSS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.BLURRY_VISION_AND_SUDDEN_VISION_LOSS_KEY));
                case MetaCodeConstants.NUMBNESS_ON_SIDE_OF_BODY -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.NUMBNESS_ON_SIDE_OF_BODY_KEY));
                case MetaCodeConstants.SWELLING_OF_LIMBS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SWELLING_OF_LIMBS_KEY));
                case MetaCodeConstants.DIZZINESS_UPON_STANDING -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.DIZZINESS_UPON_STANDING_KEY));
                case MetaCodeConstants.CHEST_PAIN_OR_DISCOMFORT -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.CHEST_PAIN_OR_DISCOMFORT_KEY));
                case MetaCodeConstants.SWELLING_OF_TONGUE_OR_LIPS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SWELLING_OF_TONGUE_OR_LIPS_KEY));
                case MetaCodeConstants.PALPITATIONS_OR_IRREGULAR_HEARTBEATS ->
                        symptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.PALPITATIONS_OR_IRREGULAR_HEARTBEATS_KEY));
                case MetaCodeConstants.SHORTNESS_OF_BREATH_WITH_USUAL_ACTIVITIES ->
                        symptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.SHORTNESS_OF_BREATH_WITH_USUAL_ACTIVITIES_KEY));
                case MetaCodeConstants.EXCESSIVE_THIRST -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.EXCESSIVE_THIRST_KEY));
                case MetaCodeConstants.SIGNIFICANT_HUNGER -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SIGNIFICANT_HUNGER_KEY));
                case MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND ->
                        symptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND_KEY));
                case MetaCodeConstants.FOOT_NUMBNESS_TINGLING_OR_PAIN -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN_KEY));
                case MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS_KEY));
                case MetaCodeConstants.UNPLANNED_WEIGHT_LOSS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.UNPLANNED_WEIGHT_LOSS_KEY));
                case MetaCodeConstants.NO_SYMPTOMS -> symptomComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.NO_SYMPTOMS_KEY));
                case MetaCodeConstants.OTHER -> {
                    symptomComponent.setCode(fhirUtils.setCodes(
                            MetaCodeConstants.OTHER_KEY));
                    if (Objects.nonNull(glucoseLogDTO.getDiabetesOtherSymptoms())) {
                        symptomComponent.setValue(new StringType(glucoseLogDTO.getDiabetesOtherSymptoms()));
                    }
                }
                default -> Logger.logInfo("No match found");
            }
            symptomObservation.addComponent(symptomComponent);
        }
        symptomObservation.setEffective(effectiveDateTime);

        return symptomObservation;
    }


    /**
     * Get the symptom name list for the given bundle of observations.
     *
     * @param bundle the bundle of observations containing the patient symptoms
     * @return the list of symptoms
     */
    public List<String> getSymptomListByBundle(Bundle bundle) {
        List<String> symptoms = new ArrayList<>();
        for (Bundle.BundleEntryComponent bundleEntryComponent : bundle.getEntry()) {
            Observation observation = ((Observation) bundleEntryComponent.getResource());
            for (Observation.ObservationComponentComponent observationComponent : observation.getComponent()) {
                switch (observationComponent.getCode().getText()) {
                    case MetaCodeConstants.FREQUENT_URINATION_KEY -> symptoms.add(MetaCodeConstants.FREQUENT_URINATION);
                    case MetaCodeConstants.FAINTING_KEY -> symptoms.add(MetaCodeConstants.FAINTING);
                    case MetaCodeConstants.SEIZURES_KEY -> symptoms.add(MetaCodeConstants.SEIZURES);
                    case MetaCodeConstants.BLURRY_VISION_AND_SUDDEN_VISION_LOSS_KEY -> symptoms.add(MetaCodeConstants.BLURRY_VISION_AND_SUDDEN_VISION_LOSS);
                    case MetaCodeConstants.NUMBNESS_ON_SIDE_OF_BODY_KEY ->
                            symptoms.add(MetaCodeConstants.NUMBNESS_ON_SIDE_OF_BODY);
                    case MetaCodeConstants.SWELLING_OF_LIMBS_KEY -> symptoms.add(MetaCodeConstants.SWELLING_OF_LIMBS);
                    case MetaCodeConstants.DIZZINESS_UPON_STANDING_KEY ->
                            symptoms.add(MetaCodeConstants.DIZZINESS_UPON_STANDING);
                    case MetaCodeConstants.CHEST_PAIN_OR_DISCOMFORT_KEY -> symptoms.add(
                            MetaCodeConstants.CHEST_PAIN_OR_DISCOMFORT);
                    case MetaCodeConstants.SWELLING_OF_TONGUE_OR_LIPS_KEY -> symptoms.add(
                            MetaCodeConstants.SWELLING_OF_TONGUE_OR_LIPS);
                    case MetaCodeConstants.PALPITATIONS_OR_IRREGULAR_HEARTBEATS_KEY -> symptoms.add(
                            MetaCodeConstants.PALPITATIONS_OR_IRREGULAR_HEARTBEATS);
                    case MetaCodeConstants.SHORTNESS_OF_BREATH_WITH_USUAL_ACTIVITIES_KEY -> symptoms.add(
                            MetaCodeConstants.SHORTNESS_OF_BREATH_WITH_USUAL_ACTIVITIES);
                    case MetaCodeConstants.EXCESSIVE_THIRST_KEY -> symptoms.add(MetaCodeConstants.EXCESSIVE_THIRST);
                    case MetaCodeConstants.SIGNIFICANT_HUNGER_KEY -> symptoms.add(MetaCodeConstants.SIGNIFICANT_HUNGER);
                    case MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND_KEY ->
                            symptoms.add(MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND);
                    case MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN_KEY ->
                            symptoms.add(MetaCodeConstants.FOOT_NUMBNESS_TINGLING_OR_PAIN);
                    case MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS_KEY ->
                            symptoms.add(MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS);
                    case MetaCodeConstants.UNPLANNED_WEIGHT_LOSS_KEY ->
                            symptoms.add(MetaCodeConstants.UNPLANNED_WEIGHT_LOSS);
                    case MetaCodeConstants.NO_SYMPTOMS_KEY -> symptoms.add(MetaCodeConstants.NO_SYMPTOMS);
                    case MetaCodeConstants.OTHER_KEY -> symptoms.add(MetaCodeConstants.OTHER);
                    default -> Logger.logInfo("No match found");
                }
            }
        }
        return symptoms;
    }
}
