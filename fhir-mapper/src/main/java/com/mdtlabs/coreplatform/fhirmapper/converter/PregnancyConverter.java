package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.pregnancy.PregnancySymptomDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

/**
 * <p>
 * Converts PregnancyAncDTO to FHIR Observation entity for Patient pregnant observations.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-14
 */
@Component
public class PregnancyConverter {

    private final FhirUtils fhirUtils;
    private final CommonConverter commonConverter;
    private final FhirAssessmentMapper fhirAssessmentMapper;

    @Autowired
    public PregnancyConverter(FhirUtils fhirUtils, CommonConverter commonConverter,
                              FhirAssessmentMapper fhirAssessmentMapper) {
        this.fhirUtils = fhirUtils;
        this.commonConverter = commonConverter;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
    }

    /**
     * Create the pregnancy observation using pregnancy details.
     *
     * @param pregnancyDetailsDTO pregnancy details to create the pregnancy observation
     * @return Observation which contains the pregnancy details
     */
    public Observation createPregnancyObservation(PregnancyDetailsDTO pregnancyDetailsDTO) {
        Observation pregnancyObservation = new Observation();

        pregnancyObservation.setText(commonConverter.getNarrative(FhirConstants.PREGNANCY));

        // status
        if (Boolean.TRUE.equals(pregnancyDetailsDTO.getIsPregnant())) {
            pregnancyObservation.setStatus(Observation.ObservationStatus.PRELIMINARY);
        } else {
            pregnancyObservation.setStatus(Observation.ObservationStatus.FINAL);
        }

        // code
        pregnancyObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.PREGNANCY_KEY));

        // identifier
        Identifier identifier = new Identifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_PREGNANCY);
        pregnancyObservation.setIdentifier(List.of(identifier));

        // observation taken date
        DateTimeType effectiveDateTime;
        if (Objects.nonNull(pregnancyDetailsDTO.getProvenance().getModifiedDate())) {
            effectiveDateTime = new DateTimeType(pregnancyDetailsDTO.getProvenance().getModifiedDate());
        } else {
            effectiveDateTime = new DateTimeType(new Date());
        }
        pregnancyObservation.setEffective(effectiveDateTime);

        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getIsInterestedToEnroll(),
                MetaCodeConstants.IS_INTERESTED_TO_ENROLL_KEY, pregnancyObservation.getComponent());
        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getIsIptDrugProvided(),
                MetaCodeConstants.IS_IPT_DRUG_PROVIDED_KEY, pregnancyObservation.getComponent());
        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getIsIronFolateProvided(),
                MetaCodeConstants.IS_IRON_FOLATE_PROVIDED_KEY, pregnancyObservation.getComponent());
        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getIsMosquitoNetProvided(),
                MetaCodeConstants.IS_MOSQUITO_NET_PROVIDED_KEY, pregnancyObservation.getComponent());
        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getAttendedAncClinic(),
                MetaCodeConstants.ATTENDED_ANC_CLINIC_KEY, pregnancyObservation.getComponent());
        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getIsPregnant(),
                MetaCodeConstants.IS_PREGNANT_KEY, pregnancyObservation.getComponent());
        fhirAssessmentMapper.createObservationComponent(pregnancyDetailsDTO.getIsOnTreatment(),
                MetaCodeConstants.IS_ON_TREATMENT_KEY, pregnancyObservation.getComponent());

        pregnancyObservation.getComponent().add(createDateComponent(pregnancyDetailsDTO.getLastMenstrualPeriod(),
                MetaCodeConstants.LAST_MENSTRUAL_PERIOD_DATE_KEY));
        pregnancyObservation.getComponent().add(createDateComponent(pregnancyDetailsDTO.getEstimatedDeliveryDate(),
                MetaCodeConstants.ESTIMATED_DELIVERY_DATE_KEY));
        pregnancyObservation.getComponent().add(createStringComponent(pregnancyDetailsDTO.getGravida(),
                MetaCodeConstants.GRAVIDA_KEY));
        pregnancyObservation.getComponent().add(createStringComponent(pregnancyDetailsDTO.getParity(),
                MetaCodeConstants.PARITY_KEY));
        pregnancyObservation.getComponent().add(createStringComponent(pregnancyDetailsDTO.getNoOfFetus(),
                MetaCodeConstants.PREGNANCY_FETUSES_NUMBER_KEY));
        pregnancyObservation.getComponent().add(createDateComponent(pregnancyDetailsDTO.getActualDeliveryDate(),
                MetaCodeConstants.ACTUAL_DELIVERY_DATE));
        pregnancyObservation.getComponent().add(createStringComponent(pregnancyDetailsDTO.getGestationalAge(),
                MetaCodeConstants.GESTATIONAL_AGE_KEY));
        pregnancyObservation.getComponent().add(createStringComponent(pregnancyDetailsDTO.getMaternalOutcomes(),
                MetaCodeConstants.MATERNAL_OUTCOMES_KEY));
        pregnancyObservation.getComponent().add(createStringComponent(pregnancyDetailsDTO.getMaternalOutcomes(),
                MetaCodeConstants.NEONATAL_OUTCOMES_KEY));
        if (Boolean.TRUE.equals(pregnancyDetailsDTO.getIsPregnancyAnc())) {
            pregnancyObservation.getComponent().add(createBooleanComponent(pregnancyDetailsDTO.getIsPregnancyRisk(),
                    MetaCodeConstants.IS_PREGNANT_RISK_KEY));
        }

        if (Objects.nonNull(pregnancyDetailsDTO.getPregnancySymptoms()) &&
                !pregnancyDetailsDTO.getPregnancySymptoms().isEmpty()) {
            setPregnancySymptoms(pregnancyObservation, pregnancyDetailsDTO);
        }

        if (Objects.nonNull(pregnancyDetailsDTO.getPregnancyOtherSymptoms())) {
            commonConverter.setObservationNote(pregnancyObservation, MetaCodeConstants.PREGNANCY_OTHER_SYMPTOMS_KEY,
                    pregnancyDetailsDTO.getPregnancyOtherSymptoms());
        }

        return pregnancyObservation;
    }

    /**
     * Set the string component
     *
     * @param value to set the value
     * @param metaCode to get the meta code details
     * @return observation component
     */
    private Observation.ObservationComponentComponent createBooleanComponent(Boolean value, String metaCode) {
        Observation.ObservationComponentComponent component = null;
        if (Objects.nonNull(value)) {
            component = new Observation.ObservationComponentComponent();
            component.setCode(fhirUtils.setCodes(metaCode));
            component.setValue(new BooleanType(value));
        }
        return component;
    }

    /**
     * Set the string component
     *
     * @param value to set the value
     * @param metaCode to get the meta code details
     * @return observation component
     */
    private Observation.ObservationComponentComponent createStringComponent(String value, String metaCode) {
        Observation.ObservationComponentComponent component = null;
        if (Objects.nonNull(value)) {
            component = new Observation.ObservationComponentComponent();
            component.setCode(fhirUtils.setCodes(metaCode));
            component.setValue(new StringType(value));
        }
        return component;
    }

    /**
     * Set the string component
     *
     * @param value to set the value
     * @param metaCode to get the meta code details
     * @return observation component
     */
    private Observation.ObservationComponentComponent createDateComponent(Date value, String metaCode) {
        Observation.ObservationComponentComponent component = null;
        if (Objects.nonNull(value)) {
            component = new Observation.ObservationComponentComponent();
            component.setCode(fhirUtils.setCodes(metaCode));
            component.setValue(new DateTimeType(value));
        }
        return component;
    }

    /**
     * <p>
     * Set pregnant symptoms to FHIR observation entity using pregnancy details.
     * </p>
     *
     * @param observation       The FHIR Observation entity.
     * @param pregnancyDetailsDTO   The pregnancy details DTO.
     *
     */
    private void setPregnancySymptoms(Observation observation,
                                      PregnancyDetailsDTO pregnancyDetailsDTO) {
        for (PregnancySymptomDTO pregnancySymptomDTO: pregnancyDetailsDTO.getPregnancySymptoms()) {
            Observation.ObservationComponentComponent pregnancySymptomComponent =
                    new Observation.ObservationComponentComponent();

            switch (pregnancySymptomDTO.getName()) {
                case MetaCodeConstants.VAGINAL_BLEEDING ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.VAGINAL_BLEEDING_KEY));

                case MetaCodeConstants.CONVULSIONS ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.CONVULSIONS_KEY));

                case MetaCodeConstants.HEADACHE ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.HEADACHE_KEY));

                case MetaCodeConstants.BLURRED_VISION_DIFFICULTY_SEEING_CLEARLY ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.BLURRED_VISION_DIFFICULTY_SEEING_CLEARLY_KEY));

                case MetaCodeConstants.REDUCED_ABSENT_BABY_MOVEMENTS ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.REDUCED_ABSENT_BABY_MOVEMENTS_KEY));

                case MetaCodeConstants.FATIGUE_FEELING_TIRED_FEELING_ILL_WEAK ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.FATIGUE_FEELING_TIRED_FEELING_ILL_WEAK_KEY));

                case MetaCodeConstants.DIFFICULTY_BREATHING_FAST_BREATHING_COUGH_CHEST_PAIN ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.DIFFICULTY_BREATHING_FAST_BREATHING_COUGH_CHEST_PAIN_KEY));

                case MetaCodeConstants.BREAKING_OF_WATER ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.BREAKING_OF_WATER_KEY));

                case MetaCodeConstants.PAIN_IN_THE_ABDOMEN ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.PAIN_IN_THE_ABDOMEN_KEY));

                case MetaCodeConstants.FEVER_HOTNESS_OF_BODY ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.FEVER_HOTNESS_OF_BODY_KEY));

                case MetaCodeConstants.SWELLING_OF_THE_LEGS_HANDS_FACE ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.SWELLING_OF_THE_LEGS_HANDS_FACE_KEY));

                case MetaCodeConstants.PAINFUL_BURNING_FEELING_WHEN_PASSING_URINE ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.PAINFUL_BURNING_FEELING_WHEN_PASSING_URINE_KEY));

                case MetaCodeConstants.VAGINAL_DISCHARGE_ITCHINESS ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.VAGINAL_DISCHARGE_ITCHINESS_KEY));

                case MetaCodeConstants.NO_SYMPTOMS ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.NO_SYMPTOMS_KEY));

                case MetaCodeConstants.OTHER ->
                        pregnancySymptomComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.OTHER_KEY));

                default -> Logger.logInfo(pregnancySymptomDTO.getName());
            }
            observation.addComponent(pregnancySymptomComponent);
        }
    }

    /**
     * Converts the observation to {@link PregnancyDetailsDTO}.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO}
     * @param observation the observation to convert to the pregnancy details
     */
    public void convertObservationToPregnancyDetails(PregnancyDetailsDTO pregnancyDetailsDTO, Observation observation) {
        for (Observation.ObservationComponentComponent observationComponentComponent : observation.getComponent()) {
            switch (observationComponentComponent.getCode().getText()) {
                case MetaCodeConstants.LAST_MENSTRUAL_PERIOD_DATE_KEY -> pregnancyDetailsDTO.setLastMenstrualPeriod(
                        observationComponentComponent.getValueDateTimeType().getValue());
                case MetaCodeConstants.ESTIMATED_DELIVERY_DATE_KEY -> pregnancyDetailsDTO.setEstimatedDeliveryDate(
                        observationComponentComponent.getValueDateTimeType().getValue());
                case MetaCodeConstants.GRAVIDA_KEY -> pregnancyDetailsDTO.setGravida(
                        observationComponentComponent.getValueStringType().getValue());
                case MetaCodeConstants.PARITY_KEY -> pregnancyDetailsDTO.setParity(
                        observationComponentComponent.getValueStringType().getValue());
                case MetaCodeConstants.PREGNANCY_FETUSES_NUMBER_KEY -> pregnancyDetailsDTO.setNoOfFetus(
                        observationComponentComponent.getValueStringType().getValue());
                case MetaCodeConstants.MATERNAL_OUTCOMES_KEY -> pregnancyDetailsDTO.setMaternalOutcomes(
                        observationComponentComponent.getValueStringType().getValue());
                case MetaCodeConstants.NEONATAL_OUTCOMES_KEY -> pregnancyDetailsDTO.setNeonatalOutcomes(
                        observationComponentComponent.getValueStringType().getValue());
                case MetaCodeConstants.ACTUAL_DELIVERY_DATE -> pregnancyDetailsDTO.setActualDeliveryDate(
                        observationComponentComponent.getValueDateTimeType().getValue());
                case MetaCodeConstants.GESTATIONAL_AGE_KEY -> pregnancyDetailsDTO.setGestationalAge(
                        observationComponentComponent.getValueStringType().getValue());
                case MetaCodeConstants.IS_ON_TREATMENT_KEY -> pregnancyDetailsDTO.setIsOnTreatment(
                        Constants.YES.equals(observationComponentComponent.getValueCodeableConcept().getText()));
                case MetaCodeConstants.IS_PREGNANT_KEY -> pregnancyDetailsDTO.setIsPregnant(
                        Constants.YES.equals(observationComponentComponent.getValueCodeableConcept().getText()));
                case MetaCodeConstants.IS_PREGNANT_RISK_KEY -> pregnancyDetailsDTO.setIsPregnancyRisk(
                        observationComponentComponent.getValueBooleanType().getValue());
                default -> Logger.logInfo("No match found");
            }
        }
    }

    /**
     * Update the pregnancy details.
     *
     * @param oldPregnancyObservation old pregnancy observation to get the observation details
     * @param pregnancyDetailsDTO patient pregnancy details
     * @return observation.
     */
    public Observation updatePregnancyObservation(Observation oldPregnancyObservation,
                                                  PregnancyDetailsDTO pregnancyDetailsDTO) {
        Observation pregnancyObservation = new Observation();

        // id
        pregnancyObservation.setId(oldPregnancyObservation.getIdPart());

        // text
        pregnancyObservation.setText(commonConverter.getNarrative(FhirConstants.PREGNANCY));

        // status
        if (Boolean.TRUE.equals(pregnancyDetailsDTO.getIsPregnant())) {
            pregnancyObservation.setStatus(Observation.ObservationStatus.PRELIMINARY);
        } else {
            pregnancyObservation.setStatus(Observation.ObservationStatus.FINAL);
        }

        // code
        pregnancyObservation.setCode(fhirUtils.setCodes(MetaCodeConstants.PREGNANCY_KEY));

        // identifier
        Identifier identifier = new Identifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_PREGNANCY);
        pregnancyObservation.setIdentifier(List.of(identifier));

        // observation taken date
        DateTimeType effectiveDateTime;
        if (Objects.nonNull(pregnancyDetailsDTO.getProvenance().getModifiedDate())) {
            effectiveDateTime = new DateTimeType(pregnancyDetailsDTO.getProvenance().getModifiedDate());
        } else {
            effectiveDateTime = new DateTimeType(new Date());
        }
        pregnancyObservation.setEffective(effectiveDateTime);

        // component
        updatePregnancyComponent(pregnancyObservation, oldPregnancyObservation, pregnancyDetailsDTO);
        return pregnancyObservation;
    }

    /**
     * Update the pregnancy symptoms
     *
     * @param pregnancyObservation pregnancy observation
     * @param oldPregnancyObservation old pregnancy observation
     * @param pregnancyDetailsDTO contains pregnancy details
     */
    private void updatePregnancyComponent(Observation pregnancyObservation, Observation oldPregnancyObservation,
                                          PregnancyDetailsDTO pregnancyDetailsDTO) {

        Map<String, Observation.ObservationComponentComponent> componentMap = new HashMap<>();
        oldPregnancyObservation.getComponent().forEach(observationComponentComponent ->
            componentMap.put(observationComponentComponent.getCode().getText(), observationComponentComponent)
        );
        List<Observation.ObservationComponentComponent> components = new ArrayList<>();

        updateBooleanObservationComponent(MetaCodeConstants.IS_INTERESTED_TO_ENROLL_KEY,
                pregnancyDetailsDTO.getIsInterestedToEnroll(), components, componentMap);
        updateBooleanObservationComponent(MetaCodeConstants.IS_IPT_DRUG_PROVIDED_KEY,
                pregnancyDetailsDTO.getIsIptDrugProvided(), components, componentMap);
        updateBooleanObservationComponent(MetaCodeConstants.IS_IRON_FOLATE_PROVIDED_KEY,
                pregnancyDetailsDTO.getIsIronFolateProvided(), components, componentMap);
        updateBooleanObservationComponent(MetaCodeConstants.IS_MOSQUITO_NET_PROVIDED_KEY,
                pregnancyDetailsDTO.getIsMosquitoNetProvided(), components, componentMap);
        updateBooleanObservationComponent(MetaCodeConstants.ATTENDED_ANC_CLINIC_KEY,
                pregnancyDetailsDTO.getAttendedAncClinic(), components, componentMap);
        updateBooleanObservationComponent(MetaCodeConstants.IS_PREGNANT_KEY,
                pregnancyDetailsDTO.getIsPregnant(), components, componentMap);
        updateBooleanObservationComponent(MetaCodeConstants.IS_ON_TREATMENT_KEY,
                pregnancyDetailsDTO.getIsOnTreatment(), components, componentMap);
        updateDateObservationComponent(MetaCodeConstants.LAST_MENSTRUAL_PERIOD_DATE_KEY,
                pregnancyDetailsDTO.getLastMenstrualPeriod(), components, componentMap);
        updateDateObservationComponent(MetaCodeConstants.ESTIMATED_DELIVERY_DATE_KEY,
                pregnancyDetailsDTO.getEstimatedDeliveryDate(), components, componentMap);
        updateDateObservationComponent(MetaCodeConstants.ACTUAL_DELIVERY_DATE,
                pregnancyDetailsDTO.getActualDeliveryDate(), components, componentMap);
        updateStringObservationComponent(MetaCodeConstants.GRAVIDA_KEY,
                pregnancyDetailsDTO.getGravida(), components, componentMap);
        updateStringObservationComponent(MetaCodeConstants.PARITY_KEY,
                pregnancyDetailsDTO.getParity(), components, componentMap);
        updateStringObservationComponent(MetaCodeConstants.PREGNANCY_FETUSES_NUMBER_KEY,
                pregnancyDetailsDTO.getNoOfFetus(), components, componentMap);
        updateStringObservationComponent(MetaCodeConstants.GESTATIONAL_AGE_KEY,
                pregnancyDetailsDTO.getGestationalAge(), components, componentMap);
        updateStringObservationComponent(MetaCodeConstants.MATERNAL_OUTCOMES_KEY,
                pregnancyDetailsDTO.getMaternalOutcomes(), components, componentMap);
        updateStringObservationComponent(MetaCodeConstants.NEONATAL_OUTCOMES_KEY,
                pregnancyDetailsDTO.getNeonatalOutcomes(), components, componentMap);

        if (Objects.nonNull(pregnancyDetailsDTO.getIsPregnancyRisk())) {
            components.add(createBooleanComponent(pregnancyDetailsDTO.getIsPregnancyRisk(),
                    MetaCodeConstants.IS_PREGNANT_RISK_KEY));
        } else if (Objects.nonNull(fhirUtils.setCodes(MetaCodeConstants.IS_PREGNANT_RISK_KEY).getText())) {
            components.add(componentMap.get(
                    fhirUtils.setCodes(MetaCodeConstants.IS_PREGNANT_RISK_KEY).getText()));
        }
        componentMap.remove(fhirUtils.setCodes(MetaCodeConstants.IS_PREGNANT_RISK_KEY).getText());

        if (Objects.nonNull(pregnancyDetailsDTO.getPregnancySymptoms()) &&
                !pregnancyDetailsDTO.getPregnancySymptoms().isEmpty()) {
            setPregnancySymptoms(pregnancyObservation, pregnancyDetailsDTO);
        } else {
            componentMap.forEach((key, component) -> pregnancyObservation.getComponent().add(component));
        }

        if (Objects.nonNull(pregnancyDetailsDTO.getPregnancyOtherSymptoms())) {
            commonConverter.setObservationNote(pregnancyObservation, MetaCodeConstants.PREGNANCY_OTHER_SYMPTOMS_KEY,
                    pregnancyDetailsDTO.getPregnancyOtherSymptoms());
        } else {
            pregnancyObservation.setNote(oldPregnancyObservation.getNote());
        }

        components.forEach(component -> pregnancyObservation.getComponent().add(component));
    }

    /**
     * Update string observation component
     *
     * @param metaCode meta code for obtain the component
     * @param value value as a boolean
     * @param components list of observation component
     * @param componentMap map of observation component
     */
    private void updateStringObservationComponent(String metaCode, String value,
                                                  List<Observation.ObservationComponentComponent> components,
                                                  Map<String, Observation.ObservationComponentComponent> componentMap) {
        if (Objects.nonNull(value)) {
            components.add(createStringComponent(value, metaCode));
        } else if (Objects.nonNull(fhirUtils.setCodes(metaCode).getText())) {
            components.add(componentMap.get(fhirUtils.setCodes(metaCode).getText()));
        }
        componentMap.remove(fhirUtils.setCodes(metaCode).getText());
    }

    /**
     * Update Date observation component
     *
     * @param metaCode meta code for obtain the component
     * @param value value as a boolean
     * @param components list of observation component
     * @param componentMap map of observation component
     */
    private void updateDateObservationComponent(String metaCode, Date value,
                                                List<Observation.ObservationComponentComponent> components,
                                                Map<String, Observation.ObservationComponentComponent> componentMap) {
        if (Objects.nonNull(value)) {
            components.add(createDateComponent(value, metaCode));
        } else if (Objects.nonNull(fhirUtils.setCodes(metaCode).getText())) {
            components.add(componentMap.get(fhirUtils.setCodes(metaCode).getText()));
        }
        componentMap.remove(fhirUtils.setCodes(metaCode).getText());
    }

    /**
     * Update boolean observation component
     *
     * @param metaCode meta code for obtain the component
     * @param value value as a boolean
     * @param components list of observation component
     * @param componentMap map of observation component
     */
    private void updateBooleanObservationComponent(String metaCode, Boolean value,
                                                   List<Observation.ObservationComponentComponent> components,
                                                   Map<String, Observation.ObservationComponentComponent> componentMap) {
        if (Objects.nonNull(value)) {
            components.add(fhirAssessmentMapper.createObservationComponent(value, metaCode));
        } else if (Objects.nonNull(fhirUtils.setCodes(metaCode).getText())) {
            components.add(componentMap.get(fhirUtils.setCodes(metaCode).getText()));
        }
        componentMap.remove(fhirUtils.setCodes(metaCode).getText());
    }

}
