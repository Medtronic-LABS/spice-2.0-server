package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiabetesDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.pregnancy.PregnancySymptomDTO;

/**
 * <p>
 * This spice converter helps to convert FHIR entity to spice DTO's
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-21
 */
@Component
public class SpiceConverter {


    /**
     * <p>
     * Set patient height details in biometrics DTO using FHIR Observation entity.
     * </p>
     *
     * @param observation          The FHIR Observation entity.
     * @param bioMetricsDTO        The biometrics DTO
     *
     */
    public void setHeightDetails(Observation observation, BioMetricsDTO bioMetricsDTO) {
        if (Objects.nonNull(observation)) {
            Quantity quantity = observation.getValueQuantity();
            if (Objects.nonNull(quantity)) {
                bioMetricsDTO.setHeight(quantity.getValue().doubleValue());
            }
        }
    }

    /**
     * <p>
     * Set patient weight details in biometrics DTO using FHIR Observation entity.
     * </p>
     *
     * @param observation          The FHIR Observation entity.
     * @param bioMetricsDTO        The biometrics DTO
     *
     */
    public void setWeightDetails(Observation observation, BioMetricsDTO bioMetricsDTO) {
        if (Objects.nonNull(observation)) {
            Quantity quantity = observation.getValueQuantity();
            if (Objects.nonNull(quantity)) {
                bioMetricsDTO.setWeight(quantity.getValue().doubleValue());
            }
        }
    }

    /**
     * <p>
     * Set patient bmi details in biometrics DTO using FHIR Observation entity.
     * </p>
     *
     * @param observation          The FHIR Observation entity.
     * @param bioMetricsDTO        The biometrics DTO
     *
     */
    public void setBmiDetails(Observation observation, BioMetricsDTO bioMetricsDTO) {
        if (Objects.nonNull(observation)) {
            Quantity quantity = observation.getValueQuantity();
            if (Objects.nonNull(quantity)) {
                bioMetricsDTO.setBmi(quantity.getValue().doubleValue());
            }
            if (!observation.getComponent().isEmpty()
                    && observation.getComponent().get(Constants.ZERO).getCode()
                    .getText().equals(MetaCodeConstants.BMI_CATEGORY_KEY)) {
                bioMetricsDTO.setBmiCategory(observation
                        .getComponent().get(Constants.ZERO).getValue().toString());
            }
        }
    }

    /**
     * <p>
     * Set smoker details in biometrics DTO using FHIR Observation entity.
     * </p>
     *
     * @param observation          The FHIR Observation entity.
     * @param bioMetricsDTO        The biometrics DTO
     *
     */
    public void setRegularSmokerDetails(Observation observation, BioMetricsDTO bioMetricsDTO) {
        bioMetricsDTO.setIsRegularSmoker(Boolean.FALSE);
        if (Objects.nonNull(observation) && observation.hasValueCodeableConcept()) {
            bioMetricsDTO.setIsRegularSmoker(Objects.equals(Constants.YES, observation.getValueCodeableConcept().getText())
                    ? Boolean.TRUE : Boolean.FALSE);
        }
    }

    /**
     * <p>
     * Set patient suicide details using FHIR Observation entity.
     * </p>
     *
     * @param observation          The FHIR Observation entity.
     * @param suicideDetails       The suicide details map entity
     *
     */
    public void setSuicideScreenerDetails(Observation observation, Map<String, String> suicideDetails) {
        if (!observation.getComponent().isEmpty()) {
            for (Observation.ObservationComponentComponent component: observation.getComponent()) {
                    suicideDetails.put(component.getCode().getText(), component.getValueCodeableConcept().getText().toLowerCase());
            }
        }
    }

    /**
     * <p>
     * Set patient substance abuse details using FHIR Observation entity.
     * </p>
     *
     * @param observation          The FHIR Observation entity.
     * @param substanceDetails     The substance details map entity
     *
     */
    public void setSubstanceDetails(Observation observation, Map<String, String> substanceDetails) {
        if (!observation.getComponent().isEmpty()) {
            for (Observation.ObservationComponentComponent component: observation.getComponent()) {
                substanceDetails.put(component.getCode().getText(), Constants.YES.toLowerCase());
            }
        }
    }

    /**
     * <p>
     * Set patient risk details using FHIR Observation entity.
     * </p>
     *
     * @param observation     The FHIR Observation entity.
     * @param riskDetails     The risk details map entity
     *
     */
    public void setRiskDetails(Observation observation, Map<String, String> riskDetails) {
        if (!observation.getComponent().isEmpty()) {
            for (Observation.ObservationComponentComponent component: observation.getComponent()) {
                riskDetails.put(component.getCode().getText(), component.getValueStringType().getValue());
            }
        }
    }

    /**
     * <p>
     * Set patient glucose log details using FHIR Observation entity.
     * </p>
     *
     * @param observation        The FHIR Observation entity.
     * @param glucoseLogDTO      The glucose log entity.
     *
     */
    public void setGlucoseLogDTO(Observation observation, GlucoseLogDTO glucoseLogDTO) {

        for (Observation.ObservationComponentComponent component : observation.getComponent()) {
            if (FhirConstants.FBS.equalsIgnoreCase(component.getCode().getText()) ||
                    FhirConstants.RBS.equalsIgnoreCase(component.getCode().getText())) {
                glucoseLogDTO.setGlucoseType(component.getCode().getText());
            }
        }

        if (Objects.nonNull(observation.getValueQuantity().getValue())) {
            glucoseLogDTO.setGlucoseValue(observation.getValueQuantity().getValue().doubleValue());
            glucoseLogDTO.setGlucoseDateTime(observation.getEffectiveDateTimeType().getValue());
        }
        glucoseLogDTO.setGlucoseUnit(observation.getValueQuantity().getUnit());

        List<DiabetesDTO> diabetesDTOS = new ArrayList<>();
        for (Observation.ObservationComponentComponent component : observation.getComponent()) {
            DiabetesDTO diabetesDTO = getDiabetesSymptom(component, glucoseLogDTO);

            if (Objects.nonNull(diabetesDTO.getName())) {
                diabetesDTOS.add(diabetesDTO);
            }
        }

        if (Objects.nonNull(glucoseLogDTO.getHba1c())) {
            glucoseLogDTO.setHba1cDateTime(observation.getEffectiveDateTimeType().getValue());
        }
        glucoseLogDTO.setDiabetes(diabetesDTOS);
    }

    /**
     * <p>
     * Set patient diabetes details using FHIR Observation component entity.
     * </p>
     *
     * @param component        The FHIR Observation component entity.
     * @param glucoseLogDTO    The glucose log entity.
     *
     */
    private DiabetesDTO getDiabetesSymptom(Observation.ObservationComponentComponent
                                     component, GlucoseLogDTO glucoseLogDTO) {
        DiabetesDTO diabetesDTO = new DiabetesDTO();

        switch (component.getCode().getText()) {
            case MetaCodeConstants.FREQUENT_URINATION_KEY ->
                    diabetesDTO.setName(MetaCodeConstants.FREQUENT_URINATION);
            case MetaCodeConstants.EXCESSIVE_THIRST_KEY ->
                diabetesDTO.setName(MetaCodeConstants.EXCESSIVE_THIRST);
            case MetaCodeConstants.SIGNIFICANT_HUNGER_KEY ->
                diabetesDTO.setName(MetaCodeConstants.SIGNIFICANT_HUNGER);
            case MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND_KEY ->
                diabetesDTO.setName(MetaCodeConstants.SLOW_HEALING_OR_INFECTED_FOOT_WOUND);
            case MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN_KEY ->
                diabetesDTO.setName(MetaCodeConstants.FOOT_NUMBNESS_TINGLING_AND_OR_PAIN);
            case MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS_KEY ->
                diabetesDTO.setName(MetaCodeConstants.SIGNIFICANT_FATIGUE_OR_WEAKNESS);
            case MetaCodeConstants.UNPLANNED_WEIGHT_LOSS_KEY ->
                diabetesDTO.setName(MetaCodeConstants.UNPLANNED_WEIGHT_LOSS);
            case MetaCodeConstants.NO_SYMPTOMS_KEY ->
                diabetesDTO.setName(MetaCodeConstants.NO_SYMPTOMS);
            case MetaCodeConstants.OTHER_KEY -> {
                diabetesDTO.setName(MetaCodeConstants.OTHER);
                glucoseLogDTO.setDiabetesOtherSymptoms(component.getValueStringType().getValue());
            }
            case MetaCodeConstants.LAST_MEAL_TIME_KEY ->
                 glucoseLogDTO.setLastMealTime(component.getValueDateTimeType().getValue());
            case MetaCodeConstants.HBA1C -> {
                glucoseLogDTO.setHba1c(component.getValueQuantity().getValue().doubleValue());
                glucoseLogDTO.setHba1cUnit(component.getValueQuantity().getUnit());
            }
            case MetaCodeConstants.HAVE_YOU_BEEN_DIAGNOSED_WITH_HIGH_BLOOD_SUGAR_BEFORE_KEY ->
                glucoseLogDTO.setIsBeforeDiabetesDiagnosis(Constants.YES.equals(component.getValueCodeableConcept().getText()));
            default -> Logger.logInfo(component.getCode().getText());
        }
        return diabetesDTO;
    }

    /**
     * <p>
     * Set patient blood pressure log using FHIR Observation entity.
     * </p>
     *
     * @param observation  The FHIR Observation entity.
     * @param bpLogDTO     The blood pressure log entity.
     *
     */
    public void setBpLogDTO(Observation observation, BpLogDTO bpLogDTO) {
        if (Objects.nonNull(observation.getCode()) && !observation.getCode().getCoding().isEmpty()) {
            bpLogDTO.setIsBeforeHtnDiagnosis(observation.getCode().getCoding().getLast().getDisplay()
                    .equals(MetaCodeConstants.HAVE_YOU_BEEN_DIAGNOSED_WITH_HIGH_BLOOD_PRESSURE_BEFORE));
            bpLogDTO.setIsBeforeHtnDiagnosis(true);
        } else {
            bpLogDTO.setIsBeforeHtnDiagnosis(false);
        }
        if (!observation.getComponent().isEmpty()) {
            setBpLogDetails(observation, bpLogDTO);
        }
    }

    /**
     * <p>
     * Set patient blood pressure log details using FHIR Observation entity.
     * </p>
     *
     * @param observation  The FHIR Observation entity.
     * @param bpLogDTO     The blood pressure log DTO.
     *
     */
    private void setBpLogDetails(Observation observation, BpLogDTO bpLogDTO) {
        List<BpLogDetailsDTO> bpLogDetailsDTOS = new ArrayList<>();
        BpLogDetailsDTO bloodPressureOne = new BpLogDetailsDTO();
        BpLogDetailsDTO bloodPressureTwo = new BpLogDetailsDTO();
        BpLogDetailsDTO bloodPressureThree = new BpLogDetailsDTO();

        for (Observation.ObservationComponentComponent component : observation.getComponent()) {
            String codeText = component.getCode().getText();

            switch (codeText) {
                case MetaCodeConstants.SYSTOLIC_BLOOD_PRESSURE + Constants.EMPTY_SPACE + Constants.ONE ->
                        setSystolic(bloodPressureOne, component.getValueQuantity());
                case MetaCodeConstants.DIASTOLIC_BLOOD_PRESSURE + Constants.EMPTY_SPACE + Constants.ONE ->
                        setDiastolic(bloodPressureOne, component.getValueQuantity());
                case MetaCodeConstants.SYSTOLIC_BLOOD_PRESSURE + Constants.EMPTY_SPACE + Constants.TWO ->
                        setSystolic(bloodPressureTwo, component.getValueQuantity());
                case MetaCodeConstants.DIASTOLIC_BLOOD_PRESSURE + Constants.EMPTY_SPACE + Constants.TWO ->
                        setDiastolic(bloodPressureTwo, component.getValueQuantity());
                case MetaCodeConstants.SYSTOLIC_BLOOD_PRESSURE + Constants.EMPTY_SPACE + Constants.THREE ->
                        setSystolic(bloodPressureThree, component.getValueQuantity());
                case MetaCodeConstants.DIASTOLIC_BLOOD_PRESSURE + Constants.EMPTY_SPACE + Constants.THREE ->
                        setDiastolic(bloodPressureThree, component.getValueQuantity());
                case MetaCodeConstants.PULSE + Constants.EMPTY_SPACE + Constants.ONE ->
                        setPulse(bloodPressureOne, component.getValueQuantity());
                case MetaCodeConstants.PULSE + Constants.EMPTY_SPACE + Constants.TWO ->
                        setPulse(bloodPressureTwo, component.getValueQuantity());
                case MetaCodeConstants.PULSE + Constants.EMPTY_SPACE + Constants.THREE ->
                        setPulse(bloodPressureThree, component.getValueQuantity());
                case MetaCodeConstants.AVERAGE_SYSTOLIC_BLOOD_PRESSURE ->
                        bpLogDTO.setAvgSystolic(component.getValueQuantity().getValue().doubleValue());
                case MetaCodeConstants.AVERAGE_DIASTOLIC_BLOOD_PRESSURE ->
                        bpLogDTO.setAvgDiastolic(component.getValueQuantity().getValue().doubleValue());
                case FhirConstants.CVD_RISK_SCORE ->
                        bpLogDTO.setCvdRiskScore(component.getValue() instanceof StringType ?
                                component.getValueStringType().getValue() :
                                component.getValueIntegerType().getValueAsString());
                case FhirConstants.CVD_RISK_LEVEL ->
                        bpLogDTO.setCvdRiskLevel(component.getValueStringType().getValue());
                case FhirConstants.CVD_RISK_SCORE_DISPLAY ->
                        bpLogDTO.setCvdRiskScoreDisplay(component.getValueStringType().getValue());
                case MetaCodeConstants.HAVE_YOU_BEEN_DIAGNOSED_WITH_HIGH_BLOOD_PRESSURE_BEFORE ->
                        bpLogDTO.setIsBeforeHtnDiagnosis(Constants.YES.equals(component.getValueCodeableConcept().getText()));
                default -> Logger.logInfo(codeText);
            }
        }

        if (Objects.nonNull(bloodPressureOne.getSystolic())) {
            bpLogDetailsDTOS.add(bloodPressureOne);
        }
        if (Objects.nonNull(bloodPressureTwo.getSystolic())) {
            bpLogDetailsDTOS.add(bloodPressureTwo);
        }
        if (Objects.nonNull(bloodPressureThree.getSystolic())) {
            bpLogDetailsDTOS.add(bloodPressureThree);
        }
        bpLogDTO.setBpLogDetails(bpLogDetailsDTOS);
    }

    /**
     * <p>
     * Set patient blood pressure systolic details using FHIR Quantity entity.
     * </p>
     *
     * @param quantity         The FHIR Quantity entity.
     * @param bpLogDetailsDTO  The blood pressure details DTO.
     *
     */
    private void setSystolic(BpLogDetailsDTO bpLogDetailsDTO, Quantity quantity) {
        bpLogDetailsDTO.setSystolic(quantity.getValue().toString());
    }

    /**
     * <p>
     * Set patient blood pressure diastolic details using FHIR Quantity entity.
     * </p>
     *
     * @param quantity         The FHIR Quantity entity.
     * @param bpLogDetailsDTO  The blood pressure details DTO.
     *
     */
    private void setDiastolic(BpLogDetailsDTO bpLogDetailsDTO, Quantity quantity) {
        bpLogDetailsDTO.setDiastolic(quantity.getValue().toString());
    }

    /**
     * <p>
     * Set patient blood pressure pulse details using FHIR Quantity entity.
     * </p>
     *
     * @param quantity         The FHIR Quantity entity.
     * @param bpLogDetailsDTO  The blood pressure details DTO.
     *
     */
    private void setPulse(BpLogDetailsDTO bpLogDetailsDTO, Quantity quantity) {
        bpLogDetailsDTO.setPulse(quantity.getValue().toString());
    }

    /**
     * <p>
     * Set patient bio details using FHIR RelatedPerson
     * and patient assessment details.
     * </p>
     *
     * @param relatedPerson  The FHIR Quantity entity.
     * @param assessmentDTO  The patient assessment details DTO.
     *
     */
    public void setPatientBioDetails(RelatedPerson relatedPerson, AssessmentDTO assessmentDTO) {
        BioMetricsDTO bioMetricsDTO = assessmentDTO.getBioMetrics();
        BioDataDTO bioDataDTO = assessmentDTO.getBioData();

        if (Objects.isNull(bioMetricsDTO)) {
            bioMetricsDTO = new BioMetricsDTO();
        }
        if (Objects.isNull(bioDataDTO)) {
            bioDataDTO = new BioDataDTO();
        }
        setBioDataDetails(relatedPerson, bioMetricsDTO, bioDataDTO);
        if (!relatedPerson.getName().getFirst().getPrefix().isEmpty()) {
            bioDataDTO.setInitial(relatedPerson.getName().getFirst()
                    .getPrefix().getFirst().getValue());
        }
        assessmentDTO.setBioMetrics(bioMetricsDTO);
        assessmentDTO.setBioData(bioDataDTO);
    }

    /**
     * <p>
     * Set patient bio data details using FHIR RelatedPerson entity.
     * </p>
     *
     * @param relatedPerson  The FHIR Quantity entity.
     * @param bioMetricsDTO  The bioMetrics DTO.
     * @param bioDataDTO     The bioData DTO.
     *
     */
    public void setBioDataDetails(RelatedPerson relatedPerson,
                                  BioMetricsDTO bioMetricsDTO,
                                  BioDataDTO bioDataDTO) {
        bioMetricsDTO.setGender(getSpiceGender(relatedPerson.getGender()).toLowerCase());
        bioDataDTO.setFirstName(relatedPerson.getName().getFirst().getGiven().getFirst().getValue());
        bioDataDTO.setLastName(relatedPerson.getName().getFirst().getGiven().getLast().getValue());

        if (Constants.TWO < relatedPerson.getName().getFirst().getGiven().size()) {
            bioDataDTO.setMiddleName(relatedPerson.getName().getFirst()
                    .getGiven().get(Constants.ONE).getValue());
        }
        relatedPerson.getTelecom().stream()
                .filter(contactPoint -> ContactPoint.ContactPointSystem.PHONE.getDisplay()
                        .equals(contactPoint.getSystem().getDisplay()))
                .map(ContactPoint::getValue)
                .findFirst()
                .ifPresent(bioDataDTO::setPhoneNumber);
        Optional<ContactPoint.ContactPointUse> phoneNumberCategory = relatedPerson.getTelecom().stream()
                .filter(contactPoint -> ContactPoint.ContactPointSystem.PHONE.getDisplay()
                        .equals(contactPoint.getSystem().getDisplay()))
                .map(ContactPoint::getUse).filter(Objects::nonNull).findFirst();
        if (phoneNumberCategory.isPresent()) {
            bioDataDTO.setPhoneNumberCategory(
                    this.getSpicePhoneNumberCategory(phoneNumberCategory.get()));
        }
        bioDataDTO.setIdentityType(FhirConstants.NATIONAL_ID);
        BioDataDTO.DataDTO village = new BioDataDTO.DataDTO();
        if (Objects.nonNull(relatedPerson.getAddress()) &&
                !relatedPerson.getAddress().isEmpty()) {
            village.setName(relatedPerson.getAddress().getFirst().getText());
        }
        relatedPerson.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID.equals(identifier.getSystem()))
                .map(Identifier::getValue)
                .findFirst()
                .ifPresent(bioDataDTO::setIdentityValue);
        relatedPerson.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.VILLAGE_SYSTEM_URL.equals(identifier.getSystem()))
                .map(Identifier::getValue)
                .map(Long::parseLong)
                .findFirst()
                .ifPresent(village::setId);
        bioDataDTO.setVillage(village);
        bioMetricsDTO.setDateOfBirth(relatedPerson.getBirthDate());
    }

    /**
     * <p>
     * Set patient pregnancy details using FHIR Observation entity.
     * </p>
     *
     * @param observation      The FHIR Observation entity.
     * @param pregnancyAncDTO  The PregnancyAnc DTO.
     *
     */
    public void setPregnancyDetails(Observation observation, PregnancyDetailsDTO pregnancyAncDTO) {
        pregnancyAncDTO.setPregnancyOtherSymptoms(String.valueOf(getObservationAnnotation(observation,
                MetaCodeConstants.PREGNANCY_OTHER_SYMPTOMS_KEY)));
        pregnancyAncDTO.setIsInterestedToEnroll(Boolean.valueOf(getObservationAnnotation(observation,
                MetaCodeConstants.IS_INTERESTED_TO_ENROLL_KEY)));
        setPregnancySymptomDetails(observation, pregnancyAncDTO);
    }

    /**
     * <p>
     * Set patient pregnancy symptom details using FHIR Observation entity.
     * </p>
     *
     * @param observation      The FHIR Observation entity.
     * @param pregnancyAncDTO  The PregnancyAnc DTO.
     *
     */
    public void setPregnancySymptomDetails(Observation observation, PregnancyDetailsDTO pregnancyAncDTO) {
        List<PregnancySymptomDTO> pregnancySymptoms = new ArrayList<>();

        for (Observation.ObservationComponentComponent component: observation.getComponent()) {
            String text = component.getCode().getText();
            PregnancySymptomDTO pregnancySymptomDTO = new PregnancySymptomDTO();
            switch (text) {
                case MetaCodeConstants.VAGINAL_BLEEDING_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.VAGINAL_BLEEDING);
                case MetaCodeConstants.CONVULSIONS_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.CONVULSIONS);
                case MetaCodeConstants.HEADACHE_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.HEADACHE);
                case MetaCodeConstants.BLURRED_VISION_DIFFICULTY_SEEING_CLEARLY_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.BLURRED_VISION_DIFFICULTY_SEEING_CLEARLY);
                case MetaCodeConstants.REDUCED_ABSENT_BABY_MOVEMENTS_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.REDUCED_ABSENT_BABY_MOVEMENTS);
                case MetaCodeConstants.FATIGUE_FEELING_TIRED_FEELING_ILL_WEAK_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.FATIGUE_FEELING_TIRED_FEELING_ILL_WEAK);
                case MetaCodeConstants.DIFFICULTY_BREATHING_FAST_BREATHING_COUGH_CHEST_PAIN_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.DIFFICULTY_BREATHING_FAST_BREATHING_COUGH_CHEST_PAIN);
                case MetaCodeConstants.BREAKING_OF_WATER_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.BREAKING_OF_WATER);
                case MetaCodeConstants.FEVER_HOTNESS_OF_BODY_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.FEVER_HOTNESS_OF_BODY);
                case MetaCodeConstants.SWELLING_OF_THE_LEGS_HANDS_FACE_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.SWELLING_OF_THE_LEGS_HANDS_FACE);
                case MetaCodeConstants.PAINFUL_BURNING_FEELING_WHEN_PASSING_URINE_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.PAINFUL_BURNING_FEELING_WHEN_PASSING_URINE);
                case MetaCodeConstants.VAGINAL_DISCHARGE_ITCHINESS_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.VAGINAL_DISCHARGE_ITCHINESS);
                case MetaCodeConstants.NO_SYMPTOMS_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.NO_SYMPTOMS);
                case MetaCodeConstants.OTHER_KEY ->
                        pregnancySymptomDTO.setName(MetaCodeConstants.OTHER);
                case MetaCodeConstants.LAST_MENSTRUAL_PERIOD_DATE_KEY ->
                        pregnancyAncDTO.setLastMenstrualPeriod(component.getValueDateTimeType().getValue());
                case MetaCodeConstants.GESTATIONAL_PERIOD_KEY ->
                        pregnancyAncDTO.setGestationalAge(component.getValueStringType().getValue());
                default -> Logger.logInfo(text);
            }
            if (Objects.nonNull(pregnancySymptomDTO.getName())) {
                pregnancySymptoms.add(pregnancySymptomDTO);
            }
        }
        pregnancyAncDTO.setPregnancySymptoms(pregnancySymptoms);
    }


    /**
     * <p>
     * Get spice gender details using FHIR gender enumerations
     * </p>
     *
     * @param gender  The FhIR enum of the gender.
     * @return  spice gender details
     *
     */
    private String getSpiceGender(Enumerations.AdministrativeGender gender) {
        switch (gender) {
            case Enumerations.AdministrativeGender.MALE -> {
                return Constants.MALE;
            }
            case Enumerations.AdministrativeGender.FEMALE -> {
                return Constants.FEMALE;
            }
            default -> {
                Logger.logInfo(gender.getDisplay());
                return Constants.OTHER_LOWER_CASE;
            }
        }
    }

    /**
     * <p>
     * Get FHIR Contact Point Use entity based on
     * given mobile number category
     * </p>
     *
     * @param phoneNumberCategory  The category of the mobile number.
     * @return  The FHIR ContactPointUse entity
     *
     */
    private String getSpicePhoneNumberCategory(ContactPoint.ContactPointUse phoneNumberCategory) {
        switch (phoneNumberCategory) {
            case ContactPoint.ContactPointUse.MOBILE -> {
                return Constants.PERSONAL;
            }
            case ContactPoint.ContactPointUse.HOME -> {
                return Constants.FAMILY_MEMBER;
            }
            case ContactPoint.ContactPointUse.TEMP -> {
                return Constants.FRIEND;
            }
            default -> {
                Logger.logInfo(phoneNumberCategory.getDisplay());
                return null;
            }
        }
    }

    /**
     * <p>
     * Get observation annotation details based on provided key
     * </p>
     *
     * @param observation   The FHIR Observation entity.
     * @param key           key to get specific annotation details from observation
     *
     * @return specific annotation details as string
     */
    private String getObservationAnnotation(Observation observation, String key) {
        return observation.getNote().stream()
                .filter(annotation -> annotation.getText().equals(key + Constants.HIGHFIN))
                .map(Annotation::getText)
                .findFirst()
                .orElse(null);
    }

    /**
     * <p>
     * Set patient observation details in personDetailsDTO
     * </p>
     *
     * @param observation             The FHIR Observation entity.
     * @param searchPersonDetailsDTO  The search person details DTO.
     *
     */
public void setObservationDetails(Observation observation,
                                  SearchPersonDetailsDTO searchPersonDetailsDTO) {
    String text = observation.getCode().getText();

    if (MetaCodeConstants.BLOOD_PRESSURE_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setBpObservation(observation);
    } else if (MetaCodeConstants.BLOOD_GLUCOSE_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setBgObservation(observation);
    } else if (MetaCodeConstants.BMI_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setBmiObservation(observation);
    } else if (MetaCodeConstants.HEIGHT_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setHeightObservation(observation);
    } else if (MetaCodeConstants.WEIGHT_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setWeightObservation(observation);
    } else if (MetaCodeConstants.SUICIDE_SCREENER_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setSuicideScreenerObservation(observation);
    } else if (MetaCodeConstants.SUBSTANCE_ABUSE_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setSubstanceAbuseObservation(observation);
    } else if (MetaCodeConstants.REGULAR_SMOKER_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setRegularSmokerObservation(observation);
    } else if (MetaCodeConstants.PREGNANCY_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setPregnancyObservation(observation);
    } else if (MetaCodeConstants.MENTAL_HEALTH_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setMentalHealthObservation(observation);
    } else if (MetaCodeConstants.TEMPERATURE_KEY.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setTemperatureObservation(observation);
    } else if (MetaCodeConstants.RISK_LEVEL.equalsIgnoreCase(text)) {
        searchPersonDetailsDTO.setRedRiskObservation(observation);
    } else if (Constants.OBSERVATION_VITAL_SIGNS.equalsIgnoreCase(observation.getIdentifier().getFirst().getValue())) {
        searchPersonDetailsDTO.setVitalSignsObservation(observation);
    }
}
    /**
     * <p>
     * Add FHIR bundle entries from source bundle to destination
     * bundle
     * </p>
     *
     * @param source       The source bundle
     * @param destination  The destination bundle
     *
     */
    public void addBundleEntries(Bundle source, Bundle destination) {
       for(Bundle.BundleEntryComponent bundleEntryComponent: source.getEntry()) {
           destination.addEntry(bundleEntryComponent);
       }
    }
}
