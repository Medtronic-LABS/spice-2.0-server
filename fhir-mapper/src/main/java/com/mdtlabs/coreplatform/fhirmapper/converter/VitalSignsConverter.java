package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

/**
 * <p>
 * This vital signs converter helps to add patient recent risk details, bp, bg, height, weight, bmi details
 * in patient vital signs observation
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-09-03
 */
@Component
public class VitalSignsConverter {
    private final RestApiUtil restApiUtil;

    @Autowired
    public VitalSignsConverter(RestApiUtil restApiUtil) {
        this.restApiUtil = restApiUtil;
    }

    /**
     * Create or update patient vital signs details based on
     * given patient vital signs details
     *
     * @param vitalSignsDTO   The vital signs details of the patient.
     * @param bundle          The Fhir bundle entity.
     *
     * @return Converted FHIR Observation entity.
     */
    public Observation createOrUpdateVitalSigns(VitalSignsDTO vitalSignsDTO, Bundle bundle) {
        String searchVitalSigns = String.format(
                Constants.SEARCH_RELATED_PERSON_VITAL_SIGNS, FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                Constants.OBSERVATION_VITAL_SIGNS,
                vitalSignsDTO.getRelatedPersonId());
        Bundle vitalResponseBundle = restApiUtil.getBatchRequest(searchVitalSigns);
        Observation vitalSignsObservation = null;

        if (!vitalResponseBundle.getEntry().isEmpty()) {
            vitalSignsObservation = (Observation) vitalResponseBundle.getEntry().getFirst().getResource();
            List<Reference> referenceList = processVitalSigns(vitalSignsDTO, vitalSignsObservation);
            if (!referenceList.isEmpty()) {
                vitalSignsObservation.setDerivedFrom(referenceList);
            }
        } else {
            vitalSignsObservation = createVitalSignsObservation(vitalSignsDTO);
        }
        if (Objects.nonNull(vitalSignsDTO.getBpObservation())) {
            setBloodPressureInVitals(vitalSignsObservation, vitalSignsDTO.getBpObservation());
            setCvdRiskLevelInVitalsComponent(vitalSignsObservation, vitalSignsDTO.getBpObservation());
        }
        if (Objects.nonNull(vitalSignsDTO.getBgObservation())) {
            setBloodGlucoseInVitals(vitalSignsObservation, vitalSignsDTO.getBgObservation());
        }
        if (Objects.nonNull(vitalSignsDTO.getRedRiskObservation())) {
            setRedRiskInVitalsComponent(vitalSignsObservation, vitalSignsDTO.getRedRiskObservation());
        }
        return vitalSignsObservation;
    }

    /**
     * Set Cvd risk level in patient vital signs observation component
     *
     * @param vitalSignsObservation     The patient Vital Observation entity
     * @param riskObservation           The patient risk Observation entity
     *
     */
    private void setCvdRiskLevelInVitalsComponent(Observation vitalSignsObservation, Observation riskObservation) {
        Optional<Observation.ObservationComponentComponent> cvdRiskLevelComponentOpt = riskObservation.getComponent().stream()
                .filter(component -> FhirConstants.CVD_RISK_LEVEL.equals(component.getCode().getText()))
                .findFirst();
        Observation.ObservationComponentComponent cvdRiskLevelComponent = cvdRiskLevelComponentOpt.orElse(null);

        Optional<Observation.ObservationComponentComponent> existingCvdRiskLevel = vitalSignsObservation.getComponent().stream()
                .filter(component -> FhirConstants.CVD_RISK_LEVEL.equals(component.getCode().getText()))
                .findFirst();

        if (Objects.nonNull(cvdRiskLevelComponent)) {
            existingCvdRiskLevel.ifPresentOrElse(
                    component -> {
                        component.setCode(cvdRiskLevelComponent.getCode());
                        component.setValue(cvdRiskLevelComponent.getValueStringType());
                    },
                    () -> {
                        Observation.ObservationComponentComponent newCvdRiskLevelComponent
                                = new Observation.ObservationComponentComponent();
                        newCvdRiskLevelComponent.setCode(cvdRiskLevelComponent.getCode());
                        newCvdRiskLevelComponent.setValue(cvdRiskLevelComponent.getValueStringType());
                        vitalSignsObservation.addComponent(newCvdRiskLevelComponent);
                    }
            );
        }
    }

    /**
     * Set red risk details in patient vital signs observation
     *
     * @param vitalSignsObservation     The patient Vital Observation entity
     * @param riskObservation           The patient risk Observation entity
     *
     */
    private void setRedRiskInVitalsComponent(Observation vitalSignsObservation, Observation riskObservation) {
        String riskLevel = riskObservation.getValueStringType().getValue();
        Optional<Observation.ObservationComponentComponent> existingRiskLevel = vitalSignsObservation.getComponent().stream()
                .filter(component -> FhirConstants.RISK_LEVEL.equals(component.getCode().getText()))
                .findFirst();

        if (Objects.nonNull(riskLevel)) {
            existingRiskLevel.ifPresentOrElse(
                    component -> {
                        component.setCode(riskObservation.getCode());
                        component.setValue(riskObservation.getValueStringType());
                    },
                    () -> {
                        Observation.ObservationComponentComponent newRiskLevelComponent
                                = new Observation.ObservationComponentComponent();
                        newRiskLevelComponent.setCode(riskObservation.getCode());
                        newRiskLevelComponent.setValue(riskObservation.getValueStringType());
                        vitalSignsObservation.addComponent(newRiskLevelComponent);
                    }
            );
        }
    }

    /**
     * Set blood pressure details in patient vital signs observation
     *
     * @param vitalSignsObservation     The patient Vital Observation entity
     * @param bpObservation             The patient Blood Pressure Observation entity
     *
     */
    private void setBloodPressureInVitals(Observation vitalSignsObservation, Observation bpObservation) {
        Optional<Observation.ObservationComponentComponent> systolicComponentOpt = bpObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.AVERAGE_SYSTOLIC_BLOOD_PRESSURE.equals(component.getCode().getText()))
                .findFirst();
        Optional<Observation.ObservationComponentComponent> diastolicComponentOpt = bpObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.AVERAGE_DIASTOLIC_BLOOD_PRESSURE.equals(component.getCode().getText()))
                .findFirst();
        Observation.ObservationComponentComponent systolicComponent = systolicComponentOpt.orElse(null);
        Observation.ObservationComponentComponent diastolicComponent = diastolicComponentOpt.orElse(null);

        Optional<Observation.ObservationComponentComponent> existingSystolic = vitalSignsObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.AVERAGE_SYSTOLIC_BLOOD_PRESSURE.equals(component.getCode().getText()))
                .findFirst();
        Optional<Observation.ObservationComponentComponent> existingDiastolic = vitalSignsObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.AVERAGE_DIASTOLIC_BLOOD_PRESSURE.equals(component.getCode().getText()))
                .findFirst();

        if (Objects.nonNull(systolicComponent)) {
            existingSystolic.ifPresentOrElse(
                    component -> {
                        component.setCode(systolicComponent.getCode());
                        component.setValue(systolicComponent.getValueQuantity());
                    },
                    () -> {
                        Observation.ObservationComponentComponent newSystolicComponent
                                = new Observation.ObservationComponentComponent();
                        newSystolicComponent.setCode(systolicComponent.getCode());
                        newSystolicComponent.setValue(systolicComponent.getValueQuantity());
                        vitalSignsObservation.addComponent(newSystolicComponent);
                    }
            );
        }

        if (Objects.nonNull(diastolicComponent)) {
            existingDiastolic.ifPresentOrElse(
                    component -> {
                        component.setCode(diastolicComponent.getCode());
                        component.setValue(diastolicComponent.getValueQuantity());
                    },
                    () -> {
                        Observation.ObservationComponentComponent newDiastoliComponent
                                = new Observation.ObservationComponentComponent();
                        newDiastoliComponent.setCode(diastolicComponent.getCode());
                        newDiastoliComponent.setValue(diastolicComponent.getValueQuantity());
                        vitalSignsObservation.addComponent(newDiastoliComponent);
                    }
            );
        }
    }

    /**
     * Set blood glucose details in patient vital signs observation
     *
     * @param vitalSignsObservation     The patient Vital Observation entity
     * @param bgObservation             The patient Blood Glucose Observation entity
     *
     */
    private void setBloodGlucoseInVitals(Observation vitalSignsObservation, Observation bgObservation) {
        Optional<Observation.ObservationComponentComponent>
                hba1cComponentOpt = bgObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.HBA1C.equals(component.getCode().getText()))
                .findFirst();
        Observation.ObservationComponentComponent hba1cComponent = hba1cComponentOpt.orElse(null);

        Optional<Observation.ObservationComponentComponent> existingGlucose = vitalSignsObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.BLOOD_GLUCOSE_KEY.equals(component.getCode().getText()))
                .findFirst();
        Optional<Observation.ObservationComponentComponent> existingHba1c = vitalSignsObservation.getComponent().stream()
                .filter(component -> MetaCodeConstants.HBA1C.equals(component.getCode().getText()))
                .findFirst();

        if (Objects.nonNull(bgObservation.getCode())) {
            existingGlucose.ifPresentOrElse(
                    component -> {
                        if (Objects.nonNull(bgObservation.getValueQuantity().getValue())) {
                            component.setCode(bgObservation.getCode());
                            component.setValue(bgObservation.getValueQuantity());
                        }
                    },
                    () -> {
                        Observation.ObservationComponentComponent newGlucoseomponent
                                = new Observation.ObservationComponentComponent();
                        newGlucoseomponent.setCode(bgObservation.getCode());
                        newGlucoseomponent.setValue(bgObservation.getValueQuantity());
                        vitalSignsObservation.addComponent(newGlucoseomponent);
                    }
            );
        }
        if (Objects.nonNull(hba1cComponent)) {
            existingHba1c.ifPresentOrElse(
                    component -> {
                        if (Objects.nonNull(hba1cComponent.getValueQuantity().getValue())) {
                            component.setCode(hba1cComponent.getCode());
                            component.setValue(hba1cComponent.getValueQuantity());
                        }
                    },
                    () -> {
                        Observation.ObservationComponentComponent newHba1cComponent
                                = new Observation.ObservationComponentComponent();
                        newHba1cComponent.setCode(hba1cComponent.getCode());
                        newHba1cComponent.setValue(hba1cComponent.getValueQuantity());
                        vitalSignsObservation.addComponent(newHba1cComponent);
                    }
            );
        }
    }

    /**
     * Create patient vital signs observation based on
     * given patient vital signs details
     *
     * @param vitalSignsDTO   The vital signs details of the patient.
     *
     * @return Converted FHIR Observation entity.
     */
    public Observation createVitalSignsObservation(VitalSignsDTO vitalSignsDTO) {
        Observation vitalSignsObservation = new Observation();
        vitalSignsObservation.setStatus(Observation.ObservationStatus.FINAL);
        vitalSignsObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(FhirConstants.VITAL_SIGNS_IDENTIFIER);

        if (Objects.nonNull(vitalSignsDTO.getRelatedPersonId())) {
            vitalSignsObservation.addPerformer(new Reference(
                    String.format(FhirConstants.RELATED_PERSON_ID, vitalSignsDTO.getRelatedPersonId())));
        } else {
            vitalSignsObservation.addPerformer(new Reference(FhirConstants.RELATED_PERSON_IDENTIFIER_URL));
        }
        if (Objects.nonNull(vitalSignsDTO.getScreenedLandmark())) {
            vitalSignsObservation.addIdentifier().setSystem(FhirIdentifierConstants.SCREENED_LANDMARK_SYSTEM_URL)
                    .setValue(vitalSignsDTO.getScreenedLandmark());
        }
        if (Objects.nonNull(vitalSignsDTO.getBpObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getBgObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.BLOOD_SUGAR_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getHeightObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.HEIGHT_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getWeightObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.WEIGHT_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getBmiObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.BMI_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getTemperatureObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.TEMPERATURE_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getRegularSmokerObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.REGULAR_SMOKER_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getSuicideObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getSubstanceAbuseObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getPregnancyObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.PREGNANCY_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getMentalHealthObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL)));
        }
        if (Objects.nonNull(vitalSignsDTO.getRedRiskObservation())) {
            vitalSignsObservation.addDerivedFrom((new Reference(FhirConstants.RED_RISK_OBSERVATION_IDENTIFIER_URL)));
        }
        return vitalSignsObservation;
    }

    /**
     * Process the patient vital details from given vitalSignsDTO
     * and set vital observation details in personDetailsDTO
     *
     * @param vitalSignsDTO          The vital signs details of the patient.
     * @param vitalSignsObservation  The FHIR observation entity
     *
     * @return Converted FHIR Observation entity.
     */
    private List<Reference> processVitalSigns(VitalSignsDTO vitalSignsDTO,
                                  Observation vitalSignsObservation) {
        String searchVitalSigns = String.format(
                Constants.SEARCH_VITAL_SIGNS,
                vitalSignsObservation.getIdPart());
        Bundle vitalResponsBundle = restApiUtil.getBatchRequest(searchVitalSigns);
        SearchPersonDetailsDTO personDetailsDTO = new SearchPersonDetailsDTO();
        for (Bundle.BundleEntryComponent entry : vitalResponsBundle.getEntry()) {
            Resource resource = entry.getResource();

            if (resource instanceof Observation observation) {
                this.setObservationDetails(personDetailsDTO, observation,
                        observation.getCode().getText());
            }
        }
        setScreenedLandmarkInVitals(vitalSignsDTO, vitalSignsObservation);
        return constructVitalSignsReference(vitalSignsDTO, personDetailsDTO);
    }

    /**
     * Sets observation details in Person Details DTO based on given text
     *
     * @param searchPersonDetailsDTO  The Search PersonDetails DTO
     * @param observation       The FHIR observation entity
     * @param text              The observation type
     *
     * @return Converted FHIR Observation entity.
     */
    private void setObservationDetails(SearchPersonDetailsDTO searchPersonDetailsDTO,
                                   Observation observation, String text) {
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
        }
}

    /**
     * Set weight details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setHeightInVitals(VitalSignsDTO vitalSignsDTO,
            SearchPersonDetailsDTO personDetailsDTO,
            List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getHeightObservation())) {
            referenceList.add(new Reference(FhirConstants.HEIGHT_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getHeightObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getHeightObservation().getIdPart())));
        }
    }

    /**
     * Set height details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setWeightInVitals(VitalSignsDTO vitalSignsDTO,
                                   SearchPersonDetailsDTO personDetailsDTO,
                                   List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getWeightObservation())) {
            referenceList.add(new Reference(FhirConstants.WEIGHT_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getWeightObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getWeightObservation().getIdPart())));
        }
    }

    /**
     * Set bmi details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setTemperatureInVitals(VitalSignsDTO vitalSignsDTO,
                                SearchPersonDetailsDTO personDetailsDTO,
                                List<Reference> referenceList) {

        if (Objects.nonNull(vitalSignsDTO.getTemperatureObservation())) {
            referenceList.add(new Reference(FhirConstants.TEMPERATURE_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getTemperatureObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getTemperatureObservation().getIdPart())));
        }
    }

    /**
     * Set bmi details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setBmiInVitals(VitalSignsDTO vitalSignsDTO,
                                   SearchPersonDetailsDTO personDetailsDTO,
                                   List<Reference> referenceList) {

        if (Objects.nonNull(vitalSignsDTO.getBmiObservation())) {
            referenceList.add(new Reference(FhirConstants.BMI_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getBmiObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getBmiObservation().getIdPart())));
        }
    }

    /**
     * Set regular smoker details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setRegularSmokerInVitals(VitalSignsDTO vitalSignsDTO,
                                   SearchPersonDetailsDTO personDetailsDTO,
                                   List<Reference> referenceList) {

        if (Objects.nonNull(vitalSignsDTO.getRegularSmokerObservation())) {
            referenceList.add(new Reference(FhirConstants.REGULAR_SMOKER_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getRegularSmokerObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID,
                        personDetailsDTO.getRegularSmokerObservation().getIdPart())));
        }
    }

    /**
     * Set blood glucose details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setBloodGlucoseInVitals(VitalSignsDTO vitalSignsDTO,
                                   SearchPersonDetailsDTO personDetailsDTO,
                                   List<Reference> referenceList) {

        if (Objects.nonNull(vitalSignsDTO.getBgObservation())) {
            referenceList.add(new Reference(FhirConstants.BLOOD_SUGAR_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getBgObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getBgObservation().getIdPart())));
        }
    }

    /**
     * Set blood pressure details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setBloodPressureInVitals(VitalSignsDTO vitalSignsDTO,
                                   SearchPersonDetailsDTO personDetailsDTO,
                                   List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getBpObservation())) {
            referenceList.add(new Reference(FhirConstants.BLOOD_PRESSURE_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getBpObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getBpObservation().getIdPart())));
        }
    }

    /**
     * Set suicide details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setSuicideDetailsInVitals(VitalSignsDTO vitalSignsDTO,
                               SearchPersonDetailsDTO personDetailsDTO,
                               List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getSuicideObservation())) {
            referenceList.add(new Reference(FhirConstants.SUICIDE_SCREENER_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getSuicideScreenerObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getSuicideScreenerObservation().getIdPart())));
        }
    }

    /**
     * Set substance abuse details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setSubstanceDetailsInVitals(VitalSignsDTO vitalSignsDTO,
                               SearchPersonDetailsDTO personDetailsDTO,
                               List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getSubstanceAbuseObservation())) {
            referenceList.add(new Reference(FhirConstants.SUBSTANCE_ABUSE_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getSubstanceAbuseObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getSubstanceAbuseObservation().getIdPart())));
        }
    }

    /**
     * Set pregnancy details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setPregnancyDetailsInVitals(VitalSignsDTO vitalSignsDTO,
                                             SearchPersonDetailsDTO personDetailsDTO,
                                             List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getPregnancyObservation())) {
            referenceList.add(new Reference(FhirConstants.PREGNANCY_IDENTIFIER_URL));
        } else if (Objects.nonNull(personDetailsDTO.getPregnancyObservation())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, personDetailsDTO.getPregnancyObservation().getIdPart())));
        }
    }

    /**
     * Set mental health details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setMentalHealthDetailsInVitals(VitalSignsDTO vitalSignsDTO,
                                             SearchPersonDetailsDTO personDetailsDTO,
                                             List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getMentalHealthObservation())) {
            if (Objects.nonNull(vitalSignsDTO.getMentalHealthObservation().getIdPart())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, vitalSignsDTO.getMentalHealthObservation().getIdPart())));
            } else {
                referenceList.add(new Reference(FhirConstants.MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL));
            }
        } else if (Objects.nonNull(personDetailsDTO.getMentalHealthObservation())) {
            referenceList.add(new Reference(String.format(
                    FhirConstants.OBSERVATION_ID, personDetailsDTO.getMentalHealthObservation().getIdPart())));
        }
    }

    /**
     * <p>
     * Set screened location details in patient vital signs observation
     * </p>
     *
     * @param vitalSignsDTO             The Vital Signs DTO
     * @param vitalSignsObservation     The patient vital signs observation
     *
     */
    private void setScreenedLandmarkInVitals(VitalSignsDTO vitalSignsDTO,
                                     Observation vitalSignsObservation) {
        if (Objects.nonNull(vitalSignsDTO.getScreenedLandmark())) {
            List<Identifier> updatedIdentifiers = vitalSignsObservation.getIdentifier().stream()
                    .filter(identifier -> !FhirIdentifierConstants.SCREENED_LANDMARK_SYSTEM_URL
                            .equals(identifier.getSystem()))
                    .toList();
            vitalSignsObservation.getIdentifier().clear();
            vitalSignsObservation.getIdentifier().addAll(updatedIdentifiers);
            vitalSignsObservation.addIdentifier()
                    .setSystem(FhirIdentifierConstants.SCREENED_LANDMARK_SYSTEM_URL)
                    .setValue(vitalSignsDTO.getScreenedLandmark());
        }
    }

    /**
     * Set red risk details in patient vital signs observation
     *
     * @param vitalSignsDTO     The Vital Signs DTO
     * @param personDetailsDTO  The SearchPersonDetailsDTO entity
     * @param referenceList     The patient observation reference list
     *
     */
    private void setRedRiskDetailsInVitals(VitalSignsDTO vitalSignsDTO,
                                                SearchPersonDetailsDTO personDetailsDTO,
                                                List<Reference> referenceList) {
        if (Objects.nonNull(vitalSignsDTO.getRedRiskObservation())) {
            if (Objects.nonNull(vitalSignsDTO.getRedRiskObservation())
                    && Objects.nonNull(vitalSignsDTO.getRedRiskObservation().getIdPart())) {
                referenceList.add(new Reference(String.format(
                        FhirConstants.OBSERVATION_ID, vitalSignsDTO.getRedRiskObservation().getIdPart())));
            } else {
                referenceList.add(new Reference(FhirConstants.RED_RISK_OBSERVATION_IDENTIFIER_URL));
            }
        } else if (Objects.nonNull(personDetailsDTO.getRedRiskObservation())) {
            referenceList.add(new Reference(String.format(
                    FhirConstants.OBSERVATION_ID, personDetailsDTO.getRedRiskObservation().getIdPart())));
        }
    }

    /**
     * Process the patient vital details from given vitalSignsDTO
     * and set vital observations references in reference list
     *
     * @param vitalSignsDTO      The vital signs details of the patient.
     * @param personDetailsDTO   The PersonDetailsDTO entity
     *
     * @return list of vital observation references.
     */
    private List<Reference> constructVitalSignsReference(VitalSignsDTO vitalSignsDTO,
                                                         SearchPersonDetailsDTO personDetailsDTO) {
        List<Reference> referenceList = new ArrayList<>();
        setHeightInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setWeightInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setBmiInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setTemperatureInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setBloodPressureInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setBloodGlucoseInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setRegularSmokerInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setSuicideDetailsInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setSubstanceDetailsInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setPregnancyDetailsInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setMentalHealthDetailsInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        setRedRiskDetailsInVitals(vitalSignsDTO, personDetailsDTO, referenceList);
        return referenceList;
    }
}
