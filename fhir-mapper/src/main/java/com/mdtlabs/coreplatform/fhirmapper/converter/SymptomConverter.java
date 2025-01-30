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
            symptomComponent.setCode(fhirUtils.setCodes(symptomDTO.getValue()));
            if (MetaCodeConstants.OTHER_KEY.equals(symptomDTO.getValue())) {
                symptomComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.OTHER_KEY));
                if (Objects.nonNull(symptomDTO.getOtherSymptom())) {
                    symptomComponent.setValue(new StringType(symptomDTO.getOtherSymptom()));
                }
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

            if (Objects.equals(MetaCodeConstants.NO_SYMPTOMS_KEY, symptomDTO.getValue())) {
                symptomDTO.setName(MetaCodeConstants.NO_SYMPTOMS_DIABETES_KEY);
            }
            symptomComponent.setCode(fhirUtils.setCodes(symptomDTO.getValue()));
            if (MetaCodeConstants.OTHER_KEY.equals(symptomDTO.getValue())) {
                symptomComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.OTHER_KEY));
                if (Objects.nonNull(glucoseLogDTO.getDiabetesOtherSymptoms())) {
                    symptomComponent.setValue(new StringType(glucoseLogDTO.getDiabetesOtherSymptoms()));
                }
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
    public List<String> getSymptomListByBundle(Bundle bundle, boolean isBpLog) {
        List<String> symptoms = new ArrayList<>();
        for (Bundle.BundleEntryComponent bundleEntryComponent : bundle.getEntry()) {
            Observation observation = ((Observation) bundleEntryComponent.getResource());
            for (Observation.ObservationComponentComponent observationComponent : observation.getComponent()) {
                symptoms.add(fhirUtils.getText(observationComponent.getCode().getText()));
            }
        }
        return symptoms;
    }
}