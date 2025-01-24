package com.mdtlabs.coreplatform.fhirmapper.converter;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;

import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Map;
import java.util.Objects;

/**
 * <p>
 * Converts BpLogDTO to FHIR Observation entity for Blood Pressure observations.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class BloodPressureConverter {
    private final FhirUtils fhirUtils;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    @Autowired
    public BloodPressureConverter(FhirUtils fhirUtils,
            FhirAssessmentMapper fhirAssessmentMapper) {
        this.fhirUtils = fhirUtils;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
    }
    /**
     * Converts BpLogDTO to FHIR Observation entity.
     *
     * @param bpLogDTO    The blood pressure details to convert
     * @param bpTakenTime The blood pressure taken time
     *
     * @return The FHIR Observation entity representing the Blood Pressure observation.
     */
    public Observation createBloodPressureObservation(BpLogDTO bpLogDTO, Date bpTakenTime,
                                                      Map<String, String> cvdRiskDetails) {
        Observation observation = new Observation();
        observation.setStatus(Observation.ObservationStatus.FINAL);
        observation.setCode(fhirUtils.setCodes(MetaCodeConstants.BLOOD_PRESSURE_KEY));
        observation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_BLOOD_PRESSURE);
        if (Objects.nonNull(bpTakenTime)) {
            observation.setEffective(new DateTimeType(bpTakenTime));
        } else {
            observation.setEffective(new DateTimeType(new Date()));
        }

        if (Objects.nonNull(bpLogDTO.getAvgSystolic())) {
            Observation.ObservationComponentComponent systolicComponent = new Observation.ObservationComponentComponent();
            systolicComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.SYSTOLIC_BLOOD_PRESSURE_KEY));
            systolicComponent.getCode().setText(MetaCodeConstants.AVERAGE_SYSTOLIC_BLOOD_PRESSURE);
            systolicComponent.setValue(new Quantity().setValue(bpLogDTO.getAvgSystolic()).setUnit(FhirConstants.BLOOD_PRESSURE_UNIT).setSystem(FhirConstants.UNIT_SOFT_MEASURE_ORG_URL)
                    .setCode(FhirConstants.BLOOD_PRESSURE_UNIT_CODE));
            observation.addComponent(systolicComponent);
        }
        if (Objects.nonNull(bpLogDTO.getAvgDiastolic())) {
            Observation.ObservationComponentComponent diastolicComponent = new Observation.ObservationComponentComponent();
            diastolicComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.DIASTOLIC_BLOOD_PRESSURE_KEY));
            diastolicComponent.getCode().setText(MetaCodeConstants.AVERAGE_DIASTOLIC_BLOOD_PRESSURE);
            diastolicComponent.setValue(new Quantity().setValue(bpLogDTO.getAvgDiastolic()).setUnit(FhirConstants.BLOOD_PRESSURE_UNIT).setSystem(FhirConstants.UNIT_SOFT_MEASURE_ORG_URL)
                    .setCode(FhirConstants.BLOOD_PRESSURE_UNIT_CODE));
            observation.addComponent(diastolicComponent);
        }
        if (Objects.nonNull(bpLogDTO.getBpLogDetails())
                && !bpLogDTO.getBpLogDetails().isEmpty()) {
            setBpLogDetails(observation, bpLogDTO);
        }

        fhirAssessmentMapper.createObservationComponent(bpLogDTO.getIsBeforeHtnDiagnosis(), 
            MetaCodeConstants.HAVE_YOU_BEEN_DIAGNOSED_WITH_HIGH_BLOOD_PRESSURE_BEFORE, 
            observation.getComponent());
        for (Map.Entry<String, String> entry : cvdRiskDetails.entrySet()) {
            Observation.ObservationComponentComponent
                    cvdRiskComponent = new Observation.ObservationComponentComponent();
            cvdRiskComponent.getCode().setText(entry.getKey());
            cvdRiskComponent.setValue(new StringType(entry.getValue()));
            observation.addComponent(cvdRiskComponent);
        }
        return observation;
    }

    /**
     * Set blood pressure details to FHIR observation entity using bp log details.
     *
     * @param observation   The FHIR Observation entity.
     * @param bpLogDTO      The blood pressure log details.
     *
     */
    private void setBpLogDetails(Observation observation, BpLogDTO bpLogDTO) {
        int index = 1;

        for (BpLogDetailsDTO bpLogDetailsDTO: bpLogDTO.getBpLogDetails()) {
            Observation.ObservationComponentComponent systolicComponent = new Observation.ObservationComponentComponent();
            systolicComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.SYSTOLIC_BLOOD_PRESSURE_KEY));
            systolicComponent.getCode().setText(MetaCodeConstants.SYSTOLIC_BLOOD_PRESSURE + " " + index);
            systolicComponent.setValue(new Quantity().setValue(Long.parseLong(bpLogDetailsDTO.getSystolic())).setUnit(FhirConstants.BLOOD_PRESSURE_UNIT).setSystem(FhirConstants.UNIT_SOFT_MEASURE_ORG_URL)
                    .setCode(FhirConstants.BLOOD_PRESSURE_UNIT_CODE));
            observation.addComponent(systolicComponent);

            Observation.ObservationComponentComponent diastolicComponent = new Observation.ObservationComponentComponent();
            diastolicComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.DIASTOLIC_BLOOD_PRESSURE_KEY));
            diastolicComponent.getCode().setText(MetaCodeConstants.DIASTOLIC_BLOOD_PRESSURE + " " + index);
            diastolicComponent.setValue(new Quantity().setValue(Long.parseLong(bpLogDetailsDTO.getDiastolic())).setUnit(FhirConstants.BLOOD_PRESSURE_UNIT).setSystem(FhirConstants.UNIT_SOFT_MEASURE_ORG_URL)
                    .setCode(FhirConstants.BLOOD_PRESSURE_UNIT_CODE));
            observation.addComponent(diastolicComponent);
            
            if (!Objects.isNull(bpLogDetailsDTO.getPulse())) {
                Observation.ObservationComponentComponent pulseComponent = new Observation.ObservationComponentComponent();
                pulseComponent.setCode(fhirUtils.setCodes(MetaCodeConstants.PULSE_KEY));
                pulseComponent.getCode().setText(MetaCodeConstants.PULSE + " " + index);
                pulseComponent.setValue(new Quantity().setValue(Long.parseLong(bpLogDetailsDTO.getPulse())).setUnit(FhirConstants.PULSE_UNIT).setSystem(FhirConstants.UNIT_SOFT_MEASURE_ORG_URL)
                        .setCode(FhirConstants.PULSE_UNIT));
                observation.addComponent(pulseComponent);
            }
            index ++;
        }
    }
}
