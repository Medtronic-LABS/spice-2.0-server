package com.mdtlabs.coreplatform.fhirmapper.converter;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ComplianceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * Converts compliance DTO to FHIR Observation entity.
 * </p>
 *
 * @author Gopinath
 * @version 1.0
 * @since 2024-08-21
 */
@Component
public class ComplianceConverter {
    private final FhirUtils fhirUtils;

    @Autowired
    public ComplianceConverter(FhirUtils fhirUtils) {
        this.fhirUtils = fhirUtils;
    }

    /**
     * Converts compliance to FHIR Observation entity.
     *
     * @param complianceDTOs   List of patient compliance.
     *
     * @return The FHIR Observation entity representing.
     */
    public Observation createComplianceObservation(List<ComplianceDTO> complianceDTOs, Date recorderDate) {
        Observation complianceObservation = new Observation();
        complianceObservation.addIdentifier().setSystem(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL)
                .setValue(Constants.OBSERVATION_COMPLIANCE);
        DateTimeType effectiveDateTime;
        if (Objects.nonNull(recorderDate)) {
            effectiveDateTime = new DateTimeType(recorderDate);
        } else {
            effectiveDateTime = new DateTimeType(new Date());
        }
        for (ComplianceDTO compliance : complianceDTOs) {
            Observation.ObservationComponentComponent complianceComponent =
                    new Observation.ObservationComponentComponent();
            switch (compliance.getName()) {
                case MetaCodeConstants.RAN_OUT_OF_MEDICATION -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.RAN_OUT_OF_MEDICATION_KEY));
                case MetaCodeConstants.MEDICATION_CAUSED_SIDE_EFFECTS -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.MEDICATION_CAUSED_SIDE_EFFECTS_KEY));
                case MetaCodeConstants.STARTED_TAKING_MEDICATION_BUT_STOPPED ->
                        complianceComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.STARTED_TAKING_MEDICATION_BUT_STOPPED_KEY));
                case MetaCodeConstants.TOOK_ALL_MEDICATION -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.TOOK_ALL_MEDICATION_KEY));
                case MetaCodeConstants.COULD_NOT_AFFORD_PAY_FOR_MEDICATION ->
                        complianceComponent.setCode(fhirUtils.setCodes(
                                MetaCodeConstants.COULD_NOT_AFFORD_PAY_FOR_MEDICATION_KEY));
                case MetaCodeConstants.NOT_PRESCRIBED_ANY_MEDICATION -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.NOT_PRESCRIBED_ANY_MEDICATION_KEY));
                case MetaCodeConstants.MISSED_A_LOT_OF_MEDICATION -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.MISSED_A_LOT_OF_MEDICATION_KEY));
                case MetaCodeConstants.MISSED_SOME_MEDICATION -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.MISSED_SOME_MEDICATION_KEY));
                case MetaCodeConstants.DID_NOT_TAKE_ANY_MEDICATION -> complianceComponent.setCode(fhirUtils.setCodes(
                        MetaCodeConstants.DID_NOT_TAKE_ANY_MEDICATION_KEY));
                case MetaCodeConstants.OTHER -> {
                    complianceComponent.setCode(fhirUtils.setCodes(
                            MetaCodeConstants.OTHER_KEY));
                    if (Objects.nonNull(compliance.getOtherCompliance())) {
                        complianceComponent.setValue(new StringType(compliance.getOtherCompliance()));
                    }
                }
            }
            complianceObservation.addComponent(complianceComponent);
        }
        complianceObservation.setEffective(effectiveDateTime);
        return complianceObservation;
    }
}
