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
            complianceComponent.setCode(fhirUtils.setCodes(compliance.getValue()));
            if (MetaCodeConstants.OTHER_KEY.equals(compliance.getValue())
                    && Objects.nonNull(compliance.getOtherCompliance())) {
                complianceComponent.setValue(new StringType(compliance.getOtherCompliance()));
            }
            complianceObservation.addComponent(complianceComponent);
        }
        complianceObservation.setEffective(effectiveDateTime);
        return complianceObservation;
    }
}
