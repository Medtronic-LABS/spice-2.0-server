package com.mdtlabs.coreplatform.fhirmapper.converter;


import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Identifier;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 * This condition converter helps to convert FHIR condition resource.
 * </p>
 *
 */
@Component
public class ConditionConverter {

    private final FhirUtils fhirUtils;

    @Autowired
    public ConditionConverter(FhirUtils fhirUtils) {
        this.fhirUtils = fhirUtils;
    }

    /**
     * Convert the diagnosis which given in the pregnancy to the FHIR condition resource.
     *
     * @param condition the FHIR condition
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} object containing the diagnosis details
     */
    public void convertPregnancyDiagnosisToCondition(Condition condition, PregnancyDetailsDTO pregnancyDetailsDTO) {

        // identifier for identify the condition associated with the pregnancy
        condition.setIdentifier(List.of(new Identifier().setSystem(FhirIdentifierConstants.PREGNANT_SYSTEM_URL)
                .setValue(FhirConstants.PREGNANCY)));

        // status
        condition.setClinicalStatus(fhirUtils.setCodes(FhirConstants.ACTIVE));
        condition.setVerificationStatus(fhirUtils.setCodes(FhirConstants.CONFIRMED));

        // list of diagnosis as a categories
        List<CodeableConcept> codeableConceptList = new ArrayList<>();
        for (Map<String, String> diagnosis : pregnancyDetailsDTO.getDiagnosis()) {
                CodeableConcept codeableConcept = fhirUtils.setCodes(Objects.isNull(diagnosis.get(Constants.VALUE))
                        ? diagnosis.get(Constants.NAME) : diagnosis.get(Constants.VALUE));
                codeableConceptList.add(codeableConcept);
        }
        condition.setCategory(codeableConceptList);

        // Diagnosis time
        condition.setOnset(new DateTimeType(pregnancyDetailsDTO.getDiagnosisTime()));
    }

}
