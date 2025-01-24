package com.mdtlabs.coreplatform.fhirmapper.converter;

import org.hl7.fhir.r4.model.Condition;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ConditionConverterTest {

    @InjectMocks
    private ConditionConverter conditionConverter;

    @Mock
    private FhirUtils fhirUtils;

    @Test
    void convertPregnancyDiagnosisToCondition() {
        //given
        Condition condition = new Condition();
        PregnancyDetailsDTO pregnancyDetailsDTO = TestDataProvider.getPregnancyDetailsDTO();

        //then
        conditionConverter.convertPregnancyDiagnosisToCondition(condition, pregnancyDetailsDTO);
        Assertions.assertNotNull(condition.getCategory());
    }
}