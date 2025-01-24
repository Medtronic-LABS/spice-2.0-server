package com.mdtlabs.coreplatform.fhirmapper.converter;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ComplianceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

import org.hl7.fhir.r4.model.Observation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Date;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CompilanceConverterTest {

    @Mock
    FhirUtils fhirUtils;

    @InjectMocks
    ComplianceConverter complianceConverter;

    @Test
    void createComplianceTest() {
        List<ComplianceDTO> complianceList = TestDataProvider.getComplianceList();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = complianceConverter.createComplianceObservation(complianceList, new Date());
        Assertions.assertNotNull(observation);
    }

    @Test
    void createComplianceTestWithDateAsNull() {
        List<ComplianceDTO> complianceList = TestDataProvider.getComplianceList();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = complianceConverter.createComplianceObservation(complianceList, null);
        Assertions.assertNotNull(observation);
    }
}
