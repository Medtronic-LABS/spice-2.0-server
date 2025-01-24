package com.mdtlabs.coreplatform.fhirmapper.converter;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
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
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class BloodPressureConverterTest {
    @Mock
    FhirUtils fhirUtils;

    @InjectMocks
    BloodPressureConverter bloodPressureConverter;

    @Mock
    FhirAssessmentMapper fhirAssessmentMapper;

    /**
     * Creates new blood pressure observation
     * Unit Test case
     */
    @Test
    void createBloodPressure() {
        BpLogDTO bpLogDTO = TestDataProvider.getBpLogRequest();
        Map<String, String> cvdRiskDetails = TestDataProvider.getCvdRiskDetails();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = bloodPressureConverter.createBloodPressureObservation(bpLogDTO, new Date(), cvdRiskDetails);
        Assertions.assertNotNull(observation);
    }

    /**
     * Creates new blood pressure observation with date time as null
     * Unit Test case
     */
    @Test
    void createBloodPressureWithNullDate() {
        BpLogDTO bpLogDTO = TestDataProvider.getBpLogRequest();
        Map<String, String> cvdRiskDetails = TestDataProvider.getCvdRiskDetails();
        Observation observation = null;

        // when
        when(fhirUtils.setCodes(anyString())).thenReturn(TestDataProvider.getCodeableConceptTest());
        observation = bloodPressureConverter.createBloodPressureObservation(bpLogDTO, null, cvdRiskDetails);
        Assertions.assertNotNull(observation);
    }
}
