package com.mdtlabs.coreplatform.fhirmapper.glucoselog.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Observation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodGlucoseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class GlucoseLogServiceImplTest {

    @InjectMocks
    private GlucoseLogServiceImpl glucoseLogService;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private BloodGlucoseConverter bloodGlucoseConverter;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private SymptomConverter symptomConverter;

    @Test
    void getPatientGlucoseLogsWithSymptoms() {
        //given
        RequestDTO patientGlucoseLogRequestData = TestDataProvider.getRequestDTO();
        Map<String, MetaCodeDetails> codeDetails = new HashMap<>();
        Bundle bundle = TestDataProvider.getObservationBundle();
        codeDetails.put("bloodGlucose", TestDataProvider.getMetaCodeDetails());
        GlucoseLogDTO glucoseLogDTO = TestDataProvider.getGlucoseLogRequest();
        Observation observation = null;
        for (Bundle.BundleEntryComponent bundleEntryComponent : bundle.getEntry()) {
            observation = ((Observation) bundleEntryComponent.getResource());
        }

        //when
        when(fhirUtils.getCodeDetails()).thenReturn(codeDetails);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(bloodGlucoseConverter.convertObservationToGlucoseLogDTO(observation)).thenReturn(glucoseLogDTO);

        //then
        PatientGlucoseLogDTO response = glucoseLogService.getPatientGlucoseLogsWithSymptoms(patientGlucoseLogRequestData);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientGlucoseLogs() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Bundle bundle = TestDataProvider.getObservationBundle();
        Map<String, MetaCodeDetails> codeDetails = new HashMap<>();
        codeDetails.put("bloodGlucose", TestDataProvider.getMetaCodeDetails());

        //when
        when(fhirUtils.getCodeDetails()).thenReturn(codeDetails);
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        List<GlucoseLogDTO> response = glucoseLogService.getPatientGlucoseLogs(requestDTO);
        Assertions.assertNotNull(response);
    }
}