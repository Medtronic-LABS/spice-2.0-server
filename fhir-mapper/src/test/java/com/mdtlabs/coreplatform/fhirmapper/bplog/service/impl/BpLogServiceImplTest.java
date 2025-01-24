package com.mdtlabs.coreplatform.fhirmapper.bplog.service.impl;

import java.util.HashMap;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
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
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class BpLogServiceImplTest {

    @InjectMocks
    private BpLogServiceImpl bpLogService;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private SymptomConverter symptomConverter;

    @Test
    void getPatientBpLogsWithSymptoms() {
        //given
        Bundle bundle = TestDataProvider.getObservationBundle();

        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Map<String, MetaCodeDetails> codeDetailsMap = new HashMap<>();
        codeDetailsMap.put("bloodPressure", TestDataProvider.getMetaCodeDetails());

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(fhirUtils.getCodeDetails()).thenReturn(codeDetailsMap);

        //then
        PatientBpLogsDTO response = bpLogService.getPatientBpLogsWithSymptoms(requestDTO);
        Assertions.assertNotNull(response);
    }
}