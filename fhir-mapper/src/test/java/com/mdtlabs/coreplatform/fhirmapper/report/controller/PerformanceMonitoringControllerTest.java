package com.mdtlabs.coreplatform.fhirmapper.report.controller;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.fhirmapper.report.service.PerformanceMonitoringService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PerformanceMonitoringControllerTest {

    @InjectMocks
    private PerformanceMonitoringController performanceMonitoringController;

    @Mock
    private PerformanceMonitoringService performanceMonitoringService;

    @Test
    void getChwPerformanceReport() {
        //given
        FilterRequestDTO filterRequestDTO = new FilterRequestDTO();

        //when
        when(performanceMonitoringService.getChwPerformanceReport(filterRequestDTO)).thenReturn(new HashMap<>());

        //then
        Map<String, Map<String, PerformanceReport>> response = performanceMonitoringController.getChwPerformanceReport(filterRequestDTO);
        Assertions.assertNotNull(response);
    }

}