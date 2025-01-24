package com.mdtlabs.coreplatform.spiceservice.report.controller;

import com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.report.service.PerformanceMonitoringService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class PerformanceMonitoringControllerTest {

    @Mock
    PerformanceMonitoringService performanceMonitoringService;

    @InjectMocks
    PerformanceMonitoringController performanceMonitoringController;

    @Test
    public void testGetChwPerformanceMonitoringReport() {
        performanceMonitoringController.getChwPerformanceMonitoringReport(new FilterRequestDTO());
        verify(performanceMonitoringService, times(1)).getChwPerformanceMonitoringReport(new FilterRequestDTO());
    }
}