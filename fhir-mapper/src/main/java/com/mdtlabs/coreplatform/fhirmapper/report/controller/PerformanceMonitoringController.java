package com.mdtlabs.coreplatform.fhirmapper.report.controller;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.fhirmapper.report.service.PerformanceMonitoringService;

/**
 * <p>
 * Performance Monitoring Controller used to get report details for roles like
 * peer-supervisor.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on June 24, 2024
 */
@RestController
@RequestMapping(value = "/report")
@Validated
public class PerformanceMonitoringController {

    private final PerformanceMonitoringService performanceMonitoringService;

    @Autowired
    public PerformanceMonitoringController(PerformanceMonitoringService performanceMonitoringService) {
        this.performanceMonitoringService = performanceMonitoringService;
    }

    /**
     * This method is used to get performance monitoring report for the given request.
     *
     * @param request - Contains the request to get the report.
     * @return Map     - Response of performance monitoring report retrieved.
     */
    @PostMapping("/chw-performance")
    public Map<String, Map<String, PerformanceReport>> getChwPerformanceReport(@RequestBody FilterRequestDTO request) {
        return performanceMonitoringService.getChwPerformanceReport(request);
    }

}
