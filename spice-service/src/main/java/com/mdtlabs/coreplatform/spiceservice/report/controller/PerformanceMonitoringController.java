package com.mdtlabs.coreplatform.spiceservice.report.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.report.service.PerformanceMonitoringService;

/**
 * <p>
 * Performance Monitoring Controller used to get report details for roles like
 * peer-supervisor.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on July 30, 2024
 */
@RestController
@RequestMapping(value = "/report")
public class PerformanceMonitoringController {

    private final PerformanceMonitoringService performanceMonitoringService;

    public PerformanceMonitoringController(PerformanceMonitoringService performanceMonitoringService) {
        this.performanceMonitoringService = performanceMonitoringService;
    }

    /**
     * This method is used to get performance monitoring report for the given request.
     *
     * @param requestDTO - Contains the filter request object.
     * @return Map        - Response of performance monitoring report.
     */
    @PostMapping("/chw-performance")
    public SuccessResponse<PerformanceReport> getChwPerformanceMonitoringReport(@RequestBody FilterRequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_USER_PERFORMANCE_REPORT,
                performanceMonitoringService.getChwPerformanceMonitoringReport(requestDTO), null, HttpStatus.OK);
    }
}
