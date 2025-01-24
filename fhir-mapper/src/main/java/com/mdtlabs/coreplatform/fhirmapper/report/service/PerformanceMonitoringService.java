package com.mdtlabs.coreplatform.fhirmapper.report.service;

import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PerformanceReport;

/**
 * <p>
 * This an interface class for performance monitoring module and you can implement this
 * class in any class.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on July 09, 2024
 */
public interface PerformanceMonitoringService {

    /**
     * Get performance monitoring report for the given request.
     *
     * @param request  - This is the request to get the performance monitoring report detail.
     * @return Map     - Report data in the desired format.
     */
    public Map<String, Map<String, PerformanceReport>> getChwPerformanceReport(FilterRequestDTO request);
}
