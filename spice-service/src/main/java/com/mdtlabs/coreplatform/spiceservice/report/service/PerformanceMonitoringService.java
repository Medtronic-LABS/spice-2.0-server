package com.mdtlabs.coreplatform.spiceservice.report.service;

import java.util.List;

import  com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PerformanceReport;


/**
 * <p>
 * This an interface class for performance monitoring module and you can implement this
 * class in any class.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on July 30, 2024
 */
public interface PerformanceMonitoringService {

    /**
     * Get performance monitoring report for the given request.
     *
     * @param requestDTO - This is the filter request to get the report detail.
     * @return Map        - Report data in the desired format.
     */
    List<PerformanceReport> getChwPerformanceMonitoringReport(FilterRequestDTO requestDTO);

}
