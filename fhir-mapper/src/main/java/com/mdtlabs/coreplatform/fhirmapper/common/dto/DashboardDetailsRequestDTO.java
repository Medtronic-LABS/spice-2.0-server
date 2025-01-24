package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for request patient count details.
 * </p>
 *
 * @author shrikanth Created on September 17, 2024.
 */
@Data
public class DashboardDetailsRequestDTO {
    private String userId;

    private String sortField;

    private Map<String, Date> customDate;
}
