package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.time.OffsetDateTime;
import java.util.Map;

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

    private Map<String, OffsetDateTime> customDate;
}
