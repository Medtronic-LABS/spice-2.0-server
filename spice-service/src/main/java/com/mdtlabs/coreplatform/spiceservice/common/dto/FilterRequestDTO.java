package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.Map;
import java.util.Set;

import lombok.Data;

/**
 * This class is for filter request DTO
 *
 * @author Nandhakumar Karthikeyan created on July 09, 2024
 */
@Data
public class FilterRequestDTO extends PaginateDTO {

    private Set<Long> userIds;

    private Set<String> fhirIds;

    private Set<String> villageIds;

    private Map<String, String> userFhirIds;

    private Integer year;

    private Integer month;

    private Date fromDate;

    private Date toDate;

}
