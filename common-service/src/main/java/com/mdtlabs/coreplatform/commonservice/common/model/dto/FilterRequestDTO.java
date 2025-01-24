package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

import java.util.Date;
import java.util.Set;

/**
 * This class is for filter request DTO
 *
 * @author Nandhakumar Karthikeyan created on July 09, 2024
 */
@Data
public class FilterRequestDTO extends PaginateDTO{

    private Set<Long> userIds;

    private Set<String> fhirIds;

    private Set<Long> villageIds;

    private Integer year;

    private Integer month;

    private Date fromDate;

    private Date toDate;

}
