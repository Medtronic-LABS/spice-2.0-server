package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Map;

import lombok.Data;

/**
 * This class is for paginate DTO
 *
 * @author Nandhakumar Karthikeyan created on Aug 13, 2024
 */
@Data
public class PaginateDTO {

    private String searchTerm;

    private int limit;

    private int skip;

    private Map<String, String> sort;

}
