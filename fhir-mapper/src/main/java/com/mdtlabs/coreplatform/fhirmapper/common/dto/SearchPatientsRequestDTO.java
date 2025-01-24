package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

@Data
public class SearchPatientsRequestDTO {
    private int skip;
    private String searchText;
    private int limit;
}
