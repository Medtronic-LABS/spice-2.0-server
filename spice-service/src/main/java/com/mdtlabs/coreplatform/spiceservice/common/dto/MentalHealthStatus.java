package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

@Data
public class MentalHealthStatus {

    private String id;

    private String status;

    private List<String> mentalHealthDisorder;

    private String comments;

    private String yearOfDiagnosis;
}
