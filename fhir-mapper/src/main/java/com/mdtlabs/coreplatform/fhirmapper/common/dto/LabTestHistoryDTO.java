package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.Data;

@Data
public class LabTestHistoryDTO {

    private String encounterId;

    private Date dateOfReview;

    private String patientReference;

    private List<Map<String, Object>> history = new ArrayList<>();

    private List<LabTestDTO> investigations = new ArrayList<>();
}
