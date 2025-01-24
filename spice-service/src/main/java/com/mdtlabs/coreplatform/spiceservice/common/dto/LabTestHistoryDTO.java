package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 *     This is the DTO class for the LabTestHistory.
 * </p>
 *
 * @author Jaganathan R created on Aug 07, 2024
 */
@Data
public class LabTestHistoryDTO {

    private String encounterId;

    private Date dateOfReview;

    private String patientReference;

    private List<Map<String, Object>> history = new ArrayList<>();

    private List<LabTestDTO> investigations = new ArrayList<>();
}
