package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * This is a DTO class to collect the labTest Details.
 *
 * @author Jaganathan R Created on 30 Jul 2024
 */
@Data
public class LabTestDTO {

    private String id;

    private String testName;

    private Date testedOn;

    private String recommendedBy;

    private String recommendedName;

    private Date recommendedOn;

    private String patientId;

    private List<LabTestResultDTO> labTestResults;

    private Code codeDetails;

    private LabTestCustomizationDTO labTestCustomization;

    private String comments;

    private String roleName;

    private Boolean isReview;

    private String resultUpdatedBy;

}
