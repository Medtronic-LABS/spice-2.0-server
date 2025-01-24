package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * This is a DTO class to collect the LabTestResult Details.
 *
 * @author Jaganathan R Created on 30 Jul 2024
 */
@Data
public class LabTestResultDTO {

    private String id;

    private String name;

    private String value;

    private String unit;

    private String resource;

    private Code codeDetails;

    private Date testedOn;

    private String patientId;

    private Long performedBy;

    private String performedName;
}
