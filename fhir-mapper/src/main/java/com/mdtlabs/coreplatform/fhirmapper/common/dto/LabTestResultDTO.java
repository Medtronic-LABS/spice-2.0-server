package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * This is a DTO class to collect the labTestResult Details.
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

    private String performedBy;

    private String performedName;
}
