package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This is a DTO class to collect the labTestRequest Details.
 *
 * @author Jaganathan R Created on 30 Jul 2024
 */
@Data
public class LabTestRequestDTO {

    private List<LabTestDTO> labTests;

    private EncounterDetailsDTO encounter;

    private String type;

    private LabTestDTO labTest;

    private String requestFrom;

}
