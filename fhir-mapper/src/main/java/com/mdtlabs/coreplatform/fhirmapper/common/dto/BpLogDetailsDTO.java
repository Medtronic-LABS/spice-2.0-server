package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for Bp log details.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class BpLogDetailsDTO {
    private String systolic;
    private String diastolic;
    private String pulse;
}
