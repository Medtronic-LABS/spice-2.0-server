package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * This is a frequencyDTO to get meta-data for medication frequency.
 * This DTO class for frequency details.
 *
 * @author Karthick M created on Aug 13, 2024
 */
@Data
public class FrequencyDTO {

    private String name;

    private String description;

    private String value;

    private String category;

    private String type;

    private Integer duration;

    private String period;

    private String riskLevel;

    private String title;

    private Integer displayOrder;    
}
