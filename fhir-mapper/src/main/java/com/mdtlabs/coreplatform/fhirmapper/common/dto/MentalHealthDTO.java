package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class for Mental health details.
 *
 * @author Niramathi S created on Sept 06, 2024
 */
@Data
public class MentalHealthDTO {

    private int score;
    
    private List<MentalHealthDetailsDTO> mentalHealthDetails;
    
    private String riskLevel;

    private int firstScore;

    private int secondScore;

    private String questionnaireId;

    private String encounterId;
}
