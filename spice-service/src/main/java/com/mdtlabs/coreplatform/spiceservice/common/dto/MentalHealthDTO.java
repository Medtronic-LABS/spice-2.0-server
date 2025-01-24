package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class for mental health details.
 *
 * @author Niraimathi S created on Sept 06, 2024
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
