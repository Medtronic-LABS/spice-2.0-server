package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * This DTO class for Mental health details.
 *
 * @author Niraimathi S created on Sept 06, 2024
 */
@Data
public class MentalHealthDetailsDTO {
    
    private double answerId;

    private int score;

    private double questionId;

    private String answer;

    private String question;

    private double displayOrder;
}
