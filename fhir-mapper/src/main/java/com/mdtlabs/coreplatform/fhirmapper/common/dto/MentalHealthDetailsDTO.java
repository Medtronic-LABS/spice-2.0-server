package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for mental health details.
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
