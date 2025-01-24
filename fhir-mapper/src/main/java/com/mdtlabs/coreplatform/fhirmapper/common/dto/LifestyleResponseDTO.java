package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This is DTO class for lifestyle response.
 * 
 * @author Karthick M on Nov 15
 */
@Data
public class LifestyleResponseDTO {
    
    private String comments;

    private String lifestyle;

    private String lifestyleType;

    private String lifestyleAnswer;

    private String value;

}
