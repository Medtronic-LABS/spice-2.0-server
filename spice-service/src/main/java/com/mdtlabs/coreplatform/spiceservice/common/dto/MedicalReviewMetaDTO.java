package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * This class is a Data Transfer object for Medical review meta dto.
 * 
 * @author Karthick M
 *
 */
@Data
public class MedicalReviewMetaDTO implements Serializable {
    
    private Long id;

    private String name;
    
    private String value;

    private boolean isOther;

    private String comments;

    private MedicalReviewMetaDTO answer;
    
}
