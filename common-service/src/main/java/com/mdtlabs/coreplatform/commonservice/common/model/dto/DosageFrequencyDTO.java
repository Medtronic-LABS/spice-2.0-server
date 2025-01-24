package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

/**
 * <p>
 * This class is an DTO class for dosage frequency entity.
 * </p>
 * 
 * @author Yogeshwaran Mohan created on May 7, 2024
 *
 */
@Data
public class DosageFrequencyDTO {

    private long id;
    
    private String name;
    
    private String description;

    private int displayOrder;

    private Long quantity;

}
