package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Obstetric Examination Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 20, 2024.
 */
@Data
public class ObstetricExaminationDTO {

    private Long id;

    private String name;

    private Integer displayOrder;

    private String type;

    private String value;
}
