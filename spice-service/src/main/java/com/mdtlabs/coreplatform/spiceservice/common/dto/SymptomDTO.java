package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Symptom.
 * </p>
 *
 * @author Gopinath Created on Aug 21, 2024.
 */
@Data
public class SymptomDTO {

    private Long id;

    private String name;

    private String otherSymptom;
}
