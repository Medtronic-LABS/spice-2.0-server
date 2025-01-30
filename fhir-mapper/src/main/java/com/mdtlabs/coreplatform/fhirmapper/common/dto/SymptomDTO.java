package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for Symptom.
 *
 * @author Gopinath R created on Aug 21, 2024
 */
@Data
public class SymptomDTO {

    private String id;

    private String name;

    private String otherSymptom;

    private String type;

    private String value;
}
