package com.mdtlabs.coreplatform.fhirmapper.common.dto.pregnancy;

import lombok.Data;

/**
 * This DTO class for Pregnancy Symptom details.
 *
 * @author Gokul A created on Aug 14, 2024
 */
@Data
public class PregnancySymptomDTO {
    private String name;
    private double id;
    private String value;
    private String otherSymptom;
}
