package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Other Symptoms Details.
 * </p>
 *
 * @author Praveen Created on April 05, 2024.
 */
@Data
public class OtherSymptomsDTO {

    private FeverDTO fever;

    private SignsAndSymptomsDTO signsAndSymptoms;

}
