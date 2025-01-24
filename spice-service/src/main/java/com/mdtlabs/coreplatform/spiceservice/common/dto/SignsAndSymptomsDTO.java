package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for SignsAndSymptoms Details .
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Apr 08, 2024.
 */
@Data
public class SignsAndSymptomsDTO {

    private List<String> symptoms;

    private String otherConcerningSymptoms;

}
