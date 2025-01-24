package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class handles the physical exam details
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 14, 2024
 */
@Data
public class PhysicalExamDTO {

    private List<String> physicalExaminations;

    private String physicalExaminationsNote;
}
