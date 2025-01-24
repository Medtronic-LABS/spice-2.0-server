package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * This DTO class for frequency response details.
 *
 * @author Gokul A created on Nov 13, 2024
 */
@Data
public class AssessmentTreatmentPlanDTO {

    private String carePlanId;

    private List<TreatmentPlanFrequencyDTO> treatmentPlan = new ArrayList<>();
}
