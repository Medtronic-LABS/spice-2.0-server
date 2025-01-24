package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * <p>
 *  DTO class for Patient Nutrition Lifestyle.
 *  This class represents the nutritional and lifestyle information of a patient.
 *  It includes details such as the patient's lifestyle habits, assessment details,
 *  references to related entities, and notes from clinicians.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@Data
public class PatientNutritionLifestyleUpdateDTO {

    private List<PatientNutritionLifestyle> lifestyles;

    private String patientReference;

    private String referredBy;

    private Date referredDate;

    private String assessedBy;

    private Date assessedDate;

    private boolean isViewed;

    private String tenantId;

    private ProvenanceDTO provenance;

    private String memberReference;

    private Boolean isNutritionist;

    private String patientVisitId;
}
