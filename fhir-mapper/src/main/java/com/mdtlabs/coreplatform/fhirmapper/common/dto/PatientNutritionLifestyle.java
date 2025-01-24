package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.Set;

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
public class PatientNutritionLifestyle {

    private String id;

    private Set<String> lifestyles;

    private String lifestyleAssessment;

    private String patientReference;

    private String visitId;

    private String referredBy;

    private String referredByDisplay;

    private Date referredDate;

    private String assessedBy;

    private String assessedByDisplay;

    private Date assessedDate;

	private String clinicianNote;

    private String otherNote;

    private boolean isViewed;

    private String tenantId;

    private ProvenanceDTO provenance;

    private String memberReference;

    private Boolean isNutritionist;
}
