package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * This DTO class for PNC mother observation.
 *
 * @author Karthick M created on Mar 11, 2024
 */
@Data
public class PncMotherDTO {

    private List<String> pncMotherSigns;

    private String otherSigns;

    private Date dateOfDelivery;

    private String noOfNeonates;

    private String neonatePatientId;

    private String neonatePatientReferenceId;

    private Boolean fatherPresent;

    private Boolean exclusivelyBreastfeeding;

    private List<DiagnosisDTO.DiseaseDTO> diagnosis;

    private Boolean sleepsUnderBedNet;

    private Boolean chlorhexidine;

    private long visitNo;
}
