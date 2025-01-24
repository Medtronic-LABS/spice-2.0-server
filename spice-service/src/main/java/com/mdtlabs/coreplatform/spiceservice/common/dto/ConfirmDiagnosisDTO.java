package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Diagnosis Details.
 * </p>
 *
 * @author Divya Created on Sep 09, 2024.
 */
@Data
public class ConfirmDiagnosisDTO {

    private String type;
    
    private String patientReference;

    private String memberReference;

    private String nationalId;

    List<DiagnosisDTO> confirmDiagnosis;

    private String diagnosisNotes;

    private ProvenanceDTO provenanceDTO;

    private Boolean isConfirmDiagnosis;

    private List<Map<String, String>> diagnosis;

    private List<String> mentalHealthLevels;

}