package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.Date;

/**
 * <p>
 *      This class used to get patient status.
 * </p>
 *
 * @author - Gopinath
 */
@Data
public class PatientStatusDTO {

    private String id;

    private NcdPatientStatus ncdPatientStatus;

    private MentalHealthStatus mentalHealthStatus;

    private MentalHealthStatus substanceUseStatus;

    private String memberReference;

    private ProvenanceDTO provenance;

    private String patientReference;

    private Date createdAt;

    private String encounterReference;

    private String patientVisitId;
}
