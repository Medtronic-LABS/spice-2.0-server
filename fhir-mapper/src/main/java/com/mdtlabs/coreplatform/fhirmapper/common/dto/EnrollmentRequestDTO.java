package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * EnrollmentRequestDTO to handle request for Enrollment.
 * 
 * @author Karthick M created on Aug 05, 2024
 */
@Data
public class EnrollmentRequestDTO {

    private BioDataDTO bioData;

    private BioMetricsDTO bioMetrics;

    private Long healthFacilityId;

    private String healthFacilityFhirId;

    private Long tenantId;
    
    private String qrCode;

    private String memberId;

    private String virtualId;

    private ProvenanceDTO provenance;

    private Date dateOfEnrollment = new Date();

    private String patientReference;

    private String memberReference;

    private String id;

    private String patientId;
}
