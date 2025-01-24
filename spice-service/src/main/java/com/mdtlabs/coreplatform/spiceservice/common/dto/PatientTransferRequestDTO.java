package com.mdtlabs.coreplatform.spiceservice.common.dto;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;

/**
 * This DTO class handles the patient transfer requests
 *
 * @author Tamilarasi Shanmugasundaram A created on Oct 07, 2024
 */
@Data
public class PatientTransferRequestDTO {
    private String patientReference;

    private Long transferTo;

    private Long transferSite;

    private Long oldSite;

    private String transferReason;

    private String memberReference;
}