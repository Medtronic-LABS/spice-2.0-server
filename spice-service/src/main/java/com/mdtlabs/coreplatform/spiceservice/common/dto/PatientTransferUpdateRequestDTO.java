package com.mdtlabs.coreplatform.spiceservice.common.dto;


import jakarta.validation.constraints.NotNull;
import lombok.Data;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;

/**
 * This DTO class handles the patient transfer update requests
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 23, 2024
 */
@Data
public class PatientTransferUpdateRequestDTO {

    private Long id;

    private PatientTransferStatus transferStatus;

    private String rejectReason;

    private ProvenanceDTO provenance;

    private String memberReference;
}