package com.mdtlabs.coreplatform.cqlservice.model.dto;

import lombok.Data;

/**
 * <p>
 * This is the DTO class for ANC (Antenatal Care) results.
 * It contains the necessary fields to store and transfer data related to ANC results.
 * </p>
 *
 * @author Vishwaeaswaran M on May 28, 2024.
 */
@Data
public class AncResultDTO {

    private String memberId;

    private String patientId;

    private String smartAncContactDetails;
}
