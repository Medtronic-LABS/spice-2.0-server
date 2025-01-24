package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Prescription History Details.
 * </p>
 *
 * @author Karthick M Created on May 07, 2024.
 */
@Data
public class PrescriptionHistoryDTO {

    private String encounterId;

    private Date dateOfReview;

    private String patientReference;

    private List<Map<String, Object>> history = new ArrayList<>();

    private List<PrescriptionDTO> prescriptions = new ArrayList<>();

}
