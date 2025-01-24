package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.springframework.web.multipart.MultipartFile;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Prescription request Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class PrescriptionRequestDTO {

    private EncounterDetailsDTO encounter;

    private List<PrescriptionDTO> prescriptions;

    private String signature;

    private MultipartFile signatureFile;

    private String prescriptionId;

    private String discontinuedReason;

    private ProvenanceDTO provenance;

}
