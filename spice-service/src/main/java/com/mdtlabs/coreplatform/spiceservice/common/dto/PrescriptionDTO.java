package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

/**
 * <p>
 * This is a DTO class for Prescription Details.
 * </p>
 *
 * @author Yogeshwaran Mohan Created on Mar 27, 2024.
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class PrescriptionDTO {

    private Long prescribedDays;

    private String medicationName;

    private String medicationId;

    private int frequency;

    private String frequencyName;

    private Date prescribedSince;

    private Date endDate;

    private String prescriptionId;

    private String discontinuedReason;

    private Date discontinuedDate;

    private String encounterId;

    private Boolean isActive;

    private Boolean isDeleted;

    private Code codeDetails;

    // africa
    private String classificationName;

    private String brandName;

    private String dosageFrequencyName;

    private String dosageUnitName;

    private String dosageUnitValue;

    private String instructionNote;

    private String dosageFormName;

    private int prescriptionRemainingDays;

    private int prescriptionFilledDays;

    private int dispenseRemainingDays;

    private Date lastReFillDate;

    private String reason;

    @Data
    public static class Code {

        private String code;

        private String url;
    }

}