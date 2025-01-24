package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Prescription Details.
 * </p>
 *
 * @author Yogeshwaran Mohan Created on Mar 27, 2024.
 */
@Data
public class PrescriptionDTO {

    public PrescriptionDTO() {}
    
    public PrescriptionDTO(String medicationName, String dosageUnitValue,
                    String dosageUnitName, String dosageFrequencyName,
                    String dosageFormName) {
        this.medicationName = medicationName;
        this.dosageUnitValue = dosageUnitValue;
        this.dosageUnitName = dosageUnitName;
        this.dosageFrequencyName = dosageFrequencyName;
        this.dosageFormName = dosageFormName;
    }

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

    private String signature;

    private Boolean isActive;

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

    private Boolean isDeleted;
}