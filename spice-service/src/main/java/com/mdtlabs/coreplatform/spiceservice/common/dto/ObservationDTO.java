package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Observation Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on Mar 27, 2024.
 */
@Data
public class ObservationDTO {

    private String patientReference;

    private Double weight;

    private String type;

    private Double systolic;

    private int numberValue;

    private BirthHistoryDTO birthHistoryDTO;

    private Date dateValue;

    private Double pulse;

    private Double diastolic;

    EncounterDetailsDTO encounter;

}


