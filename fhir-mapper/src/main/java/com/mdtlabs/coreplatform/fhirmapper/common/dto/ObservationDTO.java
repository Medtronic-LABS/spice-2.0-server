package com.mdtlabs.coreplatform.fhirmapper.common.dto;

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

    private Double height;

    private BirthHistoryDTO birthHistoryDTO;

    private String type;

    private Double systolic;

    private int numberValue;

    private Date dateValue;

    private String stringValue;

    private Double pulse;

    private Double diastolic;

    private Boolean booleanValue;

    EncounterDetailsDTO encounter;

}


