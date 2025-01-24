package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.List;

/**
 * <p>
 * This is a DTO class for Neonate Details.
 * </p>
 *
 * @author Nanthinee sugumar Created on April 24, 2024.
 */
@Data
public class NeonateDTO {

    private String id;

    private String neonateOutcome;

    private String gender;

    private Double birthWeight;

    private String stateOfBaby;

    private List<String> signs;

    private Integer gestationalAge;

    private EncounterDetailsDTO encounter;

    private int visitNumber;

    private String patientStatus;

    private APGARScoreDTO apgarScoreOneMinuteDTO;

    private APGARScoreDTO apgarScoreFiveMinuteDTO;

    private APGARScoreDTO apgarScoreTenMinuteDTO;

    private int total;

}
