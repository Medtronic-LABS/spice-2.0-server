package com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for very severe disease.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class VerySevereDiseaseDTO {

    private Boolean stoppedFeeding;

    private Boolean convulsions;

    private Boolean severeChestIndrawing;

    private Boolean movementOnStimulation;

    private Boolean lowBodyTemperature;

    private Boolean umbilicusRedOrDrainingPus;

    private Boolean skinPustules;

}
