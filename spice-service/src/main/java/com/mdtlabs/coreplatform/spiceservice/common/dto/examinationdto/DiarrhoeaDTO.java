package com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto;

import java.util.List;

import lombok.Data;

/**
 * This is a Request DTO class for diarrhoea ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class DiarrhoeaDTO {

    private String timePeriod;

    private Boolean bloodInStool;

    private Boolean movementOnStimulation;

    private Boolean noMovementOnStimulation;

    private Boolean restlessOrIrritable;

    private Boolean sunkenEyes;

    private String skinPinch;

    private Boolean hasDiarrhoea;

    private Boolean bloodyDiarrhoea;

    private List<String> signs;

}
