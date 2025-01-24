package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for GeneralDangerSigns ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class GeneralDangerSigns {

    private Boolean unableToDrinkOrBreastfeed;

    private Boolean vomitingEverything;

    private Boolean historyOfConvulsion;

    private Boolean convulsingNow;

    private Boolean lethargicOrUnconscious;

}
