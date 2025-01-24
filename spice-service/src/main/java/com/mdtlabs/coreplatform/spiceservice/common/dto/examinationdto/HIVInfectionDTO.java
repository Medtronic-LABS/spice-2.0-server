package com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for HIV infection ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class HIVInfectionDTO {

    private Boolean hasPositiveVirologicalTestForInfant;

    private Boolean isMotherPostiveAndChildNegative;

    private Boolean hasPositiveAntibodyTestForInfant;

    private Boolean isMotherPostiveAndInfantNotTested;

    private Boolean hasNegativeForMotherAndChild;

}
