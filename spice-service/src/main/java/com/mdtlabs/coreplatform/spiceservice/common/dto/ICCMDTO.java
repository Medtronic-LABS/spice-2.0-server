package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for ICCM Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class ICCMDTO {

    private GeneralDangerSignsDTO generalDangerSigns;

    private NutritionalStatusDTO nutritionalStatusDetails;

    private DiarrhoeaDTO diarrhoea;

    private FeverDTO fever;

    private CoughDTO cough;

}
