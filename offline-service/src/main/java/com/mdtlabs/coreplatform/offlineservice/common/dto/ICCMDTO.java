package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for ICCM Details.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class ICCMDTO {

    private GeneralDangerSignsDTO generalDangerSigns;

    private NutritionalStatusDTO nutritionalStatusDetails;

    private DiarrhoeaDTO diarrhoea;

    private FeverDTO fever;

    private CoughDTO cough;

}
