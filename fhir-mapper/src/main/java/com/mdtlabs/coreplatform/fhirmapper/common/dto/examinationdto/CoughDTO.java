package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for cough ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class CoughDTO {

    private Boolean coughOrDIfficultBreathing;

    private Long noOfDays;

    private Boolean chestIndrawing;

    private Boolean stridor;
}
