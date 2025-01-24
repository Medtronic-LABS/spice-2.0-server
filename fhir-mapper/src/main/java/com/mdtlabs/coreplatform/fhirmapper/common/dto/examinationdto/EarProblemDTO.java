package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for ear problem ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class EarProblemDTO {

    private Boolean hasEarPain;

    private Long noOfDays;

    private String earDischarge;

}
