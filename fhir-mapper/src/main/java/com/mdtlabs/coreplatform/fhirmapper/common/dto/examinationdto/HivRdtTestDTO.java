package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for HIV rdt test ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class HivRdtTestDTO {

    private String mother;

    private String child;
}
