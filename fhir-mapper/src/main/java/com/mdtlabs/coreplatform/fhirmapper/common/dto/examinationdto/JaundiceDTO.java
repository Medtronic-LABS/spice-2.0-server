package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;

import lombok.Data;

/**
 * This is a Request DTO class for jaundice under five years ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class JaundiceDTO {

    private Boolean yellowSkinLessThan24hrs;

    private Boolean yellowPalmsAndSoles;

    private Boolean jaundiceAppearing;

    private Boolean noJaundice;

    private Boolean solesNotYellow;

}
