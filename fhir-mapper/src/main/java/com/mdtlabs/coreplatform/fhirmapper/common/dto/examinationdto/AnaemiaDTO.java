package com.mdtlabs.coreplatform.fhirmapper.common.dto.examinationdto;


import java.util.List;

import lombok.Data;

/**
 * This is a Request DTO class for anaemia ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class AnaemiaDTO {

    private Boolean appetiteTest;

    private List<String> signs;

}
