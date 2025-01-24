package com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto;

import java.util.List;

import lombok.Data;

/**
 * This is a Request DTO class for fever ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class FeverDTO {

    private Boolean hasFever;

    private Long noOfDays;

    private String isMotherHasFever;

    private String microscopyResult;

    private List<String> signs;
}
