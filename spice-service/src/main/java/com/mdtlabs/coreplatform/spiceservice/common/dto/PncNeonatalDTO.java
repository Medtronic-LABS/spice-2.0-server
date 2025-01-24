package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class for PNC Neonatal observation.
 *
 * @author Karthick M created on Mar 11, 2024
 */
@Data
public class PncNeonatalDTO {

    private Boolean lowBirthWeight;

    private Boolean deathOfNewborn;

    private Boolean newbornReferredToSBCU;

    private Integer gestationalAge;

    private List<String> pncNeonatalSigns;

    private String otherSigns;

}
