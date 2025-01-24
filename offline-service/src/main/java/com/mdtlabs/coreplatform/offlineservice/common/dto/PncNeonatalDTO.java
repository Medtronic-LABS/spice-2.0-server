package com.mdtlabs.coreplatform.offlineservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This DTO class for PNC Neonatal observation.
 *
 * @author Praveen created on Mar 26, 2024
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
