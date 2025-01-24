package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for TB Details.
 * <p>
 *
 * @author jeyaharini.a Created on Feb 26, 2024.
 */

@Data
public class TbDTO {

    private Long id;

    private Long householdNo;

    private Long patientId;

    private String name;

    private Long villageId;

    private boolean isHasCough;

    private boolean isCoughLastedThan2Weeks;

    private boolean isDrenchingSweats;

    private boolean isHasFever;

    private boolean isWeightLoss;

    private String houseHoldUUID;

}
