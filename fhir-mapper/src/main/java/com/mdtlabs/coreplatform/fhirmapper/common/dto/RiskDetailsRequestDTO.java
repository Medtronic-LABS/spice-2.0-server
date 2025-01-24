package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.HashMap;
import java.util.Map;

/**
 * This DTO class for risk details request to
 * create risk observations.
 *
 * @author Gokul A created on Aug 20, 2024
 */
@Data
public class RiskDetailsRequestDTO {
    private Map<String, String> riskDetails = new HashMap<>();
}
